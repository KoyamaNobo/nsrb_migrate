       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR200I.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    振替伝票入力                                               *
      *                            --- 90/01/17 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       INPUT-OUTPUT               SECTION.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  WKSP                PIC  X(40) VALUE SPACE.
       77  WKNSP               PIC  N(10) VALUE SPACE.
       77  WKZERO              PIC  9(10) VALUE ZERO.
       01  CRT-WK1.
           02  CRT-ITEM    OCCURS  5.
             03  C-SIWAKE.
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
       01  CRT-WK2.
           02  C-ACT           PIC  9(01).
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
       01  SAVE-AREA.
           02  WKONYMD.
             03  WKONYY        PIC  9(04).
             03  WKONYYL  REDEFINES WKONYY.
               04  WKONYY1     PIC  9(02).
               04  WKONYY2     PIC  9(02).
             03  WKONMM        PIC  9(02).
             03  WKONDD        PIC  9(02).
           02  WKONYMDR REDEFINES WKONYMD   PIC  9(08).
           02  WGESYMD.
             03  WGESYY        PIC  9(04).
             03  WGESMM        PIC  9(02).
             03  WGESDD        PIC  9(02).
           02  WGESYMDR REDEFINES WGESYMD   PIC  9(08).
           02  WGEMYMD.
             03  WGEMYY        PIC  9(04).
             03  WGEMMM        PIC  9(02).
             03  WGEMDD        PIC  9(02).
           02  WGEMYMDR REDEFINES WGEMYMD   PIC  9(08).
           02  WSUB5           PIC  9(01).
           02  W-KRCDM         PIC  9(04).
           02  W-KSCDM         PIC  9(04).
      *********
       COPY LWMSG_PR.
      *********
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
      *
       01  SDH_PR201I.
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
       COPY SIWAID.
       COPY LNSDNO.
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
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-ACT                   PIC 9(01).
           02  ACP-CRDATE  .
               03  ACP-CRYY              PIC 9(02).
               03  ACP-CRMM              PIC 9(02).
               03  ACP-CRDD              PIC 9(02).
           02  ACP-DNNO                  PIC 9(06).
           02  ACP-TKCD                  PIC 9(05).
           02  ACP-NAMEN                 PIC N(10).
           02  ACP-OKC                   PIC X(01).
       01  ACP-AREA1.
           02  ACP-KRCDM                 PIC 9(04).
           02  ACP-KRCDS                 PIC 9(04).
           02  ACP-KRSECT                PIC 9(04).
           02  ACP-KRKIN                 PIC S9(10).
           02  ACP-KRTAX                 PIC X(01).
           02  ACP-KSCDM                 PIC 9(04).
           02  ACP-KSCDS                 PIC 9(04).
           02  ACP-KSSECT                PIC 9(04).
           02  ACP-KSKIN                 PIC S9(10).
           02  ACP-KSTAX                 PIC X(01).
           02  ACP-TEKICD                PIC 9(03).
           02  ACP-TEKI                  PIC N(20).
       01  DSP-AREA.
           02  DSP-SYSDATE .
               03  FILLER                PIC 9(02).
               03  FILLER                PIC 9(02).
               03  FILLER                PIC 9(02).
           02  DSP-TKNM                  PIC N(10).
       01  DSP-AREA1.
           02  DSP-KRCDMN                PIC N(10).
           02  DSP-KRCDSN                PIC N(10).
           02  DSP-KRKIN                 PIC ZZZZZZZZZZ-.
           02  DSP-KSCDMN                PIC N(10).
           02  DSP-KSCDSN                PIC N(10).
           02  DSP-KSKIN                 PIC ZZZZZZZZZZ-.
      ***
       01  DSP-AREA2.
           02  DSP-KRTOT                 PIC ZZZZZZZZZZ-.
           02  DSP-KSTOT                 PIC ZZZZZZZZZZ-.
      ***
       01  CLE-AREA.
           02  CLE-CRDATE  .
               03  CLE-CRYY              PIC Z(02).
               03  CLE-CRMM              PIC Z(02).
               03  CLE-CRDD              PIC Z(02).
           02  CLE-DNNO                  PIC Z(06).
           02  CLE-TKCD                  PIC Z(05).
      ***
       01  CLE-AREA1.
           02  CLE-KRSECT                PIC Z(04).
           02  CLE-KSSECT                PIC Z(04).
       01  SP-AREA.
           02  CLE-SP20-KR1              PIC X(20).
           02  CLE-SP20-KR2              PIC X(20).
           02  CLE-SP20-KS1              PIC X(20).
           02  CLE-SP20-KS2              PIC X(20).
           02  CLE-SP40                  PIC X(40).
       01  MSG-AREA   .
           02  MG-01.
               03  FILLER   PIC N(20) VALUE
                   "後日取消　変更不可".
           02  MG-02.
               03  FILLER   PIC N(20) VALUE
                   "後日取消　処理済み".
           02  MG-03.
               03  FILLER   PIC N(20) VALUE
                   "金額アンマッチ　入力不可".
           02  MG-04.
               03  FILLER   PIC N(20) VALUE
                   "金額バランスエラー　処理不可".
           02  MG-07.
               03  FILLER   PIC N(20) VALUE
                   "自動振替済　消費税赤伝票入力要".
           02  MG-11.
               03  FILLER   PIC N(20) VALUE
                   "税区分エラー　処理不可　　　　　　　　　".
           02  MG-12.
               03  FILLER   PIC N(20) VALUE
                   "取引先マスタ　ＲＥＷＲＩＴＥエラー　　　".
      *********
       COPY LSMSG_PR.
       COPY LIBSCR.
      **********
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
            "ACP-AREA" " " "0" "0" "39" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "56" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE C-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CRDATE" " " "2" "0" "6" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CRYY" "9" "2" "7" "2" " " "ACP-CRDATE" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CRYY" BY REFERENCE CRYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CRMM" "9" "2" "10" "2" "ACP-CRYY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CRMM" BY REFERENCE CRMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CRDD" "9" "2" "13" "2" "ACP-CRMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CRDD" BY REFERENCE CRDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DNNO" "9" "2" "25" "6" "ACP-CRDATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-DNNO" BY REFERENCE C-DNNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKCD" "9" "2" "42" "5" "ACP-DNNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TKCD" BY REFERENCE C-TKCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NAMEN" "N" "2" "48" "20" "ACP-TKCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NAMEN" BY REFERENCE C-NAMEN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "X" "24" "77" "1" "ACP-NAMEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE C-OKC "1" "0" RETURNING RESU.
      *ACP-AREA1
       CALL "SD_Init" USING 
            "ACP-AREA1" " " "0" "0" "89" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KRCDM" "9" "LIN" "2" "4" " " "ACP-AREA1"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KRCDM" BY REFERENCE C-KRCDM(1) "4" "1"
            BY REFERENCE I 167 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KRCDS" "9" "LIN1" "2" "4" "ACP-KRCDM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KRCDS" BY REFERENCE C-KRCDS(1) "4" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KRSECT" "9" "LIN" "23" "4" "ACP-KRCDS" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KRSECT" BY REFERENCE C-KRSECT(1) "4" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KRKIN" "S9" "LIN" "28" "10" "ACP-KRSECT" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KRKIN" BY REFERENCE C-KRKIN(1) "10" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KRTAX" "X" "LIN1" "37" "1" "ACP-KRKIN" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KRTAX" BY REFERENCE C-KRTAX(1) "1" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KSCDM" "9" "LIN" "40" "4" "ACP-KRTAX" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KSCDM" BY REFERENCE C-KSCDM(1) "4" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KSCDS" "9" "LIN1" "40" "4" "ACP-KSCDM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KSCDS" BY REFERENCE C-KSCDS(1) "4" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KSSECT" "9" "LIN" "61" "4" "ACP-KSCDS" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KSSECT" BY REFERENCE C-KSSECT(1) "4" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KSKIN" "S9" "LIN" "66" "10" "ACP-KSSECT" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KSKIN" BY REFERENCE C-KSKIN(1) "10" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KSTAX" "X" "LIN1" "75" "1" "ACP-KSKIN" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KSTAX" BY REFERENCE C-KSTAX(1) "1" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TEKICD" "9" "LIN2" "40" "3" "ACP-KSTAX" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TEKICD" BY REFERENCE C-TEKICD(1) "3" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TEKI" "N" "LIN2" "40" "40" "ACP-TEKICD" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TEKI" BY REFERENCE C-TEKI(1) "40" "1"
            BY REFERENCE I 187 RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SYSDATE" " " "1" "0" "6" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SYSDATE" "9" "1" "72" "2" " " "DSP-SYSDATE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-SYSDATE" BY REFERENCE WKONYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-SYSDATE" "9" "1" "75" "2" "01DSP-SYSDATE" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-SYSDATE" BY REFERENCE WKONMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-SYSDATE" "9" "1" "78" "2" "02DSP-SYSDATE" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-SYSDATE" BY REFERENCE WKONDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TKNM" "N" "2" "48" "20" "DSP-SYSDATE" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TKNM" BY REFERENCE C-NAMEN "20" "0" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KRCDMN" "N" "LIN" "2" "20" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KRCDMN" BY REFERENCE C-KR-CDMN(1) "20" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KRCDSN" "N" "LIN1" "2" "20" "DSP-KRCDMN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KRCDSN" BY REFERENCE C-KR-CDSN(1) "20" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KRKIN" "ZZZZZZZZZZ-" "LIN" "28" "11" "DSP-KRCDSN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KRKIN" BY REFERENCE C-KRKIN(1) "10" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KSCDMN" "N" "LIN" "40" "20" "DSP-KRKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KSCDMN" BY REFERENCE C-KS-CDMN(1) "20" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KSCDSN" "N" "LIN1" "40" "20" "DSP-KSCDMN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KSCDSN" BY REFERENCE C-KS-CDSN(1) "20" "1"
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KSKIN" "ZZZZZZZZZZ-" "LIN" "66" "11" "DSP-KSCDSN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KSKIN" BY REFERENCE C-KSKIN(1) "10" "1"
            BY REFERENCE I 187 RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING 
            "DSP-AREA2" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KRTOT" "ZZZZZZZZZZ-" "21" "28" "11" " " "DSP-AREA2"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KRTOT" BY REFERENCE C-KRTOT "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KSTOT" "ZZZZZZZZZZ-" "21" "66" "11" "DSP-KRTOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KSTOT" BY REFERENCE C-KSTOT "10" "0" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-CRDATE" " " "2" "0" "6" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-CRYY" "Z" "2" "7" "2" " " "CLE-CRDATE" RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-CRYY" BY REFERENCE CRYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-CRMM" "Z" "2" "10" "2" "CLE-CRYY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-CRMM" BY REFERENCE CRMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-CRDD" "Z" "2" "13" "2" "CLE-CRMM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-CRDD" BY REFERENCE CRDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-DNNO" "Z" "2" "25" "6" "CLE-CRDATE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-DNNO" BY REFERENCE C-DNNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-TKCD" "Z" "2" "42" "5" "CLE-DNNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-TKCD" BY REFERENCE C-TKCD "5" "0" RETURNING RESU.
      *CLE-AREA1
       CALL "SD_Init" USING 
            "CLE-AREA1" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KRSECT" "Z" "LIN" "23" "4" " " "CLE-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KRSECT" BY REFERENCE C-KRSECT(1) "4" "1" 
            BY REFERENCE I 187 RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KSSECT" "Z" "LIN" "61" "4" "CLE-KRSECT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KSSECT" BY REFERENCE C-KSSECT(1) "4" "1" 
            BY REFERENCE I 187 RETURNING RESU.
      *SP-AREA
       CALL "SD_Init" USING 
            "SP-AREA" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SP20-KR1" "X" "LIN" "2" "20" " " "SP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-SP20-KR1" BY REFERENCE WKSP "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SP20-KR2" "X" "LIN1" "2" "20" "CLE-SP20-KR1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-SP20-KR2" BY REFERENCE WKSP "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SP20-KS1" "X" "LIN" "40" "20" "CLE-SP20-KR2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-SP20-KS1" BY REFERENCE WKSP "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SP20-KS2" "X" "LIN1" "40" "20" "CLE-SP20-KS1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-SP20-KS2" BY REFERENCE WKSP "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SP40" "X" "LIN2" "40" "40" "CLE-SP20-KS2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-SP40" BY REFERENCE WKSP "40" "0" RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING 
            "MSG-AREA" " " "24" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-01" " " "24" "0" "40" " " "MSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-01" "N" "24" "2" "40" " " "MG-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-02" " " "24" "0" "40" "MG-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-02" "N" "24" "2" "40" " " "MG-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-03" " " "24" "0" "40" "MG-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-03" "N" "24" "2" "40" " " "MG-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-04" " " "24" "0" "40" "MG-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-04" "N" "24" "2" "40" " " "MG-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-07" " " "24" "0" "40" "MG-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-07" "N" "24" "2" "40" " " "MG-07" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-11" " " "24" "0" "40" "MG-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-11" "N" "24" "2" "40" " " "MG-11" RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-12" " " "24" "0" "40" "MG-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MG-12" "N" "24" "2" "40" " " "MG-12" RETURNING RESU.
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
           PERFORM DISP-000 THRU DISP-EX.
       MR015.
           PERFORM DATE-RTN THRU DATE-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR020.
           PERFORM DNNO-RTN THRU DNNO-EX.
           IF  ESTAT = "09"
               GO TO MR015
           END-IF.
           MOVE 0     TO ERR-SW.
           IF  C-ACT NOT = 1
               PERFORM SDIG-RTN THRU SDIG-EX
               IF  ERR-SW = 1
                   GO TO MR020
               END-IF
           END-IF.
           IF  C-ACT = 4
               PERFORM SDHG-RTN THRU SDHG-EX
               IF  ERR-SW = 1
                   GO TO MR020
               END-IF
           END-IF.
           PERFORM GET-RTN THRU GET-EX.
           PERFORM DISP-RTN THRU DISP-EX.
           PERFORM GOKEI-RTN THRU GOKEI-EX.
           IF  C-ACT = 3 OR 4
               GO TO MR400
           END-IF.
       MR025.
           PERFORM TK-RTN THRU TK-EX.
           IF  ESTAT = "09"
               GO TO MR020
           END-IF.
           MOVE 6     TO LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                          RETURNING RESU.
           MOVE 7     TO LIN1.
           CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                          RETURNING RESU.
           MOVE 8     TO LIN2.
           CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                          RETURNING RESU.
           MOVE 1     TO I.
       MR030.
           PERFORM KRCDM-RTN THRU KRCDM-EX.
           IF  ESTAT = "04"
               MOVE I     TO SV-I
               MOVE LIN   TO SV-LIN
               CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                           RETURNING RESU
               MOVE LIN1  TO SV-LIN1
               CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                           RETURNING RESU
               MOVE LIN2  TO SV-LIN2
               CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                           RETURNING RESU
               PERFORM CRE-RTN THRU CRE-EX
               MOVE SV-I     TO I
               PERFORM DISP-100 THRU DISP-EX
               GO TO MR300
           END-IF.
           IF  ESTAT = "09"
               IF  I NOT = 1
                   SUBTRACT 3     FROM LIN LIN1 LIN2
                   SUBTRACT 1     FROM I
                   GO TO MR030
               ELSE
                   GO TO MR025
               END-IF
           END-IF.
       MR035.
           PERFORM KRCDS-RTN THRU KRCDS-EX.
           IF  ESTAT = "09"
               GO TO MR030
           END-IF.
       MR040.
           PERFORM KRSECT-RTN THRU KRSECT-EX.
           IF  ESTAT = "09"
               GO TO MR035
           END-IF.
       MR050.
           PERFORM KRKIN-RTN THRU KRKIN-EX.
           IF  ESTAT = "09"
               GO TO MR040
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
       MR110.
           PERFORM KSSECT-RTN THRU KSSECT-EX.
           IF  ESTAT = "09"
               GO TO MR105
           END-IF.
       MR120.
           PERFORM KSKIN-RTN THRU KSKIN-EX.
           IF  ESTAT = "09"
               GO TO MR110
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
               CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                           RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                           RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                           RETURNING RESU
               GO TO MR030
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
               CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                           RETURNING RESU
               MOVE SV-LIN1  TO LIN1
               CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                           RETURNING RESU
               MOVE SV-LIN2  TO LIN2
               CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                           RETURNING RESU
               GO TO MR030
           END-IF.
       MR400.
           PERFORM OKC-RTN THRU OKC-EX.
           IF  ESTAT = "09"
               IF  C-ACT = 3 OR 4
                   GO TO MR020
               ELSE
                   MOVE SV-I     TO I
                   MOVE SV-LIN   TO LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                            RETURNING RESU
                   MOVE SV-LIN1  TO LIN1
                   CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                            RETURNING RESU
                   MOVE SV-LIN2  TO LIN2
                   CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                            RETURNING RESU
                   GO TO MR030
               END-IF
           END-IF.
           IF  C-OKC = "9"
               GO TO MR500
           END-IF
      **
           IF  C-ACT = 1
               PERFORM DNOU-RTN THRU DNOU-EX
           END-IF.
           IF  C-ACT = 2 OR 3
               PERFORM SDIDU-RTN THRU SDIDU-EX
           END-IF.
           IF  C-ACT NOT = 3
               PERFORM SDIWU-RTN THRU SDIWU-EX
           END-IF.
       MR500.
           MOVE 1     TO I.
           PERFORM CRE-RTN THRU CRE-EX.
           MOVE ZERO     TO C-TKCD.
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
           GO TO MR020.
       MR999.
           PERFORM CLSE-ENT THRU CLSE-EXT.
           CALL "DB_Close".
           STOP RUN.
      ************************
       INI-RTN.
           CALL "SD_Screen_Output" USING "GR2010" RETURNING RESU.
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
           MOVE FCTL-KONYMD     TO WKONYMD.
           MOVE FCTL-GESYMD     TO WGESYMD.
           MOVE FCTL-GEMYMD     TO WGEMYMD.
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
           CALL "SD_Output" USING "DSP-SYSDATE" DSP-SYSDATE "p"
                                         RETURNING RESU.
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
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1  "SHARED" BY REFERENCE AM_IDLST "1"
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
            "I-O" SDI_PNAME1 "SHARED" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" NS-DNO_PNAME1 "SHARED" BY REFERENCE NS-DNO_IDLST "1"
            "DNO1-KEY" BY REFERENCE DNO1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           COPY LIBCPR.
       OPEN-EX.
           EXIT.
      ***
       ACT-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO ACT-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO ACT-RTN
           END-IF.
           IF  C-ACT NOT = 1 AND 2 AND 3 AND 4
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO ACT-RTN
           END-IF.
       ACT-EX.
           EXIT.
      ***
       DATE-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-CRDATE "ACP-CRDATE" " " "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           IF (CRMM < 1 OR > 12) OR
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
           IF  C-ACT = 1
               IF (CRDATE NOT < WGESYMD) AND
                  (CRDATE NOT > WGEMYMD)
                   CONTINUE
               ELSE
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO DATE-RTN
               END-IF
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
           IF  ESTAT = "09"
               GO TO DNNO-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO DNNO-000
           END-IF.
       DNNO-EX.
           EXIT.
      *********
       SDIG-RTN.
           MOVE 1     TO I.
           PERFORM CRE-RTN THRU CRE-EX.
           MOVE 1          TO ERR-SW.
           MOVE CRDATE     TO SDIYMD.
           MOVE C-DNNO     TO SDIJNO.
           MOVE ZERO       TO SDILNO.
      *           START SDI KEY NOT LESS SDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT LESS " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO TO SDIG-999
           END-IF.
       SDIG-000.
      *           READ SDI NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDIG-999
           END-IF.
           IF  CRDATE NOT = SDIYMD
               GO TO SDIG-999
           END-IF.
           IF  C-DNNO NOT = SDIJNO
               GO TO SDIG-999
           END-IF.
           IF  C-ACT = 2
               IF  SDIDEL NOT = SPACE
                   CALL "SD_Output" USING "MG-01" MG-01 "p"
                                               RETURNING RESU
                   GO TO SDIG-EX
               END-IF
           END-IF.
           IF  C-ACT = 4
               CALL "SD_Output" USING "MG-02" MG-02 "p"
                                              RETURNING RESU
               GO TO SDIG-EX
           END-IF.
           MOVE 0     TO ERR-SW.
      *
           IF  SDILNO = 1
               MOVE SDICUST     TO C-TKCD
               MOVE SDINAMEN    TO C-NAMEN
           END-IF.
           MOVE SDIKARI     TO C-KARI(SDILNO).
           MOVE SDIKASI     TO C-KASI(SDILNO).
           MOVE SDITEKICD   TO C-TEKICD(SDILNO).
           MOVE SDITEKI     TO C-TEKI(SDILNO).
           IF  C-KRCDM(SDILNO) = W-KRCDM OR W-KSCDM
               MOVE SDIETAX    TO C-KRTAX(SDILNO)
           END-IF.
           IF  C-KSCDM(SDILNO) = W-KRCDM OR W-KSCDM
               MOVE SDIETAX    TO C-KSTAX(SDILNO)
           END-IF.
           GO TO SDIG-000.
       SDIG-999.
           IF  C-ACT = 4
               MOVE 0     TO ERR-SW
           ELSE
               IF  ERR-SW = 1
                   CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                               RETURNING RESU
               END-IF
           END-IF.
       SDIG-EX.
           EXIT.
       SDHG-RTN.
           MOVE 1          TO ERR-SW.
           MOVE CRDATE     TO HTRDATE.
           MOVE C-DNNO     TO HJUNLNO.
           MOVE ZERO       TO HLINENO HDR-CR.
      *           START SDH KEY NOT LESS SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT LESS " SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO TO SDHG-999
           END-IF.
       SDHG-000.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDHG-999
           END-IF.
           IF  CRDATE NOT = HTRDATE
               GO TO SDHG-999
           END-IF.
           IF  C-DNNO NOT = HJUNLNO
               GO TO SDHG-999
           END-IF.
           MOVE 0     TO ERR-SW.
      *
           IF  HLINENO = 1
               MOVE HCUSTCD     TO C-TKCD
               MOVE HNAMEN      TO C-NAMEN
           END-IF.
           IF  HDR-CR NOT = 1
               GO TO SDHG-010
           END-IF.
           MOVE HACCNTCD     TO C-KRCDM(HLINENO).
           MOVE HHOACCNT     TO C-KRCDS(HLINENO).
           MOVE HSECTCD      TO C-KRSECT(HLINENO).
           MOVE HTAXKB       TO C-KRTAX(HLINENO).
           COMPUTE C-KRKIN(HLINENO) = HAMOUNT * -1.
           MOVE HTEG-BAN     TO C-KR-TB(HLINENO).
           IF  C-KRCDM(HLINENO) = W-KRCDM OR W-KSCDM
               MOVE HETAX      TO C-KRTAX(HLINENO)
           END-IF.
           GO TO SDHG-100.
       SDHG-010.
           MOVE HACCNTCD     TO C-KSCDM(HLINENO).
           MOVE HHOACCNT     TO C-KSCDS(HLINENO).
           MOVE HSECTCD      TO C-KSSECT(HLINENO).
           MOVE HTAXKB       TO C-KSTAX(HLINENO).
           COMPUTE C-KSKIN(HLINENO) = HAMOUNT * -1.
           MOVE HTEG-BAN     TO C-KS-TB(HLINENO).
           MOVE HTEKICD     TO C-TEKICD(HLINENO).
           IF  C-KSCDM(HLINENO) = W-KRCDM OR W-KSCDM
               MOVE HETAX      TO C-KSTAX(HLINENO)
           END-IF.
       SDHG-100.
           MOVE HTEKIYO     TO C-TEKI(HLINENO).
           IF  HCOM = 1
               CALL "SD_Output" USING "MG-07" MG-07 "p"
                                              RETURNING RESU
           END-IF.
           GO TO SDHG-000.
       SDHG-999.
           IF  ERR-SW = 1
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                              RETURNING RESU
           END-IF.
       SDHG-EX.
           EXIT.
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
           IF  C-TKCD = ZERO
               CALL "SD_Output" USING "CLE-TKCD" CLE-TKCD "p"
                                              RETURNING RESU
           ELSE
               CALL "SD_Output" USING "ACP-TKCD" ACP-TKCD "p"
                                              RETURNING RESU
           END-IF.
           CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                         RETURNING RESU.
       DISP-000.
           MOVE 1     TO I.
           MOVE 6     TO LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                          RETURNING RESU.
           MOVE 7     TO LIN1.
           CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                          RETURNING RESU.
           MOVE 8     TO LIN2.
           CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                          RETURNING RESU.
       DISP-100.
           CALL "SD_Output" USING "DSP-AREA1" DSP-AREA1 "p"
                            RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRTAX" ACP-KRTAX "p"
                            RETURNING RESU.
           CALL "SD_Output" USING "CLE-KRSECT" CLE-KRSECT "p"
                            RETURNING RESU.
           CALL "SD_Output" USING "ACP-KSTAX" ACP-KSTAX "p"
                            RETURNING RESU.
           CALL "SD_Output" USING "CLE-KSSECT" CLE-KSSECT "p"
                            RETURNING RESU.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                            RETURNING RESU.
           IF  I NOT = 5
               ADD 1     TO I
               ADD 3     TO LIN LIN1 LIN2
               CALL "SD_Arg_Match_Line" USING "LIN" "2"  LIN
                           RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN1" "2"  LIN1
                           RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN2" "2"  LIN2
                           RETURNING RESU
               GO TO DISP-100
           END-IF.
       DISP-EX.
           EXIT.
      ***
       TK-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TKCD "ACP-TKCD" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           GO TO TK-999.
       TK-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-NAMEN "ACP-NAMEN" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
      ****************** ＜　借方　＞ **********************************
       KRCDM-RTN.
           CALL "SD_Output" USING "CLE-SP20-KR1" CLE-SP20-KR1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRCDM "ACP-KRCDM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
                       CALL "SD_Output" USING "ACP-KRSECT" ACP-KRSECT
                                          "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF.
       KRSECT-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRSECT "ACP-KRSECT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           CALL "SD_Output" USING "CLE-KRSECT" CLE-KRSECT
                                        "p" RETURNING RESU.
       KRSECT-EX.
           EXIT.
       KRKIN-RTN.
           IF  C-KRCDM(I) = ZERO
               MOVE ZERO     TO C-KRKIN(I)
               GO TO KRKIN-999
           END-IF.
       KRKIN-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRKIN "ACP-KRKIN" "S9" "10"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           IF  C-DNNO    < 300000
               IF  C-KRCDM(I) = W-KRCDM OR W-KSCDM
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
      ****************** ＜　貸方　＞ **********************************
       KSCDM-RTN.
           CALL "SD_Output" USING "CLE-SP20-KS1" CLE-SP20-KS1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSCDM "ACP-KSCDM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           IF  INV-SW = 1
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
                       CALL "SD_Output" USING "ACP-KSSECT" ACP-KSSECT
                                          "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF.
       KSSECT-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSSECT "ACP-KSSECT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
       KSKIN-RTN.
           IF  C-KSCDM(I) = ZERO
               MOVE ZERO     TO C-KSKIN(I)
               GO TO KSKIN-999
           END-IF.
       KSKIN-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSKIN "ACP-KSKIN" "S9" "10"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           IF  C-DNNO    < 300000
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           IF  C-KS-TAX(I) = "6"
               IF  C-KSTAX(I) NOT = " " AND "5" AND "7"
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
       TEKI-RTN.
           IF (C-KRCDM(I) = ZERO) AND
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
      ***
       OKC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
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
       CRE-EX.
           EXIT.
      *********
       DNOU-RTN.
           MOVE "14"     TO DNO1-KEY ERR-K.
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
           MOVE CRDATE     TO SDIYMD.
           MOVE DNO1-023   TO SDIJNO.
           MOVE ZERO       TO SDILNO.
      *           START SDI KEY NOT LESS SDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT LESS " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO TO DNOU-100
           END-IF
      *           READ SDI NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DNOU-100
           END-IF.
           IF (CRDATE = SDIYMD) AND
              (DNO1-023 = SDIJNO)
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
       SDIDU-RTN.
           MOVE CRDATE     TO SDIYMD.
           MOVE C-DNNO     TO SDIJNO.
           MOVE ZERO       TO SDILNO.
      *           START SDI KEY NOT LESS SDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT LESS " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO TO SDIDU-EX
           END-IF.
       SDIDU-000.
      *           READ SDI NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC " "
            RETURNING RET.
           IF  RET = 1
                GO TO SDIDU-999
           END-IF.
           IF  CRDATE NOT = SDIYMD
               GO TO SDIDU-999
           END-IF.
           IF  C-DNNO NOT = SDIJNO
               GO TO SDIDU-999
           END-IF.
           MOVE SDI-KEY     TO ERR-K.
      *           DELETE SDI INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SDI_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "SDI"     TO ERR-F
               MOVE "D"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO SDIDU-000.
       SDIDU-999.
           MOVE LOW-VALUE     TO SDI-KEY.
      *           START SDI KEY NOT LESS SDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT LESS " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO TO SDIDU-EX
           END-IF
      *           READ SDI NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDIDU-EX
           END-IF.
       SDIDU-EX.
           EXIT.
       SDIWU-RTN.
           MOVE 0     TO SOE.
           MOVE 1     TO I.
       SDIWU-000.
           IF (C-KRCDM(I) = ZERO) AND
              (C-KSCDM(I) = ZERO)
               GO TO SDIWU-999
           END-IF.
           ADD 1     TO SOE.
           MOVE SPACE     TO SDI-REC.
           INITIALIZE SDI-REC.
           MOVE CRDATE          TO SDIYMD.
           MOVE C-DNNO          TO SDIJNO.
           MOVE SOE             TO SDILNO.
           MOVE C-KARI(I)       TO SDIKARI.
           MOVE C-KASI(I)       TO SDIKASI.
           MOVE C-TKCD          TO SDICUST.
           MOVE C-NAMEN         TO SDINAMEN.
           MOVE C-TEKICD(I)     TO SDITEKICD.
           MOVE C-TEKI(I)       TO SDITEKI.
           IF  C-KRCDM(I) = W-KRCDM OR W-KSCDM
               MOVE C-KRTAX(I) TO SDIETAX
               MOVE " " TO KRTAX
           END-IF.
           IF  C-KSCDM(I) = W-KRCDM OR W-KSCDM
               MOVE C-KSTAX(I) TO SDIETAX
               MOVE " " TO KSTAX
           END-IF.
           MOVE SPACE           TO SDISIN.
           IF  C-ACT = 4
               MOVE "1"          TO SDIDEL
           ELSE
               MOVE SPACE        TO SDIDEL
           END-IF.
           MOVE SDI-KEY     TO ERR-K.
      *           WRITE SDI-REC INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            SDI_PNAME1 SDI_LNAME SDI-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDI"     TO ERR-F
               MOVE "W"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       SDIWU-999.
           IF  I NOT = 5
               ADD 1     TO I
               GO TO SDIWU-000
           END-IF.
           IF  C-ACT   =  3  OR  4
               GO TO SDIWU-EX
           END-IF.
           IF  C-TKCD  =  ZERO OR 99999
               GO TO SDIWU-EX
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE TK-NG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           IF  CRYM NOT > W-NG
               GO TO SDIWU-EX
           END-IF.
           MOVE CRYM TO W-NG.
           MOVE W-NGS TO TK-NG.
      *           REWRITE TK-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TK_PNAME1 TK_LNAME TK-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "MG-12" MG-12 "p"
                                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU
           END-IF.
       SDIWU-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BM_IDLST BM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TKI_IDLST TKI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-DNO_IDLST NS-DNO_PNAME1.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
       CLSE-EXT.
           EXIT.
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
               GO TO KNGG-EX.
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
           IF DRCR-SW = 1
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
