       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD710.
      *********************************************************
      *    PROGRAM         :  発注・入庫明細表                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  発注日別=0 , 納期別=1           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0768ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0768".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  H-MID          PIC  N(008) VALUE
                "＊＊＊　　発注日".
           02  F              PIC  N(015) VALUE
                "別　発注・入庫明細表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　発注日".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　ロット№　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "仕　入　先　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "納　　期".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(012) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(049) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "SS".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "M".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "L".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "LL".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "XL".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(003) VALUE "XXL".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(015) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(049) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(015) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(049) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(015) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(049) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　購買№".
       01  W-P1.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-RSN          PIC  9(002).
           02  P-V1           PIC  X(001).
           02  P-RNG          PIC  9(004).
           02  P-V2           PIC  X(001).
           02  P-RND          PIC  9(002).
           02  F              PIC  X(001).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-NDD          PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-F            PIC  X(001).
           02  P-T            PIC ZZ,ZZ9.
           02  P-R            PIC  X(001).
           02  F              PIC  X(002).
           02  P-KRM          PIC  N(004).
           02  F              PIC  X(004).
       01  W-P2.
           02  F              PIC  X(021).
           02  P-TM           PIC  N(010).
           02  P-GET          PIC  Z(002).
           02  P-GPV          PIC  X(001).
           02  P-PEY          PIC  Z(002).
           02  F              PIC  X(001).
           02  P-KBN          PIC  N(004).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-ASU   OCCURS  10.
             03  P-SU         PIC --,---.
           02  P-GSU          PIC ---,--9.
           02  P-KIN          PIC ---,---,--9.
           02  F              PIC  X(002).
           02  P-KBNO         PIC  9(006).
       01  W-DATA.
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SSCD         PIC  9(004).
           02  W-ESCD         PIC  9(004).
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-GP.
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KBN          PIC  N(004).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SUD   OCCURS  10.
                 05  W-SU     PIC S9(004).
           02  W-GSU          PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-TKIN         PIC S9(008).
           02  W-KBNO         PIC  X(006).
           02  W-AZCD.
             03  W-ZCD   OCCURS   4.
               04  W-ZC       PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-DEC          PIC  9(001).
           02  W-CC           PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHIM.
           COPY LIHSHN.
           COPY LIHSSF.
           COPY LSPF.
      *FD  HSHF
       01  HSHF_KBD710.
           02  HSHF_PNAME1      PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  HSHF_LNAME       PIC  X(011) VALUE "HSHF_KBD710".
           02  F                PIC  X(001).
           02  HSHF_KEY1        PIC  X(100) VALUE SPACE.
           02  HSHF_SORT        PIC  X(100) VALUE SPACE.
           02  HSHF_IDLST       PIC  X(100) VALUE SPACE.
           02  HSHF_RES         USAGE  POINTER.
       01  HSH-R.
           02  HSH-KEY3.
             03  HSH-SCD        PIC  9(004).
             03  HSH-KEY2.
               04  HSH-HCD      PIC  9(006).
               04  HSH-KEY      PIC  X(008).
               04  HSH-RNO   REDEFINES HSH-KEY.
                 05  HSH-RSN    PIC  9(002).
                 05  HSH-RNG    PIC  9(004).
                 05  HSH-RND    PIC  9(002).
           02  HSH-HDD          PIC  9(008).
           02  HSH-HDDD  REDEFINES HSH-HDD.
             03  HSH-HNEN       PIC  9(004).
             03  HSH-HGP        PIC  9(004).
           02  HSH-HDDL  REDEFINES HSH-HDD.
             03  F              PIC  9(002).
             03  HSH-HNGPS      PIC  9(006).
           02  HSH-AHSUD.
             03  HSH-HSUD  OCCURS   4.
               04  HSH-AHSU  OCCURS  10.
                 05  HSH-HSU    PIC S9(004).
           02  HSH-ANSUD.
             03  HSH-NSUD  OCCURS   4.
               04  HSH-ANSU  OCCURS  10.
                 05  HSH-NSU    PIC S9(004).
           02  HSH-AISUD.
             03  HSH-ISUD  OCCURS   4.
               04  HSH-AISU  OCCURS  10.
                 05  HSH-ISU    PIC S9(004).
           02  HSH-T            PIC  9(005).
           02  HSH-NDD          PIC  9(008).
           02  HSH-NDDD  REDEFINES HSH-NDD.
             03  HSH-NNG.
               04  HSH-NNEN     PIC  9(004).
               04  HSH-NNENL REDEFINES HSH-NNEN.
                 05  HSH-NNEN1  PIC  9(002).
                 05  HSH-NNEN2  PIC  9(002).
               04  HSH-NGET     PIC  9(002).
             03  HSH-NPEY       PIC  9(002).
           02  HSH-NDDL  REDEFINES HSH-NDD.
             03  F              PIC  9(002).
             03  HSH-NNGPS      PIC  9(006).
           02  HSH-ENGP         PIC  9(006).
           02  F                PIC  X(243).
       77  F                    PIC  X(001).
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
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　発注日別　発注・入庫明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "発注日  00/00/00 ～ 99/99/99".
           02  FILLER  PIC  X(024) VALUE
                "仕入先  0000     ～ 9999".
           02  FILLER  PIC  X(026) VALUE
                "品　名  000000   ～ 999999".
           02  FILLER  PIC  X(041) VALUE
                "全体=0 , 未完了のみ=1 , 完了済みのみ=2   ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　納期別　発注・入庫明細表　　＊＊＊".
           02  FILLER  PIC  N(003) VALUE "納　期".
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  FILLER.
             03  A-SSCD  PIC  9(004).
             03  A-ESCD  PIC  9(004).
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
           COPY LIBSCR.
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "463" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "12" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "12" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "12" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "12" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "12" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "12" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "12" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "20" "28" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "20" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "16" "20" "26" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "18" "20" "41" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-MID" "X" "22" "30" "22" "11C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "6" "12" "46" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "N" "12" "20" "6" "01C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "12" "28" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "12" "31" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "12" "34" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "12" "40" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "12" "43" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "12" "46" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "8" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSCD" "9" "14" "28" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSCD" BY REFERENCE W-SSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESCD" "9" "14" "40" "4" "A-SSCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESCD" BY REFERENCE W-ESCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "12" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "16" "28" "6" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "16" "40" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "18" "60" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               MOVE "　＊＊＊　　納期" TO H-MID
           END-IF
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-ENEN2 W-EGET W-EPEY.
           MOVE 9999 TO W-ESCD.
           MOVE 999999 TO W-EHCD. 
           COPY LIBCPR.
      *
           PERFORM ACT-RTN THRU ACT-EX.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0768ID.
           MOVE WK0768ID TO HSHF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSHF_PNAME1 " " BY REFERENCE HSHF_IDLST "0".
       M-10.
      *           READ HSHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSHF_PNAME1 BY REFERENCE HSH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSHF_IDLST HSHF_PNAME1
               MOVE "***  ＤＡＴＡ　なし  ***      " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HDD < W-SNGP
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HDD > W-ENGP
                   CALL "DB_F_Close" USING
                    BY REFERENCE HSHF_IDLST HSHF_PNAME1
                   MOVE "***  ＤＡＴＡ　なし  ***      " TO W-EM
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-NDD < W-SNGP
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-NDD > W-ENGP
                   CALL "DB_F_Close" USING
                    BY REFERENCE HSHF_IDLST HSHF_PNAME1
                   MOVE "***  ＤＡＴＡ　なし  ***      " TO W-EM
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF
           IF  HSH-SCD < W-SSCD OR > W-ESCD
               GO TO M-10
           END-IF
           IF  HSH-HCD < W-SHCD OR > W-EHCD
               GO TO M-10
           END-IF
           IF  W-SEN = 1
               IF  HSH-ENGP NOT = ZERO
                   GO TO M-10
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  HSH-ENGP = ZERO
                   GO TO M-10
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-020 THRU MID-EX.
       M-15.
           MOVE HSH-SCD TO S-KEY.
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
           MOVE HSH-HCD TO HI-KEY.
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
           PERFORM PRI1-RTN THRU PRI1-EX.
      *
           MOVE ZERO TO W-CC W-DEC.
           MOVE HSH-HGP TO W-GP.
           MOVE "《発注》" TO W-KBN.
           MOVE ZERO TO W-KBNO.
           MOVE HSH-AHSUD TO W-ASUD.
           PERFORM CHK-RTN THRU CHK-EX.
           PERFORM PRI2-RTN THRU PRI2-EX.
      *
           MOVE SPACE TO HSHN-KEY.
           MOVE HSH-RNO TO HSHN-RNO.
      *           START HSHNF KEY NOT < HSHN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHNF_PNAME1 "HSHN-KEY" " NOT < " HSHN-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
       M-20.
      *           READ HSHNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HSH-RNO NOT = HSHN-RNO
               GO TO M-25
           END-IF
      *
           MOVE HSHN-GP TO W-GP.
           IF  HSHN-HPC = 0
               MOVE "（入庫）" TO W-KBN
           ELSE
               MOVE "｛返品｝" TO W-KBN
           END-IF
           MOVE HSHN-KBNO TO W-KBNO.
           MOVE ZERO TO W-ASUD.
           MOVE HSHN-ASUD TO W-ASUD.
           PERFORM CHK-RTN THRU CHK-EX.
           PERFORM PRI2-RTN THRU PRI2-EX.
           GO TO M-20.
       M-25.
           CALL "DB_F_Open" USING
            "INPUT" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-30.
      *           READ HSS-F NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  HSS-HKC NOT = 0
               GO TO M-30
           END-IF
           IF  HSH-RNO NOT = HSS-RNO
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO W-GP.
           IF  HSS-HPC = 0
               MOVE "（入庫）" TO W-KBN
           ELSE
               MOVE "｛返品｝" TO W-KBN
           END-IF
           MOVE HSS-DNO TO W-KBNO.
           MOVE ZERO TO W-ASUD.
           MOVE HSS-ASUD TO W-ASUD.
           PERFORM CHK-RTN THRU CHK-EX.
           PERFORM PRI2-RTN THRU PRI2-EX.
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
           IF  HSH-ENGP NOT = ZERO
               GO TO M-40
           END-IF
           MOVE 1 TO W-DEC.
           PERFORM ZAN-RTN THRU ZAN-EX.
           PERFORM CHK-RTN THRU CHK-EX.
           COMPUTE W-TKIN = W-TKIN + (W-GSU * HSH-T).
           IF  W-CC NOT = 2
               GO TO M-40
           END-IF
           MOVE "＜残高＞" TO W-KBN.
           PERFORM PRI2-RTN THRU PRI2-EX.
       M-40.
      *           READ HSHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSHF_PNAME1 BY REFERENCE HSH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HDD < W-SNGP
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HDD > W-ENGP
                   GO TO M-90
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-NDD < W-SNGP
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-NDD > W-ENGP
                   GO TO M-90
               END-IF
           END-IF
           IF  HSH-SCD < W-SSCD OR > W-ESCD
               GO TO M-40
           END-IF
           IF  HSH-HCD < W-SHCD OR > W-EHCD
               GO TO M-40
           END-IF
           IF  W-SEN = 1
               IF  HSH-ENGP NOT = ZERO
                   GO TO M-40
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  HSH-ENGP = ZERO
                   GO TO M-40
               END-IF
           END-IF
           GO TO M-15.
       M-90.
           PERFORM TOT-RTN THRU TOT-EX.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACT-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-RTN
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 = ZERO
               GO TO ACT-020
           END-IF
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-RTN
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-020
           END-IF.
       ACT-040.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-040
           END-IF.
       ACT-060.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-040
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-060
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       ACT-080.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-060
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-080
           END-IF
           IF  W-EGET = 99
              IF  W-ENEN2 = 99
                  MOVE 99 TO W-ENEN1
              END-IF
           END-IF.
       ACT-100.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-100
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO ACT-060
           END-IF.
       ACT-120.
           CALL "SD_Accept" USING BY REFERENCE A-SSCD "A-SSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-120
           END-IF.
       ACT-140.
           CALL "SD_Accept" USING BY REFERENCE A-ESCD "A-ESCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-140
           END-IF
           IF  W-SSCD > W-ESCD
               GO TO ACT-140
           END-IF.
       ACT-160.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-160
           END-IF.
       ACT-180.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-180
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO ACT-180
           END-IF.
       ACT-200.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-200
           END-IF
           IF  W-SEN > 2
               GO TO ACT-200
           END-IF.
       ACT-220.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-220
           END-IF
           IF  W-DMM = 9
               GO TO ACT-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACT-220
           END-IF.
       ACT-EX.
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
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-SNA P-HNA P-KRM.
           MOVE HSH-HNGPS TO P-DATE.
           MOVE HSH-RSN TO P-RSN.
           MOVE HSH-RNG TO P-RNG.
           MOVE HSH-RND TO P-RND.
           MOVE "-" TO P-V1 P-V2.
           MOVE HSH-SCD TO P-SCD.
           MOVE S-NAME TO P-SNA.
           MOVE HSH-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE HSH-NNGPS TO P-NDD.
           MOVE "(" TO P-F.
           MOVE HSH-T TO P-T.
           MOVE ")" TO P-R.
           IF  HSH-ENGP NOT = ZERO
               MOVE "完了済み" TO P-KRM
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI1-EX.
           EXIT.
       CHK-RTN.
           MOVE ZERO TO W-S W-AZCD W-GSU.
       CHK-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO CHK-EX
           END-IF
           MOVE ZERO TO CNT.
       CHK-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO CHK-020
           END-IF
           IF  W-SU(W-S,CNT) NOT = ZERO
               ADD W-SU(W-S,CNT) TO W-GSU
               IF  W-ZC(W-S) = 0
                   MOVE 1 TO W-ZC(W-S)
               END-IF
           END-IF
           GO TO CHK-040.
       CHK-EX.
           EXIT.
       PRI2-RTN.
           IF  W-CC = 1
               MOVE 2 TO W-CC
           END-IF
           IF  W-CC = 0
               MOVE 1 TO W-CC
           END-IF
      *
           MOVE 0 TO W-S.
       PRI2-080.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO PRI2-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO PRI2-080
           END-IF
      *
           MOVE 0 TO W-SC W-EC.
           IF  W-S = 1
               MOVE 1 TO W-SC
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(1)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(1) AND W-ZC(2)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
               IF  0 = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-KBN.
           IF  W-SC = 1
               IF  W-DEC = 1
                   MOVE W-KBN TO P-KBN
               ELSE
                   MOVE W-GET TO P-GET
                   MOVE "/" TO P-GPV
                   MOVE W-PEY TO P-PEY
                   MOVE W-KBN TO P-KBN
                   IF  W-KBNO NOT = ZERO
                       MOVE W-KBNO TO P-KBNO
                   END-IF
               END-IF
           END-IF
           IF  W-EC = 1
               COMPUTE W-KIN = W-GSU * HSH-T
               MOVE W-GSU TO P-GSU
               MOVE W-KIN TO P-KIN
           END-IF
           MOVE W-S TO P-SIZ.
      *
           MOVE ZERO TO CNT.
       PRI2-100.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE W-SU(W-S,CNT) TO P-SU(CNT)
               GO TO PRI2-100
           END-IF.
       PRI2-120.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO PRI2-140
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           PERFORM PRI1-RTN THRU PRI1-EX.
           IF  W-DEC = 1
               MOVE W-KBN TO P-KBN
               GO TO PRI2-140
           END-IF
           MOVE W-GET TO P-GET.
           MOVE "/" TO P-GPV.
           MOVE W-PEY TO P-PEY.
           MOVE W-KBN TO P-KBN.
           IF  W-KBNO NOT = ZERO
               MOVE W-KBNO TO P-KBNO
           END-IF.
       PRI2-140.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI2-080.
       PRI2-EX.
           EXIT.
       ZAN-RTN.
           MOVE ZERO TO W-ASUD CNT.
       ZAN-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO ZAN-EX
           END-IF
           COMPUTE W-SU(1,CNT) = HSH-HSU(1,CNT) -
                                 HSH-NSU(1,CNT) - HSH-ISU(1,CNT).
           COMPUTE W-SU(2,CNT) = HSH-HSU(2,CNT) -
                                 HSH-NSU(2,CNT) - HSH-ISU(2,CNT).
           COMPUTE W-SU(3,CNT) = HSH-HSU(3,CNT) -
                                 HSH-NSU(3,CNT) - HSH-ISU(3,CNT).
           COMPUTE W-SU(4,CNT) = HSH-HSU(4,CNT) -
                                 HSH-NSU(4,CNT) - HSH-ISU(4,CNT).
           GO TO ZAN-020.
       ZAN-EX.
           EXIT.
       TOT-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-KBN.
           MOVE "【　残金額合計　】　" TO P-TM.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       TOT-EX.
           EXIT.
