       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD750.
      *********************************************************
      *    PROGRAM         :  発注入庫残明細表                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  品名別=0 , 得意先別=1           *
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
                "　＊＊＊　　品名".
           02  F              PIC  N(015) VALUE
                "別　発注入庫残明細表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  H-CM1          PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-NM1          PIC  N(008) VALUE "品　　　名　　　".
           02  F              PIC  X(117) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(015) VALUE SPACE.
           02  H-CM2          PIC  X(005) VALUE " ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-NM2          PIC  N(008) VALUE "仕　入　先　名　".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　ロット№　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　発注日".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "納　　期".
           02  F              PIC  X(048) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(050) VALUE SPACE.
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
           02  F              PIC  X(025) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(050) VALUE SPACE.
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
           02  F              PIC  X(025) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(050) VALUE SPACE.
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
           02  F              PIC  X(025) VALUE SPACE.
       01  HEAD7.
           02  F              PIC  X(050) VALUE SPACE.
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
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P1.
           02  P-HCD1         PIC  9(006).
           02  P-CD1   REDEFINES P-HCD1.
             03  F            PIC  X(002).
             03  P-SCD1       PIC  9(004).
           02  F              PIC  X(001).
           02  P-NA1          PIC  N(024).
           02  F              PIC  X(093).
       01  W-P2.
           02  F              PIC  X(014).
           02  P-HCD2         PIC  9(006).
           02  P-CD2   REDEFINES P-HCD2.
             03  F            PIC  X(002).
             03  P-SCD2       PIC  9(004).
           02  F              PIC  X(001).
           02  P-NA2          PIC  N(024).
           02  F              PIC  X(002).
           02  P-RSN          PIC  9(002).
           02  P-V1           PIC  X(001).
           02  P-RNG          PIC  9(004).
           02  P-V2           PIC  X(001).
           02  P-RND          PIC  9(002).
           02  F              PIC  X(002).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-NDD          PIC 99/99/99.
           02  F              PIC  X(047).
       01  W-P3.
           02  F              PIC  X(015).
           02  P-TM           PIC  N(010).
           02  F              PIC  X(020).
           02  P-SIZ          PIC  9(001).
           02  P-ASU   OCCURS  10.
             03  P-SU         PIC --,---.
           02  P-GSU          PIC ---,--9.
           02  F              PIC  X(001).
           02  P-T            PIC ZZ,ZZ9.
           02  P-KIN          PIC ---,---,--9.
       01  W-DATA.
           02  W-MID          PIC  N(006).
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
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-SCD          PIC  9(004).
           02  W-PC           PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SUD   OCCURS  10.
                 05  W-SU     PIC S9(004).
           02  W-GSU          PIC S9(006).
           02  W-KIN          PIC S9(007).
           02  W-ATSUD.
             03  W-ATSU  OCCURS   4.
               04  W-TSUD  OCCURS  10.
                 05  W-TSU    PIC S9(004).
           02  W-SGSU         PIC S9(006).
           02  W-SKIN         PIC S9(008).
           02  W-AKIN         PIC S9(008).
           02  W-AZCD.
             03  W-ZCD   OCCURS   4.
               04  W-ZC       PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-CC           PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHIM.
           COPY LSPF.
      *FD  HSHF
       01  HSHF_KBD750.
           02  HSHF_PNAME1      PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  HSHF_LNAME       PIC  X(011) VALUE "HSHF_KBD750".
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
             03  HSH-HSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-AHSU  OCCURS  10.
                 05  HSH-HSU    PIC S9(004).                            数量
           02  HSH-ANSUD.
             03  HSH-NSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-ANSU  OCCURS  10.
                 05  HSH-NSU    PIC S9(004).                            数量
           02  HSH-AISUD.
             03  HSH-ISUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-AISU  OCCURS  10.
                 05  HSH-ISU    PIC S9(004).                            数量
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　発注入庫残明細表　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "品　名  000000   ～ 999999".
           02  FILLER  PIC  X(026) VALUE
                "仕入先  0000     ～ 9999  ".
           02  FILLER  PIC  X(028) VALUE
                "納　期  00/00/00 ～ 99/99/99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-MID   PIC  N(006).
           02  D-MSG.
             03  FILLER  PIC  X(026) VALUE
                  "仕入先  0000     ～ 9999  ".
             03  FILLER  PIC  X(026) VALUE
                  "品　名  000000   ～ 999999".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD1 PIC  9(006).
             03  A-EHCD1 PIC  9(006).
             03  A-SSCD2 PIC  9(004).
             03  A-ESCD2 PIC  9(004).
           02  FILLER.
             03  A-SHCD2 PIC  9(006).
             03  A-EHCD2 PIC  9(006).
             03  A-SSCD1 PIC  9(004).
             03  A-ESCD1 PIC  9(004).
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
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
            "C-MID" " " "0" "0" "354" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "12" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "12" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "12" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "12" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "12" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "12" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "12" "36" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "17" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "17" "26" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "16" "17" "28" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "22" "19" "22" "10C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" "N" "7" "24" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MID" BY REFERENCE W-MID "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG" " " "0" "0" "52" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MSG" "X" "12" "17" "26" " " "D-MSG" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MSG" "X" "14" "17" "26" "01D-MSG" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "20" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD1" "9" "12" "25" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD1" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD1" "9" "12" "37" "6" "A-SHCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD1" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSCD2" "9" "12" "25" "4" "A-EHCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSCD2" BY REFERENCE W-SSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESCD2" "9" "12" "37" "4" "A-SSCD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESCD2" BY REFERENCE W-ESCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "20" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD2" "9" "14" "25" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD2" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD2" "9" "14" "37" "6" "A-SHCD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD2" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSCD1" "9" "14" "25" "4" "A-EHCD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSCD1" BY REFERENCE W-SSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESCD1" "9" "14" "37" "4" "A-SSCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESCD1" BY REFERENCE W-ESCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "12" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "16" "25" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "16" "28" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "16" "31" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "37" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "40" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "16" "43" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "36" "1" "03C-ACP" " " RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-ENEN2 W-EGET W-EPEY.
           MOVE 9999 TO W-ESCD.
           MOVE 999999 TO W-EHCD.
           IF  JS-SIGN = 0
               MOVE "【品名別】　" TO W-MID
           ELSE
               MOVE "【仕入先別】" TO W-MID
               MOVE "＊＊＊　　仕入先" TO H-MID
               MOVE "  ｺｰﾄﾞ" TO H-CM1
               MOVE "仕　入　先　名　" TO H-NM1
               MOVE "ｺｰﾄﾞ " TO H-CM2
               MOVE "品　　　名　　　" TO H-NM2
               CALL "SD_Output" USING "D-MSG" D-MSG "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
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
               IF  HSH-HCD < W-SHCD
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HCD > W-EHCD
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
           IF  JS-SIGN = 0
               IF  HSH-SCD < W-SSCD OR > W-ESCD
                   GO TO M-10
               END-IF
           END-IF
      *
           IF  JS-SIGN = 1
               IF  HSH-SCD < W-SSCD
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-SCD > W-ESCD
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
               IF  HSH-HCD < W-SHCD OR > W-EHCD
                   GO TO M-10
               END-IF
           END-IF
      *
           IF  HSH-NDD < W-SNGP OR > W-ENGP
               GO TO M-10
           END-IF
           IF  HSH-ENGP NOT = ZERO
               GO TO M-10
           END-IF
           PERFORM ZAN-RTN THRU ZAN-EX.
           IF  W-AZCD = ZERO
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-020 THRU MID-EX.
       M-15.
           MOVE ZERO TO W-PC W-CC W-ATSUD W-SGSU W-SKIN.
           IF  JS-SIGN = 1
               GO TO M-20
           END-IF
           MOVE HSH-HCD TO W-HCD.
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
           GO TO M-25.
       M-20.
           MOVE HSH-SCD TO W-SCD.
           MOVE HSH-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "＊＊＊　仕入先　なし　　＊＊＊" TO S-NAME
           END-IF.
       M-25.
           PERFORM ZAN-RTN THRU ZAN-EX.
           PERFORM PRI2-RTN THRU PRI2-EX.
           PERFORM PRI3-RTN THRU PRI3-EX.
       M-30.
      *           READ HSHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSHF_PNAME1 BY REFERENCE HSH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HCD < W-SHCD
                   GO TO M-30
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-HCD > W-EHCD
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HSH-SCD < W-SSCD OR > W-ESCD
                   GO TO M-30
               END-IF
           END-IF
      *
           IF  JS-SIGN = 1
               IF  HSH-SCD < W-SSCD
                   GO TO M-30
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-SCD > W-ESCD
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HSH-HCD < W-SHCD OR > W-EHCD
                   GO TO M-30
               END-IF
           END-IF
      *
           IF  HSH-NDD < W-SNGP OR > W-ENGP
               GO TO M-30
           END-IF
           IF  HSH-ENGP NOT = ZERO
               GO TO M-30
           END-IF
           PERFORM ZAN-RTN THRU ZAN-EX.
           IF  W-AZCD = ZERO
               GO TO M-30
           END-IF
           IF  JS-SIGN = 0
               IF  W-HCD = HSH-HCD
                   GO TO M-25
               ELSE
                   GO TO M-35
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SCD = HSH-SCD
                   GO TO M-25
               END-IF
           END-IF.
       M-35.
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-15.
       M-80.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACT-RTN.
           IF  JS-SIGN = 1
               GO TO ACT-100
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD1 "A-SHCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-020
           END-IF.
       ACT-040.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD1 "A-EHCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-040
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO ACT-040
           END-IF.
       ACT-060.
           CALL "SD_Accept" USING BY REFERENCE A-SSCD1 "A-SSCD1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-060
           END-IF.
       ACT-080.
           CALL "SD_Accept" USING BY REFERENCE A-ESCD1 "A-ESCD1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-080
           END-IF
           IF  W-SSCD > W-ESCD
               GO TO ACT-080
           END-IF
           GO TO ACT-180.
       ACT-100.
           CALL "SD_Accept" USING BY REFERENCE A-SSCD2 "A-SSCD2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-100
           END-IF.
       ACT-120.
           CALL "SD_Accept" USING BY REFERENCE A-ESCD2 "A-ESCD2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-120
           END-IF
           IF  W-SSCD > W-ESCD
               GO TO ACT-120
           END-IF.
       ACT-140.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD2 "A-SHCD2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-140
           END-IF.
       ACT-160.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD2 "A-EHCD2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-160
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO ACT-160
           END-IF.
       ACT-180.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO ACT-080
               ELSE
                   GO TO ACT-160
               END-IF
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-180
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 = ZERO
               GO TO ACT-200
           END-IF
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       ACT-200.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-180
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-200
           END-IF.
       ACT-220.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-220
           END-IF.
       ACT-240.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-220
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-240
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       ACT-260.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-240
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-260
           END-IF
           IF  W-EGET = 99
               IF  W-ENEN2 = 99
                  MOVE 99 TO W-ENEN1
               END-IF
           END-IF.
       ACT-280.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-280
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO ACT-240
           END-IF.
       ACT-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-300
           END-IF
           IF  W-DMM = 9
               GO TO ACT-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACT-300
           END-IF.
       ACT-EX.
           EXIT.
       ZAN-RTN.
           MOVE ZERO TO W-ASUD CNT W-GSU W-AZCD.
       ZAN-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO ZAN-EX
           END-IF
           COMPUTE W-SU(1,CNT) = HSH-HSU(1,CNT) -
                                 HSH-NSU(1,CNT) - HSH-ISU(1,CNT).
           IF  W-ZC(1) = 0
               IF  W-SU(1,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(1)
               END-IF
           END-IF
           COMPUTE W-SU(2,CNT) = HSH-HSU(2,CNT) -
                                 HSH-NSU(2,CNT) - HSH-ISU(2,CNT).
           IF  W-ZC(2) = 0
               IF  W-SU(2,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(2)
               END-IF
           END-IF
           COMPUTE W-SU(3,CNT) = HSH-HSU(3,CNT) -
                                 HSH-NSU(3,CNT) - HSH-ISU(3,CNT).
           IF  W-ZC(3) = 0
               IF  W-SU(3,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(3)
               END-IF
           END-IF
           COMPUTE W-SU(4,CNT) = HSH-HSU(4,CNT) -
                                 HSH-NSU(4,CNT) - HSH-ISU(4,CNT).
           IF  W-ZC(4) = 0
               IF  W-SU(4,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(4)
               END-IF
           END-IF
           COMPUTE W-GSU = W-GSU + W-SU(1,CNT) + W-SU(2,CNT)
                                 + W-SU(3,CNT) + W-SU(4,CNT).
           GO TO ZAN-020.
       ZAN-EX.
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
           MOVE HEAD7 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       PRI1-RTN.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NA1.
           IF  JS-SIGN = 0
               MOVE W-HCD TO P-HCD1
               MOVE HI-NAME TO P-NA1
           ELSE
               MOVE W-SCD TO P-SCD1
               MOVE S-NAME TO P-NA1
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI1-EX.
           EXIT.
       PRI2-RTN.
           IF  JS-SIGN = 1
               GO TO PRI2-020
           END-IF
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
           GO TO PRI2-040.
       PRI2-020.
           MOVE HSH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF.
       PRI2-040.
           IF  W-PC = 0
               MOVE 1 TO W-PC
               PERFORM PRI1-RTN THRU PRI1-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NA2.
           IF  JS-SIGN = 0
               MOVE HSH-SCD TO P-SCD2
               MOVE S-NAME TO P-NA2
           ELSE
               MOVE HSH-HCD TO P-HCD2
               MOVE HI-NAME TO P-NA2
           END-IF
           MOVE HSH-RSN TO P-RSN.
           MOVE HSH-RNG TO P-RNG.
           MOVE HSH-RND TO P-RND.
           MOVE "-" TO P-V1 P-V2.
           MOVE HSH-HNGPS TO P-DATE.
           MOVE HSH-NNGPS TO P-NDD.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM MID-RTN THRU MID-EX
               PERFORM PRI1-RTN THRU PRI1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-CC = 1
               MOVE 2 TO W-CC
           END-IF
           IF  W-CC = 0
               MOVE 1 TO W-CC
           END-IF.
       PRI2-EX.
           EXIT.
       PRI3-RTN.
           COMPUTE W-KIN = W-GSU * HSH-T.
           ADD W-GSU TO W-SGSU.
           ADD W-KIN TO W-SKIN.
           MOVE 0 TO W-S W-EC.
       PRI3-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO PRI3-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO PRI3-020
           END-IF
      *
           IF  W-S = 1
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
           END-IF
      *
           MOVE SPACE TO W-P3.
           MOVE SPACE TO P-TM.
           IF  W-EC = 1
               MOVE W-GSU TO P-GSU
               MOVE HSH-T TO P-T
               MOVE W-KIN TO P-KIN
           END-IF
           MOVE W-S TO P-SIZ.
      *
           MOVE ZERO TO CNT.
       PRI3-040.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE W-SU(W-S,CNT) TO P-SU(CNT)
               ADD W-SU(W-S,CNT) TO W-TSU(W-S,CNT)
               GO TO PRI3-040
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
               PERFORM PRI1-RTN THRU PRI1-EX
               PERFORM PRI2-RTN THRU PRI2-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI3-020.
       PRI3-EX.
           EXIT.
       SUB-RTN.
           ADD W-SKIN TO W-AKIN.
           IF  W-CC < 2
               GO TO SUB-EX
           END-IF
           MOVE ZERO TO W-AZCD W-S.
       SUB-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SUB-060
           END-IF
           MOVE ZERO TO CNT.
       SUB-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SUB-020
           END-IF
           IF  W-TSU(W-S,CNT) NOT = ZERO
               MOVE 1 TO W-ZC(W-S)
               GO TO SUB-020
           END-IF
           GO TO SUB-040.
       SUB-060.
           MOVE 0 TO W-S.
       SUB-080.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SUB-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO SUB-080
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
               IF  0 = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
           END-IF
      *
           MOVE SPACE TO W-P3.
           MOVE SPACE TO P-TM.
           IF  W-SC = 1
               MOVE "　　［　小　計　］　" TO P-TM
           END-IF
           IF  W-EC = 1
               MOVE W-SGSU TO P-GSU
               MOVE W-SKIN TO P-KIN
           END-IF
           MOVE W-S TO P-SIZ.
      *
           MOVE ZERO TO CNT.
       SUB-100.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE W-TSU(W-S,CNT) TO P-SU(CNT)
               GO TO SUB-100
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
               PERFORM PRI1-RTN THRU PRI1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           IF W-SC = 1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           GO TO SUB-080.
       SUB-EX.
           EXIT.
       ALL-RTN.
           MOVE SPACE TO W-P3.
           MOVE "【　総　合　計　】　" TO P-TM.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ALL-EX.
           EXIT.
