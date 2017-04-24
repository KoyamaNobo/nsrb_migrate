       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV010U.
      ************************************************************
      *    PROGRAM         :  â◊éDïœä∑ópÉèÅ[ÉNçÏê¨               *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  ______                             *
      *    COMPILE TYPE    :  COBOL                              *
      *    JS-W            :  0=ñ{é– , 2=ã ìá , 3=ëÅìá           *
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
       77  JS-SIGN                   PIC X(01).
       77  JS-W                      PIC X(01).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-SEN                 PIC 9(01).
           02  W-SDNO                PIC 9(06).
           02  W-EDNO                PIC 9(06).
           02  W-NGP.
             03  F                   PIC 9(02).
             03  W-NGPS              PIC 9(06).
           02  W-NGPL              REDEFINES  W-NGP.
             03  W-NEN               PIC 9(04).
             03  W-NENL            REDEFINES  W-NEN.
               04  W-NEN1            PIC 9(02).
               04  W-NEN2            PIC 9(02).
             03  F                   PIC 9(04).
           02  W-SDATE               PIC 9(08).
           02  W-EDATE               PIC 9(08).
           02  W-SNGP.
             03  W-SNEN              PIC 9(04).
             03  W-SNENL           REDEFINES  W-SNEN.
               04  W-SNEN1           PIC 9(02).
               04  W-SNEN2           PIC 9(02).
             03  W-SGET              PIC 9(02).
             03  W-SPEY              PIC 9(02).
           02  W-ENGP.
             03  W-ENEN              PIC 9(04).
             03  W-ENENL           REDEFINES  W-ENEN.
               04  W-ENEN1           PIC 9(02).
               04  W-ENEN2           PIC 9(02).
             03  W-EGET              PIC 9(02).
             03  W-EPEY              PIC 9(02).
           02  W-DMM          PIC  X(01).
           02  W-OKJ          PIC  9(06).
           02  W-BAN          PIC  9(06).
           02  W-KSU          PIC  9(04).
           02  W-KEI          PIC  9(04).
           02  W-GYO          PIC  9(03).
           02  W-NNO.
             03  W-NNO1       PIC 9(6).
             03  W-NNO2       PIC 9(1).
           02  W-KEY.
             03  W-NO         PIC 9(06).
             03  W-GNO        PIC 9(01).
           02  W-SET          PIC 9(01).
           02  W-MAI          PIC 9(03).
           02  W-MAIC         PIC 9(03).
           02  W-UNA          PIC  N(06).
           02  W-SOK          PIC  N(06).
           02  W-TEK          PIC N(32).                                √∑÷≥
           02  W-TEKD   REDEFINES W-TEK.
             03  W-TEK1       PIC N(09).
             03  W-TEK2       PIC N(23).
           02  W-TEKI.
             03  F            PIC N(04).
             03  W-TEKI2      PIC N(19).
           02  W-UNOM.
             03  W-UNM   OCCURS  8.
               04  W-UM       PIC X(01).
           02  W-UNOD.
             03  W-UND   OCCURS  8.
               04  W-UD       PIC X(01).
           02  W-TELM.
             03  W-TEM   OCCURS 14.
               04  W-TM       PIC X(01).
           02  W-TELD.
             03  W-TED   OCCURS 14.
               04  W-TD       PIC X(01).
      *
           02  W-HCD          PIC  9(06).
           02  W-SIZN         PIC  X(04).
           02  W-SIZND REDEFINES W-SIZN.
             03  W-SIZF       PIC  X(03).
             03  W-SIZR       PIC  X(01).
           02  W-JAN          PIC  X(13).
           02  CNT            PIC  9(02).
           02  W-SCC          PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-CC           PIC  9(002).
           02  W-ASID.
             03  W-ASI   OCCURS   5.
               04  W-SID   OCCURS  10.
                 05  W-SI     PIC  X(004).
           02  W-MSI.
             03  F            PIC  X(040) VALUE
                  "          SS   S   M   L  LL  XL XXL    ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   2.
               04  W-SIZD  OCCURS  27.
                 05  W-SIZ    PIC  X(004).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.025.5".
             03  F            PIC  X(028) VALUE
                  "26.026.527.027.528.029.030.0".
             03  F            PIC  X(040) VALUE
                  "          SS   S   M   L  LL            ".
             03  F            PIC  X(040) VALUE
                  "                                        ".
             03  F            PIC  X(028) VALUE
                  "                  XL XXL    ".
           COPY LSTAT.
      *
           COPY L-JCON.
           COPY LITCM.
           COPY LIHIM.
           COPY L-JNIF-TAM.
           COPY LICODE.
      *FD  WJNIF                                                        ∆Ã¿ﬁƒ◊›
       01  WJNIF_JV010U.
           02  WJNIF_PNAME1   PIC  X(007) VALUE "NIFUDAW".
           02  F              PIC  X(001).
           02  WJNIF_LNAME    PIC  X(012) VALUE "WJNIF_JV010U".
           02  F              PIC  X(001).
           02  WJNIF_KEY1     PIC  X(100) VALUE SPACE.
           02  WJNIF_SORT     PIC  X(100) VALUE SPACE.
           02  WJNIF_IDLST    PIC  X(100) VALUE SPACE.
           02  WJNIF_RES      USAGE  POINTER.
       01  WJNIF-R.
           02  WJNIF-KEY.                                               KEY
               03  WJNIF-01   PIC 9(6).                                 √ﬁ›ÀﬂÆ≥N
               03  WJNIF-02   PIC 9(1).                                 ∑ﬁÆ≥
           02  WJNIF-03       PIC 9(6).                                 À›∫∞ƒﬁ
           02  WJNIF-04       PIC 9(6).                                 À¬ﬁπ
           02  WJNIF-05       PIC 9(7).
           02  WJNIF-06       PIC 9(1).                                 ≥›ø≥
           02  WJNIF-07       PIC 9(1).                                 ø≥∫∞ƒﬁ
           02  WJNIF-08       PIC 9(3).                                 ∫Ω≥
           02  WJNIF-09       PIC 9(3).                                 ª≤ΩﬁÕﬁ¬
           02  WJNIF-SIZ      PIC X(4).                                 ª≤Ωﬁ
           02  WJNIF-JAN      PIC X(13).                                JAN
           02  WJNIF-10       PIC 9(1).                                 ≤›ºﬁª≤›
           02  WJNIF-11       PIC 9(1).                                 ∆≠≥ÿÆ∏
           02  WJNIF-12       PIC 9(1).                                 º≠Ø∂ª≤›
           02  WJNIF-13       PIC 9(3).                                 œ≤Ω≥
           02  WJNIF-13A      PIC 9(1).                                 àÍî ã≥àÁ
           02  WJNIF-15       PIC 9(2).
           02  WJNIF-14       PIC 9(6).                                 µ∏ÿºﬁÆ≥
           02  WJNIF-SUNO     PIC X(8).                                 Åß
           02  WJNIF-SJSU     PIC N(20).                                ºﬁ≠≥ºÆ≥¥
           02  WJNIF-SJSS     PIC N(20).                                ºﬁ≠≥ºÆº¿
           02  WJNIF-SNA      PIC N(26).                                µ∏ÿª∑“≤
           02  WJNIF-STEL     PIC X(14).                                TEL
           02  WJNIF-UNA      PIC N(6).
           02  WJNIF-MUNO     PIC X(8).                                 Åß
           02  WJNIF-MJSU     PIC N(20).                                ºﬁ≠≥ºÆ≥¥
           02  WJNIF-MJSS     PIC N(20).                                ºﬁ≠≥ºÆº¿
           02  WJNIF-MNA      PIC N(26).                                µ∏ÿª∑“≤
           02  WJNIF-MTEL     PIC X(14).                                TEL
           02  WJNIF-SOK      PIC N(6).
           02  WJNIF-HNA      PIC N(24).                                À›∫∞ƒﬁ
           02  WJNIF-TEK      PIC N(32).                                √∑÷≥
           02  WJNIF-BAN      PIC 9(6).                                 NEW NO
           02  WJNIF-GYO      PIC 9(3).                                 NEW NO
       77  F                  PIC X(1).
      *FD  WKEIF                                                        ∆Ã¿ﬁƒ◊›
       01  WKEIF_JV010U.
           02  WKEIF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WKEIF_LNAME    PIC  X(012) VALUE "WKEIF_JV010U".
           02  F              PIC  X(001).
           02  WKEIF_KEY1     PIC  X(100) VALUE SPACE.
           02  WKEIF_SORT     PIC  X(100) VALUE SPACE.
           02  WKEIF_IDLST    PIC  X(100) VALUE SPACE.
           02  WKEIF_RES      USAGE  POINTER.
       01  WKEIF-R.
           02  WKEIF-BAN      PIC 9(6).                                 NEW NO
           02  WKEIF-KEI      PIC 9(4).
           02  F              PIC X(54).
       77  F                  PIC X(1).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                  "ÅñÅñÅñÅ@Å@â◊éDïœä∑ópÉèÅ[ÉNçÏê¨Å@Å@ÅñÅñÅñ".
           02  FILLER.
               03  FILLER  PIC  X(010) VALUE    "ÇPÅ@î≠Å@çs".
           02  FILLER.
               03  FILLER  PIC  X(018) VALUE    "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER  PIC  X(003) VALUE  "[ ]".
           02  FILLER.
               03  FILLER  PIC  X(008) VALUE    "ÇeÇqÇnÇl".
           02  FILLER.
               03  FILLER  PIC  X(004) VALUE    "ÇsÇn".
           02  FILLER.
               03  FILLER  PIC  X(006) VALUE    "ämîFÅi".
               03  FILLER  PIC  X(009) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  X(002) VALUE    "Åj".
               03  FILLER  PIC  X(009) VALUE  "---> ÿ¿∞›".
       01  C-DSP.
           02  D-DNOM.
             03  FILLER  PIC  X(006) VALUE    "ì`ï[áÇ".
             03  FILLER  PIC  X(008) VALUE  "        ".
             03  FILLER  PIC  X(008) VALUE  "        ".
           02  D-DATM.
             03  FILLER  PIC  X(006) VALUE    "ì˙Å@ït".
             03  FILLER  PIC  X(008) VALUE  "  /  /  ".
             03  FILLER  PIC  X(008) VALUE  "  /  /  ".
       01  C-ACP.
           02  A-SEN     PIC 9(01).
           02  A-SDNO    PIC 9(06).
           02  A-EDNO    PIC 9(06).
           02  A-SNGP.
             03  A-SNEN  PIC 9(02).
             03  A-SGET  PIC 9(02).
             03  A-SPEY  PIC 9(02).
           02  A-ENGP.
             03  A-ENEN  PIC 9(02).
             03  A-EGET  PIC 9(02).
             03  A-EPEY  PIC 9(02).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ≈º  ***".
             03  E-ME4   PIC  X(016) VALUE
                  "***  TCM ≈º  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  JCON ø≥∫ ≈º  ***".
             03  E-ME6   PIC  X(022) VALUE
                  "***  JCON ≥›ø≥ ≈º  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  √∑÷≥ ≈º  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  ª≤Ωﬁ ≈º  ***".
             03  E-ME9   PIC  X(021) VALUE
                  "***   ∞Ãª≤Ωﬁ ¥◊∞  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  JCON √ﬁ›ÀﬂÆ≥NO ≈º  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  JCON REWRITE ¥◊∞  ***".
             03  E-TCD   PIC  X(007).
             03  E-HCD   PIC  X(006).
             03  E-KEY   PIC  X(007).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "16" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "7" "0" "10" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "7" "25" "10" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" " " "9" "0" "21" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID" "X" "9" "25" "18" " " "03C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID" "X" "9" "44" "3" "0103C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" " " "14" "0" "8" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID" "X" "14" "21" "8" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" " " "16" "0" "4" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID" "X" "16" "21" "4" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "23" "0" "26" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "X" "23" "41" "6" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
           "0206C-MID" "X" "23" "47" "9" "0106C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0306C-MID" "X" "23" "56" "2" "0206C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0406C-MID" "X" "23" "58" "9" "0306C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNOM" " " "0" "0" "22" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DNOM" "X" "12" "31" "6" " " "D-DNOM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DNOM" "X" "14" "31" "8" "01D-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DNOM" "X" "16" "31" "8" "02D-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATM" " " "0" "0" "22" "D-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATM" "X" "12" "31" "6" " " "D-DATM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATM" "X" "14" "31" "8" "01D-DATM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATM" "X" "16" "31" "8" "02D-DATM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "9" "45" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "14" "31" "6" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDNO" "9" "16" "31" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EDNO" BY REFERENCE W-EDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNGP" " " "14" "0" "6" "A-EDNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "14" "31" "2" " " "A-SNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "14" "34" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "14" "37" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENGP" " " "16" "0" "6" "A-SNGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "31" "2" " " "A-ENGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "34" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "16" "37" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "62" "1" "A-ENGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "238" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "238" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "16" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "21" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "22" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "17" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "21" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "X" "24" "40" "7" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE JNIF1-05 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "X" "24" "49" "6" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE JNIF1-03 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "57" "7" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE JNIF1-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-W FROM ARGUMENT-VALUE.
           IF  JS-W       >  3
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               STOP RUN
           END-IF
           IF  JS-W       =  0
               MOVE  0       TO  JS-SIGN
           END-IF
           IF  JS-W       =  2
               MOVE  1       TO  JS-SIGN
           END-IF
           IF  JS-W       =  3
               MOVE  3       TO  JS-SIGN
           END-IF.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           MOVE W-MSIZ TO W-ASIZD.
           MOVE W-MSI TO W-ASID.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO WKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" WJNIF_PNAME1 " " BY REFERENCE WJNIF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" WKEIF_PNAME1 " " BY REFERENCE WKEIF_IDLST "0".
       M-10.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JNIF1-02 = 7
               GO TO M-10
           END-IF
           IF  W-SEN = 1
               IF  JNIF1-10  = 1
                   GO TO M-10
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  JNIF1-10  = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  JNIF1-01 < W-SDNO OR > W-EDNO
               GO TO M-10
           END-IF
           IF  JS-SIGN  =  0
               IF  JNIF1-07    NOT =  1
                   GO  TO  M-10
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JNIF1-07    NOT =  6
                   GO  TO  M-10
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JNIF1-07    NOT =  7
                   GO  TO  M-10
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JNIF1-07    NOT =  4
                   GO  TO  M-10
               END-IF
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE 20 TO W-NEN1.
           MOVE JNIF1-04 TO W-NGPS.
           IF  W-NGP < W-SNGP OR > W-ENGP
               GO TO M-10
           END-IF.
       M-15.
           MOVE JNIF1-KEY TO W-NNO.
           MOVE JNIF1-14 TO W-OKJ.
           MOVE JNIF1-08 TO W-KSU.
           MOVE JNIF1-13 TO W-MAI.
           MOVE ZERO TO W-MAIC.
      *
           MOVE JNIF1-05 TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME TC-JSU TC-JSS TC-UNO TC-TEL
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           PERFORM SPC-RTN THRU SPC-EX.
      *
           MOVE 2 TO JCON2-01.
           MOVE JNIF1-06 TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON2-03
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE JCON2-03 TO W-UNA.
      *
           MOVE 3 TO JCON3-01.
           MOVE JNIF1-07 TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE JCON3-03 TO W-SOK.
           IF  JNIF1-07    =   1
               IF  JNIF1-051   =   2654  OR  2656  OR  2657  OR  4038
                                         OR  4054  OR  4056  OR  4057
                   MOVE        "ïxÅ@émÅ@Å@Å@"  TO       W-SOK
               END-IF
           END-IF
           MOVE W-NNO TO JNIF2-KEY.
           MOVE 7 TO JNIF2-02.
      *           READ JNIF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JNIF2-02A JNIF2-03
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE JNIF2-02A TO W-TEK1.
           MOVE JNIF2-03 TO W-TEKI.
           MOVE SPACE TO W-TEK2.
           MOVE W-TEKI2 TO W-TEK2.
      *
           MOVE W-NNO TO JNIF1-KEY.
      *           READ JNIF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-13 = 1
               MOVE W-NNO1 TO W-BAN
               MOVE ZERO TO W-GYO W-KEI
               MOVE 0 TO W-SET
               GO TO M-25
           END-IF
           MOVE 1 TO W-SET.
       M-20.
           MOVE 13 TO JCON1-KEY.
      *           READ JCON INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE JCON1-06 TO W-BAN.
           ADD 1 TO W-BAN.
           IF  W-BAN = ZERO
               MOVE 900001 TO W-BAN
           END-IF
           MOVE W-BAN TO JCON1-06.
      *           REWRITE JCON1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO W-GYO W-KEI.
       M-25.
           MOVE JNIF1-KEY TO W-KEY.
           MOVE JNIF1-03 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME HI-SMS
               MOVE 0 TO HI-HKB
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT > 27
               GO TO M-40
           END-IF
           IF  JNIF1-091(CNT) = ZERO
               GO TO M-30
           END-IF
           IF  HI-SSC = 0
               MOVE W-SIZ(1,CNT) TO W-SIZN
           ELSE
               MOVE W-SIZ(2,CNT) TO W-SIZN
           END-IF
           PERFORM JAN-RTN THRU JAN-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           IF  HI-HKB = 1
               IF  W-SC = 2 OR 3
                   MOVE 5 TO W-SIZR
               END-IF
           END-IF
      *
           INITIALIZE WJNIF-R.
           MOVE SPACE TO WJNIF-MJSU WJNIF-MJSS WJNIF-MNA.
           MOVE W-BAN TO WJNIF-BAN.
           ADD 1 TO W-GYO.
           MOVE W-GYO TO WJNIF-GYO.
           MOVE W-NO TO WJNIF-01.
           MOVE JNIF1-02  TO WJNIF-02.
           MOVE JNIF1-03 TO WJNIF-03.
           MOVE JNIF1-04 TO WJNIF-04.
           MOVE JNIF1-05 TO WJNIF-05.
           MOVE JNIF1-06 TO WJNIF-06.
           MOVE JNIF1-07 TO WJNIF-07.
           MOVE W-KSU TO WJNIF-08.
           MOVE JNIF1-091(CNT) TO WJNIF-09.
           MOVE W-SIZN TO WJNIF-SIZ.
           MOVE W-JAN TO WJNIF-JAN.
           MOVE JNIF1-10 TO WJNIF-10.
           MOVE JNIF1-11 TO WJNIF-11.
           MOVE JNIF1-12 TO WJNIF-12.
           MOVE JNIF1-13 TO WJNIF-13.
           MOVE JNIF1-13A TO WJNIF-13A.
           MOVE JNIF1-15 TO WJNIF-15.
           MOVE JNIF1-14 TO WJNIF-14.
           MOVE W-UNOD TO WJNIF-SUNO.
           MOVE TC-JSU TO WJNIF-SJSU.
           MOVE TC-JSS TO WJNIF-SJSS.
           MOVE TC-NAME TO WJNIF-SNA.
           MOVE W-TELD TO WJNIF-STEL.
           MOVE W-UNA TO WJNIF-UNA.
           MOVE W-SOK TO WJNIF-SOK.
           IF  HI-SMS = SPACE
               MOVE HI-NAME TO WJNIF-HNA
           ELSE
               MOVE HI-SMS TO WJNIF-HNA
           END-IF
           MOVE W-TEK TO WJNIF-TEK.
           IF  JNIF1-051       =  4038  OR  4054  OR  4056  OR  4057
               MOVE  "542-0081"       TO  WJNIF-MUNO
               MOVE    "ëÂç„ésíÜâõãÊìÏëDèÍÇPí¨ñ⁄ÇPÇPÅ|ÇXÅ@Å@Å@Å@"
                                                         TO WJNIF-MJSU
               MOVE   "êÁã»éYã∆äîéÆâÔé–Å@Å@Å@Å@Å@Å@Å@Å@" TO WJNIF-MNA
               MOVE  "  06-6268-4561" TO  WJNIF-MTEL
               GO  TO M-35
           END-IF
           IF  JNIF1-051       =  2654  OR  2656  OR  2657
               MOVE  "520-2132"       TO  WJNIF-MUNO
               MOVE    "é†âÍåßëÂí√ésê_óÃÇPÅ|ÇXÅ|ÇRÅ@" TO  WJNIF-MJSU
               MOVE    "äîéÆâÔé–êVì˙ñ{ã≥àÁÉVÉÖÅ[ÉYÅ@" TO  WJNIF-MNA
               MOVE  "  077-543-1331" TO  WJNIF-MTEL
               GO  TO M-35
           END-IF
           IF  JNIF1-052   NOT =  001
               IF  JNIF1-051       =  0459
                   MOVE  "663-8033"       TO  WJNIF-MUNO
                   MOVE    "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO  WJNIF-MJSU
                   MOVE    "äîéÆâÔé–è„éRõâã∆Å@Å@" TO  WJNIF-MNA
                   MOVE  "  0798-64-5111" TO  WJNIF-MTEL
                   GO  TO M-35
               END-IF
           END-IF
           IF  JNIF1-052   NOT =  001
               IF  JNIF1-051       =  0460
                   MOVE  "663-8033"       TO  WJNIF-MUNO
                   MOVE    "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO  WJNIF-MJSU
                   MOVE    "äîéÆâÔé–ÉEÉGÉÑÉ}Å@Å@" TO  WJNIF-MNA
                   MOVE  "  0798-67-0810" TO  WJNIF-MTEL
                   GO  TO M-35
               END-IF
           END-IF
           IF  JNIF1-051       =  3015
               MOVE  "114-0034"       TO  WJNIF-MUNO
               MOVE    "ìåãûìsñkãÊè„è\èÇSÅ|ÇPÅ|ÇTÅ@" TO  WJNIF-MJSU
               MOVE    "äîéÆâÔé–ÉXÉMÉÑÉ}Å@Å@" TO  WJNIF-MNA
               MOVE  "  03-3909-8883" TO  WJNIF-MTEL
               GO  TO M-35
           END-IF
           IF  JS-SIGN         =  0
               MOVE  "700-0975"       TO  WJNIF-MUNO
               MOVE    "â™éRésñkãÊç°ÇWíöñ⁄ÇPÇUÅ|ÇPÇV" TO  WJNIF-MJSU
               MOVE    "ì˙êiÉSÉÄäîéÆâÔé–Å@Å@" TO  WJNIF-MNA
           ELSE
               IF  JS-SIGN         =  1
                   MOVE  "713-8103"       TO  WJNIF-MUNO
                   MOVE    "â™éRåßëqï~ésã ìáâ≥ìáéöêVñ©ÇWÇQÇUÇQÅ|ÇP"
                                                      TO  WJNIF-MJSU
                   MOVE    "Å@Å@Å@Å@Å@êÖìáç`çëç€ï®ó¨ÉZÉìÉ^Å[ì‡"
                                                      TO  WJNIF-MJSS
                   MOVE    "ì˙êiÉSÉÄáäÅ@ã ìáï®ó¨ÉZÉìÉ^Å["
                                                      TO  WJNIF-MNA
               ELSE
                   IF  JS-SIGN         =  2
                   MOVE  "709-3717"       TO  WJNIF-MUNO
                   MOVE    "â™éRåßãvïƒåSÅ@î¸çÁí¨å¥ìcÇRÇQÇQÇVÅ|ÇPÅ@"
                                                      TO  WJNIF-MJSU
                   MOVE    "ì˙êiÉSÉÄáäÅ@í√éRï®ó¨ÉZÉìÉ^Å["
                                                      TO  WJNIF-MNA
                   ELSE
                   IF  JS-SIGN         =  3
                   MOVE  "701-0304"       TO  WJNIF-MUNO
                   MOVE    "â™éRåßìsåEåSÅ@ëÅìáí¨ëÅìáÇSÇTÇOÇVÅ|ÇRÇV"
                                                      TO  WJNIF-MJSU
                   MOVE    "Å@Å@Å@óºîıÇgÇcáäÅ@íÜélçëï®ó¨ÉZÉìÉ^Å[ì‡"
                                                      TO  WJNIF-MJSS
                   MOVE    "ì˙êiÉSÉÄáäÅ@ëÅìáîzëóÉZÉìÉ^Å[Å@"
                                                      TO  WJNIF-MNA
                   END-IF
                   END-IF
               END-IF
           END-IF
           MOVE  "  086-243-2456" TO  WJNIF-MTEL.
       M-35.
      *           WRITE WJNIF-R.
      *//////////////
           CALL "DB_Insert" USING
            WJNIF_PNAME1 WJNIF_LNAME WJNIF-R RETURNING RET.
           ADD JNIF1-091(CNT) TO W-KEI.
           GO TO M-30.
       M-40.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  JNIF1-02 = 7
               GO TO M-40
           END-IF
      *
           IF  W-SEN = 1
               IF  JNIF1-10  = 1
                   GO TO M-40
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  JNIF1-10  = 0
                   GO TO M-40
               END-IF
           END-IF
           IF  JNIF1-01 < W-SDNO OR > W-EDNO
               GO TO M-40
           END-IF
           IF  JS-SIGN  =  0
               IF  JNIF1-07    NOT =  1
                   GO  TO  M-40
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JNIF1-07    NOT =  6
                   GO  TO  M-40
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JNIF1-07    NOT =  7
                   GO  TO  M-40
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JNIF1-07    NOT =  4
                   GO  TO  M-40
               END-IF
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE 20 TO W-NEN1.
           MOVE JNIF1-04 TO W-NGPS.
           IF  W-NGP < W-SNGP OR > W-ENGP
               GO TO M-40
           END-IF
      *
           IF  JNIF1-01 = W-NO
               GO TO M-25
           END-IF
           IF  JNIF1-14 = W-OKJ
               IF  W-KSU < 2
                   GO TO M-25
               END-IF
           END-IF
           MOVE SPACE TO WKEIF-R.
           MOVE W-BAN TO WKEIF-BAN.
           MOVE W-KEI TO WKEIF-KEI.
      *           WRITE WKEIF-R.
      *//////////////
           CALL "DB_Insert" USING
            WKEIF_PNAME1 WKEIF_LNAME WKEIF-R RETURNING RET.
           IF  W-SET = 0
               GO TO M-15
           END-IF
      *
           ADD 1 TO W-MAIC.
           IF  W-MAI NOT > W-MAIC
               GO TO M-15
           END-IF
           MOVE W-NNO TO JNIF1-KEY.
      *           READ JNIF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-20.
       M-50.
           MOVE SPACE TO WKEIF-R.
           MOVE W-BAN TO WKEIF-BAN.
           MOVE W-KEI TO WKEIF-KEI.
      *           WRITE WKEIF-R.
      *//////////////
           CALL "DB_Insert" USING
            WKEIF_PNAME1 WKEIF_LNAME WKEIF-R RETURNING RET.
           IF  W-SET = 0
               GO TO M-90
           END-IF
      *
           ADD 1 TO W-MAIC.
           IF  W-MAI NOT > W-MAIC
               GO TO M-90
           END-IF
           MOVE W-NNO TO JNIF1-KEY.
      *           READ JNIF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WJNIF_IDLST WJNIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WKEIF_IDLST WKEIF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SEN NOT = 1 AND 2
               GO TO ACP-RTN
           END-IF
           IF  W-SEN = 2
               MOVE ZERO TO W-SNGP
               MOVE 99999999 TO W-ENGP
               CALL "SD_Output" USING "D-DNOM" D-DNOM "p" RETURNING RESU
               GO TO ACP-100
           END-IF
           PERFORM SEL-RTN THRU SEL-EX.
           IF  W-SNGP = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           CALL "SD_Output" USING "D-DATM" D-DATM "p" RETURNING RESU.
           MOVE ZERO TO W-SDNO.
           MOVE 999999 TO W-EDNO.
           GO TO ACP-900.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO ACP-120
           END-IF
           GO TO ACP-900.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-200
           END-IF
           MOVE 20 TO W-SNEN1.
       ACP-220.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-200
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-220
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO ACP-220
           END-IF.
       ACP-240.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-220
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-240
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO ACP-240
           END-IF
           IF  W-SNGP < W-SDATE OR > W-EDATE
               GO TO ACP-200
           END-IF.
       ACP-260.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-240
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-260
           END-IF
           MOVE 20 TO W-ENEN1.
       ACP-280.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-280
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO ACP-280
           END-IF.
       ACP-300.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-280
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-300
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO ACP-300
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO ACP-260
           END-IF
           IF  W-ENGP < W-SDATE OR > W-EDATE
               GO TO ACP-260
           END-IF.
       ACP-900.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  W-SEN = 1
                   GO TO ACP-200
               ELSE
                   GO TO ACP-120
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-900
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-900
           END-IF.
       ACP-EX.
           EXIT.
       SEL-RTN.
           MOVE  ZERO  TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
       SEL-010.
      *           READ  JNIF  NEXT  RECORD  WITH UNLOCK AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  JNIF1-10 = 1
               GO  TO  SEL-010
           END-IF
           IF  JNIF1-02    =    7
               GO  TO  SEL-010
           END-IF
           IF  JS-SIGN  =  0
               IF  JNIF1-07    NOT =  1
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JNIF1-07    NOT =  6
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JNIF1-07    NOT =  7
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JNIF1-07    NOT =  4
                   GO  TO  SEL-010
               END-IF
           END-IF
           MOVE  ZERO     TO  W-NGP.
           MOVE  JNIF1-04  TO  W-NGPS.
           MOVE  20       TO  W-NEN1.
           IF  W-SDATE = ZERO
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-SDATE > W-NGP
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-EDATE < W-NGP
               MOVE  W-NGP  TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           MOVE  W-SDATE  TO  W-SNGP.
           MOVE  W-EDATE  TO  W-ENGP.
       SEL-EX.
           EXIT.
       JAN-RTN.
           MOVE ZERO TO W-SC W-JAN.
       JAN-020.
           ADD 1 TO W-SC.
           IF  W-SC > 5
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO JAN-EX
           END-IF
           MOVE ZERO TO W-C.
       JAN-030.
           ADD 1 TO W-C.
           IF  W-C > 10
               GO TO JAN-020
           END-IF
           IF  W-SIZN NOT = W-SI(W-SC,W-C)
               GO TO JAN-030
           END-IF
           IF  W-SC > 1
               COMPUTE W-SCC = W-SC - 1
           ELSE
               MOVE W-SC TO W-SCC
           END-IF
           IF  HI-S(W-SCC,W-C) = 0
               GO TO JAN-030
           END-IF
           MOVE SPACE TO CODE-KEY2.
           MOVE JNIF1-03 TO CODE-HCD20.
           MOVE W-SCC TO CODE-SIZ2.
           MOVE W-C TO CODE-SNO2.
      *           START CODEF KEY NOT < CODE-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY2" " NOT < " CODE-KEY2 RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-JAN
               GO TO JAN-090
           END-IF.
       JAN-040.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-JAN
               GO TO JAN-090
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO JAN-040
           END-IF
           IF (JNIF1-03 NOT = CODE-HCD20) OR
              (W-SCC NOT = CODE-SIZ2) OR
              (W-C NOT = CODE-SNO2)
               MOVE ZERO TO CODE-JAN
               GO TO JAN-090
           END-IF.
       JAN-090.
           MOVE CODE-JAN TO W-JAN.
       JAN-EX.
           EXIT.
       SPC-RTN.
           MOVE SPACE TO W-UNOM W-UNOD.
           MOVE TC-UNO TO W-UNOM.
           IF  W-UNOM = SPACE
               GO TO SPC-110
           END-IF
           IF  W-UM(8) NOT = SPACE
               MOVE W-UNOM TO W-UNOD
               GO TO SPC-110
           END-IF
           MOVE 9 TO W-C W-CC.
       SPC-010.
           SUBTRACT 1 FROM W-C.
           IF  W-C = ZERO
               GO TO SPC-110
           END-IF
           IF  W-UM(W-C) = SPACE
               GO TO SPC-010
           END-IF
           SUBTRACT 1 FROM W-CC.
       SPC-020.
           MOVE W-UM(W-C) TO W-UD(W-CC).
           SUBTRACT 1 FROM W-C W-CC.
           IF  W-C NOT = ZERO
               GO TO SPC-020
           END-IF.
       SPC-110.
           MOVE SPACE TO W-TELM W-TELD.
           MOVE TC-TEL TO W-TELM.
           IF  W-TELM = SPACE
               GO TO SPC-EX
           END-IF
           IF  W-TM(14) NOT = SPACE
               MOVE W-TELM TO W-TELD
               GO TO SPC-EX
           END-IF
           MOVE 15 TO W-C W-CC.
       SPC-120.
           SUBTRACT 1 FROM W-C.
           IF  W-C = ZERO
               GO TO SPC-EX
           END-IF
           IF  W-TM(W-C) = SPACE
               GO TO SPC-120
           END-IF
           SUBTRACT 1 FROM W-CC.
       SPC-130.
           MOVE W-TM(W-C) TO W-TD(W-CC).
           SUBTRACT 1 FROM W-C W-CC.
           IF  W-C NOT = ZERO
               GO TO SPC-130
           END-IF.
       SPC-EX.
           EXIT.
