       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKT210.
      ********************************************************
      *****     得意先別　売掛残高　問合せ（入金チェック）****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-SHC          PIC  X(011).
           02  W-SHC23 REDEFINES W-SHC.
             03  W-SHC23F     PIC  X(004).
             03  F            PIC  X(001).
             03  W-SHC23R     PIC  X(006).
           02  W-SHC32 REDEFINES W-SHC.
             03  W-SHC32F     PIC  X(006).
             03  F            PIC  X(001).
             03  W-SHC32R     PIC  X(004).
           02  W-SGT          PIC  N(002).
           02  W-STT          PIC  N(002).
           02  W-GPD.
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-D.
             03  W-UKIN       PIC S9(009).
             03  W-USHZ       PIC S9(007).
             03  W-ZKIN       PIC S9(009).
             03  W-ZSHZ       PIC S9(007).
             03  W-ZKIND      PIC S9(009).
             03  W-ZSHZD      PIC S9(007).
           02  W-DNO          PIC  X(006).
           02  W-CHK          PIC  X(001).
           02  W-KMN          PIC  N(002).
           02  W-DMM          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           02  W-HKNG.
             03  W-HSNG.
               04  W-HSN      PIC  9(004).
               04  W-HSG      PIC  9(002).
             03  W-HENG.
               04  W-HEN      PIC  9(004).
               04  W-HEG      PIC  9(002).
             03  W-KSNG.
               04  W-KSN      PIC  9(004).
               04  W-KSG      PIC  9(002).
             03  W-KENG.
               04  W-KEN      PIC  9(004).
               04  W-KEG      PIC  9(002).
           02  W-SPACE        PIC  X(078).
           02  W-R.
             03  WR-KEY2.
               04  WR-NTCD    PIC  9(004).
               04  WR-KEY.
                 05  WR-TCD   PIC  9(004).
             03  WR-NAME      PIC  N(026).
             03  F            PIC  X(116).
             03  WR-FKC       PIC  9(002).
             03  WR-BC        PIC  9(001).
             03  WR-TKC       PIC  9(002).
             03  WR-TNC       PIC  9(002).
             03  WR-SS        PIC  9(002).
             03  WR-NKY       PIC  9(003).
             03  F            PIC  X(219).
             03  WR-SHD       PIC  9(002).
             03  WR-SSI       PIC  9(003).
             03  WR-SHC1      PIC  9(001).
             03  WR-SHC2      PIC  9(001).
             03  WR-SGT       PIC  9(001).
             03  WR-SGR       PIC  9(001)V9(01).
             03  WR-STT       PIC  9(001).
             03  WR-STR       PIC  9(001)V9(01).
             03  WR-SKR       PIC  9(004).
             03  F            PIC  X(088).
           02  W-TCDW         PIC  9(004).
           02  W-UZ           PIC S9(009).
           02  W-WTCD.
             03  W-WTCD1      PIC  9(004).
             03  W-WTCD2      PIC  9(004).
             03  W-WTCD3      PIC  9(004).
             03  W-WTCD4      PIC  9(004).
             03  W-WTCD5      PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITUKW.
           COPY LITTM.
           COPY LIHKBM.
           COPY LITM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  A-GP    PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNM   PIC  N(014).
           02  D-NAME  PIC  N(026).
           02  D-SHJC  PIC  X(080).
           02  D-SHJ.
             03  D-SS.
               04  FILLER  PIC  N(002) VALUE "締日".
               04  FILLER  PIC  Z(002).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SHD.
               04  FILLER  PIC  N(003) VALUE "支払日".
               04  FILLER  PIC  Z(002).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SSI.
               04  FILLER  PIC  N(003) VALUE "サイト".
               04  FILLER  PIC  Z(003).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SHC.
               04  FILLER  PIC  X(011).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SG.
               04  FILLER  PIC  N(003) VALUE "現金引".
               04  FILLER  PIC  N(002).
               04  FILLER  PIC 9.9 .
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-ST.
               04  FILLER  PIC  N(003) VALUE "手数料".
               04  FILLER  PIC  N(002).
               04  FILLER  PIC 9.9 .
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SKR.
               04  FILLER  PIC  N(003) VALUE "送金料".
               04  FILLER  PIC  Z(004).
           02  FILLER.
             03  D-DATA.
               04  FILLER  PIC Z9 .
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  X(006).
               04  FILLER  PIC  N(002).
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,--- .
               04  FILLER  PIC ZZ .
               04  FILLER  PIC ZZ .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,--- .
             03  D-SGP.
               04  FILLER  PIC  X(004)      VALUE "****".
           02  FILLER.
             03  D-WNKC  PIC  X(039) VALUE
                  "                                       ".
             03  D-WNK.
               04  FILLER  PIC  X(001) VALUE "(".
               04  FILLER  PIC  N(006) VALUE "売掛残高有り".
               04  FILLER  PIC  X(001) VALUE ",".
               04  FILLER  PIC  X(001) VALUE ",".
               04  FILLER  PIC  X(001) VALUE ",".
               04  FILLER  PIC  X(001) VALUE ",".
               04  FILLER  PIC  X(001) VALUE ")".
             03  D-WTCD1 PIC  9(004).
             03  D-WTCD2 PIC  9(004).
             03  D-WTCD3 PIC  9(004).
             03  D-WTCD4 PIC  9(004).
             03  D-WTCD5 PIC  9(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  DATA ﾅｼ  **".
             03  E-ME2   PIC  X(020) VALUE
                  "**  得意先　なし  **".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
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
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "2" "8" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GP" "9" "6" "6" "4" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GP" BY REFERENCE W-GPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "76" "1" "A-GP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "379" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNM" "N" "1" "53" "28" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNM" BY REFERENCE HKB-TNNA "28" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "2" "13" "52" "D-TNM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE WR-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHJC" "X" "3" "1" "80" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHJC" BY REFERENCE W-SPACE "78" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHJ" " " "3" "0" "76" "D-SHJC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SS" " " "3" "0" "7" " " "D-SHJ" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SS" "N" "3" "3" "4" " " "D-SS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SS" "Z" "3" "7" "2" "01D-SS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SS" BY REFERENCE WR-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SS" "X" "3" "9" "1" "02D-SS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHD" " " "3" "0" "9" "D-SS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHD" "N" "3" "10" "6" " " "D-SHD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHD" "Z" "3" "16" "2" "01D-SHD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SHD" BY REFERENCE WR-SHD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHD" "X" "3" "18" "1" "02D-SHD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSI" " " "3" "0" "10" "D-SHD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SSI" "N" "3" "19" "6" " " "D-SSI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSI" "Z" "3" "25" "3" "01D-SSI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SSI" BY REFERENCE WR-SSI "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SSI" "X" "3" "28" "1" "02D-SSI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHC" " " "3" "0" "12" "D-SSI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHC" "X" "3" "29" "11" " " "D-SHC" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SHC" BY REFERENCE W-SHC "11" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHC" "X" "3" "40" "1" "01D-SHC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SG" " " "3" "0" "14" "D-SHC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SG" "N" "3" "41" "6" " " "D-SG" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SG" "N" "3" "48" "4" "01D-SG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SG" BY REFERENCE W-SGT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SG" "9.9" "3" "52" "3" "02D-SG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SG" BY REFERENCE WR-SGR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SG" "X" "3" "55" "1" "03D-SG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ST" " " "3" "0" "14" "D-SG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ST" "N" "3" "56" "6" " " "D-ST" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ST" "N" "3" "63" "4" "01D-ST" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-ST" BY REFERENCE W-STT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ST" "9.9" "3" "67" "3" "02D-ST" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-ST" BY REFERENCE WR-STR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ST" "X" "3" "70" "1" "03D-ST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKR" " " "3" "0" "10" "D-ST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SKR" "N" "3" "71" "6" " " "D-SKR" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SKR" "Z" "3" "77" "4" "01D-SKR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SKR" BY REFERENCE WR-SKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-L" "0" "66" "D-SHJ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "62" " " "05C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "Z9" "W-L" "6" "2" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE TUK-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "Z9" "W-L" "8" "2" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE TUK-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA" "X" "W-L" "11" "6" "02D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA" "N" "W-L" "18" "4" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA" BY REFERENCE W-KMN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DATA" "----,---,---" "W-L" "23" "12" "04D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DATA" BY REFERENCE W-UKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-DATA" "--,---,---" "W-L" "36" "10" "05D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-DATA" BY REFERENCE W-USHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-DATA" "ZZ" "W-L" "47" "2" "06D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-DATA" BY REFERENCE TUK-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-DATA" "ZZ" "W-L" "49" "2" "07D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-DATA" BY REFERENCE TUK-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-DATA" "----,---,---" "W-L" "52" "12" "08D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09D-DATA" BY REFERENCE W-ZKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-DATA" "--,---,---" "W-L" "65" "10" "09D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10D-DATA" BY REFERENCE W-ZSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SGP" " " "W-L" "0" "4" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SGP" "X" "W-L" "47" "4" " " "D-SGP" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "23" "0" "77" "05C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WNKC" "X" "23" "16" "39" " " "06C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WNK" " " "23" "0" "18" "D-WNKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-WNK" "X" "23" "11" "1" " " "D-WNK" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-WNK" "N" "23" "12" "12" "01D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-WNK" "X" "23" "29" "1" "02D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-WNK" "X" "23" "34" "1" "03D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-WNK" "X" "23" "39" "1" "04D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-WNK" "X" "23" "44" "1" "05D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-WNK" "X" "23" "49" "1" "06D-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WTCD1" "9" "23" "25" "4" "D-WNK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-WTCD1" BY REFERENCE W-WTCD1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WTCD2" "9" "23" "30" "4" "D-WTCD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-WTCD2" BY REFERENCE W-WTCD2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WTCD3" "9" "23" "35" "4" "D-WTCD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-WTCD3" BY REFERENCE W-WTCD3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WTCD4" "9" "23" "40" "4" "D-WTCD3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-WTCD4" BY REFERENCE W-WTCD4 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WTCD5" "9" "23" "45" "4" "D-WTCD4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-WTCD5" BY REFERENCE W-WTCD5 "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "95" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "20" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           INITIALIZE W-DATA.
           COPY LIBCPR.
      *
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-HENG.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-NEN.
           MOVE W-NG TO W-HSNG.
      *
           MOVE ZERO TO W-NGP.
           MOVE D-NKNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-KENG.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-NEN.
           MOVE W-NG TO W-KSNG.
      *
           CALL "DB_F_Open" USING
            "INPUT" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "1"
            "TUK-KEY" BY REFERENCE TUK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK21" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE T-R TO W-R.
           PERFORM S-05 THRU S-25.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE 04 TO HKB-NO.
           MOVE T-TNC TO HKB-TNC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-TNNA
           END-IF
           CALL "SD_Output" USING "D-TNM" D-TNM "p" RETURNING RESU.
      *
           MOVE SPACE TO TUK-KEY.
           MOVE W-TCD TO TUK-TCD.
      *           START TUKF KEY NOT < TUK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TUKF_PNAME1 "TUK-KEY" " NOT < " TUK-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF.
       M-20.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  W-TCD NOT = TUK-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-20
           END-IF
      *
           PERFORM S-30 THRU S-45.
           IF  W-WTCD1 NOT = ZERO
               CALL "SD_Output" USING "D-WNK" D-WNK "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-WTCD1" D-WTCD1 "p" RETURNING RESU
           END-IF
           IF  W-WTCD2 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD2" D-WTCD2 "p" RETURNING RESU
           END-IF
           IF  W-WTCD3 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD3" D-WTCD3 "p" RETURNING RESU
           END-IF
           IF  W-WTCD4 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD4" D-WTCD4 "p" RETURNING RESU
           END-IF
           IF  W-WTCD5 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD5" D-WTCD5 "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO W-D.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-GP "A-GP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-GETD > 12
               GO TO M-25
           END-IF
           IF  W-PEYD > 31
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-NGP.
           MOVE W-GETD TO W-GET.
           MOVE W-PEYD TO W-PEY.
           IF  W-GPD = ZERO
               IF  WR-BC = 0
                   MOVE W-HSNG TO W-NG
                   GO TO M-30
               ELSE
                   MOVE W-KSNG TO W-NG
                   GO TO M-30
               END-IF
           END-IF
           IF  WR-BC = 0
               MOVE W-HEN TO W-NEN
               IF  W-HEG < W-GETD
                   SUBTRACT 1 FROM W-NEN
               END-IF
           END-IF
           IF  WR-BC NOT = 0
               MOVE W-KEN TO W-NEN
               IF  W-KEG < W-GETD
                   SUBTRACT 1 FROM W-NEN
               END-IF
           END-IF.
       M-30.
           IF  TUK-NG < W-NG
               GO TO M-35
           END-IF
           GO TO M-40.
       M-35.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  W-TCD NOT = TUK-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-35
           END-IF
           GO TO M-30.
       M-40.
           MOVE "前残" TO W-KMN.
           MOVE SPACE TO W-CHK W-DNO.
           IF  TUK-DC = 0
               MOVE ZERO TO W-UKIN W-USHZ
               MOVE TUK-KIN TO W-ZKIN
               MOVE TUK-SHZ TO W-ZSHZ
           END-IF
           IF  TUK-DC = 1
               ADD TUK-KIN TO W-ZKIN
               ADD TUK-SHZ TO W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE "売上" TO W-KMN
                   MOVE TUK-DNO TO W-DNO
                   MOVE TUK-KIN TO W-UKIN
                   MOVE TUK-SHZ TO W-USHZ
               END-IF
           END-IF
           IF  TUK-DC = 2
               SUBTRACT TUK-KIN FROM W-ZKIN
               SUBTRACT TUK-SHZ FROM W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE "値引" TO W-KMN
                   MOVE TUK-DNO TO W-DNO
                   COMPUTE W-UKIN = TUK-KIN * -1
                   COMPUTE W-USHZ = TUK-SHZ * -1
               END-IF
           END-IF
           IF  TUK-DC = 3
               SUBTRACT TUK-KIN FROM W-ZKIN
               SUBTRACT TUK-SHZ FROM W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE "入金" TO W-KMN
                   MOVE TUK-DNO TO W-DNO
                   COMPUTE W-UKIN = TUK-KIN * -1
                   COMPUTE W-USHZ = TUK-SHZ * -1
               END-IF
           END-IF
           IF  TUK-DC = 4
               MOVE "請求" TO W-KMN
               MOVE TUK-DNO TO W-DNO
               MOVE ZERO TO W-UKIN W-USHZ
               MOVE W-ZKIN TO W-ZKIND
               MOVE W-ZSHZ TO W-ZSHZD
               MOVE TUK-KIN TO W-ZKIN
               MOVE TUK-SHZ TO W-ZSHZ
           END-IF
           IF  W-DC = 0
               IF  TUK-NGP < W-NGP
                   GO TO M-47
               END-IF
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       M-45.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               MOVE 1 TO W-NC
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
           IF  TUK-SKD = 99999999
               CALL "SD_Output" USING "D-SGP" D-SGP "p" RETURNING RESU
           END-IF.
       M-47.
           IF  TUK-DC = 4
               MOVE W-ZKIND TO W-ZKIN
               MOVE W-ZSHZD TO W-ZSHZ
           END-IF.
       M-50.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 2 TO W-NC
               GO TO M-55
           END-IF
           IF  W-TCD NOT = TUK-TCD
               MOVE 2 TO W-NC
               GO TO M-55
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-50
           END-IF
           GO TO M-40.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
                GO TO M-55
           END-IF
           IF  W-NC = 2
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 0
               GO TO M-55
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK21" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           PERFORM S-05 THRU S-25.
           CALL "SD_Output" USING "D-TNM" D-TNM "p" RETURNING RESU.
           IF  W-WTCD1 NOT = ZERO
               CALL "SD_Output" USING "D-WNK" D-WNK "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-WTCD1" D-WTCD1 "p" RETURNING RESU
           END-IF
           IF  W-WTCD2 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD2" D-WTCD2 "p" RETURNING RESU
           END-IF
           IF  W-WTCD3 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD3" D-WTCD3 "p" RETURNING RESU
           END-IF
           IF  W-WTCD4 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD4" D-WTCD4 "p" RETURNING RESU
           END-IF
           IF  W-WTCD5 NOT = ZERO
               CALL "SD_Output" USING
                "D-WTCD5" D-WTCD5 "p" RETURNING RESU
           END-IF
           GO TO M-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHJC" D-SHJC "p" RETURNING RESU.
           IF  WR-SS NOT = ZERO
               CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU
           END-IF
           IF  WR-SHD NOT = ZERO
               CALL "SD_Output" USING "D-SHD" D-SHD "p" RETURNING RESU
           END-IF
           IF  WR-SSI NOT = ZERO
               CALL "SD_Output" USING "D-SSI" D-SSI "p" RETURNING RESU
           END-IF
           IF  WR-SHC1 = ZERO
               GO TO S-10
           END-IF
           MOVE SPACE TO W-SHC
           IF  WR-SHC1 = 1
               MOVE "現金" TO W-SHC23F
               IF  WR-SHC2 = 2
                   MOVE "小切手" TO W-SHC23R
               ELSE
                   IF  WR-SHC2 = 3
                       MOVE "手形" TO W-SHC23R
                   END-IF
               END-IF
           END-IF
           IF  WR-SHC1 = 2
               MOVE "小切手" TO W-SHC32F
               IF  WR-SHC2 = 1
                   MOVE "現金" TO W-SHC32R
               ELSE
                   IF  WR-SHC2 = 3
                       MOVE "手形" TO W-SHC32R
                   END-IF
               END-IF
           END-IF
           IF  WR-SHC1 = 3
               MOVE "手形" TO W-SHC23F
               IF  WR-SHC2 = 1
                   MOVE "現金" TO W-SHC23R
               ELSE
                   IF  WR-SHC2 = 2
                       MOVE "小切手" TO W-SHC23R
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SHC" D-SHC "p" RETURNING RESU.
       S-10.
           IF  WR-SGT = 0
               GO TO S-15
           END-IF
           MOVE SPACE TO W-SGT.
           IF  WR-SGT = 1
               MOVE "全体" TO W-SGT
           END-IF
           IF  WR-SGT = 2
               MOVE "商品" TO W-SGT
           END-IF
           CALL "SD_Output" USING "D-SG" D-SG "p" RETURNING RESU.
       S-15.
           IF  WR-STT = 0
               GO TO S-20
           END-IF
           MOVE SPACE TO W-STT.
           IF  WR-STT = 1
               MOVE "全体" TO W-STT
           END-IF
           IF  WR-STT = 2
               MOVE "商品" TO W-STT
           END-IF
           CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU.
       S-20.
           IF  WR-SKR NOT = ZERO
               CALL "SD_Output" USING "D-SKR" D-SKR "p" RETURNING RESU
           END-IF
           MOVE 0 TO W-DC.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-25.
           EXIT.
       S-30.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           MOVE ZERO TO W-WTCD.
           MOVE WR-NTCD TO W-TCDW.
           MOVE SPACE TO T-KEY2.
           MOVE W-TCDW TO T-NTCD.
      *           START T-M KEY NOT < T-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY2" " NOT < " T-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF.
       S-35.
      *           READ T-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           IF  W-TCD = T-TCD
               GO TO S-35
           END-IF
           IF  WR-NTCD NOT = T-NTCD
               GO TO S-40
           END-IF
           MOVE T-TCD TO TT-KEY.
      *           READ TT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-35
           END-IF
           COMPUTE W-UZ = TT-TUZ + TT-TUZZ.
           IF  W-UZ = ZERO
               GO TO S-35
           END-IF
           IF  W-WTCD1 = ZERO
               MOVE T-TCD TO W-WTCD1
               GO TO S-35
           END-IF
           IF  W-WTCD2 = ZERO
               MOVE T-TCD TO W-WTCD2
               GO TO S-35
           END-IF
           IF  W-WTCD3 = ZERO
               MOVE T-TCD TO W-WTCD3
               GO TO S-35
           END-IF
           IF  W-WTCD4 = ZERO
               MOVE T-TCD TO W-WTCD4
               GO TO S-35
           END-IF
           IF  W-WTCD5 = ZERO
               MOVE T-TCD TO W-WTCD5
           END-IF.
       S-40.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
       S-45.
           EXIT.
