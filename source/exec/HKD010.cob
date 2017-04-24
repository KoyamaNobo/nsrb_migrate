       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKD010.
      *********************************************************
      *    PROGRAM         :  ì¸ã‡ì`ï[Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHK01                          *
      *        ïœçXÅ@Å@Å@  :  62/04/15                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "ÅñÅñÅñÅ@Å@ì¸ã‡ì`ï[Å@ÉvÉãÅ[ÉtÉäÉXÉgÅ@Å@ÅñÅñÅñ".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(004) VALUE "ACT ".
           02  F              PIC  N(004) VALUE "ì˙Å@Å@ït".
           02  F              PIC  X(007) VALUE "  ∫∞ƒﬁ ".
           02  F              PIC  N(010) VALUE
                "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ì¸ã‡áÇÅ@".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ãÊï™".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ëäéE".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "éËå`ä˙ì˙".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ã‡Å@Å@äz".
           02  F              PIC  N(004) VALUE "Å@êøãÅì˙".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "Å@è¡îÔê≈ì¸ã‡".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "îÑè„ì¸ã‡".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "Å@êUë÷ì`ï[áÇ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "êøãÅèëÅ@".
       01  W-P.
           02  F              PIC  X(001).
           02  P-ACT          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-NO           PIC  9(006).
           02  P-V1           PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-NC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-NSC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-TD           PIC 99/99/99.
           02  P-KIN          PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-SS           PIC 99/99.
           02  P-SHZ          PIC --,---,--9.
           02  P-TKIN         PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-FDNO         PIC  9(006).
           02  P-V2           PIC  X(001).
           02  P-FGNO         PIC  9(001).
           02  F              PIC  X(001).
           02  P-SKD          PIC 99/99/99.
       01  W-R.
           02  W-DATE         PIC  9(008).
           02  W-DATEL REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-DATES      PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-KIN          PIC S9(008).
           02  W-NC.
             03  W-NC1        PIC  9(001).
             03  W-NC2        PIC  9(001).
           02  W-NSC          PIC  9(001).
           02  W-TD           PIC  9(008).
           02  W-TNGP  REDEFINES W-TD.
             03  W-TNEN       PIC  9(004).
             03  W-TNENL REDEFINES W-TNEN.
               04  W-TNEN1    PIC  9(002).
               04  W-TNEN2    PIC  9(002).
             03  W-TGET       PIC  9(002).
             03  W-TPEY       PIC  9(002).
           02  W-TNGPL REDEFINES W-TD.
             03  F            PIC  9(002).
             03  W-TNGPS      PIC  9(006).
           02  W-SS           PIC  9(006).
           02  W-SNGD  REDEFINES W-SS.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGSD REDEFINES W-SS.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-BC           PIC  9(001).
           02  W-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  W-KEY.
             03  W-NO         PIC  9(006).
             03  W-GNO        PIC  9(001).
           02  W-FNO.
             03  W-FDNO       PIC  9(006).
             03  W-FGNO       PIC  9(002).
           02  W-SKD          PIC  9(008).
           02  W-DCC          PIC  9(001).
           02  F              PIC  X(013).
           02  W-DC           PIC  9(001).
           02  F              PIC  X(002).
           02  W-ACTD         PIC  9(001).
           02  W-PRC          PIC  9(001).
           02  F              PIC  X(017).
       01  W-ARD.
           02  W-AR    OCCURS   8  PIC  X(102).
       01  W-DATA.
           02  W-AD.
             03  W-GKIN       PIC S9(009).
             03  W-TKIN       PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  W-BAD.
             03  W-BTKIN      PIC S9(009).
             03  W-BSHZ       PIC S9(007).
           02  W-ACT          PIC  9(001).
           02  W-NRC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-DNOD         PIC  9(006).
           02  W-TCDB         PIC  9(004).
           02  CNT            PIC  9(001).
           02  W-FN           PIC  9(006).
           02  W-FND.
             03  W-FND1       PIC  9(006).
             03  W-FND2       PIC  9(001).
           02  W-RD.
             03  W-NGP        PIC  9(008).
             03  W-NGPD  REDEFINES W-NGP.
               04  W-NGD.
                 05  W-NEN    PIC  9(004).
                 05  W-NENL  REDEFINES W-NEN.
                   06  W-NEN1 PIC  9(002).
                   06  W-NEN2 PIC  9(002).
                 05  W-GET    PIC  9(002).
               04  W-PEY      PIC  9(002).
             03  W-NGPSD REDEFINES W-NGP.
               04  F          PIC  9(002).
               04  W-NGPS     PIC  9(006).
             03  W-TCDD       PIC  9(004).
             03  W-SKDD.
               04  W-SKDN     PIC  9(004).
               04  W-SKDG     PIC  9(002).
               04  W-SKDP     PIC  9(002).
             03  W-SKDL  REDEFINES W-SKDD.
               04  F          PIC  9(002).
               04  W-SKDS     PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-NCN1         PIC  X(001).
           02  W-HNG          PIC  9(006).
           02  W-HNGD  REDEFINES W-HNG.
             03  W-HNEN       PIC  9(004).
             03  W-HNENL REDEFINES W-HNEN.
               04  W-HNEN1    PIC  9(002).
               04  W-HNEN2    PIC  9(002).
             03  W-HGET       PIC  9(002).
           02  W-HNGSD REDEFINES W-HNG.
             03  F            PIC  9(002).
             03  W-HNGS       PIC  9(004).
           02  W-KNG          PIC  9(006).
           02  W-KNGD  REDEFINES W-KNG.
             03  W-KNEN       PIC  9(004).
             03  W-KNENL REDEFINES W-KNEN.
               04  W-KNEN1    PIC  9(002).
               04  W-KNEN2    PIC  9(002).
             03  W-KGET       PIC  9(002).
           02  W-KNGSD REDEFINES W-KNG.
             03  F            PIC  9(002).
             03  W-KNGS       PIC  9(004).
           02  W-PC           PIC  9(001) VALUE 0.
           02  W-END          PIC  9(001) VALUE 0.
           02  W-INV.
             03  W-INV1       PIC  9(001).
             03  W-INV2       PIC  9(001).
           02  W-FC           PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-DTC          PIC  9(001).
           02  W-DTW.
             03  W-DTW1       PIC  9(003).
             03  W-DTW2       PIC  9(001).
           02  W-SDATE        PIC  9(008).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY SIWAIW.
           COPY LITSKF.
           COPY LSPF.
      *FD  NYU-F
       01  NYU-F_HKD010.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HKD010".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  N-DATE.
             03  F            PIC  9(002).
             03  N-NGPS       PIC  9(006).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC           PIC  9(002).
           02  N-NSC          PIC  9(001).
           02  N-TD           PIC  9(008).
           02  N-SS           PIC  9(006).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  N-FNO.
             03  N-FDNO       PIC  9(006).
             03  N-FGNO       PIC  9(002).
           02  N-SKD          PIC  9(008).
           02  N-DCC          PIC  9(001).
           02  F              PIC  X(013).
           02  N-DC           PIC  9(001).
           02  F              PIC  X(001).
           02  N-DPC          PIC  9(001).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
           02  F              PIC  X(017).
       77  F                  PIC  X(001).
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-NRC   PIC  9(001).
             03  A-FN    PIC  9(006).
           02  A-DNO   PIC  9(006).
           02  A-NGP   PIC  9(006).
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-FND   PIC  9(007).
             03  A-NC    PIC  9(002).
             03  A-NSC   PIC  9(001).
             03  A-TD    PIC  9(006).
             03  A-KIN   PIC S9(008).
             03  A-SS    PIC  9(004).
           02  A-SKD   PIC  9(008).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-NCN.
               04  01D-NCN  PIC  X(001).
               04  02D-NCN  PIC  N(006).
             03  D-NSNA  PIC  N(006).
             03  D-KIN   PIC ZZZZZZZ9- .
           02  D-AD.
             03  01D-AD  PIC ZZZ,ZZZ,ZZ9- .
             03  02D-AD  PIC Z,ZZZ,ZZ9- .
             03  03D-AD  PIC ZZZ,ZZZ,ZZ9- .
           02  D-PRN   PIC  N(020) VALUE
                "ÅñÅñÅñÅ@Å@ì¸ã‡ì`ï[Å@ì¸óÕÉäÉXÉgÅ@Å@ÅñÅñÅñ".
           02  D-SPACE.
             03  FILLER.
               04  S-NRC   PIC  X(001) VALUE " ".
               04  S-FN    PIC  X(006) VALUE "      ".
             03  FILLER.
               04  S-DNO   PIC  X(006) VALUE "      ".
             03  D-SPC.
               04  S-FND   PIC  X(007) VALUE "       ".
               04  S-NCN   PIC  X(015) VALUE
                    "               ".
               04  S-NSC   PIC  X(014) VALUE
                    "              ".
               04  S-TD    PIC  X(006) VALUE "      ".
               04  S-KIN   PIC  X(009) VALUE "         ".
               04  S-SS    PIC  X(004) VALUE "    ".
             03  FILLER.
               04  S-SKD   PIC  X(008) VALUE "        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ≈º  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ÀΩﬁπ ¥◊∞  ***".
             03  E-ME9   PIC  X(024) VALUE
                  "***  HKBM WRITE ¥◊∞  ***".
             03  E-ME10  PIC  X(026) VALUE
                  "***  HKBM REWRITE ¥◊∞  ***".
             03  E-ME11  PIC  X(017) VALUE
                  "***  HKBM ≈º  ***".
             03  E-ME12  PIC  X(024) VALUE
                  "***  NYUF WRITE ¥◊∞  ***".
             03  E-ME14  PIC  X(026) VALUE
                  "***  NYUF REWRITE ¥◊∞  ***".
             03  E-ME15  PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME16  PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME17  PIC  X(025) VALUE
                  "***  NYUF DELETE ¥◊∞  ***".
             03  E-ME18  PIC  X(024) VALUE
                  "***  ì¸ã‡ï[Å@î≠çsçœ  ***".
             03  E-ME19  PIC  X(024) VALUE
                  "***  êUë÷ì`ï[Å@Ç»Çµ  ***".
             03  E-ME20  PIC  X(026) VALUE
                  "***  ìæà”êÊÅ@É`ÉFÉbÉN  ***".
             03  E-ME21  PIC  X(024) VALUE
                  "***  ì˙ïtÅ@É`ÉFÉbÉN  ***".
             03  E-ME22  PIC  X(024) VALUE
                  "***  ãÊï™Å@É`ÉFÉbÉN  ***".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "61" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "3" "43" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "5" "0" "7" "A-ACT" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-NRC" "9" "5" "17" "1" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-NRC" BY REFERENCE W-NRC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-FN" "9" "5" "26" "6" "A-NRC" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-FN" BY REFERENCE W-FN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DNO" "9" "6" "17" "6" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NGP" "9" "7" "17" "6" "A-DNO" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCD" "9" "8" "17" "4" "A-NGP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06C-ACP" " " "W-L" "0" "28" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-FND" "9" "W-L" "9" "7" " " "06C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-FND" BY REFERENCE W-FND "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NC" "9" "W-L" "17" "2" "A-FND" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-NC" BY REFERENCE W-NC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NSC" "9" "W-L" "33" "1" "A-NC" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-NSC" BY REFERENCE W-NSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-TD" "9" "W-L" "34" "6" "A-NSC" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-TD" BY REFERENCE W-TNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-KIN" "S9" "W-L" "48" "8" "A-TD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SS" "9" "W-L" "60" "4" "A-KIN" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SS" BY REFERENCE W-SNGS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKD" "9" "20" "15" "8" "06C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKD" BY REFERENCE W-SKDD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "45" "1" "A-SKD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "236" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNA" "N" "8" "22" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-DSP" " " "W-L" "0" "34" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NCN" " " "W-L" "0" "13" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NCN" "X" "W-L" "19" "1" " " "D-NCN" RETURNING RESU.
       CALL "SD_From" USING
            "01D-NCN" BY REFERENCE W-NCN1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NCN" "N" "W-L" "20" "12" "01D-NCN" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-NCN" BY REFERENCE HKB-NKNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NSNA" "N" "W-L" "35" "12" "D-NCN" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-NSNA" BY REFERENCE HKB-NKNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KIN" "ZZZZZZZ9-" "W-L" "48" "9" "D-NSNA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-AD" " " "W-L" "0" "34" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-AD" "ZZZ,ZZZ,ZZ9-" "19" "45" "12" " " "D-AD"
            RETURNING RESU.
       CALL "SD_From" USING
            "01D-AD" BY REFERENCE W-GKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-AD" "Z,ZZZ,ZZ9-" "20" "38" "10" "01D-AD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02D-AD" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-AD" "ZZZ,ZZZ,ZZ9-" "20" "54" "12" "02D-AD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03D-AD" BY REFERENCE W-TKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-PRN" "N" "1" "20" "40" "D-AD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SPACE" " " "1" "0" "76" "D-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-SPACE" " " "5" "0" "7" " " "D-SPACE" RETURNING RESU.
       CALL "SD_Init" USING
            "S-NRC" "X" "5" "17" "1" " " "01D-SPACE" RETURNING RESU.
       CALL "SD_Init" USING
            "S-FN" "X" "5" "26" "6" "S-NRC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "02D-SPACE" " " "6" "0" "6" "01D-SPACE" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-DNO" "X" "6" "17" "6" " " "02D-SPACE" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SPC" " " "W-L" "0" "55" "02D-SPACE" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-FND" "X" "W-L" "9" "7" " " "D-SPC" RETURNING RESU.
       CALL "SD_Init" USING
            "S-NCN" "X" "W-L" "17" "15" "S-FND" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-NSC" "X" "W-L" "33" "14" "S-NCN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-TD" "X" "W-L" "34" "6" "S-NSC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-KIN" "X" "W-L" "48" "9" "S-TD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SS" "X" "W-L" "60" "4" "S-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04D-SPACE" " " "20" "0" "8" "D-SPC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SKD" "X" "20" "15" "8" " " "04D-SPACE" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "332" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "332" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME9" "X" "24" "15" "24" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "26" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "17" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME12" "X" "24" "15" "24" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME14" "X" "24" "15" "26" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME15" "X" "24" "15" "17" "E-ME14" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME16" "X" "24" "15" "18" "E-ME15" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME17" "X" "24" "15" "25" "E-ME16" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME18" "X" "24" "15" "24" "E-ME17" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME19" "X" "24" "15" "24" "E-ME18" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME20" "X" "24" "15" "26" "E-ME19" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME21" "X" "24" "15" "24" "E-ME20" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME22" "X" "24" "15" "24" "E-ME21" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           COPY LIBCPR.
           MOVE ZERO TO W-HNG W-KNG.
           MOVE D-NHNG TO W-HNGS.
           MOVE D-NKNG TO W-KNGS.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF
           IF  W-KNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-KNEN
           END-IF
           IF  W-KNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-KNEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  D-NANG NOT = D-NHNG AND D-NKNG
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 "SHARED" BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
      *
           PERFORM DNO1-RTN THRU DNO1-EX.
           IF  W-END = 8
               GO TO M-900
           END-IF.
       M-060.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 8
               GO TO M-100
           END-IF
           IF  W-ACT = 1
               GO TO M-080
           END-IF
           PERFORM DEL-RTN THRU DEL-EX.
           IF  W-END = 8
               GO TO M-100
           END-IF
           IF  W-ACT = 3
               GO TO M-060
           END-IF.
       M-080.
           PERFORM WRI-RTN THRU WRI-EX.
           IF  W-END = 8
               GO TO M-100
           END-IF
      *
           GO TO M-060.
       M-100.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
       M-120.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           MOVE ZERO TO W-R.
           MOVE NYU-R TO W-R.
           IF  W-PRC = 9
               GO TO M-120
           END-IF.
       M-140.
           MOVE W-DATE TO W-NGP.
           MOVE ZERO TO CHK.
       M-160.
           MOVE ZERO TO W-AD CHK2.
           MOVE W-TCD TO W-TCDD.
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "Å@ÅñÅñÅ@É}ÉXÉ^Å[Å@Ç»ÇµÅ@ÅñÅñÅ@" TO T-NAME
           END-IF.
       M-180.
           PERFORM PRI-RTN THRU PRI-EX.
           MOVE 9 TO N-PRC.
      *           REWRITE NYU-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-200.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-220
           END-IF
           MOVE ZERO TO W-R.
           MOVE NYU-R TO W-R.
           IF  W-PRC = 9
               GO TO M-200
           END-IF
           IF (W-DATE = W-NGP) AND (W-TCD = W-TCDD)
               GO TO M-180
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-DATE NOT = W-NGP
               GO TO M-140
           END-IF
           GO TO M-160.
       M-220.
           PERFORM KEI-RTN THRU KEI-EX.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *-------------  ì`ï[áÇÅ@ÉZÉbÉgÅ@----------------------------------
       DNO1-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
               GO TO DNO1-EX
           END-IF
           MOVE HKB-NKN TO W-DNOD.
       DNO1-EX.
           EXIT.
      *-------------  ì¸Å@Å@óÕÅ@----------------------------------------
       ACP-RTN.
           CALL "SD_Screen_Output" USING "SCHK01" RETURNING RESU.
           IF  W-ACT = 1
               MOVE ZERO TO W-SKDD
               GO TO ACP-007
           END-IF
           IF  W-ACT = 2
               GO TO ACP-010
           END-IF
           MOVE ZERO TO W-RD.
       ACP-005.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-005
           END-IF
           IF  W-ACT = 9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO ACP-005
           END-IF
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING "S-NRC" S-NRC "p" RETURNING RESU
               CALL "SD_Output" USING "S-FN" S-FN "p" RETURNING RESU
               GO TO ACP-010
           END-IF.
       ACP-007.
           CALL "SD_Accept" USING BY REFERENCE A-NRC "A-NRC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-005
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-005
           END-IF
           IF  W-NRC > 1
               GO TO ACP-007
           END-IF
           CALL "SD_Output" USING "S-DNO" S-DNO "p" RETURNING RESU.
           IF  W-NRC = 0
               GO TO ACP-008
           END-IF
           IF  W-NGP NOT = ZERO
               CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU
           END-IF
           GO TO ACP-020.
       ACP-008.
           CALL "SD_Accept" USING BY REFERENCE A-FN "A-FN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-007
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-008
           END-IF
           MOVE ZERO TO SDW-KEY.
           MOVE W-FN TO SDWJNO.
      *           START SDW KEY NOT < SDW-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDW_PNAME1 "SDW-KEY" " NOT < " SDW-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-008
           END-IF
           MOVE W-FN TO SDWJNO.
      *           READ SDW NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDW_PNAME1 BY REFERENCE SDW-REC
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-008
           END-IF
           IF  W-FN NOT = SDWJNO
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-008
           END-IF
           MOVE SDWYMD TO W-NGP.
           MOVE SDWTCD TO W-TCDD.
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE T-NTCD TO W-TCDB.
           GO TO ACP-030.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-005
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           IF  W-INV1 = 1
               GO TO ACP-010
           END-IF
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-INV2 NOT = 0
               GO TO ACP-010
           END-IF
           MOVE 1 TO W-NRC.
           IF  W-ACT = 3
               GO TO ACP-520
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-NGP "A-NGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO ACP-007
               ELSE
                   GO TO ACP-010
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-02R TO W-NGPS
               CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO ACP-020
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-020
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       ACP-030.
           IF  W-NGD NOT = W-HNG AND W-KNG
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               IF  W-ACT = 2
                   GO TO ACP-020
               ELSE
                   IF  W-NRC = 0
                       GO TO ACP-008
                   ELSE
                       GO TO ACP-020
                   END-IF
               END-IF
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-NRC = 0
                   GO TO ACP-008
               ELSE
                   GO TO ACP-020
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-NRC = 1
               GO TO ACP-070
           END-IF
           IF  SDWTCD = W-TCDD
               GO TO ACP-070
           END-IF
      *
           PERFORM TMS-RTN THRU TMS-EX.
           IF  W-DTC = 1
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-008
           END-IF
      *
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF.
       ACP-070.
           IF  T-BC NOT = ZERO
               GO TO ACP-080
           END-IF
           IF  W-NGD NOT = W-HNG
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           GO TO ACP-100.
       ACP-080.
           IF  W-NGD NOT = W-KNG
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF.
       ACP-100.
           MOVE 0 TO CNT.
           IF  W-ACT NOT = 2
               MOVE ZERO TO W-ARD
           END-IF.
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       ACP-120.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 9
               MOVE ZERO TO CNT W-AD
               GO TO ACP-300
           END-IF
           MOVE ZERO TO W-R.
           MOVE W-AR(CNT) TO W-R.
           MOVE W-NGP TO W-DATE.
           MOVE W-TCDD TO W-TCD.
           MOVE T-BC TO W-BC.
           MOVE T-TNC TO W-TC.
           MOVE T-DCC TO W-DCC.
           IF  W-NRC = 0
               IF  CNT = 1
                   MOVE W-FN TO W-FND1
                   CALL "SD_Output" USING
                    "A-FND" A-FND "p" RETURNING RESU
               END-IF
           END-IF.
       ACP-130.
           CALL "SD_Accept" USING BY REFERENCE A-FND "A-FND" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-280
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-130
           END-IF
           IF  W-FND = ZERO
               MOVE ZERO TO W-FNO
               CALL "SD_Output" USING "S-FND" S-FND "p" RETURNING RESU
               GO TO ACP-140
           END-IF
      *
           MOVE W-FND1 TO W-FDNO SDWJNO.
           MOVE W-FND2 TO W-FGNO SDWLNO.
      *           READ SDW WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SDW_PNAME1 BY REFERENCE SDW-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-130
           END-IF
           IF  W-NGP NOT = SDWYMD
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           IF  W-TCDD = SDWTCD
               GO TO ACP-140
           END-IF
      *
           MOVE SDWTCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           MOVE T-NTCD TO W-TCDB.
      *
           PERFORM TMS-RTN THRU TMS-EX.
           IF  W-DTC = 1
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
      *
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-130
           END-IF.
       ACP-140.
           CALL "SD_Accept" USING BY REFERENCE A-NC "A-NC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-140
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE W-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-140
           END-IF
           IF  W-NC2 NOT = 0 AND 8 AND 9
               GO TO ACP-140
           END-IF
           IF  W-NC = "78" OR "98"
               GO TO ACP-140
           END-IF
           IF  W-NC1 NOT = 7
               IF  W-NC2 = 9
                   GO TO ACP-140
               END-IF
           END-IF
           MOVE " " TO W-NCN1.
           IF  W-NC2 NOT = 0
               MOVE "(" TO W-NCN1
           END-IF
           IF  W-NC1 = 9
               MOVE "(" TO W-NCN1
           END-IF
           CALL "SD_Output" USING "D-NCN" D-NCN "p" RETURNING RESU.
           IF  W-FND NOT = ZERO
               IF  KRCDMW = 0110
                   IF  W-NC1 NOT = 0
                       CALL "SD_Output" USING
                        "E-ME22" E-ME22 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  W-FND NOT = ZERO
               IF  KRCDMW = 0120
                   IF  W-NC1 NOT = 1 AND 2
                       CALL "SD_Output" USING
                        "E-ME22" E-ME22 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  W-FND NOT = ZERO
               IF  KRCDMW = 0130
                   IF  W-NC1 NOT = 3 AND 4
                       CALL "SD_Output" USING
                        "E-ME22" E-ME22 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  W-NC = "60"
               MOVE ZERO TO W-TD
               GO TO ACP-150
           END-IF
           IF  W-NC NOT = "30" AND "40"
               CALL "SD_Output" USING "S-NSC" S-NSC "p" RETURNING RESU
               MOVE ZERO TO W-NSC W-TD
               GO TO ACP-180
           ELSE
               MOVE ZERO TO W-NSC
               GO TO ACP-160
           END-IF.
       ACP-150.
           CALL "SD_Accept" USING BY REFERENCE A-NSC "A-NSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-150
           END-IF
           CALL "SD_Output" USING "S-NSC" S-NSC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NSC" A-NSC "p" RETURNING RESU.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE W-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-150
           END-IF
           CALL "SD_Output" USING "D-NSNA" D-NSNA "p" RETURNING RESU.
           GO TO ACP-180.
       ACP-160.
           CALL "SD_Accept" USING BY REFERENCE A-TD "A-TD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF
           CALL "SD_Output" USING "S-NSC" S-NSC "p" RETURNING RESU.
           IF  W-TNGPS = ZERO
               CALL "SD_Output" USING "S-TD" S-TD "p" RETURNING RESU
               GO TO ACP-180
           ELSE
               CALL "SD_Output" USING "A-TD" A-TD "p" RETURNING RESU
           END-IF
           IF  W-TGET < 1 OR > 12
               GO TO ACP-160
           END-IF
           IF  W-TPEY < 1 OR > 31
               GO TO ACP-160
           END-IF
           MOVE ZERO TO W-TNEN1.
           IF  W-TNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-TNEN
           END-IF
           IF  W-TNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-TNEN
           END-IF
           IF  W-TD < W-DATE
               GO TO ACP-160
           END-IF.
       ACP-180.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-NC = "30" OR "40"
                   GO TO ACP-160
               ELSE
                   IF  W-NC = "60"
                       GO TO ACP-150
                   ELSE
                       GO TO ACP-140
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-180
           END-IF
           IF  W-KIN = ZERO
               GO TO ACP-180
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-NC2 > 7
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
               MOVE ZERO TO W-SS
               GO TO ACP-240
           END-IF.
       ACP-220.
           CALL "SD_Accept" USING BY REFERENCE A-SS "A-SS" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-220
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SS = ZERO
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
               GO TO ACP-240
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO ACP-220
           END-IF
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-SS > W-NGD
               GO TO ACP-220
           END-IF.
       ACP-240.
           MOVE W-R TO W-AR(CNT).
           GO TO ACP-120.
       ACP-260.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO ACP-060
           END-IF
           MOVE W-AR(CNT) TO W-R.
           SUBTRACT 1 FROM W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-KIN = ZERO
               GO TO ACP-260
           END-IF
           IF  W-NC2 > 7
               GO TO ACP-180
           END-IF
           GO TO ACP-220.
       ACP-280.
           MOVE ZERO TO W-R.
           MOVE W-R TO W-AR(CNT).
           CALL "SD_Output" USING "D-SPC" D-SPC "p" RETURNING RESU.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT NOT = 9
               MOVE W-AR(CNT) TO W-R
               GO TO ACP-280
           END-IF
           MOVE ZERO TO CNT W-AD.
       ACP-300.
           ADD 1 TO CNT.
           IF  CNT = 9
               CALL "SD_Output" USING "D-AD" D-AD "p" RETURNING RESU
               GO TO ACP-320.
           MOVE W-AR(CNT) TO W-R.
           ADD W-KIN TO W-GKIN.
           IF  W-NC2 > 7
               ADD W-KIN TO W-SHZ
           ELSE
               ADD W-KIN TO W-TKIN.
           GO TO ACP-300.
       ACP-320.
           IF  W-ACT = 2
               GO TO ACP-520
           END-IF
           IF  T-SS = ZERO OR 99
               MOVE ZERO TO W-SKDD
               CALL "SD_Output" USING "S-SKD" S-SKD "p" RETURNING RESU
               GO TO ACP-520
           END-IF
      *
           MOVE W-TCDD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           MOVE TSK-ZNGP(4) TO W-SDATE.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE 99999999 TO W-SDATE
               MOVE ZERO TO W-SKDD
               CALL "SD_Output" USING "S-SKD" S-SKD "p" RETURNING RESU
               GO TO ACP-520
           END-IF
      *
           MOVE W-NGP TO W-SKDD.
       ACP-340.
           IF  W-SKDG = 13
               MOVE 1 TO W-SKDG
               ADD 1 TO W-SKDN
           END-IF
           MOVE T-SS TO W-SKDP.
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
           END-IF
           IF  W-NGP > W-SKDD
               IF  W-GET NOT = W-SKDG
                   MOVE ZERO TO W-SKDD
                   CALL "SD_Output" USING
                    "S-SKD" S-SKD "p" RETURNING RESU
                   GO TO ACP-360
               ELSE
                   ADD 1 TO W-SKDG
                   GO TO ACP-340
               END-IF
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SKDD
                   ADD 1 TO W-SKDG
                   GO TO ACP-340
               END-IF
           END-IF
           CALL "SD_Output" USING "A-SKD" A-SKD "p" RETURNING RESU.
           GO TO ACP-520.
       ACP-360.
           CALL "SD_Accept" USING BY REFERENCE A-SKD "A-SKD" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-360
           END-IF
           IF  W-SKDD = ZERO
               CALL "SD_Output" USING "S-SKD" S-SKD "p" RETURNING RESU
               GO TO ACP-520
           END-IF
           IF  W-SKDG < 1 OR > 12
               GO TO ACP-360
           END-IF
           IF  W-SKDP < 1 OR > 31
               GO TO ACP-360
           END-IF
           IF  W-NGP > W-SKDD
               GO TO ACP-360
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SKDD
                   GO TO ACP-360
               END-IF
           END-IF.
       ACP-520.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-010
               ELSE
                   IF  T-SS = 00 OR 99
                       GO TO ACP-260
                   ELSE
                       GO TO ACP-360
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-520
           END-IF
           IF  W-DMM = 9
               IF  W-ACT = 3
                   GO TO ACP-010
               ELSE
                   GO TO ACP-060
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-520
           END-IF.
       ACP-EX.
           EXIT.
      *-------------  ÇcÇ`ÇsÇ`Å@ÉZÉbÉgÅiÇqÇdÇvÇqÇhÇsÇdÅEÇcÇdÇkÇdÇsÇdÅjÅ@
       SET-RTN.
           MOVE 0 TO W-INV.
           MOVE SPACE TO N-KEY.
           MOVE W-DNO TO N-NO.
      *           START NYU-F KEY NOT < N-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            NYU-F_PNAME1 "N-KEY" " NOT < " N-KEY RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           IF  W-DNO NOT = N-NO
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           IF  N-DPC NOT = 0
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
      *
           MOVE N-DATE TO W-NGP.
           MOVE N-TCD TO W-TCDD.
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV2
               MOVE SPACE TO T-NAME
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE N-SKD TO W-SKDD.
      *
       SET-010.
           MOVE ZERO TO W-ARD CNT.
       SET-020.
           ADD 1 TO CNT.
           IF  CNT = 9
               GO TO SET-EX
           END-IF
           MOVE ZERO TO W-R.
           MOVE NYU-R TO W-R.
           MOVE W-R TO W-AR(CNT).
      *
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-EX
           END-IF
           IF  W-DNO = N-NO
               GO TO SET-020
           END-IF.
       SET-EX.
           EXIT.
      *-------------  ÇcÇ`ÇsÇ`Å@ï\é¶Å@ÅiÇqÇdÇvÇqÇhÇsÇdÅEÇcÇdÇkÇdÇsÇdÅjÅ@
       DSP-RTN.
           CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-SKDD = ZERO
               CALL "SD_Output" USING "S-SKD" S-SKD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-SKD" A-SKD "p" RETURNING RESU
           END-IF
           MOVE ZERO TO CNT W-AD W-BAD.
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       DSP-020.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 9
               GO TO DSP-040
           END-IF
           MOVE ZERO TO W-R.
           MOVE W-AR(CNT) TO W-R.
           IF  W-KIN = ZERO
               GO TO DSP-040
           END-IF
      *
           MOVE W-FDNO TO W-FND1.
           MOVE W-FGNO TO W-FND2.
           CALL "SD_Output" USING "A-FND" A-FND "p" RETURNING RESU.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE W-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE " " TO W-NCN1.
           IF  W-NC2 NOT = 0
               MOVE "(" TO W-NCN1
           END-IF
           IF  W-NC1 = 9
               MOVE "(" TO W-NCN1
           END-IF
           CALL "SD_Output" USING "A-NC" A-NC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NCN" D-NCN "p" RETURNING RESU.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE W-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NSNA
           END-IF
           IF  W-NC = "30" OR "40"
               IF  W-TNGPS NOT = ZERO
                   CALL "SD_Output" USING "A-TD" A-TD "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING "S-TD" S-TD "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-NC NOT = "30" AND "40"
               IF  W-NC = "60"
                   CALL "SD_Output" USING
                    "A-NSC" A-NSC "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-NSNA" D-NSNA "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "S-NSC" S-NSC "p" RETURNING RESU
               END-IF
           END-IF
      *
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
      *
           IF  W-NC2 > 7
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
           ELSE
               IF  W-SS = ZERO
                   CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING "A-SS" A-SS "p" RETURNING RESU
               END-IF
           END-IF
      *
           ADD W-KIN TO W-GKIN.
           IF  W-NC2 > 7
               ADD W-KIN TO W-SHZ W-BSHZ
           ELSE
               ADD W-KIN TO W-TKIN W-BTKIN
           END-IF
           GO TO DSP-020.
       DSP-040.
           CALL "SD_Output" USING "D-AD" D-AD "p" RETURNING RESU.
       DSP-EX.
           EXIT.
       TMS-RTN.
           MOVE 0 TO W-DTC.
           MOVE ZERO TO T-KEY2.
           MOVE W-TCDB TO T-NTCD.
      *           START T-M KEY NOT < T-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY2" " NOT < " T-KEY2 RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-DTC
               GO TO TMS-EX
           END-IF.
       TMS-010.
      *           READ T-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-DTC
               GO TO TMS-EX
           END-IF
           IF  W-TCDB NOT = T-NTCD
               MOVE 1 TO W-DTC
               GO TO TMS-EX
           END-IF
           IF  T-KEY NOT = W-TCDD
               GO TO TMS-010
           END-IF.
       TMS-EX.
           EXIT.
      *-------------  ÇmÇxÇtÇeÅ@ÇvÇqÇhÇsÇdÅ@----------------------------
       WRI-RTN.
           IF  W-ACT = 1
               ADD 1 TO W-DNOD
               IF  W-DNOD = ZERO
                   MOVE 1 TO W-DNOD
               END-IF
           END-IF
           MOVE ZERO TO CNT.
       WRI-010.
           ADD 1 TO CNT.
           IF  CNT = 9
               GO TO WRI-900
           END-IF
           MOVE W-AR(CNT) TO W-R.
           IF  W-KIN = ZERO
               GO TO WRI-900
           END-IF.
       WRI-020.
           IF  W-ACT = 1
               MOVE W-DNOD TO W-NO
           ELSE
               MOVE W-DNO TO W-NO
           END-IF
           MOVE W-SKDD TO W-SKD.
           MOVE CNT TO W-GNO.
           MOVE 0 TO W-PRC.
           MOVE W-ACT TO W-ACTD.
           MOVE ZERO TO NYU-R.
           MOVE W-R TO NYU-R.
      *           WRITE NYU-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-040
           END-IF
           GO TO WRI-010.
       WRI-040.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
               GO TO WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           MOVE "NYUF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           GO TO WRI-020.
       WRI-900.
           IF  W-ACT = 1
               PERFORM DNO2-RTN THRU DNO2-EX
           END-IF.
       WRI-EX.
           EXIT.
      *-------------  ÇmÇxÇtÇeÅ@ÇcÇdÇkÇdÇsÇdÅ@--------------------------
       DEL-RTN.
           MOVE SPACE TO N-KEY.
           MOVE W-DNO TO N-NO.
      *           START NYU-F KEY NOT < N-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            NYU-F_PNAME1 "N-KEY" " NOT < " N-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF.
       DEL-010.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF
           IF  W-DNO NOT = N-NO
               GO TO DEL-EX
           END-IF
      *
      *           DELETE NYU-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING NYU-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
           END-IF
           GO TO DEL-010.
       DEL-EX.
           EXIT.
      *-------------  ì`ï[áÇÅ@ÇqÇdÇ`ÇcÅ@--------------------------------
       DNO2-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO DNO2-020
           END-IF
           MOVE W-DNOD TO HKB-NKN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
           END-IF
           GO TO DNO2-EX.
       DNO2-020.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
           MOVE W-DNOD TO HKB-NKN.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
           END-IF.
       DNO2-EX.
           EXIT.
      *-------------  çÏÅ@Å@ï\Å@----------------------------------------
       PRI-RTN.
           IF  W-PC = 0
               MOVE 9 TO W-PC
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               PERFORM MID-020 THRU MID-EX
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA.
           IF  W-GNO = 1
               MOVE W-ACTD TO P-ACT
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-DATES TO P-DATE
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           IF  W-GNO = 1
               MOVE W-NO TO P-NO
           END-IF
           MOVE "-" TO P-V1.
           MOVE W-GNO TO P-GNO.
           MOVE W-NC TO P-NC.
           IF  W-NSC NOT = 0
               MOVE W-NSC TO P-NSC
           END-IF
           IF  W-TD NOT = ZERO
               MOVE W-TNGPS TO P-TD
           END-IF
           MOVE W-KIN TO P-KIN.
           IF  W-SS NOT = ZERO
               MOVE W-SNGS TO P-SS
           END-IF
           IF  W-FNO NOT = ZERO
               MOVE W-FDNO TO P-FDNO
               MOVE "-" TO P-V2
               MOVE W-FGNO TO P-FGNO
           END-IF
           IF  W-SKD NOT = ZERO
               MOVE W-SKD TO W-SKDD
               MOVE W-SKDS TO P-SKD
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-ACTD TO P-ACT
               MOVE W-DATES TO P-DATE
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               MOVE W-NO TO P-NO
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-KIN TO W-GKIN.
           IF  W-NC2 > 7
               ADD W-KIN TO W-SHZ
           ELSE
               ADD W-KIN TO W-TKIN
           END-IF.
       PRI-EX.
           EXIT.
      *-------------  çáåvÅ@àÛéöÅ@--------------------------------------
       KEI-RTN.
           MOVE SPACE TO W-P.
           MOVE "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@ÅÉÅ@Å@çáÅ@åvÅ@Å@ÅÑ" TO P-TNA.
           MOVE W-GKIN TO P-KIN.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-TKIN TO P-TKIN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
      *-------------  å©èoÇµÅ@àÛéöÅ@------------------------------------
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE  TO H-PAGE.
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
