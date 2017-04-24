       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD100.
      ************************************************
      *****     éËå`ÅEóÃé˚èëÅEîÉä|éxï•Å@ì¸óÕ     *****
      *****          ( SCREEN : SCTD10 )         *****
      ************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-R.
           02  W-KEY.
             03  W-KBN        PIC  9(002).
             03  W-NO         PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-USD          PIC  9(006).
           02  W-USDD  REDEFINES  W-USD.
             03  W-USD1       PIC  9(002).
             03  W-UGP.
               04  W-USD2     PIC  9(002).
               04  W-USD3     PIC  9(002).
           02  W-MAN          PIC  9(006).
           02  W-MAND  REDEFINES  W-MAN.
             03  W-MAN1       PIC  9(002).
             03  W-MGP.
               04  W-MAN2     PIC  9(002).
               04  W-MAN3     PIC  9(002).
           02  W-KIN          PIC S9(010).
           02  W-BK           PIC  9(004).
           02  W-HAC          PIC  N(024).
           02  W-HACD  REDEFINES W-HAC  PIC  X(048).
           02  W-AK.
             03  W-ZR         PIC S9(008).
             03  W-SS         PIC S9(008).
             03  W-SB         PIC  9(008).
             03  W-GC         PIC  9(008).
             03  W-SZ         PIC  9(008).
             03  W-EG         PIC  9(008).
             03  W-ST         PIC  9(008).
           02  W-ZSHZ         PIC S9(007).
           02  W-SSHZ         PIC S9(007).
           02  F              PIC  X(006).
           02  W-NENS         PIC  9(004).
           02  W-HCR          PIC  9(001).
           02  W-HCT          PIC  9(001).
           02  W-HCK          PIC  9(001).
           02  W-HCZ          PIC  9(001).
           02  W-PCHK         PIC  9(001).
           02  W-RSC          PIC  9(001).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-DATE         PIC  9(006).
           02  W-NGP   REDEFINES  W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KEYD.
             03  W-KBND       PIC  9(002).
             03  W-NOD        PIC  9(004).
           02  W-TCDW.
             03  W-TCD1       PIC  9(001).
             03  W-TCD2       PIC  9(003).
           02  W-SNO          PIC  9(004).
           02  W-KBD          PIC  N(005).
           02  W-TNA          PIC  N(026).
           02  W-BNA          PIC  N(008).
           02  W-SNA          PIC  N(008).
           02  W-SKIN         PIC S9(010).
           02  CHK            PIC  9(001).
           02  W-ACTD         PIC  9(001).
           02  W-HNGP.
             03  W-HNG.
               04  W-HNEN     PIC  9(004).
               04  W-HNENL REDEFINES W-HNEN.
                 05  W-HNEN1  PIC  9(002).
                 05  W-HNEN2  PIC  9(002).
               04  W-HGET     PIC  9(002).
             03  W-HNGL  REDEFINES W-HNG.
               04  F          PIC  9(002).
               04  W-HNGS     PIC  9(004).
             03  W-HPEY       PIC  9(002).
           02  W-HNGPL REDEFINES W-HNGP.
             03  F            PIC  9(002).
             03  W-HNGPS      PIC  9(006).
           02  W-KNGP.
             03  W-KNG.
               04  W-KNEN     PIC  9(004).
               04  W-KNENL REDEFINES W-KNEN.
                 05  W-KNEN1  PIC  9(002).
                 05  W-KNEN2  PIC  9(002).
               04  W-KGET     PIC  9(002).
             03  W-KPEY       PIC  9(002).
           02  W-KNGPL REDEFINES W-KNGP.
             03  F            PIC  9(002).
             03  W-KNGPS      PIC  9(006).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENDL REDEFINES W-NEND.
               04  W-NEND1    PIC  9(002).
               04  W-NEND2    PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGDL  REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGDS       PIC  9(004).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGP        PIC  9(004).
           02  W-SNGPL REDEFINES W-SNGP.
             03  F            PIC  9(002).
             03  W-SNGPS      PIC  9(006).
           02  W-TNG          PIC  9(006).
           02  W-BNG          PIC  9(006).
           02  W-NC           PIC S9(002).
           02  W-RSM          PIC  N(005).
           02  W-NAD          PIC  X(048).
           02  W-NAAD  REDEFINES W-NAD.
             03  W-NA    OCCURS  48  PIC  X(001).
           02  W-HEND.
             03  W-HEN1       PIC  N(002).
             03  W-HEN2       PIC  N(002).
             03  W-HEN3       PIC  N(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LISM.
           COPY LITM.
           COPY LIBANK.
           COPY LIUKET.
           COPY LISHIT.
      *FD  TDT-M
       01  TDT-M_TSD100.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSD100".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-KBN       PIC  X(002).
             03  TD-NO        PIC  X(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DAT         PIC  9(006).
           02  TD-MAN         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BK          PIC  9(004).
           02  TD-HAC         PIC  N(024).
           02  TD-ZR          PIC S9(008).
           02  TD-SS          PIC S9(008).
           02  TD-SB          PIC  9(008).
           02  TD-GC          PIC  9(008).
           02  TD-SZ          PIC  9(008).
           02  TD-EG          PIC  9(008).
           02  TD-ST          PIC  9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PCHK        PIC  9(001).
           02  TD-RSC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TNO-M
       01  TNO-M_TSD100.
           02  TNO-M_PNAME1   PIC  X(004) VALUE "TNOM".
           02  F              PIC  X(001).
           02  TNO-M_LNAME    PIC  X(012) VALUE "TNO-M_TSD100".
           02  F              PIC  X(001).
           02  TNO-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TNO-M_SORT     PIC  X(100) VALUE SPACE.
           02  TNO-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TNO-M_RES      USAGE  POINTER.
       01  TNO-R.
           02  NO-KEY         PIC  X(002).
           02  NO-UMN         PIC  9(004).
           02  NO-SMN         PIC  9(004).
           02  NO-UDN         PIC  9(004).
           02  NO-SDN         PIC  9(004).
           02  NO-HDN         PIC  9(004).
           02  NO-NDN         PIC  9(004).
           02  F              PIC  X(230).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-KBN   PIC  9(002).
           02  A-NO    PIC  9(004).
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-MAN1  PIC  9(002).
             03  A-MAN2  PIC  9(002).
             03  A-MAN3  PIC  9(002).
           02  A-KIN   PIC S9(010).
           02  A-BK    PIC  9(004).
           02  A-HAC   PIC  N(024).
           02  A-RSC   PIC  9(001).
           02  FILLER.
             03  A-ZR    PIC S9(008).
             03  A-SS    PIC S9(008).
           02  FILLER.
             03  A-ZSHZ  PIC S9(007).
             03  A-SSHZ  PIC S9(007).
           02  FILLER.
             03  A-SB    PIC S9(008).
             03  A-GC    PIC S9(008).
           02  FILLER.
             03  A-SZ    PIC S9(008).
             03  A-EG    PIC S9(008).
           02  FILLER.
             03  A-ST    PIC S9(008).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DAV.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  X(001) VALUE "/".
           02  D-KB    PIC  N(005).
           02  D-TNA   PIC  N(026).
           02  D-MAV.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  X(001) VALUE "/".
           02  D-KIN.
             03  FILLER  PIC ZZZZZZZZZZ- .
           02  D-BK.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  N(008).
           02  D-RSC   PIC  N(005).
           02  D-S.
             03  FILLER.
               04  D-ZR    PIC ZZZZZZZZ- .
               04  D-SS    PIC ZZZZZZZZ- .
             03  FILLER.
               04  D-ZSHZ  PIC ZZZZZZZ- .
               04  D-SSHZ  PIC ZZZZZZZ- .
             03  FILLER.
               04  D-SB    PIC ZZZZZZZZ- .
               04  D-GC    PIC ZZZZZZZZ- .
             03  FILLER.
               04  D-SZ    PIC ZZZZZZZZ- .
               04  D-EG    PIC ZZZZZZZZ- .
             03  FILLER.
               04  D-ST    PIC ZZZZZZZZ- .
       01  C-SPC.
           02  S-MAN   PIC  X(008) VALUE
                "        ".
           02  S-BK    PIC  X(040) VALUE
                "                                        ".
           02  S-RSC   PIC  X(013) VALUE
                "             ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ƒ≥€∏ Ωﬁ–  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  UKETM ƒ≥€∏ Ωﬁ–  ***".
             03  E-ME4   PIC  X(024) VALUE
                  "***  SHITM ƒ≥€∏ Ωﬁ–  ***".
             03  E-ME5   PIC  X(022) VALUE
                  "***  ƒ∏≤ª∑œΩ¿∞ ≈º  ***".
             03  E-ME6   PIC  X(022) VALUE
                  "***  º≤⁄ª∑œΩ¿∞ ≈º  ***".
             03  E-ME7   PIC  X(022) VALUE
                  "***  ∑ﬁ›∫≥œΩ¿∞ ≈º  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  ±≤√∂”∏ ∑›∂ﬁ∏ ¥◊∞  ***".
             03  E-ME9   PIC  X(017) VALUE
                  "***  TNOM ≈º  ***".
             03  E-ME10  PIC  X(024) VALUE
                  "***  TDTM WRITR ¥◊∞  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  TDTM REWRITE ¥◊∞  ***".
             03  E-ME12  PIC  X(025) VALUE
                  "***  TDTM DELETE ¥◊∞  ***".
             03  E-ME13  PIC  X(026) VALUE
                  "***  TNOM REWRITE ¥◊∞  ***".
             03  E-ME14  PIC  X(018) VALUE
                  "***  À¬ﬁπ ¥◊∞  ***".
             03  E-ME15  PIC  X(025) VALUE
                  "***  ∫”ºﬁ … ΩÕﬂ∞Ω ±ÿ  ***".
             03  E-ME16  PIC  X(022) VALUE
                  "***  ∂≤∂π∏Ãﬁ› ¥◊∞  ***".
             03  E-ME17.
               04  FILLER  PIC  N(004) VALUE "ïœä∑çœÇ›".
               04  FILLER  PIC  N(002).
               04  FILLER  PIC  N(002).
               04  FILLER  PIC  N(002).
             03  E-ME18.
               04  FILLER  PIC  X(028) VALUE
                    "***  ∆≠≥∑›ª∑ ¥◊∞ (    )  ***".
               04  FILLER  PIC  9(004).
             03  E-ME19  PIC  X(020) VALUE
                  "***  œ›∑Àﬁ ¡™Ø∏  ***".
             03  E-ME78  PIC  N(002) VALUE "òAóç".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "157" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "48" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "5" "0" "6" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "5" "21" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "5" "24" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "5" "27" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KBN" "9" "6" "21" "2" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KBN" BY REFERENCE W-KBND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "7" "21" "4" "A-KBN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NOD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "8" "21" "4" "A-NO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "9" "0" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MAN1" "9" "9" "21" "2" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MAN1" BY REFERENCE W-MAN1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MAN2" "9" "9" "24" "2" "A-MAN1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MAN2" BY REFERENCE W-MAN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MAN3" "9" "9" "27" "2" "A-MAN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MAN3" BY REFERENCE W-MAN3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "10" "21" "10" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BK" "9" "11" "21" "4" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BK" BY REFERENCE W-BK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HAC" "N" "13" "21" "48" "A-BK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HAC" BY REFERENCE W-HAC "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RSC" "9" "14" "21" "1" "A-HAC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RSC" BY REFERENCE W-RSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-ACP" " " "17" "0" "16" "A-RSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZR" "S9" "17" "23" "8" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZR" BY REFERENCE W-ZR "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SS" "S9" "17" "47" "8" "A-ZR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SS" BY REFERENCE W-SS "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-ACP" " " "18" "0" "14" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZSHZ" "S9" "18" "24" "7" " " "12C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZSHZ" BY REFERENCE W-ZSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSHZ" "S9" "18" "48" "7" "A-ZSHZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSHZ" BY REFERENCE W-SSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-ACP" " " "19" "0" "16" "12C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SB" "S9" "19" "23" "8" " " "13C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SB" BY REFERENCE W-SB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GC" "S9" "19" "47" "8" "A-SB" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GC" BY REFERENCE W-GC "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-ACP" " " "20" "0" "16" "13C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SZ" "S9" "20" "23" "8" " " "14C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SZ" BY REFERENCE W-SZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EG" "S9" "20" "47" "8" "A-SZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EG" BY REFERENCE W-EG "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "15C-ACP" " " "21" "0" "8" "14C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ST" "S9" "21" "23" "8" " " "15C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ST" BY REFERENCE W-ST "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "40" "1" "15C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "198" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DAV" " " "5" "0" "2" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DAV" "X" "5" "23" "1" " " "D-DAV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DAV" "X" "5" "26" "1" "01D-DAV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KB" "N" "6" "25" "10" "D-DAV" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KB" BY REFERENCE W-KBD "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "8" "27" "52" "D-KB" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MAV" " " "9" "0" "2" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MAV" "X" "9" "23" "1" " " "D-MAV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MAV" "X" "9" "26" "1" "01D-MAV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "0" "0" "11" "D-MAV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "ZZZZZZZZZZ-" "10" "21" "11" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BK" " " "11" "0" "32" "D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BK" "N" "11" "27" "16" " " "D-BK" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BK" BY REFERENCE W-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-BK" "N" "11" "45" "16" "01D-BK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-BK" BY REFERENCE W-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-RSC" "N" "14" "24" "10" "D-BK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-RSC" BY REFERENCE W-RSM "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S" " " "0" "0" "79" "D-RSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-S" " " "17" "0" "18" " " "D-S" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZR" "ZZZZZZZZ-" "17" "23" "9" " " "01D-S" RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZR" BY REFERENCE W-ZR "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SS" "ZZZZZZZZ-" "17" "47" "9" "D-ZR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SS" BY REFERENCE W-SS "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-S" " " "18" "0" "16" "01D-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "D-ZSHZ" "ZZZZZZZ-" "18" "24" "8" " " "02D-S" RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZSHZ" BY REFERENCE W-ZSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSHZ" "ZZZZZZZ-" "18" "48" "8" "D-ZSHZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSHZ" BY REFERENCE W-SSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-S" " " "19" "0" "18" "02D-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SB" "ZZZZZZZZ-" "19" "23" "9" " " "03D-S" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SB" BY REFERENCE W-SB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GC" "ZZZZZZZZ-" "19" "47" "9" "D-SB" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GC" BY REFERENCE W-GC "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-S" " " "20" "0" "18" "03D-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SZ" "ZZZZZZZZ-" "20" "23" "9" " " "04D-S" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SZ" BY REFERENCE W-SZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EG" "ZZZZZZZZ-" "20" "47" "9" "D-SZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-EG" BY REFERENCE W-EG "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-S" " " "21" "0" "9" "04D-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ST" "ZZZZZZZZ-" "21" "23" "9" " " "05D-S" RETURNING RESU.
       CALL "SD_From" USING 
            "D-ST" BY REFERENCE W-ST "8" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "61" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-MAN" "X" "9" "21" "8" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-BK" "X" "11" "21" "40" "S-MAN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-RSC" "X" "14" "21" "13" "S-BK" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "526" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "526" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "24" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "22" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "22" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "22" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "17" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "24" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "25" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "26" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "18" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "25" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "22" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" " " "24" "0" "20" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME17" "N" "24" "15" "8" " " "E-ME17" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME17" "N" "24" "25" "4" "01E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME17" BY REFERENCE W-HEN1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME17" "N" "24" "30" "4" "02E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME17" BY REFERENCE W-HEN2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-ME17" "N" "24" "35" "4" "03E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-ME17" BY REFERENCE W-HEN3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" " " "24" "0" "32" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME18" "X" "24" "15" "28" " " "E-ME18" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME18" "9" "24" "33" "4" "01E-ME18" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME18" BY REFERENCE T-NTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "20" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           INITIALIZE W-DATA.
           COPY LIBCPR.
      *
           MOVE ZERO TO W-NGD.
           MOVE D-NTNG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE W-NGD TO W-TNG.
      *
           MOVE ZERO TO W-NGD.
           MOVE D-NBNG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE W-NGD TO W-BNG.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-M_PNAME1 "SHARED" BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
           CALL "DB_F_Open" USING
            "I-O" TNO-M_PNAME1 " " BY REFERENCE TNO-M_IDLST "1"
            "NO-KEY" BY REFERENCE NO-KEY.
      *
           MOVE "01" TO NO-KEY.
      *           READ TNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TNO-M_PNAME1 BY REFERENCE TNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
      *
           MOVE ZERO TO W-HNGP.
           MOVE DATE-04R TO W-HNGPS.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF
           SUBTRACT DATE-YC1 FROM W-HNEN.
           MOVE W-HNGPS TO W-DATE.
      *
           MOVE ZERO TO W-SNGP.
           ACCEPT W-SNGPS FROM DATE.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       M-040.
           CALL "SD_Screen_Output" USING "SCTD10" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF
           IF  W-ACT NOT = 1
               GO TO M-120
           END-IF.
       M-060.
           IF  W-DATE NOT = ZERO
               CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU
               CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU
               CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-DAV" D-DAV "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-040
               ELSE
                   GO TO M-200
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-080
           END-IF
      *
           MOVE W-NG TO W-HNGS.
           MOVE ZERO TO W-HNEN1.
           IF  W-HNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-HNEN
           ELSE
               IF  W-HNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-HNEN
               END-IF
           END-IF
           IF  W-HNG < W-TNG AND < W-BNG
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-100
           END-IF
      *
           MOVE W-PEY TO W-HPEY.
           IF  W-SNGP < W-HNGP
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
      *
           MOVE W-HNGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE W-HNGP TO W-ENGP.
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           ADD 1 TO W-EGET.
           IF  W-EGET = 13
               ADD 1 TO W-ENEN
               MOVE 1 TO W-EGET
           END-IF
           IF  W-ACT = 2
               GO TO M-300
           END-IF.
       M-120.
           CALL "SD_Screen_Output" USING "SCTD10" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU
               CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU
               CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU
               CALL "SD_Output" USING "D-DAV" D-DAV "p" RETURNING RESU
           END-IF
           IF  W-KBND NOT = ZERO
               CALL "SD_Output" USING "A-KBN" A-KBN "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-KBN "A-KBN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-060
               ELSE
                   GO TO M-040
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-KBND NOT = 00 AND 01 AND 02 AND 03 AND 04
                               AND 11 AND 12 AND 13 AND 22
                               AND 30 AND 31 AND 32 AND 33 AND 34
               GO TO M-120
           END-IF
           IF  W-KBND = 00
               MOVE "åªã‡ì¸ã‡Å@" TO W-KBD
           END-IF
           IF  W-KBND = 01
               MOVE "êUçûì¸ã‡Å@" TO W-KBD
           END-IF
           IF  W-KBND = 02
               MOVE "è¨êÿéËì¸ã‡" TO W-KBD
           END-IF
           IF  W-KBND = 03
               MOVE "îÉä|ã‡ëäéE" TO W-KBD
           END-IF
           IF  W-KBND = 04
               MOVE "ëºëäéEì¸ã‡" TO W-KBD
           END-IF
           IF  W-KBND = 11
               MOVE "éÛéÊñÒéËÅ@" TO W-KBD
           END-IF
           IF  W-KBND = 12
               MOVE "éÛéÊà◊éËÅ@" TO W-KBD
           END-IF
           IF  W-KBND = 13
               MOVE "éÛéÊìdç¬Å@" TO W-KBD
           END-IF
           IF  W-KBND = 22
               MOVE "éxï•à◊éËÅ@" TO W-KBD
           END-IF
           IF  W-KBND = 30
               MOVE "åªã‡éxï•Å@" TO W-KBD
           END-IF
           IF  W-KBND = 31
               MOVE "êUçûéxï•Å@" TO W-KBD
           END-IF
           IF  W-KBND = 32
               MOVE "éxï•è¨êÿéË" TO W-KBD
           END-IF
           IF  W-KBND = 33
               MOVE "îÑä|ã‡ëäéE" TO W-KBD
           END-IF
           IF  W-KBND = 34
               MOVE "ëºëäéEéxï•" TO W-KBD
           END-IF.
       M-160.
           CALL "SD_Output" USING "D-KB" D-KB "p" RETURNING RESU.
           IF  W-ACT NOT = 1
               GO TO M-200
           END-IF
           IF  W-KBND < 10
               COMPUTE W-NOD = NO-NDN + 1
               GO TO M-220
           END-IF
           IF  W-KBND > 29
               COMPUTE W-NOD = NO-HDN + 1
               GO TO M-220
           END-IF
           IF  W-KBND > 19
               IF  NO-SDN = ZERO
                   COMPUTE W-NOD = NO-SMN + 1
               ELSE
                   COMPUTE W-NOD = NO-SDN + 1
               END-IF
           END-IF
           IF  W-KBND < 20
               IF  NO-UDN = ZERO
                   COMPUTE W-NOD = NO-UMN + 1
               ELSE
                   COMPUTE W-NOD = NO-UDN + 1
               END-IF
           END-IF
           IF  W-NOD = ZERO
               MOVE 1 TO W-NOD
           END-IF.
       M-200.
           CALL "SD_Output" USING "A-NO" A-NO "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-NOD = ZERO
               GO TO M-200
           END-IF.
       M-220.
           IF  W-KBND < 10 OR > 29
               IF  W-NOD = ZERO
                   ADD 1 TO W-NOD
               END-IF
           END-IF
           CALL "SD_Output" USING "A-NO" A-NO "p" RETURNING RESU.
           MOVE W-KEYD TO TD-KEY
      *           READ TDT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-260
           END-IF
           IF  W-ACT NOT = 1
               GO TO M-240
           END-IF
           IF  W-KBND < 10 OR > 29
               ADD 1 TO W-NOD
               GO TO M-220
           END-IF
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-200.
       M-240.
           PERFORM S-20 THRU S-45.
           MOVE SPACE TO W-HEN1 W-HEN2 W-HEN3.
           IF  W-RSC = 0
               IF  W-HCR = 1
                   MOVE "óÃé˚" TO W-HEN1
               END-IF
           END-IF
           IF  W-KBN > 9 AND < 30
               IF  W-HCT = 1
                   MOVE "éËå`" TO W-HEN2
               END-IF
           END-IF
           IF (W-ZR NOT = ZERO) OR (W-SS NOT = ZERO) OR
              (W-ZSHZ NOT = ZERO) OR (W-SSHZ NOT = ZERO)
               IF  W-HCK = 1
                   MOVE "çwîÉ" TO W-HEN3
               END-IF
           END-IF
           IF (W-HEN1 NOT = SPACE) OR (W-HEN2 NOT = SPACE) OR
              (W-HEN3 NOT = SPACE)
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-ACT = 3
               GO TO M-780
           END-IF
           GO TO M-060.
       M-260.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-200
           END-IF
           IF  W-KBND > 29 OR < 10
               GO TO M-300
           END-IF
           IF  W-KBND > 19
               GO TO M-280
           END-IF
           MOVE W-NOD TO UT-KEY.
      *           READ UKET-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-160.
       M-280.
           MOVE W-NOD TO ST-KEY.
      *           READ SHIT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SHIT-M_PNAME1 BY REFERENCE SHIT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-160.
       M-300.
           IF  W-ACT = 1
               INITIALIZE W-R
               MOVE SPACE TO W-HAC
           END-IF
           MOVE W-DATE TO W-USD.
           MOVE W-KEYD TO W-KEY.
           MOVE W-HNEN TO W-NENS.
           IF  W-HCT NOT = 1
               IF  W-KBND < 10 OR > 29
                   MOVE 9 TO W-HCT
               ELSE
                   MOVE 0 TO W-HCT
               END-IF
           END-IF
           IF  W-HCK NOT = 1
               IF  W-KBND < 20
                   MOVE 9 TO W-HCK
               ELSE
                   MOVE 0 TO W-HCK
               END-IF
           END-IF
           IF  W-HCR NOT = 1
               IF  W-KBND > 19
                   MOVE 9 TO W-HCR
               ELSE
                   MOVE 0 TO W-HCR
               END-IF
           END-IF.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-120
               ELSE
                   GO TO M-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-KBN > 19
               GO TO M-360
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-320
           END-IF
           MOVE T-NAME TO W-TNA.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  T-TCD NOT = T-NTCD
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-320
           END-IF
           GO TO M-380.
       M-360.
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-320
           END-IF
           MOVE S-NAME TO W-TNA.
           IF  W-TCD > 4999
               GO TO M-320
           END-IF
           IF  W-TCD = 1905
               IF  W-KBN > 29
                   CALL "SD_Output" USING
                    "E-ME16" E-ME16 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-320
               END-IF
           END-IF.
       M-380.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-KBN < 10
               MOVE ZERO TO W-MAN
               CALL "SD_Output" USING "S-MAN" S-MAN "p" RETURNING RESU
               GO TO M-460
           END-IF
           IF  W-KBN > 29
               MOVE ZERO TO W-MAN
               CALL "SD_Output" USING "S-MAN" S-MAN "p" RETURNING RESU
               GO TO M-580
           END-IF.
       M-400.
           CALL "SD_Output" USING "D-MAV" D-MAV "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-MAN1 "A-MAN1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-400
           END-IF
           MOVE ZERO TO W-KNGP.
           MOVE W-MAN1 TO W-KNEN2.
           IF  W-KNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-KNEN
           ELSE
               IF  W-KNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-KNEN
               END-IF
           END-IF
           IF  W-HNEN > W-KNEN
               GO TO M-400
           END-IF.
       M-420.
           CALL "SD_Accept" USING BY REFERENCE A-MAN2 "A-MAN2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-400
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-420
           END-IF
           IF  W-MAN2 < 1 OR > 12
               GO TO M-420
           END-IF
           MOVE W-MAN2 TO W-KGET.
           IF  W-HNG > W-KNG
               GO TO M-420
           END-IF.
       M-440.
           CALL "SD_Accept" USING BY REFERENCE A-MAN3 "A-MAN3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-420
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-440
           END-IF
           IF  W-MAN3 < 1 OR > 31
               GO TO M-440
           END-IF
           MOVE W-MAN3 TO W-KPEY.
           IF  W-HNGP > W-KNGP
               GO TO M-420
           END-IF
           IF  W-ENGP < W-KNGP
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-KNGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-400
           END-IF
           IF  W-KBN = 22
               IF  CL-SJ = 1
                   CALL "SD_Output" USING
                    "E-ME14" E-ME14 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-KBN > 19
               GO TO M-480
           END-IF.
       M-460.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "S9" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KBN < 10
                   GO TO M-320
               ELSE
                   GO TO M-400
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-460
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-KIN = ZERO
               GO TO M-460
           END-IF
           IF  W-KBN > 09 AND < 30
               IF  W-KIN < ZERO
                   GO TO M-460
               END-IF
           END-IF
           IF  W-KBN < 10
               MOVE ZERO TO W-BK W-RSC
               MOVE SPACE TO W-HAC
               CALL "SD_Output" USING "S-BK" S-BK "p" RETURNING RESU
               CALL "SD_Output" USING "A-HAC" A-HAC "p" RETURNING RESU
               GO TO M-575
           END-IF.
       M-480.
           CALL "SD_Accept" USING BY REFERENCE A-BK "A-BK" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KBN > 19
                   GO TO M-400
               ELSE
                   GO TO M-460
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-480
           END-IF
           IF  W-KBN NOT < 20
               IF  W-BK NOT = 2200 AND 3200
                   GO TO M-480
               END-IF
           END-IF
           MOVE W-BK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-480
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "D-BK" D-BK "p" RETURNING RESU.
           IF  W-KBN > 19
               MOVE SPACE TO W-HAC W-RSM
               MOVE ZERO TO W-RSC
               CALL "SD_Output" USING "A-HAC" A-HAC "p" RETURNING RESU
               CALL "SD_Output" USING "S-RSC" S-RSC "p" RETURNING RESU
               GO TO M-580
           END-IF
           MOVE ZERO TO W-ZR W-SS W-SB W-GC W-SZ W-EG W-ST
                                             W-ZSHZ W-SSHZ.
           CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU.
           IF  W-HCK NOT = 1
               MOVE 9 TO W-HCK
           END-IF.
       M-560.
           CALL "SD_Output" USING "A-HAC" A-HAC "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-HAC "A-HAC" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-480
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-560
           END-IF
           MOVE W-HACD TO W-NAD.
           IF  SPACE = W-NA(1) OR W-NA(3) OR W-NA(5) OR W-NA(7)
                         OR W-NA(9) OR W-NA(11) OR W-NA(13) OR W-NA(15)
                         OR W-NA(17) OR W-NA(19) OR W-NA(21) OR W-NA(23)
                         OR W-NA(25) OR W-NA(27) OR W-NA(29) OR W-NA(31)
                         OR W-NA(33) OR W-NA(35) OR W-NA(37) OR W-NA(39)
                         OR W-NA(41) OR W-NA(43) OR W-NA(45) OR W-NA(47)
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-560
           END-IF.
       M-570.
           CALL "SD_Accept" USING BY REFERENCE A-RSC "A-RSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KBN < 10
                   GO TO M-460
               ELSE
                   GO TO M-560
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-570
           END-IF
           IF  W-RSC NOT = 0 AND 9
               GO TO M-570
           END-IF.
       M-575.
           IF  W-RSC = 0
               MOVE "î≠çsÇ∑ÇÈÅ@" TO W-RSM
           END-IF
           IF  W-RSC = 9
               MOVE "î≠çsÇµÇ»Ç¢" TO W-RSM
               IF  W-HCR NOT = 1
                   MOVE 9 TO W-HCR
               END-IF
           END-IF
           CALL "SD_Output" USING "D-RSC" D-RSC "p" RETURNING RESU.
           GO TO M-780.
       M-580.
           CALL "SD_Accept" USING BY REFERENCE A-ZR "A-ZR" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KBN > 29
                   GO TO M-320
               ELSE
                   GO TO M-480
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-580
           END-IF
           CALL "SD_Output" USING "D-ZR" D-ZR "p" RETURNING RESU.
       M-610.
           CALL "SD_Accept" USING BY REFERENCE A-ZSHZ "A-ZSHZ" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-580
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-610
           END-IF
           CALL "SD_Output" USING "D-ZSHZ" D-ZSHZ "p" RETURNING RESU.
       M-620.
           CALL "SD_Accept" USING BY REFERENCE A-SS "A-SS" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-610
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF
           CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU.
       M-625.
           CALL "SD_Accept" USING BY REFERENCE A-SSHZ "A-SSHZ" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-620
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-625
           END-IF
           CALL "SD_Output" USING "D-SSHZ" D-SSHZ "p" RETURNING RESU.
           IF  ZERO = W-SS AND W-SSHZ
               GO TO M-630
           END-IF
           MOVE W-TCD TO W-TCDW.
           ADD 5 TO W-TCD1.
           MOVE W-TCDW TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-620
           END-IF.
       M-630.
           IF  W-KBN > 29
               MOVE ZERO TO W-SB W-GC W-SZ W-EG W-ST
               CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU
               CALL "SD_Output" USING "D-GC" D-GC "p" RETURNING RESU
               CALL "SD_Output" USING "D-SZ" D-SZ"p" RETURNING RESU
               CALL "SD_Output" USING "D-EG" D-EG "p" RETURNING RESU
               CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU
               GO TO M-740
           END-IF.
       M-640.
           CALL "SD_Accept" USING BY REFERENCE A-SB "A-SB" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-620
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-640
           END-IF
           CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU.
       M-660.
           CALL "SD_Accept" USING BY REFERENCE A-GC "A-GC" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-640
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-660
           END-IF
           CALL "SD_Output" USING "D-GC" D-GC "p" RETURNING RESU.
       M-680.
           CALL "SD_Accept" USING BY REFERENCE A-SZ "A-SZ" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-660
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-680
           END-IF
           CALL "SD_Output" USING "D-SZ" D-SZ"p" RETURNING RESU.
       M-700.
           CALL "SD_Accept" USING BY REFERENCE A-EG "A-EG" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-680
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-700
           END-IF
           CALL "SD_Output" USING "D-EG" D-EG "p" RETURNING RESU.
       M-720.
           CALL "SD_Accept" USING BY REFERENCE A-ST "A-ST" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-700
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-720
           END-IF
           CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU.
       M-740.
           COMPUTE W-KIN = W-ZR + W-SS + W-SB + W-GC + W-SZ
                                + W-EG + W-ST + W-ZSHZ + W-SSHZ.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-KIN = ZERO
               GO TO M-580
           END-IF
           IF  W-KBN > 09 AND < 30
               IF  W-KIN < ZERO
                   GO TO M-580
               END-IF
           END-IF
           IF  W-HCK NOT = 1
               IF (W-ZR = ZERO) AND (W-SS = ZERO) AND
                  (W-ZSHZ = ZERO) AND (W-SSHZ = ZERO)
                   MOVE 9 TO W-HCK
               ELSE
                   MOVE 0 TO W-HCK
               END-IF
           END-IF
           IF (W-ZR NOT = ZERO) OR (W-SS NOT = ZERO) OR
              (W-ZSHZ NOT = ZERO) OR (W-SSHZ NOT = ZERO)
               IF  W-HNGP < W-BNG
                   CALL "SD_Output" USING
                    "E-ME14" E-ME14 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   IF  W-KBN > 29
                       GO TO M-625
                   ELSE
                       GO TO M-720
                   END-IF
               END-IF
           END-IF.
       M-780.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-800
           END-IF
           IF  W-ACT = 3
               GO TO M-200
           END-IF
           IF  W-KBN < 10
               GO TO M-460
           END-IF
           IF  W-KBN < 20
               GO TO M-570
           END-IF
           IF  W-KBN > 29
               GO TO M-625
           END-IF
           GO TO M-720.
       M-800.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-780
           END-IF
           IF  W-DMM = 1
               GO TO M-820
           END-IF
           IF  W-DMM NOT = 9
               GO TO M-780
           END-IF
           GO TO M-120.
       M-820.
           IF  W-ACT = 3
               GO TO M-920
           END-IF
           IF  W-ACT = 2
               GO TO M-900
           END-IF
           INITIALIZE TDT-R.
           MOVE W-R TO TDT-R.
      *           WRITE TDT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           IF  W-KBN < 10
               GO TO M-870
           END-IF
           IF  W-KBN > 29
               GO TO M-860
           END-IF
           IF  W-KBN > 19
               GO TO M-840
           END-IF
      *
           IF (NO-UDN < W-NO) OR (NO-UDN = 9999)
               MOVE W-NO TO NO-UDN
               GO TO M-880
           END-IF
           GO TO M-120.
       M-840.
           IF (NO-SDN < W-NO) OR (NO-SDN = 9999)
               MOVE W-NO TO NO-SDN
               GO TO M-880
           END-IF
           GO TO M-120.
       M-860.
           IF (NO-HDN < W-NO) OR (NO-HDN = 9999)
               MOVE W-NO TO NO-HDN
               GO TO M-880
           END-IF
           GO TO M-120.
       M-870.
           IF (NO-NDN < W-NO) OR (NO-NDN = 9999)
               MOVE W-NO TO NO-NDN
               GO TO M-880
           END-IF
           GO TO M-120.
       M-880.
      *           REWRITE TNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TNO-M_PNAME1 TNO-M_LNAME TNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-120.
       M-900.
           MOVE W-R TO TDT-R.
           MOVE 0 TO TD-PCHK.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           GO TO M-120.
       M-920.
      *           DELETE TDT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TDT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           GO TO M-120.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TNO-M_IDLST TNO-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
      *           READ TDT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-20.
           INITIALIZE W-R.
           MOVE SPACE TO W-HAC.
           MOVE TDT-R TO W-R.
           MOVE W-USD TO W-DATE.
           MOVE ZERO TO W-HNGP.
           MOVE W-DATE TO W-HNGPS.
           IF  W-HNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-HNEN
           ELSE
               IF  W-HNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-HNEN
               END-IF
           END-IF
           MOVE SPACE TO W-TNA.
           IF  W-KBN > 19
               GO TO S-25
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "Å@ÅñÅñÅ@Å@ìæà”êÊÅ@Ç»ÇµÅ@Å@ÅñÅñ" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
           GO TO S-30.
       S-25.
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "Å@ÅñÅñÅ@Å@édì¸êÊÅ@Ç»ÇµÅ@Å@ÅñÅñ" TO S-NAME
           END-IF
           MOVE S-NAME TO W-TNA.
       S-30.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DAV" D-DAV "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-KBN > 29
               CALL "SD_Output" USING "D-ZR" D-ZR "p" RETURNING RESU
               CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZSHZ" D-ZSHZ "p" RETURNING RESU
               CALL "SD_Output" USING "D-SSHZ" D-SSHZ "p" RETURNING RESU
               GO TO S-45
           END-IF
           IF  W-KBN > 9
               GO TO S-35
           END-IF
           IF  W-RSC = 0
               MOVE "î≠çsÇ∑ÇÈÅ@" TO W-RSM
           END-IF
           IF  W-RSC = 9
               MOVE "î≠çsÇµÇ»Ç¢" TO W-RSM
           END-IF
           CALL "SD_Output" USING "A-RSC" A-RSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-RSC" D-RSC "p" RETURNING RESU.
           GO TO S-45.
       S-35.
           CALL "SD_Output" USING "A-MAN1" A-MAN1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MAN2" A-MAN2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MAN3" A-MAN3 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MAV" D-MAV "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BK" A-BK "p" RETURNING RESU.
           MOVE SPACE TO W-BNA W-SNA.
           MOVE W-BK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ÇaÇ`ÇmÇjÇlÅ@ñ≥Çµ" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "D-BK" D-BK "p" RETURNING RESU.
           IF  W-KBN < 20
               CALL "SD_Output" USING "A-HAC" A-HAC "p" RETURNING RESU
               CALL "SD_Output" USING "A-RSC" A-RSC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-KNGP.
           IF  W-MAN = ZERO
               GO TO S-45
           END-IF
           MOVE W-MAN TO W-KNGPS.
           IF  W-KNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-KNEN
           ELSE
               IF  W-KNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-KNEN
               END-IF
           END-IF.
       S-45.
           EXIT.
