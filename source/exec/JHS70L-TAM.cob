       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS70L.
      *********************************************************
      *    PROGRAM         :  ìùàÍì`ï[çÏê¨ÅiÉgÉâÉXÉRëºÅj      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=ñ{é– , 1=ì°ìc                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  JS-SIGN            PIC  9(001).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-POC              PIC  9(001) VALUE 0.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-P01.
           02  P-20K1         PIC  X(005).
           02  F              PIC  X(009).
           02  P-NAME         PIC  X(010).
           02  F              PIC  X(080).
       01  W-P02.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(007).
           02  P-TNA          PIC  N(014).
           02  P-TNAD  REDEFINES P-TNA.
             03  F            PIC  N(002).
             03  P-TNAU       PIC  N(012).
           02  F              PIC  X(003).
           02  P-SCD          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TPC          PIC  9(004).
           02  P-MCDD  REDEFINES P-TPC.
             03  F            PIC  9(001).
             03  P-MCD        PIC  9(003).
           02  F              PIC  X(003).
           02  P-BRC          PIC  Z(004).
           02  F              PIC  X(001).
           02  P-DPC          PIC  X(002).
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-DNO0         PIC  9(001).
           02  F              PIC  X(001).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(005).
           02  P-NR           PIC  N(006).
           02  F              PIC  X(011).
           02  P-NNEN         PIC  9(002).
           02  P-NGETD        PIC  9(002).
           02  P-AGET  REDEFINES P-NGETD.
             03  P-NGET       PIC Z9.
           02  P-NPEYD        PIC  9(002).
           02  P-APEY  REDEFINES P-NPEYD.
             03  P-NPEY       PIC Z9.
           02  F              PIC  X(004).
           02  P-20K2         PIC  X(005).
       01  W-P03.
           02  P-15K3         PIC  X(005).
           02  F              PIC  X(007).
           02  P-HNA          PIC  N(016).
           02  P-HNAD  REDEFINES P-HNA.
             03  F            PIC  N(008).
             03  P-HNAU       PIC  N(008).
           02  F              PIC  X(006).
           02  P-SHC          PIC  X(008).
           02  P-COR          PIC  N(004).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  X(004).
           02  P-SU           PIC  Z(006).
           02  F              PIC  X(005).
           02  P-GTN          PIC  Z(006).
           02  F              PIC  X(002).
           02  P-GKIN         PIC  Z(009).
           02  P-UTN          PIC  Z(006).
           02  P-UKIN         PIC  Z(009).
           02  P-20K3         PIC  X(005).
       01  W-DATA.
           02  W-TPC          PIC  9(001).
           02  W-HKC          PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-SED.
             03  W-SNGP       PIC  9(006).
             03  W-ENGP       PIC  9(006).
             03  W-SCYO       PIC  9(007).
             03  W-ECYO       PIC  9(007).
             03  W-STPC       PIC  9(004).
             03  W-ETPC       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-NGP          PIC  9(006).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-CYO          PIC  9(007).
           02  W-CYOD  REDEFINES W-CYO.
             03  W-TCD        PIC  9(004).
             03  W-CCD        PIC  9(003).
           02  W-DNO          PIC  9(006).
           02  W-DNO0         PIC  9(001).
           02  W-HNO          PIC  X(010).
           02  W-TNA          PIC  N(026).
           02  W-TNAD  REDEFINES W-TNA.
             03  W-TNAO       PIC  N(014).
             03  W-TNAU       PIC  N(012).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-INV          PIC  9(001).
           02  W-LC           PIC  9(002).
           02  W-NC1          PIC  9(002).
           02  W-NC2          PIC  9(002).
           02  W-ANA          PIC  N(024).
           02  W-ANAD  REDEFINES W-ANA.
             03  W-NAD   OCCURS  24.
               04  W-NA       PIC  N(001).
           02  W-AHNA         PIC  N(024).
           02  W-AHNAD REDEFINES W-AHNA.
             03  W-HNAD   OCCURS  24.
               04  W-HNA      PIC  N(001).
           02  W-AHMD  REDEFINES W-AHNA.
             03  W-HMD1       PIC  N(016).
             03  W-HMD2       PIC  N(008).
           02  W-ACOR         PIC  N(008).
           02  W-ACORD REDEFINES W-ACOR.
             03  W-CORD   OCCURS   8.
               04  W-COR      PIC  N(001).
           02  W-ACRD REDEFINES W-ACOR.
             03  W-CR1        PIC  N(004).
             03  W-CR2        PIC  N(004).
           02  W-SHC          PIC  X(008).
           02  W-SC           PIC  9(002).
           02  W-ASZD.
             03  W-SZD   OCCURS  34.
               04  W-SZ       PIC  X(003).
           02  W-MSZ.
             03  F            PIC  X(045) VALUE
                  "SS S  M  L  LL XL XXL125130135140150160170180".
             03  F            PIC  X(045) VALUE
                  "190200210215220225230235240245250255260265270".
             03  F            PIC  X(012) VALUE
                  "275280290300".
           02  W-ASIZD.
             03  W-SIZD  OCCURS  34.
               04  W-SIZ      PIC  X(004).
           02  W-MSIZ.
             03  F            PIC  X(048) VALUE
                  "SS  S   M   L   LL  XL  XXL 12.513.013.514.015.0".
             03  F            PIC  X(048) VALUE
                  "16.017.018.019.020.021.021.522.022.523.023.524.0".
             03  F            PIC  X(040) VALUE
                  "24.525.025.526.026.527.027.528.029.030.0".
           COPY LSTAT.
      *
           COPY LITCM.
           COPY LIHIM2.
           COPY LITDIF-TAM.
           COPY LRCODE.
      *FD  TDIW
       01  TDIW_JHS70L.
           02  TDIW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TDIW_LNAME     PIC  X(011) VALUE "TDIW_JHS70L".
           02  F              PIC  X(001).
           02  TDIW_KEY1      PIC  X(100) VALUE SPACE.
           02  TDIW_SORT      PIC  X(100) VALUE SPACE.
           02  TDIW_IDLST     PIC  X(100) VALUE SPACE.
           02  TDIW_RES       USAGE  POINTER.
       01  TDIW-R.
           02  TDIW-KEY.
             03  TDIW-DNO      PIC  9(006).
             03  TDIW-GNO      PIC  9(001).
           02  TDIW-DATE       PIC  9(006).
           02  TDIW-NGP   REDEFINES TDIW-DATE.
             03  TDIW-NEN      PIC  9(002).
             03  TDIW-GET      PIC  9(002).
             03  TDIW-PEY      PIC  9(002).
           02  TDIW-CYO        PIC  9(007).
           02  TDIW-CYOD  REDEFINES TDIW-CYO.
             03  TDIW-TCD      PIC  9(004).
             03  TDIW-CCD      PIC  9(003).
           02  TDIW-TPC        PIC  9(004).
           02  TDIW-HCD        PIC  9(006).
           02  TDIW-SIZ        PIC  X(003).
           02  TDIW-SKB        PIC  9(001).
           02  TDIW-SNO        PIC  9(002).
           02  TDIW-SU         PIC S9(005).
           02  TDIW-GT         PIC  9(007).
           02  TDIW-UT         PIC  9(007).
           02  TDIW-GKIN       PIC S9(008).
           02  TDIW-UKIN       PIC S9(008).
           02  TDIW-JNOD.
             03  TDIW-JNO      PIC  9(006).
             03  TDIW-JGN      PIC  9(001).
           02  TDIW-SOK        PIC  9(001).
           02  TDIW-UNS        PIC  9(001).
           02  TDIW-ISU        PIC  9(003).
           02  TDIW-HNO        PIC  X(010).
           02  TDIW-TEKI       PIC  N(028).
           02  TDIW-TED   REDEFINES TDIW-TEKI.
             03  TDIW-THT      PIC  N(009).
             03  TDIW-TTE      PIC  N(019).
           02  TDIW-TRN        PIC  X(020).
           02  TDIW-JAN        PIC  X(013).
           02  TDIW-UTC        PIC  9(001).
           02  F               PIC  X(051).
           02  TDIW-NNGP       PIC  9(006).
           02  TDIW-NHMS       PIC  9(006).
           02  F               PIC  X(008).
           02  TDIW-PRC        PIC  9(001).
           02  TDIW-UPC        PIC  9(001).
       77  F                   PIC  X(001).
      *FD  WTNAF
       01  WTNAF_JHS70L.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JHS70L".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TNC     PIC  9(004).
           02  WTNA-NAME      PIC  N(026).
           02  F              PIC  X(008).
       77  F                  PIC  X(001).
      *FD  SP-F
       77  SP-R               PIC  X(136).
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
       01  C-MID.
           02  FILLER  PIC  N(014) VALUE
                "É`ÉFÅ[ÉìÉXÉgÉAìùàÍì`ï[Å@î≠çs".
           02  FILLER  PIC  N(013) VALUE
                "ÅiÉ^ÉCÉvópÇPå^Åj".
           02  FILLER  PIC  N(007) VALUE
                "ÅyÉgÉâÉXÉRëºÅz".
           02  FILLER  PIC  X(035) VALUE
                "ÉeÉXÉgÉvÉäÉìÉgàÛéö (YES=1,NO=9) [ ]".
           02  FILLER  PIC  X(010) VALUE
                "ÇPÅDî≠Å@çs".
           02  FILLER  PIC  X(018) VALUE
                "ÇQÅDçƒî≠çs     [ ]".
           02  FILLER  PIC  X(028) VALUE
                "ämîF (OK=1,NO=9) --->   ÿ¿∞›".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-HKC   PIC  9(001).
           02  FILLER.
             03  A-SNGP  PIC  9(006).
             03  A-SCYO  PIC  9(007).
             03  A-STPC  PIC  9(004).
           02  FILLER.
             03  A-ENGP  PIC  9(006).
             03  A-ECYO  PIC  9(007).
             03  A-ETPC  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  N(004) VALUE
                  "Å@ì˙ïtÅ@".
             03  FILLER  PIC  X(010) VALUE
                    "íºëóêÊ∫∞ƒﬁ".
             03  FILLER  PIC  X(015) VALUE
                    "ìXñº∫∞ƒﬁ(‹∞∏œ›)".
             03  FILLER  PIC  N(004) VALUE
                  "ÇeÇqÇnÇl".
             03  FILLER  PIC  N(002) VALUE
                  "ÇsÇn".
           02  D-SHMC.
             03  FILLER  PIC  X(008) VALUE
                  "      ".
             03  FILLER  PIC  X(010) VALUE
                  "          ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(045) VALUE
                  "                                             ".
             03  FILLER  PIC  X(045) VALUE
                  "                                             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ƒ∏≤ª∑ ≈º  ***".
             03  E-ME4   PIC  X(020) VALUE
                  "***  ¡Æ∏ø≥ª∑ ≈º  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  À›“≤ ≈º  ***".
             03  E-ME6   PIC  X(017) VALUE
                  "***  ª≤Ωﬁ ≈º  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  √›“≤ ≈º  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  TDIF REWRITE ¥◊∞  ***".
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
            "C-MID" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "28" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "2" "20" "26" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "4" "10" "14" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "7" "22" "35" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "10" "22" "10" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "11" "22" "18" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "23" "43" "28" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "37" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "7" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKC" "9" "11" "38" "1" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKC" BY REFERENCE W-HKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "15" "0" "17" "A-HKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNGP" "9" "15" "34" "6" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNGP" BY REFERENCE W-SNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCYO" "9" "15" "45" "7" "A-SNGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCYO" BY REFERENCE W-SCYO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STPC" "9" "15" "61" "4" "A-SCYO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STPC" BY REFERENCE W-STPC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "16" "0" "17" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENGP" "9" "16" "34" "6" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENGP" BY REFERENCE W-ENGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ECYO" "9" "16" "45" "7" "A-ENGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ECYO" BY REFERENCE W-ECYO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETPC" "9" "16" "61" "4" "A-ECYO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETPC" BY REFERENCE W-ETPC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "168" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHM" " " "0" "0" "45" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHM" "N" "14" "33" "8" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHM" "X" "14" "44" "10" "01D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHM" "X" "14" "57" "15" "02D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SHM" "N" "15" "22" "8" "03D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SHM" "N" "16" "22" "4" "04D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHMC" " " "0" "0" "123" "D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHMC" "X" "14" "33" "8" " " "D-SHMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHMC" "X" "14" "44" "10" "01D-SHMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHMC" "X" "14" "57" "15" "02D-SHMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SHMC" "X" "15" "34" "45" "03D-SHMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SHMC" "X" "16" "34" "45" "04D-SHMC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "150" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "20" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "17" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-00.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TDIW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TDIW_PNAME1 " " BY REFERENCE TDIW_IDLST "0".
       M-10.
      *           READ TDIW AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDIW_PNAME1 BY REFERENCE TDIW-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TDIW-TCD = 5000
               GO TO M-10
           END-IF
           IF  W-HKC = 1
               IF  TDIW-PRC = 9
                   GO TO M-10
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDIW-PRC NOT = 9
                   GO TO M-10
               END-IF
           END-IF
           IF  W-HKC = 2
               IF (TDIW-DATE < W-SNGP OR > W-ENGP) OR
                  (TDIW-CYO < W-SCYO OR > W-ECYO) OR
                  (TDIW-TPC < W-STPC OR > W-ETPC)
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  W-HKC = 1
                   IF  TDIW-TCD NOT = 6010
                       IF  TDIW-SOK NOT = 1
                           GO TO M-10
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-HKC = 1
               CALL "DB_F_Open" USING
                "I-O" TDIF_PNAME1 " " BY REFERENCE TDIF_IDLST "1"
                "TDI-KEY" BY REFERENCE TDI-KEY
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
           MOVE W-MSZ TO W-ASZD.
           MOVE W-MSIZ TO W-ASIZD.
       M-15.
           MOVE TDIW-DNO TO W-DNO.
           MOVE TDIW-HNO TO W-HNO.
           MOVE ZERO TO WT-D.
      *
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           IF  TDIW-TCD = 9850
               MOVE SPACE TO W-P01
               MOVE W-20K TO P-20K1
               MOVE "¿—◊ " TO P-NAME
               MOVE W-P01 TO SP-R
           END-IF
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE TDIW-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO TC-NAME
           END-IF
           MOVE SPACE TO W-TNA.
           MOVE TC-NAME TO W-TNA.
      *
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-TNA.
           MOVE W-TNAO TO P-TNA.
           MOVE SPACE TO P-NR.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-TNA P-NR.
           IF  W-TNAU NOT = SPACE
               MOVE W-TNAU TO P-TNAU
           END-IF
           MOVE "ì˙êiÉSÉÄáä" TO P-NR.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-TNA.
           IF  TDIW-TCD NOT = 9850
               GO TO M-20
           END-IF
           IF  TDIW-TPC = ZERO OR 999
               GO TO M-20
           END-IF
           MOVE TDIW-TPC TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO WTNA-NAME
           END-IF
           MOVE WTNA-NAME TO W-TNA.
           GO TO M-25.
       M-20.
           MOVE TDIW-TCD TO TC-TCD.
           MOVE TDIW-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO TC-NAME
           END-IF
           MOVE TC-NAME TO W-TNA.
       M-25.
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-TNA P-NR.
           MOVE W-TNAO TO P-TNA.
           IF  TDIW-TCD = 6010
               MOVE 4440 TO P-BRC
               MOVE "01" TO P-DPC
               MOVE 351374 TO P-THC
               IF  TDIW-CCD = 007
                   MOVE 727 TO P-MCD
                   GO TO M-30
               ELSE
                   MOVE 702 TO P-MCD
                   GO TO M-30
               END-IF
           END-IF
           IF  TDIW-TCD NOT = 9850
               GO TO M-30
           END-IF
           MOVE 07 TO P-SCD.
           MOVE TDIW-TPC TO P-TPC.
           IF  TDIW-TPC = 9001
               MOVE 9000 TO P-TPC
           END-IF
           MOVE "AA" TO P-DPC.
           IF  TDIW-CCD NOT = 900 AND 901
               MOVE 976318 TO P-THC
           ELSE
               MOVE 732974 TO P-THC
           END-IF
           IF  TDIW-TPC = 9999
               MOVE 901 TO P-BRC
           ELSE
               MOVE 501 TO P-BRC
           END-IF.
       M-30.
           MOVE TDIW-DNO TO P-DNO.
           IF  TDIW-TCD = 6010
               COMPUTE W-DNO0 = TDIW-DNO - ((TDIW-DNO / 7) * 7)
               MOVE W-DNO0 TO P-DNO0
           ELSE
               MOVE 0 TO P-DNO0
           END-IF
           MOVE TDIW-NEN TO P-NNEN.
           IF  TDIW-TCD = 6010
               MOVE TDIW-GET TO P-NGETD
               MOVE TDIW-PEY TO P-NPEYD
           ELSE
               MOVE TDIW-GET TO P-NGET
               MOVE TDIW-PEY TO P-NPEY
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  W-TNAU NOT = SPACE
               MOVE SPACE TO W-P02
               MOVE W-15K TO P-15K2
               MOVE W-20K TO P-20K2
               MOVE SPACE TO P-TNA P-NR
               MOVE W-TNAU TO P-TNAU
               MOVE SPACE TO SP-R
               MOVE W-P02 TO SP-R
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE 14 TO W-LC.
       M-35.
           PERFORM NAM-RTN THRU NAM-EX.
           IF  TDIW-SIZ NOT = 000
               PERFORM SIZ-RTN THRU SIZ-EX
           END-IF
           MOVE SPACE TO W-SHC.
           IF  TDIW-TCD NOT = 9850
               GO TO M-37
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
      *           SELECT CODEF WHERE TDIW-TCD = CODE-TCD AND
      *                              TDIW-HCD = CODE-HCD AND
      *                              TDIW-SKB = CODE-SIZ AND
      *                              TDIW-SNO = CODE-SNO.
      *///////////////
           CALL "DB_Select" USING
            CODEF_PNAME1 "WHERE" 
            "CODE-TCD" "=" TDIW-TCD "AND"
            "CODE-HCD" "=" TDIW-HCD "AND"
            "CODE-SIZ" "=" TDIW-SKB "AND"
            "CODE-SNO" "=" TDIW-SNO RETURNING RET.
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING CODEF_PNAME1
               MOVE SPACE TO CODE-JAN
           END-IF
           MOVE CODE-JAN TO W-SHC.
           IF  W-SHC = 00000000
               MOVE SPACE TO W-SHC
           END-IF.
       M-37.
           MOVE SPACE TO W-P03.
           MOVE W-15K TO P-15K3.
           MOVE W-20K TO P-20K3.
           MOVE SPACE TO P-HNA P-COR.
           IF (W-HMD2 = SPACE) AND (W-CR2 = SPACE)
               GO TO M-40
           END-IF
           IF  W-HMD2 NOT = SPACE
               MOVE W-HMD1 TO P-HNA
           END-IF
           IF  W-CR2 NOT = SPACE
               MOVE W-CR1 TO P-COR
           END-IF.
       M-40.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P03.
           MOVE W-15K TO P-15K3.
           MOVE W-20K TO P-20K3.
           MOVE SPACE TO P-HNA P-COR.
           IF  W-HMD2 = SPACE
               MOVE W-HMD1 TO P-HNA
           ELSE
               MOVE W-HMD2 TO P-HNAU
           END-IF
           IF  TDIW-TCD = 9850
               MOVE W-SHC TO P-SHC
           END-IF
           IF  W-CR2 = SPACE
               MOVE W-CR1 TO P-COR
           ELSE
               MOVE W-CR2 TO P-COR
           END-IF
           IF  TDIW-TCD NOT = 4990
               IF  TDIW-SIZ NOT = 000
                   IF  W-INV = 0
                       MOVE W-SIZ(W-SC) TO P-SIZ
                   END-IF
               END-IF
           END-IF
           MOVE TDIW-SU TO P-SU.
           MOVE TDIW-GT TO P-GTN.
           IF  TDIW-TCD = 6010
               IF  TDIW-HCD = 999999
                   MOVE 1 TO P-SU
                   MOVE TDIW-GKIN TO P-GTN
               END-IF
           END-IF
           MOVE TDIW-GKIN TO P-GKIN.
           MOVE TDIW-UT TO P-UTN.
           MOVE TDIW-UKIN TO P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDIW-SU TO WT-SU.
           IF  TDIW-TCD = 6010
               IF  TDIW-HCD = 999999
                   ADD 1 TO WT-SU
               END-IF
           END-IF
           ADD TDIW-GKIN TO WT-GKIN.
           ADD TDIW-UKIN TO WT-UKIN.
       M-45.
      *           READ TDIW AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDIW_PNAME1 BY REFERENCE TDIW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  TDIW-TCD = 5000
               GO TO M-45
           END-IF
           IF  W-HKC = 1
               IF  TDIW-PRC = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF (TDIW-DATE < W-SNGP OR > W-ENGP) OR
                  (TDIW-CYO < W-SCYO OR > W-ECYO) OR
                  (TDIW-TPC < W-STPC OR > W-ETPC)
                   GO TO M-80
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDIW-PRC NOT = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  W-HKC = 1
                   IF  TDIW-TCD NOT = 6010
                       IF  TDIW-SOK NOT = 1
                           GO TO M-45
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDIW-DNO NOT = W-DNO
               GO TO M-50
           END-IF
           IF  W-LC = ZERO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-35.
       M-50.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 2
               GO TO M-15
           END-IF
           PERFORM REW-RTN THRU REW-EX.
           IF  W-END NOT = 0
               GO TO M-90
           END-IF.
       M-55.
           IF  TDIW-TCD = 5000
               GO TO M-60
           END-IF
           IF  TDIW-PRC = 9
               GO TO M-60
           END-IF
           IF  JS-SIGN = 0
               IF  TDIW-TCD NOT = 6010
                   IF  TDIW-SOK NOT = 1
                       GO TO M-60
                   END-IF
               END-IF
           END-IF
           GO TO M-15.
       M-60.
      *           READ TDIW AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDIW_PNAME1 BY REFERENCE TDIW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           GO TO M-55.
       M-80.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 1
               PERFORM REW-RTN THRU REW-EX
           END-IF.
       M-85.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       M-90.
           IF  W-HKC = 1
               CALL "DB_F_Close" USING BY
                REFERENCE TDIF_IDLST TDIF_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
       M-95.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      ******************************************************************
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-TPC = 9
               GO TO ACP-05
           END-IF
           IF  W-TPC NOT = 1
               GO TO ACP-RTN
           END-IF
           PERFORM TST-RTN THRU TST-EX.
           GO TO ACP-RTN.
       ACP-05.
           CALL "SD_Accept" USING BY REFERENCE A-HKC "A-HKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-05
           END-IF
           IF  W-HKC = 1
               CALL "SD_Output" USING "D-SHMC" D-SHMC "p" RETURNING RESU
               GO TO ACP-50
           END-IF
           IF W-HKC NOT = 2
               GO TO ACP-05
           END-IF
           CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU.
       ACP-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNGP "A-SNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-05
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-10
           END-IF
           IF  W-SNGP = ZERO
               GO TO ACP-15
           END-IF
           MOVE W-SNGP TO W-NGP.
           IF  W-NEN < 17
               GO TO ACP-10
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO ACP-10
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-10
           END-IF.
       ACP-15.
           CALL "SD_Accept" USING BY REFERENCE A-ENGP "A-ENGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-15
           END-IF
           IF  W-ENGP = 999999
               GO TO ACP-20
           END-IF
           MOVE W-ENGP TO W-NGP.
           IF  W-GET < 1 OR > 12
               GO TO ACP-10
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-10
           END-IF.
       ACP-20.
           CALL "SD_Accept" USING BY REFERENCE A-SCYO "A-SCYO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-20
           END-IF.
       ACP-25.
           CALL "SD_Accept" USING BY REFERENCE A-ECYO "A-ECYO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-25
           END-IF
           IF  W-SCYO > W-ECYO
               GO TO ACP-25
           END-IF
           IF  W-SCYO < 9850000
               IF  W-ECYO > 9850999
                   GO TO ACP-30
               END-IF
           END-IF
           MOVE 0000 TO W-STPC.
           MOVE 9999 TO W-ETPC.
           CALL "SD_Output" USING "A-STPC" A-STPC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ETPC" A-ETPC "p" RETURNING RESU.
           GO TO ACP-50.
       ACP-30.
           CALL "SD_Accept" USING BY REFERENCE A-STPC "A-STPC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-30
           END-IF.
       ACP-35.
           CALL "SD_Accept" USING BY REFERENCE A-ETPC "A-ETPC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-35
           END-IF
           IF  W-STPC > W-ETPC
               GO TO ACP-35
           END-IF.
       ACP-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-HKC = 1
                   GO TO ACP-05
                ELSE
                   IF (W-SCYO < 9850000) AND (W-ECYO > 9850999)
                       GO TO ACP-35
                   ELSE
                       GO TO ACP-25
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-50
           END-IF
           IF  W-DMM = 9
               GO TO ACP-05
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-50
           END-IF.
       ACP-EX.
           EXIT.
       NAM-RTN.
           MOVE SPACE TO W-ANA W-AHNA W-ACOR.
           MOVE ZERO TO W-NC1.
           IF  TDIW-TCD = 4990
               MOVE TDIW-TRN TO W-ANA
               GO TO NAM-05
           END-IF
           MOVE TDIW-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-ANA.
       NAM-05.
           ADD 1 TO W-NC1.
           IF  W-NC1 > 24
               GO TO NAM-EX
           END-IF
           MOVE W-NA(W-NC1) TO W-HNA(W-NC1).
           IF  W-NA(W-NC1) NOT = SPACE
               GO TO NAM-05
           END-IF
           ADD 1 TO W-NC1.
           IF  W-NC1 > 24
               GO TO NAM-EX
           END-IF
           MOVE W-NA(W-NC1) TO W-HNA(W-NC1).
           IF  W-NA(W-NC1) NOT = SPACE
               GO TO NAM-05
           END-IF
           IF  TDIW-TCD = 4990
               GO TO NAM-EX
           END-IF
           MOVE ZERO TO W-NC2.
       NAM-10.
           ADD 1 TO W-NC1.
           IF  W-NC1 > 24
               GO TO NAM-EX
           END-IF
           IF  W-NC2 = 0
               IF  W-NA(W-NC1) = SPACE
                   GO TO NAM-10
               END-IF
           END-IF
           ADD 1 TO  W-NC2.
           IF  W-NC2 < 9
               MOVE W-NA(W-NC1) TO W-COR(W-NC2)
               GO TO NAM-10
           END-IF.
       NAM-EX.
           EXIT.
       SIZ-RTN.
           MOVE ZERO TO W-SC W-INV.
       SIZ-05.
           ADD 1 TO W-SC.
           IF  W-SC > 34
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO SIZ-EX
           END-IF
           IF  TDIW-SIZ NOT = W-SZ(W-SC)
               GO TO SIZ-05
           END-IF.
       SIZ-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P03.
           MOVE W-15K TO P-15K3.
           MOVE W-20K TO P-20K3.
           MOVE SPACE TO P-HNA P-COR.
           MOVE WT-SU TO P-SU.
           MOVE WT-GKIN TO P-GKIN.
           MOVE WT-UKIN TO P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_LineFeed" USING W-LC RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-HNO NOT = SPACE
               MOVE SPACE TO W-P01
               MOVE W-20K TO P-20K1
               MOVE W-HNO TO P-NAME
               MOVE W-P01 TO SP-R
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
       KEI-EX.
           EXIT.
       REW-RTN.
           MOVE SPACE TO TDI-KEY.
           MOVE W-DNO TO TDI-DNO.
      *           START TDIF KEY NOT < TDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDIF_PNAME1 "TDI-KEY" " NOT < " TDI-KEY RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO REW-EX
           END-IF.
       REW-05.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-EX
           END-IF
           IF  TDI-DNO NOT = W-DNO
               GO TO REW-EX
           END-IF
           MOVE 9 TO TDI-PRC.
      *           REWRITE TDI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO REW-EX
           END-IF
           GO TO REW-05.
       REW-EX.
           EXIT.
       TST-RTN.
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE SPACE TO W-P01.
           MOVE W-20K TO P-20K1.
           MOVE "XXXX" TO P-NAME.
           MOVE W-P01 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE ALL "Çm" TO P-TNA.
           MOVE SPACE TO P-NR.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-TNA.
           MOVE "ì˙êiÉSÉÄáä" TO P-NR.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P02.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-NR.
           MOVE ALL "Çm" TO P-TNA.
           MOVE 99 TO P-SCD P-NNEN P-NGET P-NPEY.
           MOVE 999 TO P-MCD P-BRC.
           MOVE "XX" TO P-DPC.
           MOVE 999999 TO P-THC.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-LC.
       TST-05.
           MOVE SPACE TO W-P03.
           MOVE W-15K TO P-15K3.
           MOVE W-20K TO P-20K3.
           MOVE ALL "Çm" TO P-HNA P-COR.
           MOVE "XXXX" TO P-SIZ.
           MOVE 99999 TO P-SU.
           MOVE 999999 TO P-GTN P-UTN.
           MOVE 999999999 TO P-GKIN P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-LC.
           IF  W-LC < 6
               GO TO TST-05
           END-IF
           MOVE SPACE TO W-P03.
           MOVE W-15K TO P-15K3.
           MOVE W-20K TO P-20K3.
           MOVE SPACE TO P-HNA P-COR.
           MOVE 99999 TO P-SU.
           MOVE 999999999 TO P-GKIN P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TST-EX.
           EXIT.
