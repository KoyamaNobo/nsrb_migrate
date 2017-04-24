       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT650.
      *********************************************************
      *    PROGRAM         :  売上・値引伝票　問合せ        　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT65 , SCHT66                 *
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
       77  W-MSG              PIC  X(040).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SGPD         PIC  9(004).
           02  W-EGPD         PIC  9(004).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SGP.
               04  W-SGET     PIC  9(002).
               04  W-SPEY     PIC  9(002).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-EGP.
               04  W-EGET     PIC  9(002).
               04  W-EPEY     PIC  9(002).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  X(002).
             03  W-NGS        PIC  9(004).
           02  W-ADNG.
             03  W-DNGD  OCCURS  3.
               04  W-DNG.
                 05  W-DNEN   PIC  9(004).
                 05  W-DGET   PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006).
           02  W-DNO          PIC  9(006).
           02  W-ASDD.
             03  W-SD    OCCURS   6.
               04  WS-DNO     PIC  9(006).
               04  WS-GNO     PIC  9(001).
               04  WS-NGP.
                 05  WS-NG.
                   06  WS-NEN PIC  9(004).
                   06  WS-GET PIC  9(002).
                 05  WS-PEY   PIC  9(002).
               04  WS-TCD     PIC  9(004).
               04  WS-HCD     PIC  9(006).
               04  WS-SIZ     PIC  9(001).
               04  WS-ASUD.
                 05  WS-ASU   OCCURS  10.
                   06  WS-SU  PIC S9(004)  COMP-3.
               04  WS-SUT     PIC S9(005).
               04  WS-T       PIC S9(005).
               04  WS-KIN     PIC S9(008).
               04  WS-CSC     PIC  9(001).
               04  WS-DC      PIC  9(001).
               04  F          PIC  X(005).
               04  WS-CCD     PIC  9(003).
               04  F          PIC  X(016).
               04  WS-TCD2    PIC  9(004).
               04  F          PIC  X(022).
               04  F          PIC  X(001).
               04  WS-UNC     PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-RC           PIC  9(001).
           02  W-BC           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-GN           PIC  9(002).
           02  SCNT           PIC  9(002).
           02  DCNT           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-NAME         PIC  N(024).
           02  W-NM1    REDEFINES W-NAME.
             03  W-NM11       PIC  N(017).
             03  W-NM12       PIC  N(007).
           02  W-NM2    REDEFINES W-NAME.
             03  W-NM21       PIC  N(016).
             03  W-NM22       PIC  N(008).
           02  W-NAD    REDEFINES W-NAME.
             03  W-NA         PIC  N(001)  OCCURS  24.
           02  W-NAMEW        PIC  N(024).
           02  W-NADW   REDEFINES W-NAMEW.
             03  W-NAW        PIC  N(001)  OCCURS  24.
           02  W-TNAD1        PIC  N(004).
           02  W-TND1   REDEFINES W-TNAD1.
             03  W-TN1        PIC  N(001)  OCCURS   4.
           02  W-DSP.
             03  W-ASD.
               04  W-AS    OCCURS  10.
                 05  W-S      PIC S9(004).
             03  W-SUT        PIC S9(005).
             03  W-T          PIC S9(005).
             03  W-KIN        PIC S9(008).
           02  W-DMM          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-DCN          PIC  N(002).
           02  W-AGD.
             03  W-GD    OCCURS  14.
               04  W-SIZ      PIC  9(001).
               04  W-ASUD.
                 05  W-ASU   OCCURS  10.
                   06  W-SU   PIC S9(004).
               04  W-CNA.
                 05  W-CNAF   PIC  N(010).
                 05  W-CNAR   PIC  N(010).
               04  W-BI.
                 05  W-BIF    PIC  N(010).
                 05  W-BIR    PIC  N(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LITCM.
      *FD  WSN-F
       01  WSN-F_HMT650.
           02  WSN-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WSN-F_LNAME    PIC  X(012) VALUE "WSN-F_HMT650".
           02  F              PIC  X(001).
           02  WSN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  WSN-F_SORT     PIC  X(100) VALUE SPACE.
           02  WSN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  WSN-F_RES      USAGE  POINTER.
       01  WSN-R.
           02  WSN-DNO        PIC  9(006).
           02  WSN-GNO        PIC  9(001).
           02  WSN-NGP.
             03  WSN-NG.
               04  WSN-NEN    PIC  9(004).
               04  WSN-GET    PIC  9(002).
             03  WSN-PEY      PIC  9(002).
           02  WSN-TCD        PIC  9(004).
           02  WSN-D1.
             03  WSN-HCD      PIC  9(006).
             03  WSN-SIZ      PIC  9(001).
             03  WSN-ASUD.
               04  WSN-ASU   OCCURS  10.
                 05  WSN-SU   PIC S9(004)  COMP-3.
             03  WSN-SUT      PIC S9(005).
             03  WSN-T        PIC S9(005).
             03  WSN-KIN      PIC S9(008).
             03  WSN-CSC      PIC  9(001).
             03  WSN-DC       PIC  9(001).
             03  F            PIC  X(005).
             03  WSN-CCD      PIC  9(003).
             03  F            PIC  X(016).
             03  WSN-TCD2     PIC  9(004).
             03  F            PIC  X(022).
           02  WSN-D2    REDEFINES WSN-D1.
             03  WSN-BIF      PIC  N(010).
             03  WSN-BIR      PIC  N(010).
             03  F            PIC  X(067).
           02  F              PIC  X(001).
           02  WSN-UNC        PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-SGP   PIC  9(004).
             03  A-EGP   PIC  9(004).
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
             03  A-TCD   PIC  9(004).
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-GN    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-HNA   PIC  N(024).
             03  D-TNA   PIC  N(024).
           02  D-MD1.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC  N(002).
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(017).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(009).
             03  FILLER  PIC  9(006).
           02  D-MD2.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC  N(002).
             03  FILLER  PIC  9(006).
             03  FILLER  PIC  N(016).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(009).
             03  FILLER  PIC  9(006).
           02  D-SD.
             03  FILLER.
               04  FILLER  PIC  Z(001).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
               04  FILLER  PIC  -(005).
             03  FILLER  PIC  N(010).
             03  FILLER  PIC  N(010).
             03  FILLER  PIC  N(010).
             03  FILLER  PIC  N(010).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "41" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "1" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGP" "9" "1" "70" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGP" BY REFERENCE W-SGPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGP" "9" "1" "76" "4" "A-SGP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGP" BY REFERENCE W-EGPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "2" "0" "30" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD" "9" "2" "6" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "2" "69" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "2" "75" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCD" "9" "2" "8" "4" "A-ETCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SHCD" "9" "2" "67" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EHCD" "9" "2" "75" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-GN" "9" "22" "1" "2" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-GN" BY REFERENCE W-GN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "78" "1" "A-GN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "373" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "2" "0" "96" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-HNA" "N" "2" "13" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNA" "N" "2" "13" "48" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MD1" " " "W-L" "0" "73" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-MD1" "Z9" "W-L" "4" "2" " " "D-MD1" RETURNING RESU.
       CALL "SD_From" USING
            "01D-MD1" BY REFERENCE WSN-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-MD1" "Z9" "W-L" "6" "2" "01D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-MD1" BY REFERENCE WSN-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-MD1" "N" "W-L" "9" "4" "02D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-MD1" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-MD1" "9" "W-L" "14" "4" "03D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING
            "04D-MD1" BY REFERENCE WSN-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05D-MD1" "N" "W-L" "19" "34" "04D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-MD1" BY REFERENCE W-NM11 "34" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06D-MD1" "------" "W-L" "53" "6" "05D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06D-MD1" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07D-MD1" "------" "W-L" "59" "6" "06D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "07D-MD1" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "08D-MD1" "---------" "W-L" "65" "9" "07D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "08D-MD1" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "09D-MD1" "9" "W-L" "75" "6" "08D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING
            "09D-MD1" BY REFERENCE WSN-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MD2" " " "W-L" "0" "73" "D-MD1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-MD2" "Z9" "W-L" "4" "2" " " "D-MD2" RETURNING RESU.
       CALL "SD_From" USING
            "01D-MD2" BY REFERENCE WSN-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-MD2" "Z9" "W-L" "6" "2" "01D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-MD2" BY REFERENCE WSN-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-MD2" "N" "W-L" "9" "4" "02D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-MD2" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-MD2" "9" "W-L" "14" "6" "03D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING
            "04D-MD2" BY REFERENCE WS-HCD(1) "6" "1" BY REFERENCE
            W-DC 128 RETURNING RESU.
       CALL "SD_Init" USING
            "05D-MD2" "N" "W-L" "21" "32" "04D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-MD2" BY REFERENCE W-NM21 "32" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06D-MD2" "------" "W-L" "53" "6" "05D-MD2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06D-MD2" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07D-MD2" "------" "W-L" "59" "6" "06D-MD2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "07D-MD2" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "08D-MD2" "---------" "W-L" "65" "9" "07D-MD2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "08D-MD2" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "09D-MD2" "9" "W-L" "75" "6" "08D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING
            "09D-MD2" BY REFERENCE WSN-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SD" " " "0" "0" "131" "D-MD2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-SD" " " "22" "0" "51" " " "D-SD" RETURNING RESU.
       CALL "SD_Init" USING
            "0101D-SD" "Z" "22" "4" "1" " " "01D-SD" RETURNING RESU.
       CALL "SD_From" USING
            "0101D-SD" BY REFERENCE W-SIZ(1) "1" "1" BY REFERENCE
            W-GN 121 RETURNING RESU.
       CALL "SD_Init" USING
            "0201D-SD" "-----" "22" "5" "5" "0101D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0201D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "01" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0301D-SD" "-----" "22" "10" "5" "0201D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "02" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0401D-SD" "-----" "22" "15" "5" "0301D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0401D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "03" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0501D-SD" "-----" "22" "20" "5" "0401D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0501D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "04" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0601D-SD" "-----" "22" "25" "5" "0501D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0601D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "05" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0701D-SD" "-----" "22" "30" "5" "0601D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0701D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "06" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0801D-SD" "-----" "22" "35" "5" "0701D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0801D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "07" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "0901D-SD" "-----" "22" "40" "5" "0801D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0901D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "08" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "1001D-SD" "-----" "22" "45" "5" "0901D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "1001D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "09" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "1101D-SD" "-----" "22" "50" "5" "1001D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "1101D-SD" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 121 "10" 4 RETURNING RESU.
       CALL "SD_Init" USING
            "02D-SD" "N" "18" "60" "20" "01D-SD" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-SD" BY REFERENCE W-CNAF(1) "20" "1" BY REFERENCE
            W-GN 121 RETURNING RESU.
       CALL "SD_Init" USING
            "03D-SD" "N" "19" "60" "20" "02D-SD" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-SD" BY REFERENCE W-CNAR(1) "20" "1" BY REFERENCE
            W-GN 121 RETURNING RESU.
       CALL "SD_Init" USING
            "04D-SD" "N" "21" "60" "20" "03D-SD" " " RETURNING RESU.
       CALL "SD_From" USING
            "04D-SD" BY REFERENCE W-BIF(1) "20" "1" BY REFERENCE
            W-GN 121 RETURNING RESU.
       CALL "SD_Init" USING
            "05D-SD" "N" "22" "60" "20" "04D-SD" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-SD" BY REFERENCE W-BIR(1) "20" "1" BY REFERENCE
            W-GN 121 RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "100" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME" " " RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCHT65" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCHT66" RETURNING RESU
           END-IF
           MOVE ZERO TO W-NG W-ADNG.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-DNG(2) W-DNG(3).
           SUBTRACT 1 FROM W-DGET(2).
           IF  W-DGET(2) = ZERO
               SUBTRACT 1 FROM W-DNEN(2)
               MOVE 12 TO W-DGET(2)
           END-IF
           MOVE W-DNG(2) TO W-DNG(1).
           SUBTRACT 1 FROM W-DGET(1).
           IF  W-DGET(1) = ZERO
               SUBTRACT 1 FROM W-DNEN(1)
               MOVE 12 TO W-DGET(1)
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO WSN-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SGP "A-SGP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE W-SGPD TO W-SGP.
           IF  W-SGP = ZERO
               MOVE W-DNG(1) TO W-SNG
               GO TO M-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-10
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-10
           END-IF
           IF  W-SGET NOT = W-DGET(1) AND W-DGET(2) AND W-DGET(3)
               GO TO M-10
           END-IF
           IF  W-SGET = W-DGET(1)
               MOVE W-DNEN(1) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(2)
               MOVE W-DNEN(2) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(3)
               MOVE W-DNEN(3) TO W-SNEN
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EGP "A-EGP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-EGPD TO W-EGP.
           IF  W-EGP = 9999
               MOVE W-DNG(3) TO W-ENG
               IF  JS-SIGN = 0
                   GO TO M-20
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-15
           END-IF
           IF  W-EGET NOT = W-DGET(1) AND W-DGET(2) AND W-DGET(3)
               GO TO M-15
           END-IF
           IF  W-EGET = W-DGET(1)
               MOVE W-DNEN(1) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(2)
               MOVE W-DNEN(2) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(3)
               MOVE W-DNEN(3) TO W-ENEN
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO M-15
           END-IF
           IF  JS-SIGN = 1
               GO TO M-25
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           PERFORM SEA-RTN THRU SEA-EX.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           GO TO M-30.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾄｸｲｻｷ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  T-BC NOT = 0
               GO TO M-25
           END-IF
           PERFORM SEA-RTN THRU SEA-EX.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
       M-30.
           MOVE ZERO TO W-GN W-DNO.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           PERFORM CLR-RTN THRU CLR-EX.
           CALL "DB_F_Open" USING
            "INPUT" WSN-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            WSN-F_IDLST "0".
       M-35.
      *           READ WSN-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WSN-F_PNAME1 BY REFERENCE WSN-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-BC
               GO TO M-70
           END-IF
           IF  WSN-NGP > W-ENGP
               GO TO M-35
           END-IF
           IF  JS-SIGN = 0
               IF  WSN-TCD < W-STCD OR > W-ETCD
                   GO TO M-35
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  WSN-TCD NOT = W-TCD
                   GO TO M-35
               END-IF
           END-IF
           IF  WSN-GNO = 9
               GO TO M-35
           END-IF
           IF  W-DNO = ZERO
               IF  WSN-NGP < W-SNGP
                   GO TO M-35
               END-IF
           END-IF.
       M-40.
           MOVE WSN-DNO TO W-DNO.
           MOVE ZERO TO W-ASDD W-RC W-DC.
       M-45.
           IF  W-RC = 6
               MOVE 1 TO W-BC
               GO TO M-55
           END-IF
           ADD 1 TO W-RC.
           MOVE WSN-R TO W-SD(W-RC).
       M-50.
      *           READ WSN-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WSN-F_PNAME1 BY REFERENCE WSN-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-BC
               GO TO M-55
           END-IF
           IF  WSN-NGP > W-ENGP
               GO TO M-50
           END-IF
           IF  JS-SIGN = 0
               IF  WSN-TCD < W-STCD OR > W-ETCD
                   GO TO M-50
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  WSN-TCD NOT = W-TCD
                   GO TO M-50
               END-IF
           END-IF
           IF  WSN-DNO NOT = W-DNO
               MOVE 1 TO W-BC
               GO TO M-55
           END-IF
           IF  WSN-GNO NOT = 9
               GO TO M-45
           END-IF
           MOVE 0 TO W-BC.
       M-55.
           ADD 1 TO W-DC.
           IF  W-RC < W-DC
               GO TO M-65
           END-IF
           IF  JS-SIGN = 0
               IF  WS-HCD(W-DC) NOT = W-HCD
                   GO TO M-55
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  WS-HCD(W-DC) < W-SHCD OR > W-EHCD
                   GO TO M-55
               END-IF
           END-IF.
       M-60.
           ADD 1 TO W-L W-GN.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 18
               GO TO M-70
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-55.
       M-65.
           IF  W-BC = 9
               GO TO M-70
           END-IF
           IF  W-BC = 0
               GO TO M-35
           ELSE
               GO TO M-40
           END-IF.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE WSN-F_IDLST WSN-F_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-75
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-DMM = 9
               GO TO M-80
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-70
           END-IF
           IF  W-L NOT = 18
               IF  W-BC = 9
                   GO TO M-80
               END-IF
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCHT65" RETURNING RESU
               CALL "SD_Output" USING "A-SGP" A-SGP "p" RETURNING RESU
               CALL "SD_Output" USING "A-EGP" A-EGP "p" RETURNING RESU
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-STCD" A-STCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-ETCD" A-ETCD "p" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCHT66" RETURNING RESU
               CALL "SD_Output" USING "A-SGP" A-SGP "p" RETURNING RESU
               CALL "SD_Output" USING "A-EGP" A-EGP "p" RETURNING RESU
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-SHCD" A-SHCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-EHCD" A-EHCD "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-GN.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           PERFORM CLR-RTN THRU CLR-EX.
           GO TO M-60.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-GN "A-GN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE WSN-F_IDLST WSN-F_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-70
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-75
           END-IF
           IF  W-GN < 1 OR > 14
               GO TO M-75
           END-IF
           CALL "SD_Output" USING "D-SD" D-SD "p" RETURNING RESU.
           GO TO M-75.
       M-80.
           CALL "DB_F_Close" USING
            BY REFERENCE WSN-F_IDLST WSN-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCHT65" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCHT66" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "A-SGP" A-SGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGP" A-EGP "p" RETURNING RESU.
           IF  JS-SIGN = 0
               GO TO M-20
           ELSE
               GO TO M-25
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SEA-RTN.
           IF  JS-SIGN = 1
               GO TO SEA-110
           END-IF.
       SEA-010.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-010
           END-IF.
       SEA-020.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-020
           END-IF
           IF  W-STCD > W-ETCD
               GO TO SEA-020
           END-IF
           GO TO SEA-EX.
       SEA-110.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-110
           END-IF.
       SEA-120.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-110
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-120
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO SEA-120
           END-IF.
       SEA-EX.
           EXIT.
       CLR-RTN.
           MOVE ZERO TO W-AGD CNT.
       CLR-010.
           ADD 1 TO CNT.
           IF  CNT NOT = 15
               MOVE SPACE TO W-CNAF(CNT) W-CNAR(CNT)
                             W-BIF(CNT) W-BIR(CNT)
               GO TO CLR-010
           END-IF.
       CLR-EX.
           EXIT.
       MEI-RTN.
           MOVE SPACE TO W-DCN.
           MOVE ZERO TO W-DSP.
           IF  WS-DC(W-DC) = 1 OR 2 OR 5 OR 6
               COMPUTE W-SUT = -1 * WS-SUT(W-DC)
               COMPUTE W-S(01) = -1 * WS-SU(W-DC,01)
               COMPUTE W-S(02) = -1 * WS-SU(W-DC,02)
               COMPUTE W-S(03) = -1 * WS-SU(W-DC,03)
               COMPUTE W-S(04) = -1 * WS-SU(W-DC,04)
               COMPUTE W-S(05) = -1 * WS-SU(W-DC,05)
               COMPUTE W-S(06) = -1 * WS-SU(W-DC,06)
               COMPUTE W-S(07) = -1 * WS-SU(W-DC,07)
               COMPUTE W-S(08) = -1 * WS-SU(W-DC,08)
               COMPUTE W-S(09) = -1 * WS-SU(W-DC,09)
               COMPUTE W-S(10) = -1 * WS-SU(W-DC,10)
           ELSE
               MOVE WS-SUT(W-DC) TO W-SUT
               IF  WS-UNC(W-DC) = 0
                   MOVE WS-SU(W-DC,01) TO W-S(01)
                   MOVE WS-SU(W-DC,02) TO W-S(02)
                   MOVE WS-SU(W-DC,03) TO W-S(03)
                   MOVE WS-SU(W-DC,04) TO W-S(04)
                   MOVE WS-SU(W-DC,05) TO W-S(05)
                   MOVE WS-SU(W-DC,06) TO W-S(06)
                   MOVE WS-SU(W-DC,07) TO W-S(07)
                   MOVE WS-SU(W-DC,08) TO W-S(08)
                   MOVE WS-SU(W-DC,09) TO W-S(09)
                   MOVE WS-SU(W-DC,10) TO W-S(10)
               END-IF
           END-IF
           IF  WS-UNC(W-DC) = 1
               COMPUTE W-T = -1 * WS-T(W-DC)
           ELSE
               IF  WS-DC(W-DC) = 4 OR 7
                   MOVE ZERO TO W-T
               ELSE
                   MOVE WS-T(W-DC) TO W-T
               END-IF
           END-IF
           IF (WS-UNC(W-DC) = 1) OR (WS-DC(W-DC) = 1 OR 2 OR 5 OR 6)
               COMPUTE W-KIN = -1 * WS-KIN(W-DC)
           ELSE
               IF  WS-DC(W-DC) = 4 OR 7
                   MOVE ZERO TO W-KIN
               ELSE
                   MOVE WS-KIN(W-DC) TO W-KIN
               END-IF
           END-IF
           IF  WS-DC(W-DC) = 1
               MOVE "返品" TO W-DCN
           END-IF
           IF  WS-DC(W-DC) = 2
               MOVE "不良" TO W-DCN
           END-IF
           IF  WS-DC(W-DC) = 3
               MOVE "預り" TO W-DCN
           END-IF
           IF  WS-DC(W-DC) = 4
               MOVE "預出" TO W-DCN
           END-IF
           IF  WS-DC(W-DC) = 5 OR 6
               MOVE "振替" TO W-DCN
           END-IF
           IF  WS-UNC(W-DC) = 1
               MOVE "値引" TO W-DCN
           END-IF
           IF  WS-CSC(W-DC) = 9
               MOVE "調整" TO W-DCN
           END-IF
           IF  JS-SIGN = 1
               GO TO MEI-010
           END-IF
           MOVE WS-TCD(W-DC) TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "得意先なし" TO T-NAME
           END-IF
           MOVE T-NAME TO W-NAME.
           IF  W-NM12 NOT = SPACE
               PERFORM TNA-RTN THRU TNA-EX
           END-IF
           CALL "SD_Output" USING "D-MD1" D-MD1 "p" RETURNING RESU.
           GO TO MEI-020.
       MEI-010.
           IF  WS-CSC(W-DC) NOT = 0
               MOVE SPACE TO W-NM21
               CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU
               GO TO MEI-020
           END-IF
           MOVE WS-HCD(W-DC) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "品名なし" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           IF  W-NM22 NOT = SPACE
               PERFORM HNA-RTN THRU HNA-EX
           END-IF
           CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU.
       MEI-020.
           MOVE WS-SIZ(W-DC) TO W-SIZ(W-GN).
           MOVE W-ASD TO W-ASUD(W-GN).
           IF  W-BC = 0
               MOVE WSN-BIF TO W-BIF(W-GN)
               MOVE WSN-BIR TO W-BIR (W-GN)
           END-IF
           IF  WS-CCD(W-DC) = ZERO
               GO TO MEI-EX
           END-IF
           MOVE 0 TO W-INV.
           MOVE WS-TCD2(W-DC) TO TC-TCD.
           MOVE WS-CCD(W-DC) TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
           END-IF
           IF  W-INV = 0
               MOVE TC-NAME TO W-CNA(W-GN)
           ELSE
               MOVE "直送先なし" TO W-CNAF(W-GN)
               MOVE TC-KEY TO W-CNAR(W-GN)
           END-IF.
       MEI-EX.
           EXIT.
       TNA-RTN.
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO DCNT CNT.
       TNA-010.
           ADD 1 TO DCNT.
           IF  DCNT = 25
               GO TO TNA-100
           END-IF
           MOVE W-NAW(DCNT) TO W-NA(DCNT).
           IF  W-NAW(DCNT) = SPACE
               ADD 1 TO CNT
           ELSE
               MOVE ZERO TO CNT
           END-IF
           IF  CNT < 4
               GO TO TNA-010
           END-IF.
       TNA-100.
           IF  W-NM12 = SPACE
               GO TO TNA-EX
           END-IF
           MOVE SPACE TO W-TNAD1.
           MOVE ZERO TO SCNT.
       TNA-110.
           ADD 1 TO SCNT.
           IF  SCNT = 21
               GO TO TNA-200
           END-IF
           COMPUTE DCNT = SCNT - 1.
           MOVE ZERO TO CNT.
       TNA-120.
           ADD 1 TO CNT DCNT.
           IF  CNT NOT = 5
               MOVE W-NA(DCNT) TO W-TN1(CNT)
               GO TO TNA-120
           END-IF
           IF  W-TNAD1 NOT = "株式会社" AND "有限会社"
               GO TO TNA-110
           END-IF
           IF  W-TNAD1 = "株式会社"
               MOVE "㈱　　　" TO W-TNAD1
           END-IF
           IF  W-TNAD1 = "有限会社"
               MOVE "㈲　　　" TO W-TNAD1
           END-IF
           COMPUTE DCNT = SCNT - 1.
           MOVE ZERO TO CNT.
       TNA-130.
           ADD 1 TO CNT DCNT.
           IF  CNT NOT = 5
               MOVE W-TN1(CNT) TO W-NA(DCNT)
               GO TO TNA-130
           END-IF
           COMPUTE CNT = SCNT + 3.
       TNA-140.
           ADD 1 TO CNT SCNT.
           IF  CNT NOT = 25
               MOVE W-NA(CNT) TO W-NA(SCNT)
               GO TO TNA-140
           END-IF
           MOVE SPACE TO W-NA(22) W-NA(23) W-NA(24).
       TNA-200.
           IF  W-NM12 = SPACE
               GO TO TNA-EX
           END-IF
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO SCNT DCNT.
       TNA-210.
           ADD 1 TO DCNT.
           IF  DCNT = 25
               GO TO TNA-EX
           END-IF
           IF  W-NAW(DCNT) NOT = SPACE
               ADD 1 TO SCNT
               MOVE W-NAW(DCNT) TO W-NA(SCNT)
           END-IF
           GO TO TNA-210.
       TNA-EX.
           EXIT.
       HNA-RTN.
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO DCNT CNT.
       HNA-010.
           ADD 1 TO DCNT.
           IF  DCNT = 25
               GO TO HNA-EX
           END-IF
           IF  W-NAW(DCNT) NOT = SPACE
               ADD 1 TO CNT
               MOVE W-NAW(DCNT) TO W-NA(CNT)
           END-IF
           GO TO HNA-010.
       HNA-EX.
           EXIT.
