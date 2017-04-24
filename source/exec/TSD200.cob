       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD200.
      ****************************************
      *****     ŽóŽæŽèŒ`@ˆÙ“®@“ü—Í     *****
      *****      ( SCREEN : SCTD20 )     *****
      ****************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-R.
           02  W-KEY          PIC  9(004).
           02  W-TSC          PIC  9(002).
           02  W-IDO          PIC  9(006).
           02  W-IDOW  REDEFINES W-IDO.
             03  W-IDO12.
               04  W-IDO1     PIC  9(002).
               04  W-IDO2     PIC  9(002).
             03  W-IDO3       PIC  9(002).
           02  W-FDD          PIC  9(006).
           02  W-FDDW  REDEFINES W-FDD.
             03  W-FDD1       PIC  9(002).
             03  W-FDD2       PIC  9(002).
             03  W-FDD3       PIC  9(002).
           02  W-HKD          PIC  9(006).
           02  W-HKDW  REDEFINES W-HKD.
             03  W-HKD1       PIC  9(002).
             03  W-HKD2       PIC  9(002).
             03  W-HKD3       PIC  9(002).
           02  W-YBC          PIC  9(004).
           02  W-SNI          PIC  9(004).
           02  F              PIC  X(009).
           02  W-PC           PIC  9(001).
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TSN          PIC  N(004).
           02  W-TNA          PIC  N(026).
           02  W-BNA          PIC  N(008).
           02  W-SNA          PIC  N(008).
           02  W-NNG.
             03  W-NNEN       PIC  9(004).
             03  W-NNENL REDEFINES W-NNEN.
               04  W-NNEN1    PIC  9(002).
               04  W-NNEN2    PIC  9(002).
             03  W-NGET       PIC  9(002).
           02  W-NNGL  REDEFINES W-NNG.
             03  F            PIC  9(002).
             03  W-NNGS       PIC  9(004).
           02  W-INGP.
             03  W-INEN       PIC  9(004).
             03  W-INENL REDEFINES W-INEN.
               04  W-INEN1    PIC  9(002).
               04  W-INEN2    PIC  9(002).
             03  W-IGET       PIC  9(002).
             03  W-IPEY       PIC  9(002).
           02  W-INGPD REDEFINES W-INGP.
             03  W-ING        PIC  9(006).
             03  F            PIC  9(002).
           02  W-INGPL REDEFINES W-INGP.
             03  F            PIC  9(002).
             03  W-INGPS      PIC  9(006).
           02  W-FNGP.
             03  W-FNEN       PIC  9(004).
             03  W-FNENL REDEFINES W-FNEN.
               04  W-FNEN1    PIC  9(002).
               04  W-FNEN2    PIC  9(002).
             03  W-FGET       PIC  9(002).
             03  W-FPEY       PIC  9(002).
           02  W-FNGPL REDEFINES W-FNGP.
             03  F            PIC  9(002).
             03  W-FNGPS      PIC  9(006).
           02  W-HNGP.
             03  W-HNEN       PIC  9(004).
             03  W-HNENL REDEFINES W-HNEN.
               04  W-HNEN1    PIC  9(002).
               04  W-HNEN2    PIC  9(002).
             03  W-HGET       PIC  9(002).
             03  W-HPEY       PIC  9(002).
           02  W-HNGPL REDEFINES W-HNGP.
             03  F            PIC  9(002).
             03  W-HNGPS      PIC  9(006).
           02  W-UNGP.
             03  W-UNEN       PIC  9(004).
             03  W-UGET       PIC  9(002).
             03  W-UPEY       PIC  9(002).
           02  W-ONGP.
             03  W-ONEN       PIC  9(004).
             03  W-ONENL REDEFINES W-ONEN.
               04  W-ONEN1    PIC  9(002).
               04  W-ONEN2    PIC  9(002).
             03  W-OGP        PIC  9(004).
           02  W-ONGPL REDEFINES W-ONGP.
             03  F            PIC  9(002).
             03  W-ONGPS      PIC  9(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LITM.
           COPY LIBANK.
           COPY LIUKET.
      *FD  DK-IDOU
       01  DK-IDOU_TSD200.
           02  DK-IDOU_PNAME1 PIC  X(004) VALUE "TIDM".
           02  F              PIC  X(001).
           02  DK-IDOU_LNAME  PIC  X(014) VALUE "DK-IDOU_TSD200".
           02  F              PIC  X(001).
           02  DK-IDOU_KEY1   PIC  X(100) VALUE SPACE.
           02  DK-IDOU_SORT   PIC  X(100) VALUE SPACE.
           02  DK-IDOU_IDLST  PIC  X(100) VALUE SPACE.
           02  DK-IDOU_RES    USAGE  POINTER.
       01  IDOU-R.
           02  I-KEY          PIC  X(004).
           02  I-TSC          PIC  9(002).
           02  I-IDO          PIC  9(006).
           02  I-FDD          PIC  9(006).
           02  I-HKD          PIC  9(006).
           02  I-YBC          PIC  9(004).
           02  I-SNI          PIC  9(004).
           02  F              PIC  X(009).
           02  I-PC           PIC  9(001).
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
           02  A-KEY   PIC  9(004).
           02  FILLER.
             03  A-IDO1  PIC  9(002).
             03  A-IDO2  PIC  9(002).
             03  A-IDO3  PIC  9(002).
           02  A-TSC   PIC  9(002).
           02  A-YBC   PIC  9(004).
           02  FILLER.
             03  A-FDD1  PIC  9(002).
             03  A-FDD2  PIC  9(002).
             03  A-FDD3  PIC  9(002).
           02  FILLER.
             03  A-HKD1  PIC  9(002).
             03  A-HKD2  PIC  9(002).
             03  A-HKD3  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-MND   PIC 99B99B99 .
             03  D-MNV.
               04  FILLER  PIC  X(001) VALUE "/".
               04  FILLER  PIC  X(001) VALUE "/".
           02  D-KIN.
             03  FILLER  PIC Z,ZZZ,ZZZ,ZZ9 .
           02  D-WM.
             03  FILLER  PIC  X(023) VALUE
                  "(  ‚t‚j‚d‚s‚l ˆ—‹æ•ª ".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  X(014) VALUE " ‚Åˆ—Ï‚Ý  )".
           02  D-IDV.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  X(001) VALUE "/".
           02  D-TSN   PIC  N(004).
           02  FILLER.
             03  D-BNA   PIC  N(008).
             03  D-SNA   PIC  N(008).
           02  D-FDV.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  X(001) VALUE "/".
           02  D-HKV.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  X(001) VALUE "/".
       01  C-SPC.
           02  C-YBC   PIC  X(040) VALUE
                "                                        ".
           02  C-FDD   PIC  X(008) VALUE
                "        ".
           02  C-HKD   PIC  X(008) VALUE
                "        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  UKETM Å¼  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  TIDM Å¼  ***".
             03  E-ME3   PIC  X(025) VALUE
                  "***  TIDM Æ­³Ø®¸ ½ÞÐ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  BANKM Å¼  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  TIDM WRITE ´×°  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  TIDM REWRITE ´×°  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  TIDM DELETE ´×°  ***".
             03  E-ME8   PIC  X(018) VALUE
                  "***  ËÂÞ¹ ´×°  ***".
             03  E-ME9   PIC  X(025) VALUE
                  "***  ÏÝË·Þ ¶Þ ½·ÞÃ²Ù  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  ÜØËÞ· Áª¯¸  ***".
             03  E-ME78  PIC  N(002) VALUE "˜A—".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
            "C-ACP" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "48" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "5" "21" "4" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "11" "0" "6" "A-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDO1" "9" "11" "21" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDO1" BY REFERENCE W-IDO1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDO2" "9" "11" "24" "2" "A-IDO1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDO2" BY REFERENCE W-IDO2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDO3" "9" "11" "27" "2" "A-IDO2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDO3" BY REFERENCE W-IDO3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TSC" "9" "12" "21" "2" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TSC" BY REFERENCE W-TSC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YBC" "9" "13" "21" "4" "A-TSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YBC" BY REFERENCE W-YBC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "14" "0" "6" "A-YBC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FDD1" "9" "14" "21" "2" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FDD1" BY REFERENCE W-FDD1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FDD2" "9" "14" "24" "2" "A-FDD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FDD2" BY REFERENCE W-FDD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FDD3" "9" "14" "27" "2" "A-FDD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FDD3" BY REFERENCE W-FDD3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "15" "0" "6" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKD1" "9" "15" "21" "2" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKD1" BY REFERENCE W-HKD1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKD2" "9" "15" "24" "2" "A-HKD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKD2" BY REFERENCE W-HKD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKD3" "9" "15" "27" "2" "A-HKD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKD3" BY REFERENCE W-HKD3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "18" "37" "1" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "160" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "7" "21" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "8" "0" "10" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "D-MND" "99B99B99" "8" "21" "8" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MND" BY REFERENCE UT-MKD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MNV" " " "8" "21" "2" "D-MND" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MNV" "X" "8" "23" "1" " " "D-MNV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MNV" "X" "8" "26" "1" "01D-MNV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "9" "0" "13" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "Z,ZZZ,ZZZ,ZZ9" "9" "21" "13" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE UT-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WM" " " "10" "0" "39" "D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-WM" "X" "10" "10" "23" " " "D-WM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-WM" "9" "10" "33" "2" "01D-WM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-WM" BY REFERENCE UT-SKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-WM" "X" "10" "35" "14" "02D-WM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-IDV" " " "11" "0" "2" "D-WM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-IDV" "X" "11" "23" "1" " " "D-IDV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-IDV" "X" "11" "26" "1" "01D-IDV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TSN" "N" "12" "25" "8" "D-IDV" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TSN" BY REFERENCE W-TSN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "13" "0" "32" "D-TSN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BNA" "N" "13" "27" "16" " " "07C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BNA" BY REFERENCE W-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "13" "43" "16" "D-BNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE W-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FDV" " " "14" "0" "2" "07C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-FDV" "X" "14" "23" "1" " " "D-FDV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-FDV" "X" "14" "26" "1" "01D-FDV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HKV" " " "15" "0" "2" "D-FDV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-HKV" "X" "15" "23" "1" " " "D-HKV" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-HKV" "X" "15" "26" "1" "01D-HKV" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-YBC" "X" "13" "21" "40" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-FDD" "X" "14" "21" "8" "C-YBC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HKD" "X" "15" "21" "8" "C-FDD" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "317" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "317" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "25" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-STAT" " " RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NTNG TO W-NNGS.
           IF  W-NNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NNEN
           END-IF
           IF  W-NNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NNEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" DK-IDOU_PNAME1 " " BY REFERENCE DK-IDOU_IDLST "1"
            "I-KEY" BY REFERENCE I-KEY.
       M-040.
           CALL "SD_Screen_Output" USING "SCTD20" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-900
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Screen_Output" USING "SCTD20" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           INITIALIZE W-R.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           MOVE W-KEY TO UT-KEY.
      *           READ UKET-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE UT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "@––@@“¾ˆÓæ@–³‚µ@@––" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
       M-100.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MND" D-MND "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MNV" D-MNV "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  UT-SKC NOT = ZERO
               MOVE UT-FDD TO W-FDD
               MOVE UT-HKD TO W-HKD
               CALL "SD_Output" USING "D-WM" D-WM "p" RETURNING RESU
           END-IF
           MOVE W-KEY TO I-KEY.
      *           READ DK-IDOU INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DK-IDOU_PNAME1 BY REFERENCE IDOU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-140
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE IDOU-R TO W-R.
           PERFORM S-20 THRU S-25.
           MOVE SPACE TO W-BNA W-SNA.
           MOVE W-YBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "‚a‚`‚m‚j‚l@–³‚µ" TO W-BNA
               GO TO M-120
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
       M-120.
           CALL "SD_Output" USING "D-IDV" D-IDV "p" RETURNING RESU.
           CALL "SD_Output" USING "A-IDO1" A-IDO1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-IDO2" A-IDO2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-IDO3" A-IDO3 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TSC" A-TSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TSN" D-TSN "p" RETURNING RESU.
           IF  W-YBC NOT = ZERO
               CALL "SD_Output" USING "A-YBC" A-YBC "p" RETURNING RESU
               CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU
               CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU
           END-IF
           IF  W-FDD NOT = ZERO
               CALL "SD_Output" USING "D-FDV" D-FDV "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD1" A-FDD1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD2" A-FDD2 "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD3" A-FDD3 "p" RETURNING RESU
           END-IF
           IF  W-HKD NOT = ZERO
               CALL "SD_Output" USING "D-HKV" D-HKV "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD1" A-HKD1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD2" A-HKD2 "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD3" A-HKD3 "p" RETURNING RESU
           END-IF
           IF  W-ACT = 2
               GO TO M-160
           END-IF
           GO TO M-440.
       M-140.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-160.
           CALL "SD_Output" USING "D-IDV" D-IDV "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-IDO1 "A-IDO1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-IDO2 "A-IDO2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           IF  W-IDO2 < 1 OR > 12
               GO TO M-180
           END-IF
           MOVE ZERO TO W-INGP.
           MOVE W-IDO1 TO W-INEN2.
           MOVE W-IDO2 TO W-IGET.
           IF  W-INEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-INEN
           ELSE
               IF  W-INEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-INEN
               END-IF
           END-IF
           IF  W-ING NOT = W-NNG
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-180
           END-IF
           MOVE W-INEN TO W-SNI.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-IDO3 "A-IDO3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-IDO3 < 1 OR > 31
               GO TO M-200
           END-IF
           MOVE W-IDO3 TO W-IPEY.
           MOVE W-INGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF
           MOVE UT-SNU TO W-UNEN.
           MOVE UT-UTG TO W-UGET.
           MOVE UT-UTP TO W-UPEY.
           IF  W-INGP < W-UNGP
               GO TO M-160
           END-IF
           MOVE ZERO TO W-ONGP.
           MOVE UT-OKD TO W-ONGPS.
           IF  W-ONEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ONEN
           END-IF
           IF  W-ONEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ONEN
           END-IF
           IF  W-INGP > W-ONGP
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-TSC "A-TSC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-TSC NOT = 00 AND 19 AND 20 AND 32
                             AND 50 AND 60 AND 70 AND 90
               GO TO M-220
           END-IF
           PERFORM S-20 THRU S-25.
           CALL "SD_Output" USING "D-TSN" D-TSN "p" RETURNING RESU.
           IF  UT-KIN < 100000
               IF  W-TSC = 32
                   CALL "SD_Output" USING
                    "E-ME10" E-ME10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-TSC = 60 OR 90
               MOVE ZERO TO W-YBC
               GO TO M-280
           END-IF
           IF  W-TSC NOT = 70
               GO TO M-240
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-220
           END-IF
           MOVE UT-SBC TO W-YBC.
           CALL "SD_Output" USING "A-YBC" A-YBC "p" RETURNING RESU.
           GO TO M-260.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-YBC "A-YBC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF.
       M-260.
           MOVE W-YBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "‚a‚`‚m‚j‚l@–³‚µ" TO W-BNA
               CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU
               CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU
               GO TO M-280
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  B-YBC NOT = 1
               GO TO M-240
           END-IF.
       M-280.
           IF  W-FDD NOT = ZERO
               CALL "SD_Output" USING "D-FDV" D-FDV "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD1" A-FDD1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD2" A-FDD2 "p" RETURNING RESU
               CALL "SD_Output" USING "A-FDD3" A-FDD3 "p" RETURNING RESU
           END-IF
           IF  W-HKD NOT = ZERO
               CALL "SD_Output" USING "D-HKV" D-HKV "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD1" A-HKD1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD2" A-HKD2 "p" RETURNING RESU
               CALL "SD_Output" USING "A-HKD3" A-HKD3 "p" RETURNING RESU
           END-IF
           IF  W-TSC = 70 OR 90
               GO TO M-440
           END-IF.
       M-300.
           CALL "SD_Output" USING "D-FDV" D-FDV "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-FDD1 "A-FDD1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-TSC = 60
                   GO TO M-220
               ELSE
                   GO TO M-240
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-FDD2 "A-FDD2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-340
           END-IF
           IF  W-FDD2 < 1 OR > 12
               GO TO M-340
           END-IF.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-FDD3 "A-FDD3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-340
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-360
           END-IF
           IF  W-FDD3 < 1 OR > 31
               GO TO M-360
           END-IF
      *
           MOVE ZERO TO W-FNGP.
           MOVE W-FDD1 TO W-FNEN2.
           MOVE W-FDD2 TO W-FGET.
           MOVE W-FDD3 TO W-FPEY.
           IF  W-FNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-FNEN
           ELSE
               IF  W-FNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-FNEN
               END-IF
           END-IF
           MOVE W-FNGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  W-INGP < W-FNGP
               GO TO M-300
           END-IF
           IF  UT-TSC NOT = 12
               MOVE ZERO TO W-HKD
               CALL "SD_Output" USING "C-HKD" C-HKD "p" RETURNING RESU
               GO TO M-440
           END-IF.
       M-380.
           CALL "SD_Output" USING "D-HKV" D-HKV "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-HKD1 "A-HKD1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-360
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF.
       M-400.
           CALL "SD_Accept" USING BY REFERENCE A-HKD2 "A-HKD2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-380
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-400
           END-IF
           IF  W-HKD2 < 1 OR > 12
               GO TO M-400
           END-IF.
       M-420.
           CALL "SD_Accept" USING BY REFERENCE A-HKD3 "A-HKD3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-400
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-420
           END-IF
           IF  W-HKD3 < 1 OR > 31
               GO TO M-420
           END-IF
      *
           MOVE ZERO TO W-HNGP.
           MOVE W-HKD1 TO W-HNEN2.
           MOVE W-HKD2 TO W-HGET.
           MOVE W-HKD3 TO W-HPEY.
           IF  W-HNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-HNEN
           ELSE
               IF  W-HNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-HNEN
               END-IF
           END-IF
           MOVE W-HNGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-380
           END-IF
           IF  W-FNGP > W-HNGP
               GO TO M-380
           END-IF
           IF  W-INGP < W-HNGP
               GO TO M-380
           END-IF.
       M-440.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-460
           END-IF
           IF  W-ACT = 3
               GO TO M-060
           END-IF
           IF  W-TSC = 70 OR 90
               GO TO M-220
           END-IF
           IF  W-HKD = ZERO
               GO TO M-360
           END-IF
           GO TO M-420.
       M-460.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-440
           END-IF
           IF  W-DMM = 9
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-440
           END-IF
           IF  W-ACT = 3
               GO TO M-520
           END-IF
           IF  W-ACT = 2
               GO TO M-500
           END-IF
      *
           MOVE W-R TO IDOU-R.
      *           WRITE IDOU-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            DK-IDOU_PNAME1 DK-IDOU_LNAME IDOU-R RETURNING RET.
           IF  RET = 1
               GO TO M-480
           END-IF
           GO TO M-060.
       M-480.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-900.
       M-500.
           MOVE W-R TO IDOU-R.
      *           REWRITE IDOU-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            DK-IDOU_PNAME1 DK-IDOU_LNAME IDOU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-060.
       M-520.
      *           DELETE DK-IDOU INVALID KEY
      *///////////////
           CALL "DB_Delete" USING DK-IDOU_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-060.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE DK-IDOU_IDLST DK-IDOU_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" DK-IDOU_PNAME1 " " BY REFERENCE DK-IDOU_IDLST "1"
            "I-KEY" BY REFERENCE I-KEY.
      *           READ DK-IDOU NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" DK-IDOU_PNAME1 BY REFERENCE IDOU-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE DK-IDOU_IDLST DK-IDOU_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-20.
           MOVE SPACE TO W-TSN.
           IF  W-TSC = 19
               MOVE "Žæ—§“ü‹à" TO W-TSN
           END-IF
           IF  W-TSC = 20
               MOVE "’S•Û·“ü" TO W-TSN
           END-IF
           IF  W-TSC = 32
               MOVE "Š„@@ˆø" TO W-TSN
           END-IF
           IF  W-TSC = 50
               MOVE "Œˆ@@Ï" TO W-TSN
           END-IF
           IF  W-TSC = 60
               MOVE "•s@@“n" TO W-TSN
           END-IF
           IF  W-TSC = 70
               MOVE "Š„Žè‘g–ß" TO W-TSN
           END-IF
           IF  W-TSC = 90
               MOVE "Žæ@@Á" TO W-TSN
           END-IF.
       S-25.
           EXIT.
