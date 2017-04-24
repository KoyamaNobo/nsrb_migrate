       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK970.
      *********************************************************
      *    PROGRAM         :  年間得意先品種別教育出荷リスト  *
      *    PRINTER TYPE    :  JIPS*                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SNEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-SGET         PIC  9(002).
           02  F              PIC  X(003) VALUE " - ".
           02  H-ENEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-EGET         PIC  9(002).
           02  F              PIC  N(022) VALUE
                "　年間得意先品種別　教育出荷リスト　　＊＊＊".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(005) VALUE "   P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(003) VALUE "出荷数".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "売価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "出荷金額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNAME        PIC  N(026).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-HNAME        PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC -----,---,--9.
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-SNGD.
             03  W-SNEND      PIC  9(004).
             03  W-SNENDL REDEFINES W-SNEND.
               04  W-SNEND1   PIC  9(002).
               04  W-SNEND2   PIC  9(002).
             03  W-SGETD      PIC  9(002).
           02  W-SNGDL REDEFINES W-SNGD.
             03  F            PIC  9(002).
             03  W-SNGDS      PIC  9(004).
           02  W-ENGD.
             03  W-ENEND      PIC  9(004).
             03  W-ENENDL REDEFINES W-ENEND.
               04  W-ENEND1   PIC  9(002).
               04  W-ENEND2   PIC  9(002).
             03  W-EGETD      PIC  9(002).
           02  W-ENGDL REDEFINES W-ENGD.
             03  F            PIC  9(002).
             03  W-ENGDS      PIC  9(004).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(004).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NAMED.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NMD   REDEFINES W-NAMED PIC  N(024).
           02  W-NAME.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD   REDEFINES W-NAME  PIC  N(024).
           02  W-T            PIC S9(005).
       01  WN-D.
           02  WN-SU          PIC S9(007).
           02  WN-KIN         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(009).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKS-F
       01  HKS-F_HMK970.
           02  HKS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMK970".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  HKS-TCD        PIC  9(004).
           02  HKS-HCD1       PIC  9(004).
           02  HKS-SU         PIC S9(006).
           02  HKS-KIN        PIC S9(008).
           02  F              PIC  X(004).
           02  HKS-NG         PIC  9(006).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *FD  HKSR-F
       01  HKSR-F_HMK970.
           02  HKSR-F_PNAME1  PIC  X(006) VALUE "HKSRYF".
           02  F              PIC  X(001).
           02  HKSR-F_LNAME   PIC  X(013) VALUE "HKSR-F_HMK970".
           02  F              PIC  X(001).
           02  HKSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HKSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  HKSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HKSR-F_RES     USAGE  POINTER.
       01  HKSR-R.
           02  F              PIC  X(026).
           02  HKSR-NG.
             03  HKSR-NEN     PIC  9(004).
             03  HKSR-GET     PIC  9(002).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　年間得意先品種別　教育出荷リスト　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "データ期間    '  年   月  ～  '  年   月".
           02  FILLER  PIC  X(040) VALUE
                "作表期間      '  年   月  ～  '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NGD.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "466" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "16" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "16" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "25" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "16" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "16" "31" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "16" "36" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "47" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "52" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "42" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGD" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGD" "9" "14" "31" "2" " " "D-NGD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGD" BY REFERENCE W-SNEND2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGD" "9" "14" "36" "2" "01D-NGD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGD" BY REFERENCE W-SGETD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGD" "9" "14" "47" "2" "02D-NGD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGD" BY REFERENCE W-ENEND2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGD" "9" "14" "52" "2" "03D-NGD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NGD" BY REFERENCE W-EGETD "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NG W-SNG W-ENG.
           MOVE D-NING TO W-NGS.
           MOVE D-SKNG TO W-SNGS.
           MOVE D-EKNG TO W-ENGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-SNGD W-ENGD.
           CALL "DB_F_Open" USING
            "INPUT" HKSR-F_PNAME1 " " BY REFERENCE HKSR-F_IDLST "0".
      *           READ HKSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE HKSR-NG TO W-SNGD.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NG TO W-ENGD.
           CALL "SD_Output" USING "D-NGD" D-NGD "p" RETURNING RESU.
           IF  W-SNG >= W-SNGD OR <= W-ENGD
               IF  W-ENG >= W-SNGD OR <= W-ENGD
                   CALL "SD_Output" USING
                    "A-SNEN" A-SNEN "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-SGET" A-SGET "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-ENEN" A-ENEN "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-EGET" A-EGET "p" RETURNING RESU
               END-IF
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-SNEN < W-SNEND
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-SNG < W-SNGD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  W-ENEN > W-ENEND
               GO TO M-20
           END-IF
           IF  W-SNEN > W-ENEN
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-25
           END-IF
           IF  W-ENG > W-ENGD OR < W-SNG
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
       M-35.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKS-F_IDLST HKS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HKS-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF
           IF  ZERO = HKS-SU AND HKS-KIN
               GO TO M-35
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SNEN2 TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-ENEN2 TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-40.
           MOVE HKS-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
           MOVE ZERO TO WS-D CHK.
       M-45.
           MOVE HKS-HCD1 TO W-HCD.
           PERFORM S-40 THRU S-50.
           MOVE ZERO TO WN-D.
       M-50.
           ADD HKS-SU TO WN-SU.
           ADD HKS-KIN TO WN-KIN.
       M-55.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HKS-NG < W-SNG OR > W-ENG
               GO TO M-55
           END-IF
           IF  ZERO = HKS-SU AND HKS-KIN
               GO TO M-55
           END-IF
           IF  HKS-TCD NOT = W-TCD
               GO TO M-60
           END-IF
           IF  HKS-HCD1 = W-HCD
               GO TO M-50
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-45.
       M-60.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-40.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-TNAME P-HNAME.
           MOVE "　　　【　　総　合　計　　】" TO P-HNAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
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
       S-15.
           EXIT.
       S-20.
           IF  ZERO = WN-SU AND WN-KIN
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-TNAME P-HNAME.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNAME
           END-IF
           MOVE W-HCD TO P-HCD.
           MOVE W-NAD TO P-HNAME.
           MOVE ZERO TO W-T.
           IF  ZERO NOT = WN-SU AND WN-KIN
               COMPUTE W-T = WN-KIN / WN-SU
           END-IF
           MOVE WN-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE WN-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-SU TO WS-SU.
           ADD WN-KIN TO WS-KIN.
       S-25.
           EXIT.
       S-30.
           IF  CHK = 0
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-TNAME P-HNAME.
           MOVE "　　　　　　　　［　　小　　計　　］" TO P-HNAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO W-NAD
               GO TO S-50
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO W-NAD
               GO TO S-50
           END-IF
           IF  HI-HCD1 NOT = W-HCD
               MOVE "　＊＊　マスター　なし　＊＊　" TO W-NAD
               GO TO S-50
           END-IF
           MOVE SPACE TO W-NMD W-NAD.
           MOVE ZERO TO CNT.
           MOVE HI-NAME TO W-NMD.
       S-45.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-50
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-45
           END-IF
           ADD 1 TO CNT.
           IF  CNT NOT = 25
               MOVE W-NM(CNT) TO W-NA(CNT)
               IF  W-NM(CNT) NOT = SPACE
                   GO TO S-45
               END-IF
           END-IF.
       S-50.
           EXIT.
