       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK950.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-27.
      *********************************************************
      *    PROGRAM         :  年間教育振興会会費請求集計表    *
      *    PRINTER TYPE    :  JIPS*                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(006) VALUE "　＊＊＊　　".
           02  H-SNEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-SGET         PIC  9(002).
           02  F              PIC  X(003) VALUE " - ".
           02  H-ENEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-EGET         PIC  9(002).
           02  F              PIC  N(014) VALUE
                "　教育シューズ振興会会費請求".
           02  F              PIC  N(014) VALUE
                "　品種別出荷集計表　　＊＊＊".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(005) VALUE "   P.".
           02  H-PAGE         PIC Z.
       01  HEAD2.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "品　　　　　名".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "数　量　売　価　　　売上金額".
           02  F              PIC  N(006) VALUE "　　　振興会".
       01  HEAD3.
           02  F              PIC  X(078) VALUE SPACE.
           02  F              PIC  X(009) VALUE "(金額X3%)".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-T            PIC ----,--9.
           02  P-KIN          PIC --,---,---,--9.
           02  P-TR           PIC ----,---,--9.
       01  W-D.
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
           02  W-DMM          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-HCD          PIC  9(004).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NAMED.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NMD   REDEFINES W-NAMED PIC  N(024).
           02  W-NAME.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD   REDEFINES W-NAME  PIC  N(024).
           02  W-T            PIC S9(005).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(009).
           02  WS-TR          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(009).
           02  WA-TR          PIC S9(009).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKK-F
       01  HKK-F_HMK950.
           02  HKK-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKK-F_LNAME    PIC  X(012) VALUE "HKK-F_HMK950".
           02  F              PIC  X(001).
           02  HKK-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKK-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKK-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKK-F_RES      USAGE  POINTER.
       01  HKK-R.
           02  HKK-NC         PIC  9(001).
           02  HKK-HCD1       PIC  9(004).
           02  HKK-SU         PIC S9(007).
           02  HKK-KIN        PIC S9(009).
           02  HKK-TR         PIC S9(007).
           02  F              PIC  X(008).
           02  HKK-NG         PIC  9(006).
           02  F              PIC  X(022).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　（　年　間　）　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　教育振興会用　品種別売上集計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "452" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
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
           MOVE ZERO TO W-NGS W-SNG W-ENG.
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
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKK-F_PNAME1.
           MOVE ZERO TO W-SNGD W-ENGD.
           CALL "DB_F_Open" USING
            "INPUT" HKK-F_PNAME1 " " BY REFERENCE HKK-F_IDLST "0".
      *           READ HKK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKK-F_PNAME1 BY REFERENCE HKK-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKK-F_IDLST HKK-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE HKK-NG TO W-SNGD.
           CALL "DB_F_Close" USING
            BY REFERENCE HKK-F_IDLST HKK-F_PNAME1.
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
           CALL "DB_F_Open" USING
            "INPUT" HKK-F_PNAME1 " " BY REFERENCE HKK-F_IDLST "0".
       M-35.
      *           READ HKK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKK-F_PNAME1 BY REFERENCE HKK-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKK-F_IDLST HKK-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HKK-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF
           IF  ZERO = HKK-SU AND HKK-KIN
               GO TO M-35
           END-IF
      *
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
           MOVE HKK-NC TO W-NC.
           MOVE ZERO TO WS-D.
       M-45.
           PERFORM S-20 THRU S-35.
       M-50.
      *           READ HKK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKK-F_PNAME1 BY REFERENCE HKK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HKK-NG < W-SNG OR > W-ENG
               GO TO M-50
           END-IF
           IF  ZERO = HKK-SU AND HKK-KIN
               GO TO M-50
           END-IF
           IF  HKK-NC NOT = W-NC
               GO TO M-55
           END-IF
           GO TO M-45.
       M-55.
           PERFORM S-40 THRU S-45.
           GO TO M-40.
       M-90.
           PERFORM S-40 THRU S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　【　　総　合　計　　】" TO P-NAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           MOVE WA-TR TO P-TR.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKK-F_IDLST HKK-F_PNAME1.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE HKK-HCD1 TO P-HCD.
           MOVE ZERO TO HI-KEY.
           MOVE HKK-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
           IF  HI-HCD1 NOT = HKK-HCD1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
           MOVE SPACE TO W-NMD W-NAD.
           MOVE ZERO TO CNT.
           MOVE HI-NAME TO W-NMD.
       S-25.
           ADD 1 TO CNT.
           IF  CNT = 25
               MOVE W-NAD TO P-NAME
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               MOVE W-NAD TO P-NAME
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           MOVE W-NAD TO P-NAME.
       S-30.
           MOVE ZERO TO W-T.
           IF  ZERO NOT = HKK-SU AND HKK-KIN
               COMPUTE W-T = HKK-KIN / HKK-SU
           END-IF
           MOVE HKK-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE HKK-KIN TO P-KIN.
           MOVE HKK-TR TO P-TR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD HKK-SU TO WS-SU.
           ADD HKK-KIN TO WS-KIN.
           ADD HKK-TR TO WS-TR.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　［　　小　　計　　］" TO P-NAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           MOVE WS-TR TO P-TR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
           ADD WS-TR TO WA-TR.
       S-45.
           EXIT.
