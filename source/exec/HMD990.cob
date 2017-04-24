       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD990.
      *********************************************************
      *    PROGRAM         :  売上値引データ抽出（エクセル用）*
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM100.
       OBJECT-COMPUTER. SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-SNGP         PIC  9(008).
           02  W-SNGPD  REDEFINES W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL  REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SNGPL  REDEFINES W-SNGP.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
             03  F            PIC  9(002).
           02  W-ENGP         PIC  9(008).
           02  W-ENGPD  REDEFINES W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-TD.
             03  W-TSU        PIC S9(005).
             03  W-TKIN       PIC S9(008).
       01  W-AR.
           02  W-ARD   OCCURS  6.
             03  W-R.
               04  WR-DNO     PIC  9(006).
               04  WR-GNO     PIC  9(001).
               04  WR-DATE    PIC  9(008).
               04  WR-TCD     PIC  9(004).
               04  WR-HCD     PIC  9(006).
               04  WR-SIZ     PIC  9(001).
               04  WR-SUD   OCCURS  10.
                 05  WR-SU    PIC S9(004)  COMP-3.
               04  WR-SUT     PIC S9(005).
               04  WR-T       PIC S9(005).
               04  WR-KIN     PIC S9(008).
               04  WR-CSC     PIC  9(001).
               04  WR-DC      PIC  9(001).
               04  F          PIC  X(005).
               04  WR-CCD     PIC  9(003).
               04  WR-BCD1    PIC  9(003).
               04  F          PIC  X(040).
               04  WR-SNC     PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
      *FD  VIVF
       01  VIVF_HMD990.
           02  VIVF_PNAME1    PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  VIVF_LNAME     PIC  X(011) VALUE "VIVF_HMD990".
           02  F              PIC  X(001).
           02  VIVF_KEY1      PIC  X(100) VALUE SPACE.
           02  VIVF_SORT      PIC  X(100) VALUE SPACE.
           02  VIVF_IDLST     PIC  X(100) VALUE SPACE.
           02  VIVF_RES       USAGE  POINTER.
       01  VIV-R1.
           02  VIV-DATE       PIC  9(008).
           02  VIV-TCD        PIC  9(004).
           02  VIV-TNA        PIC  N(026).
           02  VIV-CCD        PIC  9(003).
           02  VIV-CNA        PIC  N(026).
           02  VIV-DNO        PIC  9(006).
           02  VIV-HCD        PIC  9(006).
           02  VIV-HNA        PIC  N(024).
           02  VIV-KBN        PIC  N(003).
           02  VIV-SIZ        PIC  9(001).
           02  VIV-ASU.
             03  VIV-SUD   OCCURS  10.
               04  VIV-SU     PIC S9(004).
           02  VIV-SUT        PIC S9(005).
           02  VIV-T          PIC S9(005).
           02  VIV-KIN        PIC S9(008).
           02  F              PIC  X(012).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMD990.
           02  SNTR-F_PNAME1  PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMD990".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NEN     PIC  9(004).
             03  SNTR-GET     PIC  9(002).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-D1.
             03  SNTR-HCD     PIC  9(006).
             03  SNTR-SIZ     PIC  9(001).
             03  SNTR-ASU.
               04 SNTR-SUD   OCCURS  10.
                 05  SNTR-SU  PIC S9(004)  COMP-3.
             03  SNTR-SUT     PIC S9(005).
             03  SNTR-T       PIC S9(005).
             03  SNTR-KIN     PIC S9(008).
             03  SNTR-CSC     PIC  9(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(005).
             03  SNTR-CCD     PIC  9(003).
             03  SNTR-BC1     PIC  9(002).
             03  F            PIC  X(041).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  SNTR-BI      PIC  N(024).
             03  F            PIC  X(036).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　売上値引データ　抽出　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(034) VALUE
                "ヴィヴェンディ=0 , ドット=1 ..... ".
           02  FILLER  PIC  X(038) VALUE
                "    年   月   日  ～      年   月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SNEN  PIC  9(004).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(004).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-DNO   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "374" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "12" "34" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "11" "38" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "19" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "14" "45" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "0" "0" "16" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "16" "11" "4" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "16" "18" "2" "A-SNEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "16" "23" "2" "A-SGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "33" "4" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "40" "2" "A-ENEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "16" "45" "2" "A-EGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "36" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "51" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "0" "0" "51" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DNO" "9" "24" "40" "6" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-DNO" BY REFERENCE SNTR-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-DNO" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           MOVE 1 TO W-SPEY.
           MOVE W-SNEN TO W-ENEN.
           MOVE W-SGET TO W-EGET.
           MOVE 99 TO W-EPEY.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF
           IF  W-SEN > 1
               GO TO M-06
           END-IF
      *
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SPEY" A-SPEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EPEY" A-EPEY "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-06
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SNEN < 1998
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-10
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SPEY > 31
               GO TO M-20
           END-IF.
       M-23.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-23
           END-IF
           IF  W-SNEN > W-ENEN
               GO TO M-23
           END-IF.
       M-24.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-23
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-24
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-24
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-24
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-EPEY NOT = 99
               IF  W-EPEY > 31
                   GO TO M-25
               END-IF
           END-IF
           IF  W-SNGP > W-ENGP
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
               GO TO M-06
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" VIVF_PNAME1 " " BY REFERENCE VIVF_IDLST "0".
       M-35.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-DATE < W-SNGP OR > W-ENGP
               GO TO M-35
           END-IF
           IF  SNTR-GNO = 9
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-36.
           MOVE SNTR-DNO TO W-DNO.
           INITIALIZE W-AR.
           MOVE 0 TO W-DC.
       M-37.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           INITIALIZE W-R(W-DC).
           MOVE SNTR-R TO W-R(W-DC).
       M-38.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-DATE < W-SNGP OR > W-ENGP
               GO TO M-38
           END-IF
           IF  SNTR-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-GNO NOT = 9
               GO TO M-37
           END-IF.
      *
           PERFORM S-05 THRU S-20.
           GO TO M-35.
       M-39.
           PERFORM S-05 THRU S-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE VIVF_IDLST VIVF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-SEN = 0
               IF  WR-TCD(1) NOT = 1820 AND 5359
                   IF 322 NOT = WR-BCD1(1) AND WR-BCD1(2) AND WR-BCD1(3)
                          AND WR-BCD1(4) AND WR-BCD1(5) AND WR-BCD1(6)
                      GO TO S-20
                   END-IF
               END-IF
           END-IF
           IF  W-SEN = 1
               IF  WR-TCD(1) NOT = 0342
                   GO TO S-20
               END-IF
           END-IF
           MOVE WR-TCD(1) TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           MOVE ZERO TO W-DC.
           IF  WR-CCD(1) < 002
               GO TO S-10
           END-IF
           MOVE WR-TCD(1) TO TC-TCD.
           MOVE WR-CCD(1) TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF.
       S-10.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               GO TO S-15
           END-IF
           IF  WR-GNO(W-DC) = 0
               GO TO S-15
           END-IF
      *
           MOVE WR-HCD(W-DC) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
      *
           INITIALIZE VIV-R1.
           MOVE SPACE TO VIV-TNA VIV-CNA VIV-HNA VIV-KBN.
           MOVE ZERO TO VIV-ASU.
           MOVE WR-DATE(1) TO VIV-DATE.
           MOVE WR-TCD(1) TO VIV-TCD.
           MOVE T-NAME TO VIV-TNA.
           MOVE WR-DNO(1) TO VIV-DNO.
           IF  WR-CCD(1) > 001
               MOVE WR-CCD(1) TO VIV-CCD
               MOVE TC-NAME TO VIV-CNA
           END-IF
           MOVE WR-HCD(W-DC) TO VIV-HCD.
           MOVE HI-NAME TO VIV-HNA.
           IF  WR-DC(W-DC) = 0
               MOVE "売　上" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 1
               MOVE "返　品" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 2
               MOVE "不良返" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE "預り売" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 4
               MOVE "預り出" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 5
               MOVE "振　替" TO VIV-KBN
           END-IF
           IF  WR-SNC(W-DC) = 1
               MOVE "値　引" TO VIV-KBN
           END-IF
           IF  WR-DC(W-DC) = 8
               MOVE "調　整" TO VIV-KBN
           END-IF
           MOVE WR-SIZ(W-DC) TO VIV-SIZ.
           IF  WR-SNC(W-DC) = 1
               MOVE WR-SU(W-DC,01) TO VIV-SU(01)
               MOVE WR-SU(W-DC,02) TO VIV-SU(02)
               MOVE WR-SU(W-DC,03) TO VIV-SU(03)
               MOVE WR-SU(W-DC,04) TO VIV-SU(04)
               MOVE WR-SU(W-DC,05) TO VIV-SU(05)
               MOVE WR-SU(W-DC,06) TO VIV-SU(06)
               MOVE WR-SU(W-DC,07) TO VIV-SU(07)
               MOVE WR-SU(W-DC,08) TO VIV-SU(08)
               MOVE WR-SU(W-DC,09) TO VIV-SU(09)
               MOVE WR-SU(W-DC,10) TO VIV-SU(10)
               MOVE WR-SUT(W-DC) TO VIV-SUT
               COMPUTE VIV-T = WR-T(W-DC) * -1
               COMPUTE VIV-KIN = WR-KIN(W-DC) * -1
           ELSE
               IF  WR-DC(W-DC) = 1 OR 2 OR 5
                   COMPUTE VIV-SU(01) = WR-SU(W-DC,01) * -1
                   COMPUTE VIV-SU(02) = WR-SU(W-DC,02) * -1
                   COMPUTE VIV-SU(03) = WR-SU(W-DC,03) * -1
                   COMPUTE VIV-SU(04) = WR-SU(W-DC,04) * -1
                   COMPUTE VIV-SU(05) = WR-SU(W-DC,05) * -1
                   COMPUTE VIV-SU(06) = WR-SU(W-DC,06) * -1
                   COMPUTE VIV-SU(07) = WR-SU(W-DC,07) * -1
                   COMPUTE VIV-SU(08) = WR-SU(W-DC,08) * -1
                   COMPUTE VIV-SU(09) = WR-SU(W-DC,09) * -1
                   COMPUTE VIV-SU(10) = WR-SU(W-DC,10) * -1
                   COMPUTE VIV-SUT = WR-SUT(W-DC) * -1
                   MOVE WR-T(W-DC) TO VIV-T
                   COMPUTE VIV-KIN = WR-KIN(W-DC) * -1
               ELSE
                   MOVE WR-SU(W-DC,01) TO VIV-SU(01)
                   MOVE WR-SU(W-DC,02) TO VIV-SU(02)
                   MOVE WR-SU(W-DC,03) TO VIV-SU(03)
                   MOVE WR-SU(W-DC,04) TO VIV-SU(04)
                   MOVE WR-SU(W-DC,05) TO VIV-SU(05)
                   MOVE WR-SU(W-DC,06) TO VIV-SU(06)
                   MOVE WR-SU(W-DC,07) TO VIV-SU(07)
                   MOVE WR-SU(W-DC,08) TO VIV-SU(08)
                   MOVE WR-SU(W-DC,09) TO VIV-SU(09)
                   MOVE WR-SU(W-DC,10) TO VIV-SU(10)
                   MOVE WR-SUT(W-DC) TO VIV-SUT
                   MOVE WR-T(W-DC) TO VIV-T
                   MOVE WR-KIN(W-DC) TO VIV-KIN
               END-IF
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE ZERO TO VIV-SU(10)
           END-IF
           IF  WR-DC(W-DC) = 4
               MOVE ZERO TO VIV-T VIV-KIN
           END-IF
      *           WRITE VIV-R1.
      *//////////////
           CALL "DB_Insert" USING
            VIVF_PNAME1 VIVF_LNAME VIV-R1 RETURNING RET.
           GO TO S-10.
       S-15.
           IF  SNTR-GNO NOT = 9
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-20
           END-IF.
       S-20.
           EXIT.
