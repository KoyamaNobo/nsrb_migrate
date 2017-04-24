       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG060.
      *********************************************************
      *    PROGRAM         :  請求明細表　　　　　　　　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  年月入力  ｱﾘ=1,ﾅｼ=2(月),3=ﾅｼ(年)*
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(046) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　請　求　明　細　表　　＊＊＊".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(016) VALUE
                "【　入金額”※”印は要チェック　".
           02  F              PIC  N(016) VALUE
                "，　請求額”×”印は非請求　】　".
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(003) VALUE "売上額".
           02  F              PIC  X(017) VALUE "  -----I I-----  ".
           02  F              PIC  N(003) VALUE "入金額".
           02  F              PIC  X(019) VALUE "  -----I   I-----  ".
           02  F              PIC  N(003) VALUE "請求額".
           02  F              PIC  X(008) VALUE "  -----I".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "入金".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　請求日".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売　上".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売　上".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売　上".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "月日".
       01  W-P.
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-15K          PIC  X(005).
           02  P-NA           PIC  N(026).
           02  P-20K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-SNGP         PIC 99/99/99.
           02  P-URD.
             03  P-UR         PIC -----,---,--9.
             03  P-URZ        PIC --,---,--9.
           02  P-URDD  REDEFINES P-URD.
             03  F            PIC  X(011).
             03  P-SNM        PIC  N(006).
           02  P-NKD.
             03  P-NK         PIC -----,---,--9.
             03  P-NKZ        PIC --,---,--9.
           02  P-NKDD  REDEFINES P-NKD.
             03  F            PIC  X(011).
             03  P-ZSM        PIC  N(006).
           02  P-CHK1         PIC  N(001).
           02  P-TS           PIC -----,---,--9.
           02  P-TSZ          PIC --,---,--9.
           02  P-CHK2         PIC  N(001).
           02  F              PIC  X(001).
           02  P-NGP          PIC 99/99.
       01  W-DATA.
           02  W-TNC          PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-TS           PIC S9(009).
           02  W-TSZ          PIC S9(007).
           02  W-UR           PIC S9(009).
           02  W-URZ          PIC S9(007).
           02  W-SNK          PIC S9(009).
           02  W-SNKZ         PIC S9(007).
           02  W-PAGE         PIC  9(002).
           02  W-DC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-SNG          PIC  9(006).
           02  W-SNGL  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  F            PIC  9(002).
           02  W-SNGD  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS.
               04  W-SNENS    PIC  9(002).
               04  W-SGET     PIC  9(002).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS.
               04  W-ENEN     PIC  9(002).
               04  W-EGET     PIC  9(002).
           02  W-STNC         PIC  9(002).
           02  W-ETNC         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-SKIN         PIC S9(009).
           02  W-NKIN         PIC S9(009).
           02  W-OKIN         PIC S9(009).
           02  W-UKIN         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LISKDF.
           COPY LSPF.
      *FD  SM-F
       01  SM-F_HKG060.
           02  SM-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKG060".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  9(004).
           02  SM-DATE.
             03  F            PIC  9(002).
             03  SM-SNGP      PIC  9(006).
           02  SM-NGP   REDEFINES SM-DATE.
             03  SM-SNG       PIC  9(006).
             03  F            PIC  9(002).
           02  SM-ZS          PIC S9(009).
           02  SM-ZSZ         PIC S9(007).
           02  SM-UR          PIC S9(009).
           02  SM-URZ         PIC S9(007).
           02  SM-TS          PIC S9(007).
           02  SM-TSZ         PIC S9(005).
           02  SM-NK          PIC S9(009).
           02  SM-NKZ         PIC S9(007).
           02  SM-NNGP.
             03  SM-NY        PIC  9(002).
             03  SM-NMD       PIC  9(004).
           02  SM-SIT         PIC  9(003).
           02  SM-SU          PIC  9(001).
           02  SM-DNO         PIC  9(006).
           02  F              PIC  X(004).
           02  SM-TNC         PIC  9(002).
           02  F              PIC  X(007).
           02  SM-PC          PIC  9(001).
           02  F              PIC  X(026).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　請　求　明　細　表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(020) VALUE
                "担当者ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NG.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  FILLER.
             03  A-STNC  PIC  9(002).
             03  A-ETNC  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
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
           "C-MID" " " "0" "0" "332" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "17" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "17" "19" "20" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "20" "18" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-NG" " " "15" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SNEN" "9" "15" "18" "2" " " "A-NG" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SNEN" BY REFERENCE W-SNENS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SGET" "9" "15" "23" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ENEN" "9" "15" "32" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ENEN" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EGET" "9" "15" "37" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "17" "0" "4" "A-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-STNC" "9" "17" "31" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETNC" "9" "17" "37" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "35" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 1 AND 2 AND 3
               GO TO M-05
           END-IF
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-ETNC.
           IF  JS-SIGN = 3
               MOVE D-EPNG TO W-NGS
           ELSE
               MOVE D-NHNG TO W-NGS
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               GO TO M-15
           END-IF
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-ENG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           IF  JS-SIGN NOT = 3
               GO TO M-10
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF.
       M-10.
           MOVE W-NG TO W-SNG.
           CALL "SD_Output" USING "A-NG" A-NG "p" RETURNING RESU.
           GO TO M-35.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SNENS = ZERO
               IF  W-SGET = ZERO
                   MOVE ZERO TO W-SNG
                   GO TO M-25
               END-IF
           END-IF
           IF W-SGET < 1 OR > 12
               GO TO M-20
           END-IF
           MOVE ZERO TO W-NG.
           MOVE W-SNGS TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-SNG.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-ENEN = 99
               IF  W-EGET = 99
                   MOVE 999999 TO W-ENG
                   GO TO M-35
               END-IF
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-30
           END-IF
           MOVE ZERO TO W-NG.
           MOVE W-ENGS TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-ENG.
           IF  W-SNG > W-ENG
               GO TO M-25
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 1
                   GO TO M-30
               END-IF
           END-IF
           IF  ESTAT = PF9
               IF  JS-SIGN NOT = 1
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "DB_Close"
                   STOP RUN
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-STNC > W-ETNC
               GO TO M-40
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-STNC" A-STNC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ETNC" A-ETNC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
           SUBTRACT 1 FROM W-SGET.
           IF  W-SGET = ZERO
               SUBTRACT 1 FROM W-SNEN
               MOVE 12 TO W-SGET
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SM-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       M-50.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SM-PC NOT = 0
               GO TO M-50
           END-IF
           IF  SM-SNG < W-SNG OR > W-ENG
               GO TO M-50
           END-IF
           IF  SM-TNC < W-STNC
               GO TO M-50
           END-IF
           IF  SM-TNC > W-ETNC
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           COMPUTE W-UR = SM-UR + SM-TS.
           COMPUTE W-URZ = SM-URZ + SM-TSZ.
           COMPUTE W-TS = SM-ZS + W-UR - SM-NK.
           COMPUTE W-TSZ = SM-ZSZ + W-URZ - SM-NKZ.
           IF  SM-SNG = W-SNG
               IF  ZERO = W-TS AND W-TSZ
                   GO TO M-50
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE 1 TO W-PAGE.
           PERFORM S-10 THRU S-15.
       M-55.
           MOVE SM-TNC TO W-TNC.
           MOVE 0 TO CHK.
       M-60.
           MOVE 0 TO W-DC.
           MOVE SM-TCD TO W-TCD.
           MOVE SM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
               MOVE ZERO TO T-BC
           END-IF.
      *
       M-65.
           PERFORM S-20 THRU S-25.
       M-70.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SM-PC NOT = 0
               GO TO M-70
           END-IF
           IF  SM-SNG < W-SNG OR > W-ENG
               GO TO M-70
           END-IF
           IF  SM-TNC > W-ETNC
               GO TO M-90
           END-IF
           COMPUTE W-UR = SM-UR + SM-TS.
           COMPUTE W-URZ = SM-URZ + SM-TSZ.
           COMPUTE W-TS = SM-ZS + W-UR - SM-NK.
           COMPUTE W-TSZ = SM-ZSZ + W-URZ - SM-NKZ.
           IF  SM-SNG = W-SNG
               IF  ZERO = W-TS AND W-TSZ
                   GO TO M-70
               END-IF
           END-IF
           IF  W-TNC = SM-TNC
               IF  W-TCD = SM-TCD
                   GO TO M-65
               END-IF
           END-IF
           PERFORM S-30 THRU S-50.
           IF  W-TNC = SM-TNC
               GO TO M-60
           END-IF
           GO TO M-55.
       M-90.
           PERFORM S-30 THRU S-50.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           ADD 1 TO W-PAGE.
       S-10.
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
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NA.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-TNC TO P-TNC
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NA
           END-IF
           IF  SM-SNG = W-SNG
               MOVE "（前回請求）" TO P-ZSM
               GO TO S-21
           END-IF
           MOVE SM-SNGP TO P-SNGP.
           MOVE W-UR TO P-UR.
           MOVE W-URZ TO P-URZ.
           MOVE SM-NK TO P-NK.
           MOVE SM-NKZ TO P-NKZ.
           COMPUTE W-NKIN = SM-NK + SM-NKZ.
           IF  W-NKIN > W-OKIN OR < W-UKIN
               MOVE "※" TO P-CHK1
           END-IF.
       S-21.
           MOVE W-TS TO P-TS.
           MOVE W-TSZ TO P-TSZ.
           IF  SM-DNO = ZERO
               MOVE "×" TO P-CHK2
           END-IF
           IF  SM-NMD NOT = ZERO
               MOVE SM-NMD TO P-NGP
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           COMPUTE W-SKIN = W-TS + W-TSZ.
           COMPUTE W-OKIN = W-SKIN + 9.
           COMPUTE W-UKIN = W-SKIN - 9.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" "NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-45
           END-IF
           MOVE ZERO TO W-SNK W-SNKZ.
       S-35.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           IF  W-TCD NOT = SKD-TCD
               GO TO S-40
           END-IF
           IF (SKD-DTC = 3) AND (SKD-SNO = ZERO)
               ADD SKD-KIN TO W-SNK
               ADD SKD-SHZ TO W-SNKZ
           END-IF
           GO TO S-35.
       S-40.
           IF  ZERO = W-SNK AND W-SNKZ
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NA.
           MOVE "［締後入金］" TO P-SNM.
           MOVE W-SNK TO P-NK.
           MOVE W-SNKZ TO P-NKZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.
