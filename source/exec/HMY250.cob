       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY250.
      **********************************************************
      *****     担当得意先品種月別　年間売上数量集計表     *****
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-IKCM         PIC  N(010) VALUE
                "｛　一　般　｝　　　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  N(004) VALUE "月　～　".
           02  H-EN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(019) VALUE
                "月　得意先品種月別　年間売上数量集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  H-TTM          PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(087) VALUE SPACE.
           02  H-SMID         PIC  N(016) VALUE
                "＜　合計１０足以下は，その他　＞".
       01  HEAD3.
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(024) VALUE SPACE.
           02  H-NGDM1.
             03  H-NGD1  OCCURS   6.
               04  F          PIC  X(003).
               04  H-NG1      PIC 99/99.
           02  F              PIC  X(006) VALUE " :    ".
           02  F              PIC  N(004) VALUE "小　　計".
           02  F              PIC  X(004) VALUE SPACE.
           02  H-AM1          PIC  N(004).
       01  HEAD4.
           02  F              PIC  X(062) VALUE SPACE.
           02  H-NGDM2.
             03  H-NGD2  OCCURS   6.
               04  F          PIC  X(003).
               04  H-NG2      PIC 99/99.
           02  F              PIC  X(006) VALUE " :    ".
           02  F              PIC  N(004) VALUE "小　　計".
           02  F              PIC  X(004) VALUE SPACE.
           02  H-AM2          PIC  N(004) VALUE "合　　計".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-TTC1         PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(063).
           02  P-X1           PIC  X(001).
           02  F              PIC  X(020).
       01  W-P2.
           02  F              PIC  X(001).
           02  P-TTC2         PIC  9(002).
           02  F              PIC  X(016).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SUD.
             03  P-SU    OCCURS   6  PIC ----,--9.
           02  F              PIC  X(001).
           02  P-X2           PIC  X(001).
           02  P-HSU          PIC --,---,--9.
           02  P-ASU          PIC --,---,--9.
       01  W-DATA.
           02  W-NGD1.
             03  W-NG1   OCCURS   6  PIC  9(006).
           02  W-NGD2.
             03  W-NG2   OCCURS   6  PIC  9(006).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-SETTC.
             03  W-STTC       PIC  9(002) VALUE 00.
             03  W-ETTC       PIC  9(002) VALUE 99.
           02  W-TTC.
             03  W-TTC1       PIC  9(001).
             03  W-TTC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-DTC          PIC  9(001).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  CNT            PIC  9(002).
           02  W-DC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-SUD.
             03  W-SU1   OCCURS   6  PIC S9(006).
             03  W-SU2   OCCURS   6  PIC S9(006).
           02  W-HSU1         PIC S9(007).
           02  W-HSU2         PIC S9(007).
           02  W-ASU          PIC S9(007).
           02  W-IKCM         PIC  N(010).
           02  W-DMM          PIC  9(001).
       01  WT-D.
           02  WT-SUD.
             03  WT-SU1   OCCURS   6  PIC S9(006).
             03  WT-SU2   OCCURS   6  PIC S9(006).
       01  WN-D.
           02  WN-SUD.
             03  WN-SU1   OCCURS   6  PIC S9(006).
             03  WN-SU2   OCCURS   6  PIC S9(006).
       01  WS-D.
           02  WS-SUD.
             03  WS-SU1   OCCURS   6  PIC S9(006).
             03  WS-SU2   OCCURS   6  PIC S9(006).
       01  WA-D.
           02   WA-SUD.
             03  WA-SU1   OCCURS   6  PIC S9(006).
             03  WA-SU2   OCCURS   6  PIC S9(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
       01  SSR-YF_HMY250.
           02  SSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(014)  VALUE "SSR-YFW_HMY250".
           02  F               PIC  X(001).
           02  SSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSR-YF_RES      USAGE  POINTER.
       01  SSR-YR.
           02  SSR-TCD        PIC  9(004).
           02  SSR-HCD        PIC  9(006).
           02  SSR-SU         PIC S9(007).
           02  SSR-UK         PIC S9(010).
           02  SSR-GK         PIC S9(010).
           02  SSR-TKC        PIC  9(002).
           02  SSR-TTC.
             03  SSR-TTC1     PIC  9(001).
             03  SSR-TTC2     PIC  9(001).
           02  SSR-BCD1.
             03  SSR-BC1      PIC  9(002).
             03  SSR-BC21     PIC  9(001).
           02  SSR-BC22       PIC  9(001).
           02  SSR-BC3        PIC  9(002).
           02  F              PIC  X(005).
           02  SSR-NG         PIC  9(006).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　得意先品種　月別　売上数集計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STTC    PIC  9(002).
             03  A-ETTC    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-IKCM    PIC  N(010).
           02  D-TTCM    PIC  X(026) VALUE
                "担当者ｺｰﾄﾞ 00 より 99 まで".
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-CL      PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "396" " " " " RETURNING RESU.
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
            "08C-MID" "X" "13" "23" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STTC" "9" "15" "33" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STTC" BY REFERENCE W-STTC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETTC" "9" "15" "41" "2" "A-STTC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETTC" BY REFERENCE W-ETTC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-IKCM" "N" "7" "26" "20" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-IKCM" BY REFERENCE W-IKCM "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TTCM" "X" "15" "22" "26" "D-IKCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "13" "0" "8" "D-TTCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "13" "24" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "13" "29" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "13" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "13" "43" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "95" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
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
      *
       M-020.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
           IF  COMPLETION_CODE = 110
               MOVE "｛　一　般　｝　　　" TO W-IKCM
           END-IF.
           IF  COMPLETION_CODE = 130
               MOVE SPACE TO H-SMID
               MOVE "｛ヴィヴェンディ｝　" TO W-IKCM
           END-IF.
           IF  COMPLETION_CODE = 150
               MOVE "｛　教　育　｝　　　" TO W-IKCM
           END-IF.
           CALL "SD_Output" USING "D-IKCM" D-IKCM "p" 
                                         RETURNING RESU.
           IF  COMPLETION_CODE = 110
               CALL "SD_Output" USING "D-TTCM" D-TTCM "p" 
                                         RETURNING RESU
           ELSE
               GO TO M-180
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-STTC "A-STTC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-980
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-ETTC "A-ETTC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF.
           IF  W-STTC > W-ETTC
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  COMPLETION_CODE = 110
               IF  ESTAT = BTB
                   GO TO M-160
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF.
           IF  W-DMM = 9
               IF  COMPLETION_CODE NOT = 110
                   MOVE 255 TO COMPLETION_CODE
                   GO TO M-980
               ELSE
                   GO TO M-140
               END-IF
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-180
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
       M-220.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               GO TO M-980
           END-IF.
           IF  SSR-NG < W-SNG OR > W-ENG
               GO TO M-220
           END-IF.
           IF  COMPLETION_CODE = 110
               IF  SSR-TTC < W-STTC
                   GO TO M-220
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 110
               IF  SSR-TTC > W-ETTC
                   CALL "DB_F_Close" USING
                    BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
                   CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
                   CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
                   GO TO M-980
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 110
               IF (SSR-BCD1 = 322) OR (SSR-BC3 = 30)
                   GO TO M-220
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 130
               IF  SSR-BCD1 NOT = 322
                   GO TO M-220
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 150
               IF  SSR-BC3 NOT = 30
                   GO TO M-220
               END-IF
           END-IF.
           IF  SSR-SU = ZERO
               GO TO M-220
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SN2 TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-EN2 TO H-EN.
           MOVE W-EGET TO H-EG.
           MOVE W-SNG TO W-NG.
           MOVE ZERO TO WA-D W-DTC.
           IF  W-SNEN = W-ENEN
               COMPUTE CNT = W-EGET - W-SGET
           ELSE
               COMPUTE CNT = (12 + W-EGET) - W-SGET
           END-IF.
           IF  CNT > 11
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               GO TO M-980
           END-IF.
           IF  CNT > 5
               MOVE 5 TO W-DTC
               MOVE SPACE TO H-AM1
           ELSE
               MOVE H-AM2 TO H-AM1
           END-IF.
           MOVE SPACE TO H-NGDM1 H-NGDM2.
           MOVE ZERO TO CHK W-NGD1 W-NGD2.
       M-240.
           ADD 1 TO CHK.
           IF  CHK = 7
               MOVE 0 TO CHK
               GO TO M-260
           END-IF.
           MOVE W-NG TO W-NG1(CHK).
           MOVE W-NGS TO H-NG1(CHK).
           IF  W-NG = W-ENG
               GO TO M-280
           END-IF.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF.
           GO TO M-240.
       M-260.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO M-280
           END-IF.
           MOVE W-NG TO W-NG2(CHK).
           MOVE W-NGS TO H-NG2(CHK).
           IF  W-NG = W-ENG
               GO TO M-280
           END-IF.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF.
           GO TO M-260.
       M-280.
           MOVE W-IKCM TO H-IKCM.
           PERFORM S-040 THRU S-060.
       M-300.
           IF  COMPLETION_CODE NOT = 110
               GO TO M-320
           END-IF.
           MOVE SSR-TTC TO W-TTC.
           MOVE ZERO TO WS-D W-C.
       M-320.
           MOVE ZERO TO WN-D WT-D W-DC.
           MOVE SSR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　なし　＊＊" TO T-NAME
           END-IF.
           PERFORM S-080 THRU S-100.
       M-340.
           MOVE SSR-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　品名マスター　なし　＊＊　　" TO HI-NAME
           END-IF.
           MOVE ZERO TO W-ASU W-HSU1 W-HSU2 W-SUD.
       M-360.
           MOVE 0 TO CHK.
       M-380.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO M-400
           END-IF.
           IF  SSR-NG = W-NG1(CHK)
               MOVE SSR-SU TO W-SU1(CHK)
               ADD SSR-SU TO W-HSU1 W-ASU
               GO TO M-400
           END-IF.
           IF  W-DTC = 5
               IF  SSR-NG = W-NG2(CHK)
                   MOVE SSR-SU TO W-SU2(CHK)
                   ADD SSR-SU TO W-HSU2 W-ASU
                   GO TO M-400
               END-IF
           END-IF.
           GO TO M-380.
       M-400.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF.
           IF  SSR-NG < W-SNG OR > W-ENG
               GO TO M-400
           END-IF.
           IF  COMPLETION_CODE = 110
               IF  SSR-TTC > W-ETTC
                   GO TO M-900
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 110
               IF (SSR-BCD1 = 322) OR (SSR-BC3 = 30)
                   GO TO M-400
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 130
               IF  SSR-BCD1 NOT = 322
                   GO TO M-400
               END-IF
           END-IF.
           IF  COMPLETION_CODE = 150
               IF  SSR-BC3 NOT = 30
                   GO TO M-400
               END-IF
           END-IF.
           IF  SSR-SU = ZERO
               GO TO M-400
           END-IF.
           IF  COMPLETION_CODE = 110
               IF  SSR-TTC NOT = W-TTC
                   GO TO M-440
               END-IF
           END-IF.
           IF  SSR-TCD NOT = W-TCD
               GO TO M-420
           END-IF.
           IF  SSR-HCD = W-HCD
               GO TO M-360
           END-IF.
           PERFORM S-120 THRU S-240.
           GO TO M-340.
       M-420.
           PERFORM S-120 THRU S-240.
           PERFORM S-260 THRU S-380.
           PERFORM S-400 THRU S-520.
           GO TO M-320.
       M-440.
           PERFORM S-120 THRU S-240.
           PERFORM S-260 THRU S-380.
           PERFORM S-400 THRU S-520.
           PERFORM S-540 THRU S-640.
           IF  SSR-TTC1 NOT = W-TTC1
               PERFORM S-020 THRU S-040
           END-IF.
           GO TO M-300.
       M-900.
           PERFORM S-120 THRU S-240.
           PERFORM S-260 THRU S-380.
           PERFORM S-400 THRU S-520.
           IF  COMPLETION_CODE = 110
               PERFORM S-540 THRU S-640
           END-IF.
           PERFORM S-660 THRU S-760.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-020.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-040.
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
           IF  W-DTC = 5
               MOVE HEAD4 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
           MOVE 0 TO W-C.
       S-060.
           EXIT.
       S-080.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-020 THRU S-060
           END-IF.
           MOVE SPACE TO W-P1.
           IF  W-C = 0
               MOVE 5 TO W-C
               MOVE W-TTC TO P-TTC1
           END-IF.
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           MOVE ":" TO P-X1.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-100.
           EXIT.
       S-120.
           IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
               MOVE SPACE TO W-P2
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
               MOVE ":" TO P-X2
               MOVE W-HSU1 TO P-HSU
           END-IF.
           MOVE 0 TO CHK.
       S-140.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-160
           END-IF.
           IF  W-NG1(CHK) NOT = ZERO
               ADD W-SU1(CHK) TO WN-SU1(CHK)
               IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
                   MOVE W-SU1(CHK) TO P-SU(CHK)
               ELSE
                   ADD W-SU1(CHK) TO WT-SU1(CHK)
               END-IF
           END-IF.
           GO TO S-140.
       S-160.
           IF (W-ASU < 10) AND (COMPLETION_CODE NOT = 130)
               GO TO S-180
           END-IF.
           IF  W-DTC = 0
               MOVE W-ASU TO P-ASU
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO S-180
           END-IF.
           PERFORM S-020 THRU S-060.
           PERFORM S-080 THRU S-100.
       S-180.
           IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
               PERFORM S-800 THRU S-820
           END-IF.
           IF  W-DTC = 0
               GO TO S-240
           END-IF.
           IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
               MOVE SPACE TO W-P2
               MOVE SPACE TO P-HNA
               MOVE ":" TO P-X2
               MOVE W-HSU2 TO P-HSU
               MOVE W-ASU TO P-ASU
           END-IF.
           MOVE 0 TO CHK.
       S-200.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-220
           END-IF.
           IF  W-NG2(CHK) NOT = ZERO
               ADD W-SU2(CHK) TO WN-SU2(CHK)
               IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
                   MOVE W-SU2(CHK) TO P-SU(CHK)
               ELSE
                   ADD W-SU2(CHK) TO WT-SU2(CHK)
               END-IF
           END-IF.
           GO TO S-200.
       S-220.
           IF (W-ASU > 9) OR (COMPLETION_CODE = 130)
               PERFORM S-800 THRU S-820
           END-IF.
       S-240.
           EXIT.
       S-260.
           IF  WT-D = ZERO
               GO TO S-380
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE "そ　の　他　" TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU1 W-ASU.
       S-280.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-300
           END-IF.
           IF  W-NG1(CHK) NOT = ZERO
               MOVE WT-SU1(CHK) TO P-SU(CHK)
               ADD WT-SU1(CHK) TO W-HSU1 W-ASU
           END-IF.
           GO TO S-280.
       S-300.
           MOVE W-HSU1 TO P-HSU.
           IF  W-DTC = 0
               MOVE W-ASU TO P-ASU
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO S-320
           END-IF.
           PERFORM S-020 THRU S-060.
           PERFORM S-080 THRU S-100.
       S-320.
           PERFORM S-800 THRU S-820.
           IF  W-DTC = 0
               GO TO S-380
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU2.
       S-340.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-360
           END-IF.
           IF  W-NG2(CHK) NOT = ZERO
               MOVE WT-SU2(CHK) TO P-SU(CHK)
               ADD WT-SU2(CHK) TO W-HSU2 W-ASU
           END-IF.
           GO TO S-340.
       S-360.
           MOVE W-HSU2 TO P-HSU.
           MOVE W-ASU TO P-ASU.
           PERFORM S-800 THRU S-820.
       S-380.
           EXIT.
       S-400.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　　　（　ＴＯＴＡＬ　）" TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU1 W-ASU.
       S-420.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-440
           END-IF.
           IF  W-NG1(CHK) NOT = ZERO
               MOVE WN-SU1(CHK) TO P-SU(CHK)
               ADD WN-SU1(CHK) TO W-HSU1 W-ASU
               IF  COMPLETION_CODE = 110
                   ADD WN-SU1(CHK) TO WS-SU1(CHK)
               ELSE
                   ADD WN-SU1(CHK) TO WA-SU1(CHK)
               END-IF
           END-IF.
           GO TO S-420.
       S-440.
           MOVE W-HSU1 TO P-HSU.
           IF  W-DTC = 0
               MOVE W-ASU TO P-ASU
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO S-460
           END-IF.
           PERFORM S-020 THRU S-060.
           PERFORM S-080 THRU S-100.
       S-460.
           PERFORM S-800 THRU S-820.
           IF  W-DTC = 0
               GO TO S-520
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU2.
       S-480.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-500
           END-IF.
           IF  W-NG2(CHK) NOT = ZERO
               MOVE WN-SU2(CHK) TO P-SU(CHK)
               ADD WN-SU2(CHK) TO W-HSU2 W-ASU
               IF  COMPLETION_CODE = 110
                   ADD WN-SU2(CHK) TO WS-SU2(CHK)
               ELSE
                   ADD WN-SU2(CHK) TO WA-SU2(CHK)
               END-IF
           END-IF.
           GO TO S-480.
       S-500.
           MOVE W-HSU2 TO P-HSU.
           MOVE W-ASU TO P-ASU.
           PERFORM S-800 THRU S-820.
       S-520.
           EXIT.
       S-540.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           IF  W-DTC = 0
               PERFORM S-800 THRU S-820
           END-IF.
           MOVE "　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］　" TO P-HNA.
           MOVE ZERO TO CHK W-HSU1 W-ASU.
       S-560.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-580
           END-IF.
           IF  W-NG1(CHK) NOT = ZERO
               MOVE WS-SU1(CHK) TO P-SU(CHK)
               ADD WS-SU1(CHK) TO W-HSU1 W-ASU WA-SU1(CHK)
           END-IF.
           GO TO S-560.
       S-580.
           MOVE W-HSU1 TO P-HSU.
           IF  W-DTC = 0
               MOVE W-ASU TO P-ASU
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TTC TO P-TTC2
               PERFORM S-020 THRU S-060
           END-IF.
           PERFORM S-800 THRU S-820.
           IF  W-DTC = 0
               GO TO S-640
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU2.
       S-600.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-620
           END-IF.
           IF  W-NG2(CHK) NOT = ZERO
               MOVE WS-SU2(CHK) TO P-SU(CHK)
               ADD WS-SU2(CHK) TO W-HSU2 W-ASU WA-SU2(CHK)
           END-IF.
           GO TO S-600.
       S-620.
           MOVE W-HSU2 TO P-HSU.
           MOVE W-ASU TO P-ASU.
           PERFORM S-800 THRU S-820.
       S-640.
           EXIT.
       S-660.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           IF  W-DTC = 0
               PERFORM S-800 THRU S-820
           END-IF.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　　　　　" TO P-HNA.
           MOVE ZERO TO CHK W-HSU1 W-ASU.
       S-680.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-700
           END-IF.
           IF  W-NG1(CHK) NOT = ZERO
               MOVE WA-SU1(CHK) TO P-SU(CHK)
               ADD WA-SU1(CHK) TO W-HSU1 W-ASU
           END-IF.
           GO TO S-680.
       S-700.
           MOVE W-HSU1 TO P-HSU.
           IF  W-DTC = 0
               MOVE W-ASU TO P-ASU
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-020 THRU S-060
           END-IF.
           PERFORM S-800 THRU S-820.
           IF  W-DTC = 0
               GO TO S-760
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE ":" TO P-X2.
           MOVE ZERO TO CHK W-HSU2.
       S-720.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO S-740
           END-IF.
           IF  W-NG2(CHK) NOT = ZERO
               MOVE WA-SU2(CHK) TO P-SU(CHK)
               ADD WA-SU2(CHK) TO W-HSU2 W-ASU
           END-IF.
           GO TO S-720.
       S-740.
           MOVE W-HSU2 TO P-HSU.
           MOVE W-ASU TO P-ASU.
           PERFORM S-800 THRU S-820.
       S-760.
           EXIT.
       S-800.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-820.
           EXIT.
