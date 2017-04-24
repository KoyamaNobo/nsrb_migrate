       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PRG460.
      ****************************************
      *****    科目月別　消費税内訳表    *****
      ****************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA                        DIVISION.
       WORKING-STORAGE       SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(019) VALUE
                "年度　科目月別　消費税内訳表　　＊＊＊".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(010) VALUE "  ｺｰﾄﾞ    ".
           02  F              PIC  N(006) VALUE "科　目　名　".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　５月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　６月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　７月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(010) VALUE SPACE.
           02  H-TM2          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　８月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　９月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　１０月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(010) VALUE SPACE.
           02  H-TM3          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　１１月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　１２月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　１月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(010) VALUE SPACE.
           02  H-TM4          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　２月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　３月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　　４月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(010) VALUE SPACE.
           02  H-TM5          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "税引金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "税引金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "税引金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "税引金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-KMK          PIC  9(004).
           02  P-V            PIC  X(001).
           02  P-HOJ          PIC  9(004).
           02  F              PIC  X(001).
           02  P-KNGNMN       PIC  N(010).
           02  F              PIC  X(001).
           02  P-ZR           PIC  N(002).
           02  F              PIC  X(001).
           02  P-M            PIC  X(001).
           02  P-PD    OCCURS  3.
             03  P-KIN        PIC ----,---,---.
             03  P-SHZ        PIC ---,---,---.
             03  F            PIC  X(001).
             03  P-MD         PIC  X(001).
           02  P-KINT         PIC --,---,---,---.
           02  P-SHZT         PIC ----,---,---.
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-NEN          PIC  9(002).
           02  W-ADCD.
             03  W-DCD1.
               04  W-DC1   OCCURS  12  PIC  9(001).
             03  W-DCD3  REDEFINES W-DCD1.
               04  W-DC3   OCCURS   4  PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-PC           PIC  9(002).
           02  DCNT           PIC  9(002).
           02  TCNT           PIC  9(001).
           02  ZCNT           PIC  9(001).
           02  PCNT           PIC  9(001).
           02  W-KMK1         PIC  9(001).
           02  W-TSC          PIC  9(001).
           02  W-ZR           PIC  N(002).
           02  W-D.
             03  W-KIN        PIC S9(010).
             03  W-SHZ        PIC S9(009).
           02  WT-D.
             03  WT-TKIN      PIC S9(010).
             03  WT-TSHZ      PIC S9(009).
             03  WT-NKIN      PIC S9(010).
             03  WT-NSHZ      PIC S9(009).
             03  WT-OKIN      PIC S9(010).
             03  WT-OSHZ      PIC S9(009).
             03  WT-HKIN      PIC S9(010).
           02  W-TUKIT.
             03  W-TUKID   OCCURS  12.
               04  W-NKIN     PIC S9(009).
               04  W-NSHZ     PIC S9(008).
               04  W-OKIN     PIC S9(008).
               04  W-OSHZ     PIC S9(007).
               04  W-HKIN     PIC S9(009).
           COPY  KANGEL.
           COPY  FCTL.
      *       FD  SZF
       01  SZF_PRG460.
           02  SZF_PNAME1          PIC  X(009)  VALUE SPACE.
           02  F                   PIC  X(001).
           02  SZF_LNAME           PIC  X(010)  VALUE "SZF_PRG460".
           02  F              PIC  X(001).
           02  SZF_KEY1            PIC  X(100)  VALUE SPACE.
           02  SZF_KEY2            PIC  X(100)  VALUE SPACE.
           02  SZF_SORT            PIC  X(100)  VALUE SPACE.
           02  SZF_IDLST           PIC  X(100)  VALUE SPACE.
           02  SZF_RES             USAGE  POINTER.
       01  SZ-R.
           02  SZ-KEY.
             03  SZ-KMK       PIC  9(004).
             03  SZ-KMKD  REDEFINES SZ-KMK.
               04  SZ-KMK1    PIC  9(001).
               04  SZ-KMK2    PIC  9(003).
             03  SZ-HOJ       PIC  9(004).
           02  SZ-TUKI.
             03  SZ-TUKID   OCCURS  12.
               04  SZ-NKIN    PIC S9(009).
               04  SZ-NSHZ    PIC S9(008).
               04  SZ-OKIN    PIC S9(008).
               04  SZ-OSHZ    PIC S9(007).
               04  SZ-HKIN    PIC S9(009).
           02  SZ-TSC         PIC  9(001).
           02  F              PIC  X(011).
       77  F                  PIC  X(001).
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
       01  SP-R               PIC  X(206).
       01  C-CLEAR.
           02  FILLER PIC X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　科目月別消費税内訳表　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(015) VALUE
                "【　'  年度  】".
       01  C-DSP.
           02  D-NEN  PIC  9(002).
       01  C-ERR.
           02  E-ME1.
             03  FILLER  PIC  X(032) VALUE
                  "***  ｺﾝﾄﾛｰﾙﾌｧｲﾙ ﾅｼ (      )  ***".
             03  FILLER  PIC  X(006).
           02  E-ME2.
             03  FILLER  PIC  X(036) VALUE
                  "***  ｶﾝｼﾞｶﾓｸﾏｽﾀ- ﾅｼ  (        )  ***".
             03  FILLER  PIC  X(008).
           02  E-ME9  PIC  X(017) VALUE
                "***  DATA ﾅｼ  ***".
           02  E-ME98  PIC  X(005) VALUE X"1B4A09".
           02  E-ME99  PIC  X(005) VALUE X"1B4209".
       PROCEDURE      DIVISION.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *                  01  C-CLEAR.
           CALL "SD_Init" USING
                "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "C-CL" "X" "1" "0" "12" " " "C-CLEAR"  RETURNING RESU.
      *                  01  C-MID.
           CALL "SD_Init" USING
                "C-MID" " " "0" "0" "337" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01C-MID" "N" "3" "15" "46" " " "C-MID"
                RETURNING RESU.
           CALL "SD_Init" USING
                "02C-MID" "N" "4" "15" "46" "01C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "03C-MID" "N" "5" "15" "46" "02C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "04C-MID" "N" "6" "15" "46" "03C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "05C-MID" "N" "7" "15" "46" "04C-MID" " "
                 RETURNING RESU.
           CALL "SD_Init" USING
                "06C-MID" "N" "8" "15" "46" "05C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "07C-MID" "N" "9" "15" "46" "06C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "08C-MID" "X" "15" "31" "15" "07C-MID" " "
                RETURNING RESU.
      *           01  C-DSP.
           CALL "SD_Init" USING
                "C-DSP" " " "0" "0" "2" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "D-NEN" "9" "15" "36" "2" " " "C-DSP"  RETURNING RESU.
           CALL "SD_From" USING
                "D-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
      *                  01  C-ERR   LINE  24.
           CALL "SD_Init" USING
                "C-ERR" " " "24" "0" "109" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME1" " " "24" "0" "38" " " "C-ERR"  RETURNING RESU.
           CALL "SD_Init" USING
                "01E-ME1" "X" "24" "15" "32" " " "E-ME1"
                RETURNING RESU.
           CALL "SD_Init" USING
                "02E-ME1" "X" "24" "35" "6" "01E-ME1" " "
                RETURNING RESU.
           CALL "SD_From" USING
                "02E-ME1" BY REFERENCE FCTL-KEY "6" "0" RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME2" " " "24" "0" "44" "E-ME1" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01E-ME2" "X" "24" "15" "36" " " "E-ME2"
                RETURNING RESU.
           CALL "SD_Init" USING
                "02E-ME2" "X" "24" "37" "8" "01E-ME2" " "
                RETURNING RESU.
           CALL "SD_From" USING
                "02E-ME2" BY REFERENCE KNG-KEY "8" "0" RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME9" "X" "24" "15" "17" "E-ME2" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME98" "X" "24" "75" "5" "E-ME9" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME99" "X" "24" "75" "5" "E-ME98" " "
                RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  " TO FCTL-KEY.
      *           READ FCTL-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE FCTL-UPDYY2 TO W-NEN.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p"
                                         RETURNING RESU.
           CALL "CBLSTNNO" USING  STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO SZF_PNAME1.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-DCD1 = ZERO
               CALL "SD_Output" USING "E-ME9"E-ME9 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
      *
           MOVE W-NEN TO H-NEN.
           ACCEPT H-DATE FROM DATE.
           IF  W-DC3(1) NOT = ZERO
               MOVE 1 TO CHK
               MOVE 58 TO W-PC
               MOVE "　合　計" TO H-TM2
           END-IF.
           IF  W-DC3(2) NOT = ZERO
               MOVE 2 TO CHK
               MOVE 53 TO W-PC
               MOVE "　　　　" TO H-TM2
               MOVE "　合　計" TO H-TM3
           END-IF.
           IF  W-DC3(3) NOT = ZERO
               MOVE 3 TO CHK
               MOVE 48 TO W-PC
               MOVE "　　　　" TO H-TM3
               MOVE "　合　計" TO H-TM4
           END-IF.
           IF  W-DC3(4) NOT = ZERO
               MOVE 4 TO CHK
               MOVE 43 TO W-PC
               MOVE "　　　　" TO H-TM4
               MOVE "　合　計" TO H-TM5
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" SZF_PNAME1 " " BY REFERENCE SZF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
      *
      *           READ SZF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SZF_PNAME1 BY REFERENCE SZ-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME9"E-ME9 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
               GO TO M-95
           END-IF.
           PERFORM MID-010 THRU MID-EX.
       M-20.
           MOVE SZ-KMK1 TO W-KMK1.
           MOVE SZ-TSC TO W-TSC.
           MOVE ZERO TO W-TUKIT.
       M-25.
           MOVE SZ-KEY TO KNG-KEY.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
               GO TO M-95
           END-IF.
           MOVE ZERO TO WT-D TCNT.
       M-30.
           ADD 1 TO TCNT.
           IF  TCNT = 5
               GO TO M-50
           END-IF.
           IF  TCNT = 2
               IF  0 = W-DC3(2) AND W-DC3(3) AND W-DC3(4)
                   GO TO M-50
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  0 = W-DC3(3) AND W-DC3(4)
                   GO TO M-50
               END-IF
           END-IF.
           IF  TCNT = 4
               IF  0 = W-DC3(4)
                   GO TO M-50
               END-IF
           END-IF.
           MOVE 0 TO ZCNT.
       M-35.
           ADD 1 TO ZCNT.
           IF  ZCNT = 5
               GO TO M-30
           END-IF.
           MOVE SPACE TO W-ZR.
           IF  ZCNT = 2
               MOVE "５％" TO W-ZR
           END-IF.
           IF  ZCNT = 3
               MOVE "３％" TO W-ZR
           END-IF.
           IF  ZCNT = 4
               MOVE "非　" TO W-ZR
           END-IF.
      *
           IF  TCNT = 1
               MOVE 0 TO DCNT
           END-IF.
           IF  TCNT = 2
               MOVE 3 TO DCNT
           END-IF.
           IF  TCNT = 3
               MOVE 6 TO DCNT
           END-IF.
           IF  TCNT = 4
               MOVE 9 TO DCNT
           END-IF.
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-KNGNMN P-ZR.
           MOVE ":" TO P-M.
           IF  1 = TCNT AND ZCNT
               MOVE SZ-KMK TO P-KMK
               MOVE "-" TO P-V
               MOVE SZ-HOJ TO P-HOJ
               MOVE KNGNMN TO P-KNGNMN
           END-IF.
           MOVE W-ZR TO P-ZR.
      *
           MOVE 0 TO PCNT.
       M-40.
           ADD 1 TO DCNT PCNT.
           IF  TCNT = 1
               IF  DCNT = 4
                   GO TO M-45
               END-IF
           END-IF.
           IF  TCNT = 2
               IF  DCNT = 7
                   GO TO M-45
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  DCNT = 10
                   GO TO M-45
               END-IF
           END-IF.
           IF  TCNT = 4
               IF  DCNT = 13
                   GO TO M-45
               END-IF
           END-IF.
           IF  ZCNT = 1
               COMPUTE W-KIN = SZ-NKIN(DCNT) + SZ-OKIN(DCNT) +
                               SZ-HKIN(DCNT)
               COMPUTE W-SHZ = SZ-NSHZ(DCNT) + SZ-OSHZ(DCNT)
               ADD W-KIN TO WT-TKIN
               ADD W-SHZ TO WT-TSHZ
               MOVE W-KIN TO P-KIN(PCNT)
               MOVE W-SHZ TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 2
               ADD SZ-NKIN(DCNT) TO W-NKIN(DCNT) WT-NKIN
               ADD SZ-NSHZ(DCNT) TO W-NSHZ(DCNT) WT-NSHZ
               MOVE SZ-NKIN(DCNT) TO P-KIN(PCNT)
               MOVE SZ-NSHZ(DCNT) TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 3
               ADD SZ-OKIN(DCNT) TO W-OKIN(DCNT) WT-OKIN
               ADD SZ-OSHZ(DCNT) TO W-OSHZ(DCNT) WT-OSHZ
               MOVE SZ-OKIN(DCNT) TO P-KIN(PCNT)
               MOVE SZ-OSHZ(DCNT) TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 4
               ADD SZ-HKIN(DCNT) TO W-HKIN(DCNT) WT-HKIN
               MOVE SZ-HKIN(DCNT) TO P-KIN(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           GO TO M-40.
       M-45.
           IF  TCNT = 1
               IF  ZERO = W-DC3(2) AND W-DC3(3) AND W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 2
               IF  ZERO = W-DC3(3) AND W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  ZERO = W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 4
               PERFORM TMO-RTN THRU TMO-EX
           END-IF.
           IF  1 = TCNT AND ZCNT
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > W-PC
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           IF  ZCNT = 1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
           MOVE SPACE TO SP-R.
           GO TO M-35.
       M-50.
      *           READ SZF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SZF_PNAME1 BY REFERENCE SZ-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           IF (SZ-KMK1 = W-KMK1) AND (SZ-TSC = W-TSC)
               GO TO M-25
           END-IF.
      *
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-20.
       M-80.
           PERFORM TOT-RTN THRU TOT-EX.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SZF_IDLST SZF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO W-DCD1.
           CALL "DB_F_Open" USING
            "INPUT" SZF_PNAME1 " " BY REFERENCE SZF_IDLST "0".
       CHK-010.
      *           READ SZF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SZF_PNAME1 BY REFERENCE SZ-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-030
           END-IF.
           MOVE 13 TO DCNT.
       CHK-020.
           SUBTRACT 1 FROM DCNT.
           IF  DCNT = ZERO
               GO TO CHK-010
           END-IF.
           IF  W-DC1(DCNT) NOT = 0
               GO TO CHK-010
           END-IF.
           IF (SZ-NKIN(DCNT) NOT = ZERO) OR
              (SZ-NSHZ(DCNT) NOT = ZERO) OR
              (SZ-OKIN(DCNT) NOT = ZERO) OR
              (SZ-OSHZ(DCNT) NOT = ZERO) OR
              (SZ-HKIN(DCNT) NOT = ZERO)
               MOVE 1 TO W-DC1(DCNT)
           END-IF.
           GO TO CHK-020.
       CHK-030.
           CALL "DB_F_Close" USING BY REFERENCE SZF_IDLST SZF_PNAME1.
       CHK-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
           IF (W-DC3(2) NOT = ZERO) OR (W-DC3(3) NOT = ZERO) OR
              (W-DC3(4) NOT = ZERO)
               MOVE HEAD3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
           IF (W-DC3(3) NOT = ZERO) OR (W-DC3(4) NOT = ZERO)
               MOVE HEAD4 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
           IF  W-DC3(4) NOT = ZERO
               MOVE HEAD5 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       TMO-RTN.
           IF  ZCNT = 1
               MOVE WT-TKIN TO P-KINT
               MOVE WT-TSHZ TO P-SHZT
           END-IF.
           IF  ZCNT = 2
               MOVE WT-NKIN TO P-KINT
               MOVE WT-NSHZ TO P-SHZT
           END-IF.
           IF  ZCNT = 3
               MOVE WT-OKIN TO P-KINT
               MOVE WT-OSHZ TO P-SHZT
           END-IF.
           IF  ZCNT = 4
               MOVE WT-HKIN TO P-KINT
           END-IF.
       TMO-EX.
           EXIT.
       TOT-RTN.
           MOVE ZERO TO WT-D TCNT.
       TOT-010.
           ADD 1 TO TCNT.
           IF  TCNT = 5
               GO TO TOT-EX
           END-IF.
           IF  TCNT = 2
               IF  0 = W-DC3(2) AND W-DC3(3) AND W-DC3(4)
                   GO TO TOT-EX
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  0 = W-DC3(3) AND W-DC3(4)
                   GO TO TOT-EX
               END-IF
           END-IF.
           IF  TCNT = 4
               IF  0 = W-DC3(4)
                   GO TO TOT-EX
               END-IF
           END-IF.
           MOVE 0 TO ZCNT.
       TOT-020.
           ADD 1 TO ZCNT.
           IF  ZCNT = 5
               GO TO TOT-010
           END-IF.
           MOVE SPACE TO W-ZR.
           IF  ZCNT = 2
               MOVE "５％" TO W-ZR
           END-IF.
           IF  ZCNT = 3
               MOVE "３％" TO W-ZR
           END-IF.
           IF  ZCNT = 4
               MOVE "非　" TO W-ZR
           END-IF.
      *
           IF  TCNT = 1
               MOVE 0 TO DCNT
           END-IF.
           IF  TCNT = 2
               MOVE 3 TO DCNT
           END-IF.
           IF  TCNT = 3
               MOVE 6 TO DCNT
           END-IF.
           IF  TCNT = 4
               MOVE 9 TO DCNT
           END-IF.
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-KNGNMN P-ZR.
           MOVE ":" TO P-M.
           IF  1 = TCNT AND ZCNT
               MOVE "［　合　計　］　" TO P-KNGNMN
           END-IF.
           MOVE W-ZR TO P-ZR.
      *
           MOVE 0 TO PCNT.
       TOT-030.
           ADD 1 TO DCNT PCNT.
           IF  TCNT = 1
               IF  DCNT = 4
                   GO TO TOT-040
               END-IF
           END-IF.
           IF  TCNT = 2
               IF  DCNT = 7
                   GO TO TOT-040
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  DCNT = 10
                   GO TO TOT-040
               END-IF
           END-IF.
           IF  TCNT = 4
               IF  DCNT = 13
                   GO TO TOT-040
               END-IF
           END-IF.
           IF  ZCNT = 1
               COMPUTE W-KIN = W-NKIN(DCNT) + W-OKIN(DCNT) +
                               W-HKIN(DCNT)
               COMPUTE W-SHZ = W-NSHZ(DCNT) + W-OSHZ(DCNT)
               ADD W-KIN TO WT-TKIN
               ADD W-SHZ TO WT-TSHZ
               MOVE W-KIN TO P-KIN(PCNT)
               MOVE W-SHZ TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 2
               ADD W-NKIN(DCNT) TO WT-NKIN
               ADD W-NSHZ(DCNT) TO WT-NSHZ
               MOVE W-NKIN(DCNT) TO P-KIN(PCNT)
               MOVE W-NSHZ(DCNT) TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 3
               ADD W-OKIN(DCNT) TO WT-OKIN
               ADD W-OSHZ(DCNT) TO WT-OSHZ
               MOVE W-OKIN(DCNT) TO P-KIN(PCNT)
               MOVE W-OSHZ(DCNT) TO P-SHZ(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           IF  ZCNT = 4
               ADD W-HKIN(DCNT) TO WT-HKIN
               MOVE W-HKIN(DCNT) TO P-KIN(PCNT)
               MOVE ":" TO P-MD(PCNT)
           END-IF.
           GO TO TOT-030.
       TOT-040.
           IF  TCNT = 1
               IF  ZERO = W-DC3(2) AND W-DC3(3) AND W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 2
               IF  ZERO = W-DC3(3) AND W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 3
               IF  ZERO = W-DC3(4)
                   PERFORM TMO-RTN THRU TMO-EX
               END-IF
           END-IF.
           IF  TCNT = 4
               PERFORM TMO-RTN THRU TMO-EX
           END-IF.
           IF  1 = TCNT AND ZCNT
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > W-PC
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           IF  ZCNT = 1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
           MOVE SPACE TO SP-R.
           GO TO TOT-020.
       TOT-EX.
           EXIT.
