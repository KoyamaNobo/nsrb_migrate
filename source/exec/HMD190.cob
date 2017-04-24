       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMD190.
      *********************************************************
      *    PROGRAM         :  売上伝票変換リスト              *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(004) VALUE "（　日付".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-SNGP         PIC 9999/99/99.
           02  F              PIC  X(003) VALUE " , ".
           02  F              PIC  N(003) VALUE "請求日".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-MNGP         PIC 9999/99/99.
           02  F              PIC  N(002) VALUE "　）".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　売上伝票変換リスト　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "1             SS    S    M    L   LL 28.0 29.0 30.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "2 12.5 13.0 13.5 14.0 15.0 16.0 17.0 18.0 19.0 20.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(050) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "3 21.0 21.5 22.0 22.5 23.0 23.5 24.0 24.5 25.0     ".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(007) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発送№　".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "個数".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "4 24.0 24.5 25.0 25.5 26.0 26.5 27.0 27.5          ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
       01  W-P1.
           02  P-DCM          PIC  N(004).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(085).
       01  W-P2.
           02  F              PIC  X(006).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU    OCCURS  10  PIC  -(005).
           02  P-SUT          PIC ---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC --,---,--9.
           02  F              PIC  X(001).
           02  P-BI           PIC  X(010).
       01  W-P3.
           02  F              PIC  X(017).
           02  P-HMN          PIC  9(006).
           02  F              PIC  X(005).
           02  P-KOSU         PIC  N(004).
           02  P-KO           PIC  N(002).
           02  F              PIC  X(005).
           02  P-KURA         PIC  N(006).
           02  F              PIC  X(005).
           02  P-CNAM         PIC  N(032).
           02  F              PIC  X(005).
           02  P-TME          PIC  N(004).
           02  P-GKIN         PIC --,---,--9.
           02  P-TKIN         PIC ---,---,--9.
       01  W-P8.
           02  F              PIC  X(136).
       01  W-P9.
           02  F              PIC  X(077).
           02  P-ATM          PIC  N(006).
           02  F              PIC  X(005).
           02  P-MSU          PIC  Z(003).
           02  P-MSUM         PIC  N(002).
           02  F              PIC  X(003).
           02  P-ASUT         PIC ----,--9.
           02  F              PIC  X(017).
           02  P-AKIN         PIC ---,---,--9.
       01  W-DATA.
           02  W-TD.
             03  W-ASUT       PIC S9(006).
             03  W-AKIN       PIC S9(008).
             03  W-SHZ        PIC S9(006).
             03  W-TKIN       PIC S9(008).
             03  W-KIN        PIC S9(008).
           02  W-AD.
             03  WA-ASUT      PIC S9(006).
             03  WA-TKIN      PIC S9(008).
             03  WA-MSU       PIC  9(003).
           02  W-D.
             03  W-PAGE       PIC  9(002).
             03  CNT          PIC  9(002).
             03  W-DC         PIC  9(001).
           02  W-NG           PIC  9(004).
           02  W-SNGP         PIC  9(008).
           02  W-DNO          PIC  9(006).
           02  W-KOSU         PIC ZZZZ.
           02  N              PIC  9(002).
           02  TBL-NAM        PIC  N(032).
           02  TBL-NAMR  REDEFINES TBL-NAM.
             03  T-NAM        PIC  N(001)  OCCURS  32.
           02  SV-NAM         PIC  N(032).
           02  SV-KOSU        PIC  9(003).
      *
           COPY LIBFDD.
           COPY L-JCON.
           COPY LIHIM.
           COPY LITM.
           COPY LITCM.
           COPY LSPF.
      *FD  HSMSW
       01  HSMSW_HMD190.
           02  HSMSW_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HSMSW_LNAME    PIC  X(012) VALUE "HSMSW_HMD190".
           02  F              PIC  X(001).
           02  HSMSW_KEY1     PIC  X(100) VALUE SPACE.
           02  HSMSW_SORT     PIC  X(100) VALUE SPACE.
           02  HSMSW_IDLST    PIC  X(100) VALUE SPACE.
           02  HSMSW_RES      USAGE  POINTER.
       01  HSMSW-R1.
           02  HSMSW-KEY.                                               KEY
             03  HSMSW-01     PIC 9(6).                                 ｼｭｯｶｼｽﾞ
             03  HSMSW-02     PIC 9(1).                                 ｷﾞｮｳ
           02  HSMSW-03       PIC 9(1).                                 ﾃﾞﾝｸ
           02  HSMSW-05.                                                ｼｭｯｶﾋﾞｼﾞ
             03  HSMSW-051    PIC 9(4).
             03  HSMSW-052    PIC 9(2).                                 ﾂｷ
             03  HSMSW-053    PIC 9(2).                                 ﾋ
           02  HSMSW-06.                                                ﾁｮｸｿｳ CD
             03  HSMSW-061    PIC 9(4).                                 ﾄｸｲｺｰﾄﾞ
             03  HSMSW-062    PIC 9(3).                                 ﾁｮｸ NO
           02  HSMSW-07       PIC 9(1).                                 ｸﾗ ｺｰﾄﾞ
           02  HSMSW-09       PIC 9(6).                                 ﾋﾝｺｰﾄﾞ
           02  HSMSW-10       PIC 9(1).                                 ｻｲｽﾞｸﾌﾞﾝ
           02  HSMSW-12.                                                ｼｭｯｶｼﾞﾂ
             03  HSMSW-121    OCCURS  10.                               ｻｲｽﾞﾍﾞﾂ
               04  HSMSW-1211 PIC S9(4).
             03  HSMSW-122    PIC S9(6).
           02  HSMSW-13       PIC 9(1).                                 ｱｽﾞｶﾘ KB
           02  HSMSW-14       PIC S9(03).                               個数
           02  HSMSW-21       PIC 9(01).                                ｲﾝｼﾞｸﾌﾞﾝ
           02  HSMSW-20       PIC 9(02).                                ﾀﾝﾄｳ
           02  HSMSW-16       PIC 9(02).                                ﾌﾞﾝﾙｲ2
           02  HSMSW-17       PIC 9(05).                                ﾀﾝｶ
           02  HSMSW-18       PIC 9(08).
           02  HSMSW-22       PIC X(10).
           02  HSMSW-23       PIC 9(01).                                ﾍﾝｶﾝｸﾌﾞﾝ
           02  HSMSW-24       PIC 9(01).
           02  FILLER         PIC X(14).
           02  HSMSW-26       PIC 9(01).
           02  HSMSW-25       PIC 9(01).
           02  HSMSW-19       PIC 9(01).                                ｼﾖｳｶｲｽｳ
       01  HSMSW-R2.
           02  HSMSW-KEYB.                                              KEY
             03  HSMSW-01B    PIC 9(6).                                 ｼｭｯｶｼｽﾞ
             03  HSMSW-02B    PIC 9(1).                                 ｷﾞｮｳ
           02  HSMSW-03B      PIC 9(1).
           02  HSMSW-05B.
             03  HSMSW-051B   PIC 9(4).
             03  HSMSW-052B   PIC 9(2).
             03  HSMSW-053B   PIC 9(2).
           02  HSMSW-06B.
             03  HSMSW-061B   PIC 9(4).
             03  HSMSW-062B   PIC 9(3).
           02  HSMSW-07B      PIC 9(1).
           02  HSMSW-15       PIC N(24).                                摘要
           02  FILLER         PIC X(37).
           02  HSMSW-23B      PIC 9(01).
           02  HSMSW-24B      PIC 9(01).
           02  FILLER         PIC X(14).
           02  HSMSW-26B      PIC 9(01).
           02  HSMSW-25B      PIC 9(01).
           02  HSMSW-19B      PIC 9(01).                                ｼﾖｳｶｲｽｳ
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
                "＊＊＊　　売上伝票変換リスト　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "266" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "2" "15" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "3" "15" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "15" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "5" "15" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "6" "15" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "7" "15" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "8" "15" "38" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "17" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "17" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HSMSW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSMSW_PNAME1 " " BY REFERENCE HSMSW_IDLST "0".
       M-10.
      *           READ HSMSW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HSMSW_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSW_IDLST HSMSW_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-26 NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "PR_Open" RETURNING RESP.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE D-NHNG TO W-NG.
           MOVE HSMSW-05 TO W-SNGP.
           MOVE W-SNGP TO H-SNGP.
           MOVE HSMSW-18 TO H-MNGP.
           MOVE ZERO TO W-AD W-PAGE.
           PERFORM MID-010 THRU MID-EX.
       M-15.
           MOVE HSMSW-01 TO W-DNO.
           MOVE HSMSW-03 TO W-DC.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-DCM P-TNA.
           IF  HSMSW-03 = 0
               MOVE "売　上　" TO P-DCM
           ELSE
               IF  HSMSW-03 = 1
                   MOVE "返　品　" TO P-DCM
               ELSE
                   IF  HSMSW-03 = 2
                       MOVE "不良返品" TO P-DCM
                   END-IF
               END-IF
           END-IF
           IF  HSMSW-13 = 4 OR 5
               MOVE 4 TO W-DC
               MOVE "預り出荷" TO P-DCM
           END-IF
           MOVE HSMSW-061 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "得意先なし" TO T-NAME
           END-IF
           MOVE HSMSW-061 TO P-TCD.
           MOVE T-NAME TO P-TNA.
           MOVE HSMSW-14 TO SV-KOSU.
           MOVE 3 TO JCON3-01.
           MOVE HSMSW-07 TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
               MOVE "倉庫なし" TO JCON3-03
           END-IF
           PERFORM TNA-RTN THRU TNA-EX.
           PERFORM WR1-RTN THRU WR1-EX.
           MOVE ZERO TO W-TD.
       M-20.
           PERFORM MEI-RTN THRU MEI-EX.
       M-25.
      *           READ HSMSW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HSMSW_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HSMSW-01 = W-DNO
               IF  HSMSW-02 = 7
                   GO TO M-30
               ELSE
                   GO TO M-20
               END-IF
           END-IF
           IF  HSMSW-26 NOT = 1
               GO TO M-25
           END-IF
           GO TO M-15.
       M-30.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSW_IDLST HSMSW_PNAME1.
           PERFORM TOT-RTN THRU TOT-EX.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       TNA-RTN.
           MOVE HSMSW-06 TO TC-KEY.
      *           READ TC-M UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "直送先なし" TO TC-NAME
           END-IF
           MOVE TC-NAME TO SV-NAM TBL-NAM.
           MOVE 26 TO N.
       TNA-020.
           IF  N = 0
               GO TO TNA-EX
           END-IF
           IF  T-NAM(N) NOT = SPACE AND "　"
               GO TO TNA-040
           END-IF
           COMPUTE N = N - 1.
           GO TO TNA-020.
       TNA-040.
           IF  HSMSW-03 = 1 OR 2
               ADD 1 TO N
               MOVE "様" TO T-NAM(N)
               ADD 1 TO N
               MOVE "よ" TO T-NAM(N)
               ADD 1 TO N
               MOVE "り" TO T-NAM(N)
               ADD 1 TO N
               MOVE "返" TO T-NAM(N)
               ADD 1 TO N
               MOVE "品" TO T-NAM(N)
               MOVE TBL-NAM TO SV-NAM
               GO TO TNA-EX
           END-IF
           ADD 1 TO N.
           MOVE "様" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "　" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "直" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "送" TO T-NAM(N).
           MOVE TBL-NAM TO SV-NAM.
       TNA-EX.
           EXIT.
       WR1-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       WR1-EX.
           EXIT.
       MEI-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE HSMSW-09 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "品名なし" TO HI-NAME
           END-IF
           MOVE HSMSW-09 TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE HSMSW-10 TO P-SIZ.
           MOVE ZERO TO CNT.
       MEI-020.
           ADD 1 TO CNT.
           IF  CNT < 11
               IF  HSMSW-1211(CNT) = ZERO
                   GO TO MEI-020
               ELSE
                   MOVE HSMSW-1211(CNT) TO P-SU(CNT)
                   GO TO MEI-020
               END-IF
           END-IF
           MOVE HSMSW-122 TO P-SUT.
           IF  HSMSW-09 < 999900
               ADD HSMSW-122 TO W-ASUT
           END-IF
           IF  W-DC NOT = 4
               COMPUTE W-AKIN = HSMSW-122 * HSMSW-17
               MOVE HSMSW-17 TO P-T
               MOVE W-AKIN TO P-KIN
               ADD W-AKIN TO W-KIN
           END-IF
           IF  HSMSW-22 NOT = SPACE
               MOVE HSMSW-22 TO P-BI
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P3.
           MOVE SPACE TO P-KOSU P-KO P-KURA P-CNAM P-TME.
           MOVE W-DNO TO P-HMN.
           IF  SV-KOSU NOT = ZERO
               MOVE SV-KOSU TO   W-KOSU
               MOVE W-KOSU  TO   P-KOSU
               MOVE "個口" TO  P-KO
           END-IF
           MOVE JCON3-03 TO P-KURA.
           MOVE SV-NAM TO P-CNAM.
           MOVE "合　計　" TO P-TME.
           MOVE W-KIN TO P-GKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P3.
           MOVE SPACE TO P-KOSU P-KO P-KURA P-CNAM P-TME.
           MOVE HSMSW-15 TO P-CNAM.
           MOVE "消費税　" TO P-TME.
           IF  W-NG > 1403
               COMPUTE W-SHZ ROUNDED = W-KIN * 0.08
           ELSE
               COMPUTE W-SHZ ROUNDED = W-KIN * 0.05
           END-IF
           COMPUTE W-TKIN = W-KIN + W-SHZ.
           MOVE W-SHZ TO P-GKIN.
           MOVE W-TKIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD 1 TO WA-MSU.
           ADD W-ASUT TO WA-ASUT.
           ADD W-TKIN TO WA-TKIN.
       KEI-EX.
           EXIT.
       TOT-RTN.
           MOVE ALL "-" TO W-P8.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P9.
           MOVE SPACE TO P-ATM P-MSUM.
           MOVE "【総合計】" TO P-ATM.
           MOVE WA-MSU TO P-MSU.
           MOVE "枚　" TO P-MSUM.
           MOVE WA-ASUT TO P-ASUT.
           MOVE WA-TKIN TO P-AKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TOT-EX.
           EXIT.
