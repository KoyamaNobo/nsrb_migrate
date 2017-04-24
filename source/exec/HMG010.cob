       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG010.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-27.
      *******************************************************************
      *    PROGRAM         :  担当得意先品名別売上粗利集計表　          *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *        変更　　　  :  62/05/12                                  *
      *                    :  JS-SIGN = 0   改頁なし 全体               *
      *                    :  JS-SIGN = 1   改頁あり 一般               *
      *                    :  JS-SIGN = 2   改頁なし ｳﾞｨｳﾞｪﾝﾃﾞｨ         *
      *                    :  JS-SIGN = 3   改頁なし 教育               *
      *                    :  JS-SIGN = 4   改頁あり 一般(教育･VIV含む) *
      *                    :  JS-SIGN = 5   改頁なし 得意先入力         *
      *                    :  JS-SIGN = 6   改頁なし 全体（参考）       *
      *                    :  JS-SIGN = 7   改頁なし 担当（参考）       *
      *                    :  JS-SIGN = 8   ＥＸＣＥＬ変換 ｳﾞｨｳﾞｪﾝﾃﾞｨ   *
      *                    :  JS-SIGN = 9   全体ＥＸＣＬＥ変換          *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
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
           02  H-MID          PIC  N(012) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(026) VALUE
               "＊＊＊　　担当得意先品種別　売上粗利集計表　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(030) VALUE
                "担当 ｺｰﾄﾞ 得　　意　　先　　名".
           02  F              PIC  X(103) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  X(017) VALUE
                " ｺｰﾄﾞ  品　　　名".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  X(024) VALUE
                "数量     売上金額 足単価".
           02  F              PIC  X(039) VALUE
                "    売上原価 足単価　　  粗利益 利益率%".
       01  W-P1.
           02  P-20KA         PIC  X(005).
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(070).
       01  W-P2.
           02  P-15KB         PIC  X(005).
           02  F              PIC  X(020).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UKI          PIC -----,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-SKI          PIC ----,---,--9.
           02  P-GT           PIC ---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC -----9.9.
           02  P-20KB         PIC  X(005).
       01  W-D.
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-HCD        PIC  9(006).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-AR           PIC S9(009).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(009).
           02  W-UT           PIC S9(005).
           02  W-GT           PIC S9(005).
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
       01  WN-D.
           02  WN-SU          PIC S9(007).
           02  WN-UKI         PIC S9(009).
           02  WN-SKI         PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-UKI         PIC S9(009).
           02  WT-SKI         PIC S9(009).
       01  WG-D.
           02  WG-SU          PIC S9(007).
           02  WG-UKI         PIC S9(009).
           02  WG-SKI         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-UKI         PIC S9(009).
           02  WS-SKI         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-UKI         PIC S9(009).
           02  WA-SKI         PIC S9(009).
       01  W-DATA.
           02  W-ALL          PIC S9(006)V9(05).
           02  W-PAGE         PIC  9(002).
           02  CNT.
             03  CNT1         PIC  9(003).
             03  CNT2         PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SETC.
             03  W-STC        PIC  9(002).
             03  W-ETC        PIC  9(002) VALUE 99.
           02  W-SETCD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004) VALUE 9999.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSSNTW.
           COPY LSPF.
      *FD  EXL-F
       01  EXL-F_HMG010.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HMG010".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-TNC        PIC  X(002).
           02  EXL-TCD        PIC  X(004).
           02  EXL-TNA        PIC  N(026).
           02  EXL-HCD        PIC  X(006).
           02  EXL-HNA        PIC  N(024).
           02  EXL-SU         PIC S9(007).
           02  EXL-UKI        PIC S9(009).
           02  EXL-UT         PIC S9(006).
           02  EXL-SKI        PIC S9(009).
           02  EXL-GT         PIC S9(005).
           02  EXL-AR         PIC S9(009).
           02  EXL-RR         PIC S9(003)V9(1).
           02  F              PIC  X(095).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　担当得意先品種別　売上粗利集計表　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  FILLER.
             03  A-STC   PIC  9(002).
             03  A-ETC   PIC  9(002).
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SEN   PIC  N(012).
           02  D-TCM.
             03  FILLER  PIC  X(020) VALUE
                  "担当者ｺｰﾄﾞ  00 ～ 99".
             03  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
           02  D-TCDM.
             03  FILLER  PIC  X(024) VALUE
                  "得意先ｺｰﾄﾞ  0000 ～ 9999".
             03  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
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
           "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "13" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STC" "9" "15" "36" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STC" BY REFERENCE W-STC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETC" "9" "15" "42" "2" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETC" BY REFERENCE W-ETC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STCD" "9" "15" "34" "4" "A-ETC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETCD" "9" "15" "42" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "112" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SEN" "N" "12" "23" "24" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-SEN" BY REFERENCE H-MID "24" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TCM" " " "0" "0" "42" "D-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TCM" "X" "15" "24" "20" " " "D-TCM" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TCM" "X" "20" "23" "22" "01D-TCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-TCDM" " " "0" "0" "46" "D-TCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TCDM" "X" "15" "22" "24" " " "D-TCDM" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TCDM" "X" "20" "23" "22" "01D-TCDM" " " RETURNING RESU.
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
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 2 AND 3 AND 4 AND 5 AND
                            6 AND 7 AND 8 AND 9
               GO TO M-05
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE SPACE TO H-MID.
           IF  JS-SIGN = 1 OR 4
               MOVE "《　一　　般　》　　　　" TO H-MID
           END-IF
           IF  JS-SIGN = 2 OR 8
               MOVE "《　ヴィヴェンディ　》　" TO H-MID
           END-IF
           IF  JS-SIGN = 3
               MOVE "《　教　　育　》　　　　" TO H-MID
           END-IF
           IF  JS-SIGN = 6 OR 7
               MOVE "（参考）　　　　　　　　" TO H-MID
           END-IF
           CALL "SD_Output" USING "D-SEN" D-SEN "p" RETURNING RESU.
           IF  JS-SIGN NOT = 0 AND 2 AND 3 AND 6 AND 8 AND 9
               PERFORM ACP-RTN THRU ACP-EX
           END-IF
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  JS-SIGN = 8 OR 9
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-15.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               GO TO M-95
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-15
           END-IF
           IF  JS-SIGN = 2 OR 8
               IF (SNTR-BC1 NOT = 32) OR (SNTR-BC21 NOT = 2)
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 3
               IF  SNTR-BC3 NOT = 30
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF ((SNTR-BC1 = 32) AND (SNTR-BC21 = 2)) OR
                                                       (SNTR-BC3 = 30)
                   GO TO M-15
               ELSE
                   IF  SNTR-TNC < W-STC OR > W-ETC
                       GO TO M-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 4 OR 7
               IF  SNTR-TNC < W-STC OR > W-ETC
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 5
               IF  SNTR-TCD < W-STCD OR > W-ETCD
                   GO TO M-15
               END-IF
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-15
           END-IF
      *
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN = 8 OR 9
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               GO TO M-20
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM HED-010 THRU HED-EX.
       M-20.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC1 TO W-TNC1.
       M-25.
           MOVE ZERO TO WG-D W-C.
           MOVE SNTR-TNC2 TO W-TNC2.
       M-30.
           MOVE ZERO TO WT-D CHK CNT1.
           MOVE SNTR-TCD TO W-TCD.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　　　　　" TO T-NAME
           END-IF.
       M-35.
           MOVE ZERO TO WN-D.
           MOVE SNTR-HCD TO W-HCD.
       M-40.
           ADD W-SU TO WN-SU.
           ADD W-UKIN TO WN-UKI.
           ADD W-GKIN TO WN-SKI.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-45
           END-IF
           IF  JS-SIGN = 2 OR 8
               IF (SNTR-BC1 NOT = 32) OR (SNTR-BC21 NOT = 2)
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 3
               IF  SNTR-BC3 NOT = 30
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF ((SNTR-BC1 = 32) AND (SNTR-BC21 = 2)) OR
                                                       (SNTR-BC3 = 30)
                   GO TO M-45
               ELSE
                   IF  SNTR-TNC < W-STC OR > W-ETC
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 4 OR 7
               IF  SNTR-TNC < W-STC OR > W-ETC
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 5
               IF  SNTR-TCD < W-STCD OR > W-ETCD
                   GO TO M-45
               END-IF
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-45
           END-IF
      *
           IF  W-TNC1 NOT = SNTR-TNC1
               GO TO M-65
           END-IF
           IF  W-TNC NOT = SNTR-TNC
               GO TO M-60
           END-IF
           IF  W-TCD NOT = SNTR-TCD
               GO TO M-55
           END-IF
           IF  W-HCD NOT = SNTR-HCD
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           PERFORM MPR2-RTN THRU MPR2-EX.
           GO TO M-35.
       M-55.
           PERFORM MPR2-RTN THRU MPR2-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           GO TO M-30.
       M-60.
           PERFORM MPR2-RTN THRU MPR2-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           PERFORM TPR-RTN THRU TPR-EX.
           IF  JS-SIGN = 1 OR 4
               PERFORM HED-RTN THRU HED-EX
           END-IF
           GO TO M-25.
       M-65.
           PERFORM MPR2-RTN THRU MPR2-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           IF  JS-SIGN = 1 OR 4
               PERFORM HED-RTN THRU HED-EX
           END-IF
           GO TO M-20.
       M-70.
           PERFORM MPR2-RTN THRU MPR2-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           PERFORM APR-RTN THRU APR-EX.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           IF  JS-SIGN = 8 OR 9
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           IF  JS-SIGN = 5
               CALL "SD_Output" USING
                "D-TCDM" D-TCDM "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           CALL "SD_Output" USING "D-TCM" D-TCM "p" RETURNING RESU.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-ETC "A-ETC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           IF  W-STC > W-ETC
               GO TO ACP-040
           END-IF
           GO TO ACP-100.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  W-STCD > W-ETCD
               GO TO ACP-080
           END-IF.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 5
                   GO TO ACP-080
               ELSE
                   GO TO ACP-040
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 5
                   GO TO ACP-060
               ELSE
                   GO TO ACP-020
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-100
           END-IF.
       ACP-EX.
           EXIT.
       DST-RTN.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.
       HED-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           MOVE ZERO TO W-C.
       HED-010.
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
           MOVE ZERO TO CHK.
       HED-EX.
           EXIT.
       MPR1-RTN.
           MOVE SPACE TO W-P1.
           MOVE W-20K TO P-20KA.
           IF  W-C = ZERO
               MOVE W-TNC TO P-TNC
           END-IF
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TNC TO P-TNC
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 5 TO W-C.
       MPR1-EX.
           EXIT.
       MPR2-RTN.
           IF  ZERO = WN-SU AND WN-UKI AND WN-SKI
               GO TO MPR2-EX
           END-IF
           IF  JS-SIGN = 8 OR 9
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNA EXL-HNA
           END-IF
           IF  CHK = ZERO
               IF  JS-SIGN = 8 OR 9
                   MOVE W-TCD TO EXL-TCD
                   MOVE T-NAME TO EXL-TNA
                   IF  W-C = ZERO
                       MOVE 5 TO W-C
                       MOVE W-TNC TO EXL-TNC
                   END-IF
               END-IF
           END-IF
           IF  CHK = ZERO
               IF  JS-SIGN NOT = 8 AND 9
                   PERFORM MPR1-RTN THRU MPR1-EX
               END-IF
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　" TO HI-NAME
           END-IF
           IF  HI-OL = 1
               GO TO MPR2-080
           END-IF
           IF  JS-SIGN = 8 OR 9
               MOVE W-HCD TO EXL-HCD
               MOVE HI-NAME TO EXL-HNA
               MOVE WN-SU TO EXL-SU
               MOVE WN-UKI TO EXL-UKI
               MOVE WN-SKI TO EXL-SKI
               GO TO MPR2-010
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15KB.
           MOVE W-20K TO P-20KB.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE WN-SU TO P-SU.
           MOVE WN-UKI TO P-UKI.
           MOVE WN-SKI TO P-SKI.
       MPR2-010.
           MOVE ZERO TO W-UT W-GT.
           IF  WN-SU = ZERO
               GO TO MPR2-020
           END-IF
           IF  WN-UKI NOT = ZERO
               COMPUTE W-UT ROUNDED = WN-UKI / WN-SU
           END-IF
           IF  WN-SKI NOT = ZERO
               COMPUTE W-GT ROUNDED = WN-SKI / WN-SU
           END-IF
           IF  JS-SIGN = 8 OR 9
               MOVE W-UT TO EXL-UT
               MOVE W-GT TO EXL-GT
           ELSE
               MOVE W-UT TO P-UT
               MOVE W-GT TO P-GT
           END-IF.
       MPR2-020.
           COMPUTE W-AR = WN-UKI - WN-SKI.
           IF  JS-SIGN = 8 OR 9
               MOVE W-AR TO EXL-AR
           ELSE
               MOVE W-AR TO P-AR
           END-IF
           IF  WN-UKI = ZERO
               GO TO MPR2-040
           END-IF
           MOVE WN-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           IF  JS-SIGN = 8 OR 9
               MOVE W-RR TO EXL-RR
           ELSE
               MOVE W-RR TO P-RR
           END-IF.
       MPR2-040.
           IF  JS-SIGN = 8 OR 9
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO MPR2-080
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO MPR2-060
           END-IF
           PERFORM HED-RTN THRU HED-EX.
           PERFORM MPR1-RTN THRU MPR1-EX.
       MPR2-060.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MPR2-080.
           ADD WN-SU TO WT-SU.
           ADD WN-UKI TO WT-UKI.
           ADD WN-SKI TO WT-SKI.
           ADD 1 TO CNT1.
           MOVE 5 TO CHK.
       MPR2-EX.
           EXIT.
       KPR-RTN.
           IF  ZERO = WT-SU AND WT-UKI AND WT-SKI
               GO TO KPR-EX
           END-IF
           COMPUTE W-AR = WT-UKI - WT-SKI.
           IF  JS-SIGN = 8 OR 9
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNA
               MOVE "　　　　　　　　　　（　　計　　）　" TO EXL-HNA
               MOVE WT-SU TO EXL-SU
               MOVE WT-UKI TO EXL-UKI
               MOVE WT-SKI TO EXL-SKI
               MOVE W-AR TO EXL-AR
               GO TO KPR-010
           END-IF
           IF  CNT1 = 1
               IF  HI-OL NOT = 1
                   GO TO KPR-060
               END-IF
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15KB.
           MOVE W-20K TO P-20KB.
           MOVE "　　　　　　　　　　（　　計　　）　　" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-UKI TO P-UKI.
           MOVE WT-SKI TO P-SKI.
           MOVE W-AR TO P-AR.
       KPR-010.
           IF  WT-UKI = ZERO
               GO TO KPR-020
           END-IF
           MOVE WT-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           IF  JS-SIGN = 8 OR 9
               MOVE W-RR TO EXL-RR
           ELSE
               MOVE W-RR TO P-RR
           END-IF.
       KPR-020.
           IF  JS-SIGN = 8 OR 9
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO KPR-060
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 62
               GO TO KPR-040
           END-IF
           PERFORM HED-RTN THRU HED-EX.
           PERFORM MPR1-RTN THRU MPR1-EX.
       KPR-040.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KPR-060.
           MOVE SPACE TO SP-R.
           ADD WT-SU TO WG-SU.
           ADD WT-UKI TO WG-UKI.
           ADD WT-SKI TO WG-SKI.
       KPR-EX.
           EXIT.
       TPR-RTN.
           IF  ZERO = WG-SU AND WG-UKI AND WG-SKI
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO TPR-EX
           END-IF
           COMPUTE W-AR = WG-UKI - WG-SKI.
           IF  JS-SIGN = 8 OR 9
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNA
               MOVE "　　　　　　　＜　　合　計　　＞　　" TO EXL-HNA
               MOVE WG-SU TO EXL-SU
               MOVE WG-UKI TO EXL-UKI
               MOVE WG-SKI TO EXL-SKI
               MOVE W-AR TO EXL-AR
               GO TO TPR-010
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15KB.
           MOVE W-20K TO P-20KB.
           MOVE "　　　　　　　＜　　合　計　　＞　　　" TO P-HNA.
           MOVE WG-SU TO P-SU.
           MOVE WG-UKI TO P-UKI.
           MOVE WG-SKI TO P-SKI.
           MOVE W-AR TO P-AR.
       TPR-010.
           IF  WG-UKI = ZERO
               GO TO TPR-020
           END-IF
           MOVE WG-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           IF  JS-SIGN = 8 OR 9
               MOVE W-RR TO EXL-RR
           ELSE
               MOVE W-RR TO P-RR
           END-IF.
       TPR-020.
           IF  JS-SIGN = 8 OR 9
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO TPR-030
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TPR-030.
           ADD WG-SU TO WS-SU.
           ADD WG-UKI TO WS-UKI.
           ADD WG-SKI TO WS-SKI.
       TPR-EX.
           EXIT.
       SPR-RTN.
           IF  ZERO = WS-SU AND WS-UKI AND WS-SKI
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO SPR-EX
           END-IF
           COMPUTE W-AR = WS-UKI - WS-SKI.
           IF  JS-SIGN = 8 OR 9
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNA
               MOVE "　　　　［　　小　合　計　　］　　　" TO EXL-HNA
               MOVE WS-SU TO EXL-SU
               MOVE WS-UKI TO EXL-UKI
               MOVE WS-SKI TO EXL-SKI
               MOVE W-AR TO EXL-AR
               GO TO SPR-010
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15KB.
           MOVE W-20K TO P-20KB.
           MOVE "　　　　［　　小　合　計　　］　　　　　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-UKI TO P-UKI.
           MOVE WS-SKI TO P-SKI.
           MOVE W-AR TO P-AR.
       SPR-010.
           IF  WS-UKI = ZERO
               GO TO SPR-020
           END-IF
           MOVE WS-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           IF  JS-SIGN = 8 OR 9
               MOVE W-RR TO EXL-RR
           ELSE
               MOVE W-RR TO P-RR
           END-IF.
       SPR-020.
           IF  JS-SIGN = 8 OR 9
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO SPR-030
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       SPR-030.
           ADD WS-SU TO WA-SU.
           ADD WS-UKI TO WA-UKI.
           ADD WS-SKI TO WA-SKI.
       SPR-EX.
           EXIT.
       APR-RTN.
           COMPUTE W-AR = WA-UKI - WA-SKI.
           IF  JS-SIGN = 8 OR 9
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNA
               MOVE "　【　　総　合　計　　】　　　　" TO EXL-HNA
               MOVE WA-SU TO EXL-SU
               MOVE WA-UKI TO EXL-UKI
               MOVE WA-SKI TO EXL-SKI
               MOVE W-AR TO EXL-AR
               GO TO APR-010
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15KB.
           MOVE W-20K TO P-20KB.
           MOVE "　【　　総　合　計　　】　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-UKI TO P-UKI.
           MOVE WA-SKI TO P-SKI.
           MOVE W-AR TO P-AR.
       APR-010.
           IF  WA-UKI = ZERO
               GO TO APR-020
           END-IF
           MOVE WA-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           IF  JS-SIGN = 8 OR 9
               MOVE W-RR TO EXL-RR
           ELSE
               MOVE W-RR TO P-RR
           END-IF.
       APR-020.
           IF  JS-SIGN = 8 OR 9
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO APR-EX
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       APR-EX.
           EXIT.
