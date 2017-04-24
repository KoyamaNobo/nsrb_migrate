       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN670.
      *************************************************************
      *    PROGRAM         :  品名別棚卸・帳簿差額リスト(親子ｺｰﾄﾞ)*
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　品名別　棚卸　親子　チェック　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(002) VALUE "入数".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(087) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ケース計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "足数合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　総合計".
       01  HEAD9.
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(036) VALUE
                "------------------------------------".
       01  W-P1.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-F            PIC  X(001).
           02  P-ISU          PIC ZZ9.
           02  P-R            PIC  X(001).
           02  F              PIC  X(049).
           02  P-ZM           PIC  N(010).
           02  F              PIC  X(023).
       01  W-P2.
           02  P-TM           PIC  N(012).
           02  P-SNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DNO1         PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-DNO2         PIC  X(004).
           02  F              PIC  X(001).
           02  P-SM           PIC  N(006).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ---,--9  OCCURS  10.
           02  P-KST          PIC ----,--9.
           02  P-SST          PIC --,---,--9.
           02  P-AST          PIC --,---,--9.
       01  W-ASU.
           02  W-SUD1.
             03  W-SU1        PIC S9(006)  OCCURS  10.
           02  W-SUD2.
             03  W-SU2        PIC S9(006)  OCCURS  10.
           02  W-SUD3.
             03  W-SU3        PIC S9(006)  OCCURS  10.
           02  W-SUD4.
             03  W-SU4        PIC S9(006)  OCCURS  10.
           02  W-SUD5.
             03  W-SU5        PIC S9(006)  OCCURS  10.
           02  W-SUD6.
             03  W-SU6        PIC S9(006)  OCCURS  10.
           02  W-SUD7.
             03  W-SU7        PIC S9(006)  OCCURS  10.
           02  W-SUD8.
             03  W-SU8        PIC S9(006)  OCCURS  10.
       01  W-AASU.
           02  W-ASUD1.
             03  W-ASU1        PIC S9(006)  OCCURS  10.
           02  W-ASUD2.
             03  W-ASU2        PIC S9(006)  OCCURS  10.
           02  W-ASUD3.
             03  W-ASU3        PIC S9(006)  OCCURS  10.
           02  W-ASUD4.
             03  W-ASU4        PIC S9(006)  OCCURS  10.
       01  W-SUD.
           02  W-SU           PIC S9(006)  OCCURS  10.
       01  W-DATA.
           02  W-SNO          PIC  9(001).
           02  W-DNO          PIC  X(006).
           02  W-DNOD  REDEFINES W-DNO.
             03  W-DNO1       PIC  9(002).
             03  W-DNO2       PIC  X(004).
           02  W-MHCD         PIC  9(006) VALUE ZERO.
           02  W-DCC          PIC  9(001) VALUE ZERO.
           02  W-DC           PIC  9(001) VALUE ZERO.
           02  W-HCD          PIC  9(006) VALUE ZERO.
           02  W-SIZ          PIC  9(001) VALUE ZERO.
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC         PIC  9(001) OCCURS  8.
           02  W-AZCD.
             03  W-AZC         PIC  9(001) OCCURS  4.
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
             03  CHK3         PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-SE.
             03  W-SHCD       PIC  9(006).
             03  W-EHCD       PIC  9(006) VALUE 999999.
           02  W-TC           PIC  9(001).
           02  W-TCD          PIC  9(001).
           02  W-ISU          PIC  9(003).
           02  W-T.
             03  W-SST        PIC S9(007).
             03  W-HST        PIC S9(007).
             03  W-AST        PIC S9(007).
           02  WS-T.
             03  WS-SST       PIC S9(007).
             03  WS-HST       PIC S9(007).
             03  WS-AST       PIC S9(007).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
      *FD  HTIW-M
       01  HTIW-M_HMN670.
           02  HTIW-M_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-M_LNAME   PIC  X(013) VALUE "HTIW-M_HMN670".
           02  F              PIC  X(001).
           02  HTIW-M_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-M_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-M_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-M_RES     USAGE  POINTER.
       01  HTIW-R.
           02  HTIW-KEY.
             03  HTIW-DNO.
               04  HTIW-DNO1  PIC  9(005).
               04  HTIW-DNO2  PIC  X(001).
             03  HTIW-GNO     PIC  9(001).
           02  HTIW-SNO       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  HTIW-ISU       PIC  9(003).
           02  HTIW-MHCD      PIC  9(006).
           02  HTIW-DC        PIC  9(001).
           02  F              PIC  X(037).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品名別　棚卸　親子チェック　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "　親ｺｰﾄﾞ  000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(025) VALUE
                  "***  DATA ﾅｼ  ***        ".
             03  E-ME5   PIC  X(025) VALUE
                  "***  ｲﾘｽｳ ZERO  ***      ".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "278" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "15" "20" "26" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "X" "22" "29" "22" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SHCD" "9" "15" "30" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EHCD" "9" "15" "40" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "46" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "146" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "146" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HTIW-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HTIW-M_PNAME1 " " BY REFERENCE HTIW-M_IDLST "0".
       M-25.
      *           READ HTIW-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTIW-M_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HTIW-MHCD < W-SHCD
               GO TO M-25
           END-IF
           IF  HTIW-MHCD > W-EHCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-30.
           MOVE HTIW-MHCD TO W-MHCD.
       M-35.
           MOVE HTIW-DC TO W-DC.
           MOVE ZERO TO W-AASU W-ASU WS-T W-AZCD W-ZCD CHK W-C.
       M-40.
           MOVE HTIW-HCD TO W-HCD.
           MOVE HTIW-ISU TO W-ISU.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT HI-ISU
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊　マスター　なし　＊＊　　　" TO HI-NAME
           END-IF
           PERFORM S-20 THRU S-25.
           MOVE ZERO TO W-DCC.
       M-45.
           MOVE HTIW-SNO TO W-SNO.
           MOVE ZERO TO CHK2.
       M-50.
           MOVE HTIW-DNO TO W-DNO.
           MOVE ZERO TO W-T CHK3.
       M-55.
           IF  W-DC NOT = 1
               IF  HTIW-GNO < 5
                   MOVE 1 TO W-TC
               ELSE
                   MOVE 2 TO W-TC
               END-IF
           END-IF
           MOVE 0 TO W-TCD.
       M-60.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           IF  W-DC NOT = 1
               IF  CHK2 = 0
                   MOVE 5 TO CHK2
                   MOVE W-SNO TO P-SNO
               END-IF
           END-IF
           IF  W-DC NOT = 1
               IF  CHK3 = 0
                   MOVE 5 TO CHK3
                   MOVE W-DNO1 TO P-DNO1
                   MOVE "-" TO P-V
                   MOVE W-DNO2 TO P-DNO2
               END-IF
           END-IF
           IF  W-DC = 1
               IF  W-DCC = 0
                   MOVE 1 TO W-DCC
                   MOVE "○帳簿在庫○" TO P-SM
               END-IF
           END-IF
           IF  W-DC NOT = 1
               IF  W-TC NOT = W-TCD
                   IF  W-TC = 1
                       MOVE "［ケース］　" TO P-SM
                   ELSE
                       MOVE "［端　数］　" TO P-SM
                   END-IF
               END-IF
           END-IF
           MOVE HTIW-SIZ TO P-SIZ.
           IF  W-ZC(HTIW-GNO) = 0
               MOVE 1 TO W-ZC(HTIW-GNO)
           END-IF
           MOVE 0 TO W-SIZ.
           MOVE HTIW-GNO TO W-SIZ.
           IF  W-SIZ > 4
               SUBTRACT 4 FROM W-SIZ
           END-IF
           IF  W-AZC(W-SIZ) = 0
               MOVE 1 TO W-AZC(W-SIZ)
           END-IF
           MOVE ZERO TO CNT.
       M-65.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-70
           END-IF
           IF  HTIW-GNO = 2 OR 6
               IF  CNT > 9
                   GO TO M-65
               END-IF
           END-IF
           IF  HTIW-GNO = 3 OR 7
               IF  CNT > 8
                   GO TO M-65
               END-IF
           END-IF
           IF  HTIW-GNO < 5
               COMPUTE HTIW-SU(CNT) = HTIW-SU(CNT) * HI-ISU
           END-IF
           MOVE HTIW-SU(CNT) TO P-SU(CNT).
           IF  HTIW-GNO = 1
               ADD HTIW-SU(CNT) TO W-ASU1(CNT) W-SU1(CNT) W-SST
           END-IF
           IF  HTIW-GNO = 2
               ADD HTIW-SU(CNT) TO W-ASU2(CNT) W-SU2(CNT) W-SST
           END-IF
           IF  HTIW-GNO = 3
               ADD HTIW-SU(CNT) TO W-ASU3(CNT) W-SU3(CNT) W-SST
           END-IF
           IF  HTIW-GNO = 4
               ADD HTIW-SU(CNT) TO W-ASU4(CNT) W-SU4(CNT) W-SST
           END-IF
           IF  HTIW-GNO = 5
               ADD HTIW-SU(CNT) TO W-ASU1(CNT) W-SU5(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 6
               ADD HTIW-SU(CNT) TO W-ASU2(CNT) W-SU6(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 7
               ADD HTIW-SU(CNT) TO W-ASU3(CNT) W-SU7(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 8
               ADD HTIW-SU(CNT) TO W-ASU4(CNT) W-SU8(CNT) W-HST
           END-IF
           GO TO M-65.
       M-70.
      *           READ HTIW-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTIW-M_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HTIW-HCD > W-EHCD
               GO TO M-90
           END-IF
           IF  W-DC NOT = 1
               IF  HTIW-GNO < 5
                   MOVE 1 TO W-TCD
               ELSE
                   MOVE 2 TO W-TCD
               END-IF
           END-IF
           IF  W-DC NOT = 1
               IF  HTIW-DNO NOT = W-DNO
                   GO TO M-75
               END-IF
           END-IF
           IF  W-DC NOT = 1
               IF  W-TC = W-TCD
                   GO TO M-80
               END-IF
           END-IF
           IF  W-TC = 1
               ADD W-SST TO WS-SST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           GO TO M-80.
       M-75.
           IF  W-TC = 1
               ADD W-SST TO WS-SST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-AST = W-SST + W-HST.
           ADD W-AST TO WS-AST.
           MOVE W-AST TO P-AST.
           IF  W-C = 5
               MOVE 9 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 5 TO W-C
           END-IF.
       M-80.
           PERFORM S-30 THRU S-40.
           IF  HTIW-MHCD NOT = W-MHCD
               GO TO M-85
           END-IF
           IF  HTIW-DC NOT = W-DC
               GO TO M-85
           END-IF
           IF  HTIW-HCD NOT = W-HCD
               GO TO M-40
           END-IF
           IF  HTIW-SNO NOT = W-SNO
               GO TO M-45
           END-IF
           IF  HTIW-DNO NOT = W-DNO
               GO TO M-50
           END-IF
           IF  W-TC NOT = W-TCD
               GO TO M-55
           END-IF
           GO TO M-60.
       M-85.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-C = 9
               PERFORM S-45 THRU S-90
           END-IF
           IF  HTIW-MHCD = W-MHCD
               GO TO M-35
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-30.
       M-90.
           IF  W-TC = 1
               ADD W-SST TO WS-SST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-AST = W-SST + W-HST.
           ADD W-AST TO WS-AST.
           MOVE W-AST TO P-AST.
           IF  W-C = 5
               MOVE 9 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 5 TO W-C
           END-IF
           PERFORM S-30 THRU S-40.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-C = 9
               PERFORM S-45 THRU S-90
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1.
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
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NAME P-ZM.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           IF  W-DC NOT = 1
               MOVE "(" TO P-F
               MOVE W-ISU TO P-ISU
               MOVE ")" TO P-R
               IF  W-ISU = ZERO
                   MOVE "　　　　　（入数０）" TO P-ZM
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO S-35
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-25.
           IF  W-DC = 1
               MOVE "○帳簿在庫○" TO P-SM
           ELSE
               MOVE W-SNO TO P-SNO
               MOVE W-DNO1 TO P-DNO1
               MOVE "-" TO P-V
               MOVE W-DNO2 TO P-DNO2
               IF  W-TC = 1
                   MOVE "［ケース］　" TO P-SM
               ELSE
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF.
       S-35.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.
       S-45.
           MOVE ZERO TO W-SC CHK.
       S-50.
           ADD 1 TO W-SC.
           IF  W-SC = 9
               GO TO S-65
           END-IF
           IF  W-ZC(W-SC) = 0
               GO TO S-50
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           IF  CHK1 = 0
               MOVE 5 TO CHK1
               MOVE "　　　　　　［　合計　］" TO P-TM
           END-IF
           MOVE ZERO TO W-SUD.
           IF  W-SC = 1
               MOVE W-SUD1 TO W-SUD
               MOVE 2 TO P-SIZ
               MOVE "［ケース］　" TO P-SM
           END-IF
           IF  W-SC = 2
               MOVE W-SUD2 TO W-SUD
               MOVE 3 TO P-SIZ
               IF  ZERO = W-ZC(1)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 3
               MOVE W-SUD3 TO W-SUD
               MOVE 4 TO P-SIZ
               IF  ZERO = W-ZC(1) AND W-ZC(2)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE W-SUD4 TO W-SUD
               MOVE 1 TO P-SIZ
               IF  ZERO = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 5
               MOVE W-SUD5 TO W-SUD
               MOVE 2 TO P-SIZ
               IF  W-DC = 1
                   MOVE "○帳簿在庫○" TO P-SM
               ELSE
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 6
               MOVE W-SUD6 TO W-SUD
               MOVE 3 TO P-SIZ
               IF  ZERO = W-ZC(5)
                   IF  W-DC = 1
                       MOVE "○帳簿在庫○" TO P-SM
                   ELSE
                       MOVE "［端　数］　" TO P-SM
                   END-IF
               END-IF
           END-IF
           IF  W-SC = 7
               MOVE W-SUD7 TO W-SUD
               MOVE 4 TO P-SIZ
               IF  ZERO = W-ZC(5) AND W-ZC(6)
                   IF  W-DC = 1
                       MOVE "○帳簿在庫○" TO P-SM
                   ELSE
                       MOVE "［端　数］　" TO P-SM
                   END-IF
               END-IF
           END-IF
           IF  W-SC = 8
               MOVE W-SUD8 TO W-SUD
               MOVE 1 TO P-SIZ
               IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7)
                   IF  W-DC = 1
                       MOVE "○帳簿在庫○" TO P-SM
                   ELSE
                       MOVE "［端　数］　" TO P-SM
                   END-IF
               END-IF
           END-IF
           MOVE ZERO TO CNT.
       S-55.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-60
           END-IF
           IF  W-SC = 2 OR 6
               IF  CNT > 9
                   GO TO S-55
               END-IF
           END-IF
           IF  W-SC = 3 OR 7
               IF  CNT > 8
                   GO TO S-55
               END-IF
           END-IF
           MOVE W-SU(CNT) TO P-SU(CNT).
           GO TO S-55.
       S-60.
           IF  W-SC = 1
               IF  ZERO = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE WS-SST TO P-SST
               END-IF
           END-IF
           IF  W-SC = 2
               IF  ZERO = W-ZC(3) AND W-ZC(4)
                   MOVE WS-SST TO P-SST
               END-IF
           END-IF
           IF  W-SC = 3
               IF  ZERO = W-ZC(4)
                   MOVE WS-SST TO P-SST
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE WS-SST TO P-SST
           END-IF
           IF  W-SC = 5
               IF  ZERO = W-ZC(6) AND W-ZC(7) AND W-ZC(8)
                   MOVE WS-HST TO P-SST
              END-IF
           END-IF
           IF  W-SC = 6
               IF  ZERO = W-ZC(7) AND W-ZC(8)
                   MOVE WS-HST TO P-SST
               END-IF
           END-IF
           IF  W-SC = 7
               IF  ZERO = W-ZC(8)
                   MOVE WS-HST TO P-SST
               END-IF
           END-IF
           IF  W-SC = 8
               MOVE WS-HST TO P-SST
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
               MOVE "　　　　　　［　合計　］" TO P-TM
               IF  W-SC < 5
                   MOVE "［ケース］　" TO P-SM
               ELSE
                   IF  W-DC = 1
                       MOVE "○帳簿在庫○" TO P-SM
                   ELSE
                       MOVE "［端　数］　" TO P-SM
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-50.
       S-65.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-SC CHK.
       S-70.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               GO TO S-90
           END-IF
           IF  W-AZC(W-SC) = 0
               GO TO S-70
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           MOVE ZERO TO W-SUD.
           IF  W-SC = 1
               MOVE W-ASUD1 TO W-SUD
               MOVE 2 TO P-SIZ
               MOVE "【　合計　】" TO P-SM
           END-IF
           IF  W-SC = 2
               MOVE W-ASUD2 TO W-SUD
               MOVE 3 TO P-SIZ
               IF  ZERO = W-AZC(1)
                   MOVE "【　合計　】" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 3
               MOVE W-ASUD3 TO W-SUD
               MOVE 4 TO P-SIZ
               IF  ZERO = W-AZC(1) AND W-AZC(2)
                   MOVE "【　合計　】" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE W-ASUD4 TO W-SUD
               MOVE 1 TO P-SIZ
               IF  ZERO = W-AZC(1) AND W-AZC(2) AND W-AZC(3)
                   MOVE "【　合計　】" TO P-SM
               END-IF
           END-IF
           MOVE ZERO TO CNT.
       S-75.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-80
           END-IF
           IF  W-SC = 2
               IF  CNT > 9
                   GO TO S-75
               END-IF
           END-IF
           IF  W-SC = 3
               IF  CNT > 8
                   GO TO S-75
               END-IF
           END-IF
           MOVE W-SU(CNT) TO P-SU(CNT).
           GO TO S-75.
       S-80.
           IF  W-SC = 1
               IF  ZERO = W-AZC(2) AND W-AZC(3) AND W-AZC(4)
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 2
               IF  ZERO = W-AZC(3) AND W-AZC(4)
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 3
               IF  ZERO = W-AZC(4)
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE WS-AST TO P-AST
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
               MOVE "　　　　　　［　合計　］" TO P-TM
               IF  W-SC < 5
                   MOVE "［ケース］　" TO P-SM
               ELSE
                   IF  W-DC = 1
                       MOVE "○帳簿在庫○" TO P-SM
                   ELSE
                       MOVE "【　合計　】" TO P-SM
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-70.
       S-90.
           EXIT.
