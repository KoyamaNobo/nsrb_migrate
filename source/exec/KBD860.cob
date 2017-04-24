       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD860.
      *********************************************************
      *    PROGRAM         :  品名別発注入庫残明細表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(006) VALUE "【   '".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(001) VALUE "月".
           02  H-PEY          PIC  Z(002).
           02  F              PIC  X(012) VALUE "日 現在   】".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　品名別　発注入庫残明細表　　＊＊＊".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(120) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "納　期　".
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "SS".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "M".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "L".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "LL".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "XL".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(003) VALUE "XXL".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(063) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(063) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(063) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ロット№".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注日　".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "仕　入　先　名　".
           02  F              PIC  X(003) VALUE SPACE.
       01  W-P1.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(093).
       01  W-P2.
           02  P-20K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-TM           PIC  N(005).
           02  P-MD    REDEFINES P-TM.
             03  F            PIC  X(005).
             03  P-NDD        PIC 99/99.
           02  F              PIC  X(001).
           02  P-15K          PIC  X(005).
           02  P-SIZ          PIC  9(001).
           02  P-ASU   OCCURS  10.
             03  P-SU         PIC --,---.
           02  P-GSU          PIC ---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-RSN          PIC  9(002).
           02  P-V1           PIC  X(001).
           02  P-RNG          PIC  9(004).
           02  P-V2           PIC  X(001).
           02  P-RND          PIC  9(002).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99.
           02  F              PIC  X(001).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(010).
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-SCD          PIC  9(004).
           02  W-PC           PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-GSU          PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-ATSUD.
             03  W-ATSU  OCCURS   4.
               04  W-TSUD  OCCURS  10.
                 05  W-TSU    PIC S9(004).
           02  W-SGSU         PIC S9(006).
           02  W-TKIN         PIC S9(008).
           02  W-AKIN         PIC S9(008).
           02  W-AZCD.
             03  W-ZCD   OCCURS   4.
               04  W-ZC       PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-CC           PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHIM.
           COPY LSPF.
      *FD  HSHF
       01  HSHF_KBD860.
           02  HSHF_PNAME1      PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  HSHF_LNAME       PIC  X(011) VALUE "HSHF_KBD860".
           02  F                PIC  X(001).
           02  HSHF_KEY1        PIC  X(100) VALUE SPACE.
           02  HSHF_SORT        PIC  X(100) VALUE SPACE.
           02  HSHF_IDLST       PIC  X(100) VALUE SPACE.
           02  HSHF_RES         USAGE  POINTER.
       01  HSH-R.
           02  HSH-SCD          PIC  9(004).
           02  HSH-HCD          PIC  9(006).
           02  HSH-KEY          PIC  X(008).
           02  HSH-RNO   REDEFINES HSH-KEY.
             03  HSH-RSN        PIC  9(002).
             03  HSH-RNG        PIC  9(004).
             03  HSH-RND        PIC  9(002).
           02  HSH-HDD          PIC  9(008).
           02  HSH-HDDL  REDEFINES HSH-HDD.
             03  F              PIC  9(004).
             03  HSH-HGP        PIC  9(004).
           02  HSH-AHSUD.
             03  HSH-HSUD  OCCURS   4.
               04  HSH-AHSU  OCCURS  10.
                 05  HSH-HSU    PIC S9(004).
           02  HSH-T            PIC  9(005).
           02  HSH-NDD          PIC  9(008).
           02  HSH-NDDL  REDEFINES HSH-NDD.
             03  F              PIC  9(004).
             03  HSH-NGP        PIC  9(004).
           02  HSH-ENGP         PIC  9(006).
           02  F                PIC  X(045).
           02  HSH-NEN          PIC  9(002).
           02  HSH-GET          PIC  9(002).
           02  HSH-PEY          PIC  9(002).
       77  F                    PIC  X(001).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　品名別発注入庫残明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
           COPY LIBSCR.
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "12" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "12" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "12" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "12" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "12" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "12" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "12" "42" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO HSHF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSHF_PNAME1 " " BY REFERENCE HSHF_IDLST "0".
       M-10.
      *           READ HSHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSHF_PNAME1 BY REFERENCE HSH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSHF_IDLST HSHF_PNAME1
               MOVE "***  ＤＡＴＡ　なし  ***      " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM ZAN-RTN THRU ZAN-EX.
           IF  W-AZCD = ZERO
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           MOVE HSH-NEN TO H-NEN.
           MOVE HSH-GET TO H-GET.
           MOVE HSH-PEY TO H-PEY.
           PERFORM MID-020 THRU MID-EX.
       M-15.
           MOVE ZERO TO W-PC W-CC W-ATSUD W-SGSU W-TKIN.
           MOVE HSH-HCD TO W-HCD.
           MOVE HSH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF
           PERFORM PRI1-RTN THRU PRI1-EX.
       M-25.
           PERFORM ZAN-RTN THRU ZAN-EX.
           PERFORM PRI2-RTN THRU PRI2-EX.
       M-30.
      *           READ HSHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSHF_PNAME1 BY REFERENCE HSH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           PERFORM ZAN-RTN THRU ZAN-EX.
           IF  W-AZCD = ZERO
               GO TO M-30
           END-IF
           IF  W-HCD = HSH-HCD
               GO TO M-25
           END-IF.
       M-35.
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-15.
       M-80.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ZAN-RTN.
           MOVE ZERO TO CNT W-GSU W-AZCD.
       ZAN-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO ZAN-EX
           END-IF
           IF  W-ZC(1) = 0
               IF  HSH-HSU(1,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(1)
               END-IF
           END-IF
           IF  W-ZC(2) = 0
               IF  HSH-HSU(2,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(2)
               END-IF
           END-IF
           IF  W-ZC(3) = 0
               IF  HSH-HSU(3,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(3)
               END-IF
           END-IF
           IF  W-ZC(4) = 0
               IF  HSH-HSU(4,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(4)
               END-IF
           END-IF
           COMPUTE W-GSU = W-GSU + HSH-HSU(1,CNT) + HSH-HSU(2,CNT)
                                 + HSH-HSU(3,CNT) + HSH-HSU(4,CNT).
           GO TO ZAN-020.
       ZAN-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
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
       MID-EX.
           EXIT.
       PRI1-RTN.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-HNA.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI1-EX.
           EXIT.
       PRI2-RTN.
           IF  W-CC = 1
               MOVE 2 TO W-CC
           END-IF
           IF  W-CC = 0
               MOVE 1 TO W-CC
           END-IF
           MOVE HSH-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "＊＊＊　仕入先　なし　　＊＊＊" TO S-NAME
           END-IF
           ADD W-GSU TO W-SGSU.
           COMPUTE W-KIN = W-GSU * HSH-T.
           ADD W-KIN TO W-TKIN W-AKIN.
           MOVE 0 TO W-S W-SC W-EC.
       PRI2-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO PRI2-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO PRI2-020
           END-IF
      *
           IF  W-S = 1
               MOVE 1 TO W-SC
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(1)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(1) AND W-ZC(2)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 4
               IF  0 = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-SNA.
           IF  W-SC = 1
               MOVE HSH-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               MOVE HSH-RSN TO P-RSN
               MOVE HSH-RNG TO P-RNG
               MOVE HSH-RND TO P-RND
               MOVE "-" TO P-V1 P-V2
               MOVE HSH-HGP TO P-DATE
               MOVE HSH-NGP TO P-NDD
           END-IF
           IF  W-EC = 1
               MOVE W-GSU TO P-GSU
               MOVE HSH-T TO P-T
               MOVE W-KIN TO P-KIN
           END-IF
           MOVE W-S TO P-SIZ.
      *
           MOVE ZERO TO CNT.
       PRI2-040.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE HSH-HSU(W-S,CNT) TO P-SU(CNT)
               ADD HSH-HSU(W-S,CNT) TO W-TSU(W-S,CNT)
               GO TO PRI2-040
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM PRI1-RTN THRU PRI1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI2-020.
       PRI2-EX.
           EXIT.
       SUB-RTN.
           IF  W-CC < 2
               GO TO SUB-EX
           END-IF
           MOVE ZERO TO W-AZCD W-S.
       SUB-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SUB-060
           END-IF
           MOVE ZERO TO CNT.
       SUB-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SUB-020
           END-IF
           IF  W-TSU(W-S,CNT) NOT = ZERO
               MOVE 1 TO W-ZC(W-S)
               GO TO SUB-020
           END-IF
           GO TO SUB-040.
       SUB-060.
           MOVE 0 TO W-S.
       SUB-080.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SUB-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO SUB-080
           END-IF
      *
           MOVE 0 TO W-SC W-EC.
           IF  W-S = 1
               MOVE 1 TO W-SC
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(1)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(1) AND W-ZC(2)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 4
               IF  0 = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-SNA.
           IF  W-SC = 1
               MOVE "　［小計］" TO P-TM
           END-IF
           IF  W-EC = 1
               MOVE W-SGSU TO P-GSU
               MOVE W-TKIN TO P-KIN
           END-IF
           MOVE W-S TO P-SIZ.
      *
           MOVE ZERO TO CNT.
       SUB-100.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE W-TSU(W-S,CNT) TO P-SU(CNT)
               GO TO SUB-100
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM PRI1-RTN THRU PRI1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO SUB-080.
       SUB-EX.
           EXIT.
       ALL-RTN.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-SNA.
           MOVE "【合　計】" TO P-TM.
           MOVE W-AKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ALL-EX.
           EXIT.
