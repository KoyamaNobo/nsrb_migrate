       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG350.
      *********************************************************
      *    PROGRAM         :  入金　明細表　　                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　入　金　明　細　表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(051) VALUE
                " 日　付     現金・預金       手　形     買掛相殺   ".
           02  F              PIC  X(035) VALUE
                "その他相殺       合　計    内消費税".
           02  F              PIC  X(028) VALUE
                "  :     品代戻し    内消費税".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  P-TM    REDEFINES P-DATE  PIC  N(004).
           02  F              PIC  X(001).
           02  P-GYK          PIC -----,---,--9.
           02  P-GYKD  REDEFINES P-GYK.
             03  F            PIC  X(006).
             03  P-GYKR       PIC ---9.9.
             03  P-P1         PIC  X(001).
           02  P-TGT          PIC -----,---,--9.
           02  P-TGTD  REDEFINES P-TGT.
             03  F            PIC  X(006).
             03  P-TGTR       PIC ---9.9.
             03  P-P2         PIC  X(001).
           02  P-KKS          PIC -----,---,--9.
           02  P-KKSD  REDEFINES P-KKS.
             03  F            PIC  X(006).
             03  P-KKSR       PIC ---9.9.
             03  P-P3         PIC  X(001).
           02  P-STS          PIC -----,---,--9.
           02  P-STSD  REDEFINES P-STS.
             03  F            PIC  X(006).
             03  P-STSR       PIC ---9.9.
             03  P-P4         PIC  X(001).
           02  P-KEI          PIC -----,---,--9.
           02  P-KEID  REDEFINES P-KEI.
             03  F            PIC  X(006).
             03  P-KEIR       PIC ---9.9.
             03  P-P5         PIC  X(001).
           02  P-SHZ          PIC ----,---,--9.
           02  P-SHZD  REDEFINES P-SHZ.
             03  F            PIC  X(005).
             03  P-SHZR       PIC ---9.9.
             03  P-P6         PIC  X(001).
           02  F              PIC  X(002).
           02  P-V            PIC  X(001).
           02  P-MDS          PIC -----,---,--9.
           02  P-MDZ          PIC ----,---,--9.
       01  W-D.
           02  W-DATE         PIC  9(006).
           02  W-GYK          PIC S9(009).
           02  W-TGT          PIC S9(009).
           02  W-KKS          PIC S9(009).
           02  W-STS          PIC S9(009).
           02  W-KEI          PIC S9(009).
           02  W-SHZ          PIC S9(009).
           02  W-MDS          PIC S9(009).
           02  W-MDZ          PIC S9(009).
       01  WT-D.
           02  WT-GYK         PIC S9(009).
           02  WT-TGT         PIC S9(009).
           02  WT-KKS         PIC S9(009).
           02  WT-STS         PIC S9(009).
           02  WT-KEI         PIC S9(009).
           02  WT-SHZ         PIC S9(009).
           02  WT-MDS         PIC S9(009).
           02  WT-MDZ         PIC S9(009).
       01  W-RT               PIC S9(003)V9(01).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LSPF.
      *FD  NYUR-F
       01  NYUR-F_HKG350.
           02  NYUR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUR-F_LNAME   PIC  X(013) VALUE "NYUR-F_HKG350".
           02  F              PIC  X(001).
           02  NYUR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUR-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUR-F_RES     USAGE  POINTER.
       01  WNYUR-R.
           02  F              PIC  9(002).
           02  NR-DATE        PIC  9(006).
           02  NR-TCD         PIC  9(004).
           02  NR-KI          PIC S9(008).
           02  NR-NK.
             03  NR-NK1       PIC  9(001).
             03  NR-NK2       PIC  9(001).
           02  F              PIC  X(106).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　入　金　明　細　表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
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
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE DATE-02R TO H-DATE.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE WNYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
      *
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE ZERO TO WT-D.
       M-10.
           MOVE ZERO TO W-D.
           MOVE NR-DATE TO W-DATE.
       M-15.
           IF  NR-NK1 = 0 OR 1 OR 2
               ADD NR-KI TO W-GYK W-KEI
           END-IF
           IF  NR-NK1 = 3 OR 4
               ADD NR-KI TO W-TGT W-KEI
           END-IF
           IF  NR-NK1 = 5
               ADD NR-KI TO W-KKS W-KEI
           END-IF
           IF  NR-NK1 = 6
               ADD NR-KI TO W-STS W-KEI
           END-IF
           IF  NR-NK1 = 7
               ADD NR-KI TO W-MDS
           END-IF
           IF  NR-NK1 = 9
               ADD NR-KI TO W-KKS W-KEI
           END-IF
           IF  NR-NK2 = 8
               ADD NR-KI TO W-SHZ
           END-IF
           IF  NR-NK2 = 9
               ADD NR-KI TO W-MDZ
           END-IF.
       M-20.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE WNYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-DATE = NR-DATE
               GO TO M-15
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-10.
       M-90.
           PERFORM S-05 THRU S-10.
           MOVE SPACE TO W-P.
           MOVE ":" TO P-V.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE "　合　計" TO P-TM.
           MOVE WT-GYK TO P-GYK.
           MOVE WT-TGT TO P-TGT.
           MOVE WT-KKS TO P-KKS.
           MOVE WT-STS TO P-STS.
           MOVE WT-KEI TO P-KEI.
           MOVE WT-SHZ TO P-SHZ.
           MOVE WT-MDS TO P-MDS.
           MOVE WT-MDZ TO P-MDZ.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE SPACE TO W-P.
           IF  WT-KEI = ZERO
               GO TO M-95
           END-IF
           MOVE "%" TO P-P1 P-P2 P-P3 P-P4 P-P5 P-P6.
           MOVE ZERO TO W-RT.
           IF  WT-GYK NOT = ZERO
               COMPUTE W-RT ROUNDED = (WT-GYK * 100) / WT-KEI
           END-IF
           MOVE W-RT TO P-GYKR.
           MOVE ZERO TO W-RT.
           IF  WT-TGT NOT = ZERO
               COMPUTE W-RT ROUNDED = (WT-TGT * 100) / WT-KEI
           END-IF
           MOVE W-RT TO P-TGTR.
           MOVE ZERO TO W-RT.
           IF  WT-KKS NOT = ZERO
               COMPUTE W-RT ROUNDED = (WT-KKS * 100) / WT-KEI
           END-IF
           MOVE W-RT TO P-KKSR.
           MOVE ZERO TO W-RT.
           IF  WT-STS NOT = ZERO
               COMPUTE W-RT ROUNDED = (WT-STS * 100) / WT-KEI
           END-IF
           MOVE W-RT TO P-STSR.
           MOVE 100 TO P-KEIR.
           MOVE ZERO TO W-RT.
           IF  WT-SHZ NOT = ZERO
               COMPUTE W-RT ROUNDED = (WT-SHZ * 100) / WT-KEI
           END-IF
           MOVE W-RT TO P-SHZR.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO W-P.
           MOVE W-DATE TO P-DATE.
           MOVE W-GYK TO P-GYK.
           MOVE W-TGT TO P-TGT.
           MOVE W-KKS TO P-KKS.
           MOVE W-STS TO P-STS.
           MOVE W-KEI TO P-KEI.
           MOVE W-SHZ TO P-SHZ.
           MOVE ":" TO P-V.
           MOVE W-MDS TO P-MDS.
           MOVE W-MDZ TO P-MDZ.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD W-GYK TO WT-GYK.
           ADD W-TGT TO WT-TGT.
           ADD W-KKS TO WT-KKS.
           ADD W-STS TO WT-STS.
           ADD W-KEI TO WT-KEI.
           ADD W-SHZ TO WT-SHZ.
           ADD W-MDS TO WT-MDS.
           ADD W-MDZ TO WT-MDZ.
       S-10.
           EXIT.
