       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK050.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-27.
      *********************************************************
      *    PROGRAM         :  教育出荷　チェックリスト        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　教育出荷計算　チェックリスト　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(002) VALUE "月分".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(003) VALUE "出荷数".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "売価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "出荷金額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNAME        PIC  N(026).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-HNAME        PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-UKI          PIC -----,---,--9.
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-UT           PIC S9(005).
           02  W-HNAME1.
             03  W-HNA1  OCCURS  24  PIC  N(001).
           02  W-HNAME2.
             03  W-HNA2  OCCURS  24  PIC  N(001).
           02  W-HNAME REDEFINES W-HNAME2 PIC  N(024).
           02  W-C            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  CNT            PIC  9(003).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-UKI         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-UKI         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKSR-F
       01  HKSR-F_HMK050.
           02  HKSR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKSR-F_LNAME   PIC  X(013) VALUE "HKSR-F_HMK050".
           02  F              PIC  X(001).
           02  HKSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HKSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  HKSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HKSR-F_RES     USAGE  POINTER.
       01  HKSR-R.
           02  HK-KEY.
             03  HK-TCD       PIC  9(004).
             03  HK-HCD       PIC  9(004).
           02  HK-SU          PIC S9(006).
           02  HK-UKI         PIC S9(008).
           02  F              PIC  X(006).
           02  HK-NG          PIC  9(004).
           02  F              PIC  X(032).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　教育出荷計算　チェックリスト　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(005) VALUE "［  '".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  X(003) VALUE "年 ".
             03  FILLER  PIC Z9 .
             03  FILLER  PIC  N(004) VALUE "月分　］".
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
            "C-MID" " " "0" "0" "356" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" " " "15" "0" "20" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID" "X" "15" "24" "5" " " "08C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0208C-MID" "9" "15" "29" "2" "0108C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0208C-MID" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0308C-MID" "X" "15" "31" "3" "0208C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0408C-MID" "Z9" "15" "34" "2" "0308C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0408C-MID" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0508C-MID" "N" "15" "36" "8" "0408C-MID" " " RETURNING RESU.
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
           MOVE D-NING TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKSR-F_PNAME1 " " BY REFERENCE HKSR-F_IDLST "0".
       M-10.
      *           READ HKSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HK-NG NOT = W-NG
               GO TO M-10
           END-IF
           IF  ZERO = HK-SU AND HK-UKI
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D CNT.
           MOVE SPACE TO W-P.
           MOVE HK-TCD TO P-TCD W-TCD T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　得意先マスター無し　＊＊　" TO T-NAME
           END-IF
           MOVE T-NAME TO P-TNAME.
       M-20.
           MOVE HK-HCD TO P-HCD.
           MOVE SPACE TO P-HNAME.
           MOVE ZERO TO HI-KEY.
           MOVE HK-HCD TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　品名マスター無し　＊＊　" TO W-HNAME
               GO TO M-35
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　品名マスター無し　＊＊　" TO W-HNAME
               GO TO M-35
           END-IF
           IF  HI-HCD1 NOT = HK-HCD
               MOVE "＊＊　品名マスター無し　＊＊　" TO W-HNAME
               GO TO M-35
           END-IF
           MOVE SPACE TO W-HNAME1 W-HNAME.
           MOVE HI-NAME TO W-HNAME1.
           MOVE ZERO TO CHK W-C.
       M-25.
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO M-35
           END-IF
           IF  CHK NOT = ZERO
               GO TO M-30
           END-IF
           IF  W-HNA1(W-C) = SPACE
               MOVE 5 TO CHK
           ELSE
               MOVE ZERO TO CHK
           END-IF
           MOVE W-HNA1(W-C) TO W-HNA2(W-C).
           GO TO M-25.
       M-30.
           IF  W-HNA1(W-C) NOT = SPACE
               MOVE ZERO TO CHK
               MOVE W-HNA1(W-C) TO W-HNA2(W-C)
               GO TO M-25
           END-IF.
       M-35.
           MOVE W-HNAME TO P-HNAME.
           MOVE ZERO TO W-UT.
           IF  HK-SU NOT = ZERO
               COMPUTE W-UT ROUNDED = HK-UKI / HK-SU
           END-IF
           MOVE HK-SU TO P-SU.
           MOVE W-UT TO P-UT.
           MOVE HK-UKI TO P-UKI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE HK-TCD TO P-TCD
               MOVE T-NAME TO P-TNAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE SPACE TO P-TNAME.
           ADD HK-SU TO WS-SU.
           ADD HK-UKI TO WS-UKI.
           ADD 1 TO CNT.
       M-40.
      *           READ HKSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HK-NG NOT = W-NG
               GO TO M-40
           END-IF
           IF  ZERO = HK-SU AND HK-UKI
               GO TO M-40
           END-IF
           IF  W-TCD = HK-TCD
               GO TO M-20
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-30.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　【　　ＡＬＬ　ＴＯＴＡＬ　　】" TO P-TNAME.
           MOVE SPACE TO P-HNAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-UKI TO P-UKI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1.
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
       S-15.
           EXIT.
       S-20.
           IF  CNT = 1
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE SPACE TO P-TNAME.
           MOVE "　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-HNAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-UKI TO P-UKI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
       S-25.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-UKI TO WA-UKI.
       S-30.
           EXIT.
