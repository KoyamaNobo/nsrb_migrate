       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY530.
      *********************************************************
      *    PROGRAM         :  履物都道府県別売上集計表　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/21                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　【　　".
           02  H-SNEN         PIC Z9.
           02  F              PIC  X(001) VALUE "/".
           02  H-SGET         PIC Z9.
           02  F              PIC  X(003) VALUE " - ".
           02  H-ENEN         PIC Z9.
           02  F              PIC  X(001) VALUE "/".
           02  H-EGET         PIC Z9.
           02  F              PIC  N(016) VALUE
                "　履物都道府県別　売上表　　】　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "№    都道府県　　　人口 １人／金額　　　　売上金額".
           02  F              PIC  X(010) VALUE "      率％".
       01  W-P.
           02  F              PIC  X(005).
           02  P-NO           PIC  9(002).
           02  F              PIC  X(004).
           02  P-NAME         PIC  N(004).
           02  P-N            PIC ZZZZZZ,ZZ9.
           02  P-HK           PIC -------9.99.
           02  P-KIN          PIC ----,---,---,--9.
           02  P-RIT          PIC ------9.99.
       01  WT-D.
           02  WT-N           PIC  9(006).
           02  WT-KIN         PIC S9(010).
       01  W-D.
           02  W-HK           PIC S9(003)V9(02).
           02  W-RIT          PIC S9(003)V9(02).
           02  W-NO           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LSPF.
       01  HKB-F_HMY530.
           02  HKB-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  HKB-F_LNAME   PIC  X(012)  VALUE "HKB-F_HMY530".
           02  F             PIC  X(001).
           02  HKB-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  HKB-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  HKB-F_SORT    PIC  X(100)  VALUE SPACE.
           02  HKB-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  HKB-F_RES     USAGE  POINTER.
       01  HKB-R.
           02  F              PIC  X(002).
           02  FK-KEY         PIC  9(002).
           02  F              PIC  X(003).
           02  FK-NAME        PIC  N(004).
           02  FK-N           PIC  9(005).
           02  FK-KIN         PIC S9(010).
           02  F              PIC  X(034).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　都道府県別売上集計表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～  '  年   月".
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC X(24) VALUE
                  "***  売上金額　無し  ***".
             03  E-ME98    PIC X(5)  VALUE X"1B4A05".
             03  E-ME99    PIC X(5)  VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "306" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "15" "17" "26" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "Z9" "15" "18" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "15" "23" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "Z9" "15" "34" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "15" "39" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "34" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
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
           MOVE DATE-02R TO H-DATE.
           MOVE D-SPNG TO W-SNG.
           MOVE D-EPNG TO W-ENG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
           MOVE W-SNEN TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-ENEN TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKB-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKB-F_PNAME1 " " BY REFERENCE HKB-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WT-D.
       M-15.
      *           READ HKB-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKB-F_PNAME1 BY REFERENCE HKB-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF.
           IF  FK-KEY = 99
               GO TO M-15
           END-IF.
           ADD FK-N TO WT-N.
           ADD FK-KIN TO WT-KIN.
           GO TO M-15.
       M-20.
           IF  WT-KIN = ZERO
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-NO
           CALL "DB_F_Close" USING
            BY REFERENCE HKB-F_IDLST HKB-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKB-F_PNAME1 " " BY REFERENCE HKB-F_IDLST "0".
       M-25.
      *           READ HKB-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKB-F_PNAME1 BY REFERENCE HKB-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  FK-KEY = 99
               GO TO M-25
           END-IF.
           ADD 1 TO W-NO.
           MOVE SPACE TO SP-R W-P.
           MOVE W-NO TO P-NO.
           MOVE FK-NAME TO P-NAME.
           MOVE FK-N TO P-N.
           MOVE FK-KIN TO P-KIN.
           MOVE ZERO TO W-HK.
           IF  FK-KIN NOT = ZERO
               COMPUTE W-HK ROUNDED = (FK-KIN / FK-N) / 1000
           END-IF.
           MOVE W-HK TO P-HK.
           MOVE ZERO TO W-RIT.
           IF  FK-KIN NOT = ZERO
               COMPUTE W-RIT ROUNDED = (FK-KIN * 100) / WT-KIN
           END-IF.
           MOVE W-RIT TO P-RIT.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-25.
       M-90.
           MOVE SPACE TO SP-R W-P.
           MOVE "［合計］" TO P-NAME.
           MOVE WT-N TO P-N.
           MOVE WT-KIN TO P-KIN.
           COMPUTE W-HK ROUNDED = (WT-KIN / WT-N) / 1000.
           MOVE W-HK TO P-HK.
           MOVE 100 TO P-RIT.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE HKB-F_IDLST HKB-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
