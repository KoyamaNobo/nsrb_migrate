       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBC010.
      **************************************************************
      *****     総合振込　送信データクリア・振込データ累積     *****
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE       SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-SNO          PIC  9(003).
           02  W-BKN          PIC  N(004).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-R.
             03  W-DC         PIC  9(001).
             03  F            PIC  X(053).
             03  W-FGP.
               04  W-FGET     PIC  9(002).
               04  W-FPEY     PIC  9(002).
             03  W-BKC        PIC  9(004).
             03  F            PIC  X(058).
           COPY LSTAT.
      *
      *FD  SSOUGO-F
       01  SSOUGO-F_FBC010.
           02  SSOUGO-F_PNAME1 PIC  X(007) VALUE "SSOUGOF".
           02  F               PIC  X(001).
           02  SSOUGO-F_LNAME  PIC  X(015) VALUE "SSOUGO-F_FBC010".
           02  F               PIC  X(001).
           02  SSOUGO-F_KEY1   PIC  X(100) VALUE SPACE.
           02  SSOUGO-F_SORT   PIC  X(100) VALUE SPACE.
           02  SSOUGO-F_IDLST  PIC  X(100) VALUE SPACE.
           02  SSOUGO-F_RES    USAGE  POINTER.
       01  SSOUGO-R            PIC  X(120).
       77  F                   PIC  X(001).
      *FD  SSOUGOYR
       01  SSOUGOYR_FBC010.
           02  SSOUGOYR_PNAME1 PIC  X(008) VALUE "SSOUGOYR".
           02  F               PIC  X(001).
           02  SSOUGOYR_LNAME  PIC  X(015) VALUE "SSOUGOYR_FBC010".
           02  F               PIC  X(001).
           02  SSOUGOYR_KEY1   PIC  X(100) VALUE SPACE.
           02  SSOUGOYR_SORT   PIC  X(100) VALUE SPACE.
           02  SSOUGOYR_IDLST  PIC  X(100) VALUE SPACE.
           02  SSOUGOYR_RES    USAGE  POINTER.
       01  SSOUGOY-R.
           02  F               PIC  X(111).
           02  SSOUGOY-SNO     PIC  9(003).
           02  SSOUGOY-NGP     PIC  9(006).
       77  F                   PIC  X(001).
      *FD  FKSM
       01  FKSM_FBC010.
           02  FKSM_PNAME1     PIC  X(004) VALUE "FKSM".
           02  F               PIC  X(001).
           02  FKSM_LNAME      PIC  X(011) VALUE "FKSM_FBC010".
           02  F               PIC  X(001).
           02  FKSM_KEY1       PIC  X(100) VALUE SPACE.
           02  FKSM_SORT       PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST      PIC  X(100) VALUE SPACE.
           02  FKSM_RES        USAGE  POINTER.
       01  FKS-R.
           02  FS-KEY          PIC  X(004).
           02  FS-FKC          PIC  9(001).
           02  FS-FKN1         PIC  X(030).
           02  FS-BKC1         PIC  9(007).
           02  FS-YKS1         PIC  9(001).
           02  FS-KNO1         PIC  9(007).
           02  FS-TRC1         PIC  9(001).
           02  FS-KIN1         PIC  9(009).
           02  FS-FKN2         PIC  X(030).
           02  FS-BKC2         PIC  9(007).
           02  FS-YKS2         PIC  9(001).
           02  FS-KNO2         PIC  9(007).
           02  FS-TRC2         PIC  9(001).
           02  FS-KIN2         PIC  9(009).
           02  FS-BKC          PIC  9(001).
           02  FS-FGP          PIC  9(004).
           02  F               PIC  X(002).
           02  FS-ENGP         PIC  9(006).
       77  F                   PIC  X(001).
      *FD  FKSMYR
       01  FKSMYR_FBC010.
           02  FKSMYR_PNAME1   PIC  X(006) VALUE "FKSMYR".
           02  F               PIC  X(001).
           02  FKSMYR_LNAME    PIC  X(013) VALUE "FKSMYR_FBC010".
           02  F               PIC  X(001).
           02  FKSMYR_KEY1     PIC  X(100) VALUE SPACE.
           02  FKSMYR_SORT     PIC  X(100) VALUE SPACE.
           02  FKSMYR_IDLST    PIC  X(100) VALUE SPACE.
           02  FKSMYR_RES      USAGE  POINTER.
       01  FKSMY-R             PIC  X(128).
       77  F                   PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　総合振込　送信データクリア他　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(008) VALUE
                "【　　　　　　】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-BKN   PIC  N(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(009) VALUE
                  "＜　キャンセル　＞".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  BANK ｴﾗｰ  ***".
             03  E-ME5   PIC  X(028) VALUE
                  "***  SSOUGOYR WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  FKSMYR WRITE ｴﾗｰ  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "374" " " " " RETURNING RESU.
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
            "08C-MID" "N" "14" "26" "16" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "23" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKN" "N" "14" "30" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BKN" BY REFERENCE W-BKN "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "125" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "125" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "28" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               STOP RUN
           END-IF
           IF  JS-SIGN = 0
               MOVE "中国銀行" TO W-BKN
           END-IF
           IF  JS-SIGN = 1
               MOVE "商工中金" TO W-BKN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" SSOUGO-F_PNAME1 " " BY REFERENCE SSOUGO-F_IDLST "0".
      *           READ SSOUGO-F NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SSOUGO-F_PNAME1 BY REFERENCE SSOUGO-R
            " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SSOUGO-F_IDLST SSOUGO-F_PNAME1
               GO TO M-95
           END-IF
           MOVE SSOUGO-R TO W-R.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGO-F_IDLST SSOUGO-F_PNAME1.
           IF  W-DC NOT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  JS-SIGN = 0
               IF  W-BKC NOT = 0168
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-BKC NOT = 2004
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
       M-15.
           ACCEPT W-DATE FROM DATE.
           PERFORM S-05 THRU S-20.
           CALL "DB_F_Open" USING
            "INPUT" SSOUGO-F_PNAME1 " " BY REFERENCE SSOUGO-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" SSOUGOYR_PNAME1 " " BY REFERENCE
            SSOUGOYR_IDLST "0".
       M-20.
      *           READ SSOUGO-F NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SSOUGO-F_PNAME1 BY REFERENCE SSOUGO-R
            " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
       M-25.
           MOVE SPACE TO SSOUGOY-R.
           MOVE SSOUGO-R TO SSOUGOY-R.
           MOVE W-SNO TO SSOUGOY-SNO.
           MOVE W-DATE TO SSOUGOY-NGP.
      *           WRITE SSOUGOY-R.
      *//////////////
           CALL "DB_Insert" USING
            SSOUGOYR_PNAME1 SSOUGOYR_LNAME SSOUGOY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SSOUGO-F_IDLST SSOUGO-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SSOUGOYR_IDLST SSOUGOYR_PNAME1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGOYR_IDLST SSOUGOYR_PNAME1.
           MOVE "SSOUGOYR     " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SSOUGOYR_PNAME1 " " BY REFERENCE 
            SSOUGOYR_IDLST "0".
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGO-F_IDLST SSOUGO-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGOYR_IDLST SSOUGOYR_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" FKSM_PNAME1 " " BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" FKSMYR_PNAME1 " " BY REFERENCE FKSMYR_IDLST "0".
       M-35.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  ZERO = FS-KIN1 AND FS-KIN2
               GO TO M-35
           END-IF.
       M-40.
           MOVE SPACE TO FKSMY-R.
           MOVE FKS-R TO FKSMY-R.
      *           WRITE FKSMY-R.
      *//////////////
           CALL "DB_Insert" USING
            FKSMYR_PNAME1 FKSMYR_LNAME FKSMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE FKSMYR_IDLST FKSMYR_PNAME1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE FKSMYR_IDLST FKSMYR_PNAME1.
           MOVE "FKSMYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" FKSMYR_PNAME1 " " BY REFERENCE FKSMYR_IDLST "0".
           GO TO M-40.
       M-45.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FKSMYR_IDLST FKSMYR_PNAME1.
       M-90.
           CALL "DB_F_Open" USING
            "OUTPUT" SSOUGO-F_PNAME1 " " BY REFERENCE 
            SSOUGO-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGO-F_IDLST SSOUGO-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Open" USING
            "INPUT" SSOUGOYR_PNAME1 " " BY REFERENCE SSOUGOYR_IDLST "0".
           MOVE 999 TO W-SNO.
       S-10.
      *           READ SSOUGOYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSOUGOYR_PNAME1 BY REFERENCE SSOUGOY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  SSOUGOY-NGP NOT = W-DATE
               GO TO S-10
           END-IF
           IF  SSOUGOY-SNO NOT = W-SNO
               MOVE SSOUGOY-SNO TO W-SNO
           END-IF
           GO TO S-10.
       S-15.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGOYR_IDLST SSOUGOYR_PNAME1.
           IF  W-SNO = 999
               MOVE ZERO TO W-SNO
           ELSE
               ADD 1 TO W-SNO
           END-IF.
       S-20.
           EXIT.
