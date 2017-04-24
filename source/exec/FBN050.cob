       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBN050.
      **********************************
      *****    振込先　問合せ      *****
      **********************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-L            PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SCD          PIC  9(004).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIFBKM.
      *FD  FKSM
       01  FKSM_FBN050.
           02  FKSM_PNAME1    PIC  X(004) VALUE "FKSM".
           02  F              PIC  X(001).
           02  FKSM_LNAME     PIC  X(011) VALUE "FKSM_FBN050".
           02  F              PIC  X(001).
           02  FKSM_KEY1      PIC  X(100) VALUE SPACE.
           02  FKSM_SORT      PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST     PIC  X(100) VALUE SPACE.
           02  FKSM_RES       USAGE  POINTER.
       01  FKS-R.
           02  FS-KEY         PIC  X(004).
           02  FS-FKC         PIC  9(001).
           02  FS-FKN1        PIC  X(030).
           02  FS-BKC1        PIC  9(007).
           02  FS-YKS1        PIC  9(001).
           02  FS-KNO1        PIC  9(007).
           02  FS-TRC1        PIC  9(001).
           02  FS-KIN1        PIC  9(009).
           02  FS-FKN2        PIC  X(030).
           02  FS-BKC2        PIC  9(007).
           02  FS-YKS2        PIC  9(001).
           02  FS-KNO2        PIC  9(007).
           02  FS-TRC2        PIC  9(001).
           02  FS-KIN2        PIC  9(009).
           02  FS-BKC         PIC  9(001).
           02  FS-FGP         PIC  9(004).
           02  F              PIC  X(002).
           02  FS-ENGP        PIC  9(006).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(017) VALUE
                  "＊＊＊　　振込先　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(010) VALUE
                    "仕　　入　　先　　名".
             03  FILLER  PIC  N(004) VALUE   "最終入力".
           02  FILLER.
             03  FILLER  PIC  N(005) VALUE   "口　座　名".
             03  FILLER  PIC  X(006) VALUE "BKｺｰﾄﾞ".
             03  FILLER  PIC  N(003) VALUE   "銀行名".
             03  FILLER  PIC  N(004) VALUE   "本支店名".
             03  FILLER  PIC  N(003) VALUE   "口座№".
           02  FILLER  PIC  X(037) VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=F10 , 終了=F9       ".
       01  C-ACP.
           02  A-SCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-MID.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(024).
             03  D-ENGP  PIC  99/99/99 .
             03  D-DAT1.
               04  FILLER  PIC  X(030).
               04  FILLER  PIC  9(007).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  9(007).
             03  D-DAT2.
               04  FILLER  PIC  X(030).
               04  FILLER  PIC  9(007).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  9(007).
           02  D-END   PIC  X(037) VALUE
                "              ＥＮＤ　ＤＡＴＡ       ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(022) VALUE
                  "***  ｼｲﾚｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ﾌﾘｺﾐｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "139" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "14" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "3" "0" "32" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "3" "1" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "N" "3" "6" "20" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "N" "3" "73" "8" "0202C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" " " "4" "0" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID" "N" "4" "3" "10" " " "03C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID" "X" "4" "34" "6" "0103C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0303C-MID" "N" "4" "42" "6" "0203C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0403C-MID" "N" "4" "58" "8" "0303C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0503C-MID" "N" "4" "74" "6" "0403C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "30" "37" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "5" "1" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "245" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "208" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" " " "W-L" "0" "52" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID" "9" "W-L" "1" "4" " " "D-MID" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MID" BY REFERENCE S-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID" "N" "W-L" "6" "48" "01D-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MID" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ENGP" "99/99/99" "W-L" "73" "8" "D-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ENGP" BY REFERENCE FS-ENGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DAT1" " " "W-L" "0" "74" "D-ENGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DAT1" "X" "W-L" "3" "30" " " "D-DAT1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DAT1" BY REFERENCE FS-FKN1 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DAT1" "9" "W-L" "34" "7" "01D-DAT1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DAT1" BY REFERENCE FS-BKC1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03D-DAT1" "X" "W-L" "42" "15" "02D-DAT1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DAT1" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "04D-DAT1" "X" "W-L" "58" "15" "03D-DAT1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DAT1" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DAT1" "9" "W-L" "74" "7" "04D-DAT1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DAT1" BY REFERENCE FS-KNO1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DAT2" " " "W-L" "0" "74" "D-DAT1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DAT2" "X" "W-L" "3" "30" " " "D-DAT2" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DAT2" BY REFERENCE FS-FKN2 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DAT2" "9" "W-L" "34" "7" "01D-DAT2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DAT2" BY REFERENCE FS-BKC2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03D-DAT2" "X" "W-L" "42" "15" "02D-DAT2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DAT2" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "04D-DAT2" "X" "W-L" "58" "15" "03D-DAT2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DAT2" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DAT2" "9" "W-L" "74" "7" "04D-DAT2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DAT2" BY REFERENCE FS-KNO2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-END" "X" "23" "30" "37" "01C-DSP" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "107" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE W-SCD TO FS-KEY.
      *           START FKSM KEY NOT < FS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            FKSM_PNAME1 "FS-KEY" " NOT < " FS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-10
           END-IF
      *           READ FKSM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE 0 TO W-EC.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 19
               GO TO M-30
           END-IF
           MOVE FS-KEY TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　仕入先なし　" TO S-NAME
           END-IF
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           IF  FS-ENGP NOT = ZERO
               CALL "SD_Output" USING "D-ENGP" D-ENGP "p" RETURNING RESU
           END-IF
      *
           IF  FS-BKC1 = ZERO
               GO TO M-20
           END-IF
           MOVE FS-BKC1 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO FBK-BKN FBK-HSN
               MOVE "** BK ﾅｼ **" TO FBK-BKN
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-DAT1" D-DAT1 "p" RETURNING RESU.
       M-20.
           IF  FS-BKC2 = ZERO
               GO TO M-25
           END-IF
           MOVE FS-BKC2 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO FBK-BKN FBK-HSN
               MOVE "** BK ﾅｼ **" TO FBK-BKN
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-DAT2" D-DAT2 "p" RETURNING RESU.
       M-25.
      *           READ FKSM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-EC
               CALL "SD_Output" USING "D-END" D-END "p" RETURNING RESU
               GO TO M-30
           END-IF
           GO TO M-15.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF (ESTAT = ADV) OR (W-EC = 1)
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
