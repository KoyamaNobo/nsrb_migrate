       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK900.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-27.
      ***************************************************************
      *    PROGRAM         :  教育振興会関係　年間累積・クリア      *
      *    PRINTER TYPE    :  JIPS*                                 *
      *    SCREEN          :  ******                                *
      *        変更　　　  :  62/06/16                              *
      *    COMPILE TYPE    :  COBOL                                 *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-YNG          PIC  9(006).
           02  W-YNGD  REDEFINES W-YNG.
             03  W-YN         PIC  9(004).
             03  W-YND   REDEFINES W-YN.
               04  W-YN1      PIC  9(002).
               04  W-YN2      PIC  9(002).
             03  W-YG         PIC  9(002).
           02  W-YNGL  REDEFINES W-YNG.
             03  F            PIC  9(002).
             03  W-YNGS       PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
      *FD  HKS-F
       01  HKS-F_HMK900.
           02  HKS-F_PNAME1   PIC  X(005) VALUE "HKSRF".
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMK900".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  HK-TCD         PIC  9(004).
           02  HK-HCD1        PIC  9(004).
           02  HK-SU          PIC S9(006).
           02  HK-KIN         PIC S9(008).
           02  HK-AC          PIC  9(001).
           02  HK-NC          PIC  9(001).
           02  F              PIC  X(004).
           02  HK-NG          PIC  9(004).
       77  F                  PIC  X(001).
      *FD  HKK-F
       01  HKK-F_HMK900.
           02  HKK-F_PNAME1   PIC  X(004) VALUE "HKKF".
           02  F              PIC  X(001).
           02  HKK-F_LNAME    PIC  X(012) VALUE "HKK-F_HMK900".
           02  F              PIC  X(001).
           02  HKK-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKK-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKK-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKK-F_RES      USAGE  POINTER.
       01  HKK-R              PIC  X(042).
       77  F                  PIC  X(001).
      *FD  HKSRY-F
       01  HKSRY-F_HMK900.
           02  HKSRY-F_PNAME1 PIC  X(006) VALUE "HKSRYF".
           02  F              PIC  X(001).
           02  HKSRY-F_LNAME  PIC  X(014) VALUE "HKSRY-F_HMK900".
           02  F              PIC  X(001).
           02  HKSRY-F_KEY1   PIC  X(100) VALUE SPACE.
           02  HKSRY-F_SORT   PIC  X(100) VALUE SPACE.
           02  HKSRY-F_IDLST  PIC  X(100) VALUE SPACE.
           02  HKSRY-F_RES    USAGE  POINTER.
       01  HKSRY-R.
           02  F              PIC  X(026).
           02  HKSRY-NG       PIC  9(006).
       77  F                  PIC  X(001).
      *FD  HKKY-F
       01  HKKY-F_HMK900.
           02  HKKY-F_PNAME1  PIC  X(005) VALUE "HKKYF".
           02  F              PIC  X(001).
           02  HKKY-F_LNAME   PIC  X(013) VALUE "HKKY-F_HMK900".
           02  F              PIC  X(001).
           02  HKKY-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HKKY-F_SORT    PIC  X(100) VALUE SPACE.
           02  HKKY-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HKKY-F_RES     USAGE  POINTER.
       01  HKKY-R.
           02  F              PIC  X(036).
           02  HKKY-NG        PIC  9(006).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　教育振興会　関係ファイル　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　年間累積・月次クリア　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(020) VALUE
                "［  '  年   月分  ］".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  N(009) VALUE
                  "＜　キャンセル　＞".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME4   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  HKSRYF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  HKKYF WRITE ｴﾗｰ  *** ".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "394" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "N" "10" "10" "44" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "22" "20" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "21" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "38" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "14" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "14" "27" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "9" "14" "32" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "150" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "27" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NING TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HKK-F_PNAME1 " " BY REFERENCE HKK-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" HKKY-F_PNAME1 " " BY REFERENCE HKKY-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" HKSRY-F_PNAME1 " " BY REFERENCE HKSRY-F_IDLST "0".
       M-25.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  HK-NG NOT = W-NG
               GO TO M-25
           END-IF.
       M-30.
           MOVE ZERO TO HKSRY-R.
           MOVE HKS-R TO HKSRY-R.
      *           WRITE HKSRY-R.
      *//////////////
           CALL "DB_Insert" USING
            HKSRY-F_PNAME1 HKSRY-F_LNAME HKSRY-R RETURNING RET.
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
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSRY-F_IDLST HKSRY-F_PNAME1.
           MOVE "HKSRYF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HKSRY-F_PNAME1 " " BY REFERENCE HKSRY-F_IDLST "0".
           GO TO M-30.
       M-35.
           MOVE X"FF" TO HKS-R.
      *           REWRITE HKS-R.
      *///////////////
           CALL "DB_Update" USING
            HKS-F_PNAME1 HKS-F_LNAME HKS-R RETURNING RET.
           GO TO M-25.
       M-40.
      *           READ HKK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKK-F_PNAME1 BY REFERENCE HKK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF.
       M-45.
           MOVE ZERO TO HKKY-R.
           MOVE HKK-R TO HKKY-R.
      *           WRITE HKKY-R.
      *//////////////
           CALL "DB_Insert" USING
            HKKY-F_PNAME1 HKKY-F_LNAME HKKY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HKKY-F_IDLST HKKY-F_PNAME1.
           MOVE "HKKYF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HKKY-F_PNAME1 " " BY REFERENCE HKKY-F_IDLST "0".
           GO TO M-45.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKK-F_IDLST HKK-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSRY-F_IDLST HKSRY-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKKY-F_IDLST HKKY-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NG TO D-NING.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
      *
           IF  W-GET NOT = 7
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
           ELSE
               PERFORM S-05 THRU S-30
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-YNG.
           MOVE W-NG TO W-YNGS.
           IF  W-YN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-YN
           END-IF
           IF  W-YN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-YN
           END-IF
           SUBTRACT 2 FROM W-YN.
           MOVE 6 TO W-YG.
           CALL "DB_F_Open" USING
            "I-O" HKSRY-F_PNAME1 " " BY REFERENCE HKSRY-F_IDLST "0".
       S-10.
      *           READ HKSRY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSRY-F_PNAME1 BY REFERENCE HKSRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF W-YNG < HKSRY-NG
               GO TO S-15
           END-IF
           MOVE X"FF" TO HKSRY-R.
      *           REWRITE HKSRY-R.
      *///////////////
           CALL "DB_Update" USING
            HKSRY-F_PNAME1 HKSRY-F_LNAME HKSRY-R RETURNING RET.
           GO TO S-10.
       S-15.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSRY-F_IDLST HKSRY-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HKKY-F_PNAME1 " " BY REFERENCE HKKY-F_IDLST "0".
       S-20.
      *           READ HKKY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKKY-F_PNAME1 BY REFERENCE HKKY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-25
           END-IF
           IF  W-YNG < HKKY-NG
               GO TO S-25
           END-IF
           MOVE X"FF" TO HKKY-R.
      *           REWRITE HKKY-R.
      *///////////////
           CALL "DB_Update" USING
            HKKY-F_PNAME1 HKKY-F_LNAME HKKY-R RETURNING RET.
           GO TO S-20.
       S-25.
           CALL "DB_F_Close" USING
            BY REFERENCE HKKY-F_IDLST HKKY-F_PNAME1.
       S-30.
           EXIT.
