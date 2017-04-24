       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD250.
      ******************************************
      *****     異動データ更新・クリア     *****
      ******************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-SKB          PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-FILE         PIC  X(013).
           02  W-NGD          PIC  9(006).
           02  W-NGDD  REDEFINES W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-GETD       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIBANK.
           COPY LIUKET.
           COPY LITM.
      *FD  TID-M
       01  TID-M_TSD250.
           02  TID-M_PNAME1   PIC  X(004) VALUE "TIDM".
           02  F              PIC  X(001).
           02  TID-M_LNAME    PIC  X(012) VALUE "TID-M_TSD250".
           02  F              PIC  X(001).
           02  TID-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TID-M_SORT     PIC  X(100) VALUE SPACE.
           02  TID-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TID-M_RES      USAGE  POINTER.
       01  TID-R.
           02  TI-KEY.
             03  TI-NO        PIC  X(004).
           02  TI-SKB         PIC  9(002).
           02  TI-IDO         PIC  9(006).
           02  TI-IDOD  REDEFINES TI-IDO.
             03  TI-NEN       PIC  9(002).
             03  TI-GET       PIC  9(002).
             03  TI-PEY       PIC  9(002).
           02  TI-FRI         PIC  9(006).
           02  TI-FIK         PIC  9(006).
           02  TI-YBK         PIC  9(004).
           02  TI-SNI         PIC  9(004).
           02  F              PIC  X(009).
           02  TI-PC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TYB-F
       01  TYB-F_TSD250.
           02  TYB-F_PNAME1   PIC  X(004) VALUE "TYBF".
           02  F              PIC  X(001).
           02  TYB-F_LNAME    PIC  X(012) VALUE "TYB-F_TSD250".
           02  F              PIC  X(001).
           02  TYB-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT     PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TYB-F_RES      USAGE  POINTER.
       01  TYB-R.
           02  TY-YBK         PIC  9(004).
           02  TY-IDO         PIC  9(006).
           02  TY-MAN         PIC  9(006).
           02  TY-TCD         PIC  9(004).
           02  TY-KBN         PIC  9(002).
           02  TY-NO          PIC  9(004).
           02  TY-KIN         PIC  9(010).
           02  TY-FUC         PIC  9(001).
           02  F              PIC  X(006).
           02  TY-SNI         PIC  9(004).
           02  TY-SNM         PIC  9(004).
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　異動データ　更新　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  UKETM ﾅｼ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  UKETM REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME4   PIC  X(027) VALUE
                  "***  BANKM REWRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(024) VALUE
                  "***  TYBF WRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME10  PIC  X(024) VALUE
                  "***  TM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(025) VALUE
                  "***  TIDM DELETE ｴﾗｰ  ***".
             03  E-ME20  PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張し、ＦＮＣ＋再開".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-NO    PIC  9(004).
             03  E-KEY   PIC  9(004).
             03  E-TCD   PIC  9(004).
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "274" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "36" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "22" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "358" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "358" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "27" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "15" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "24" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "25" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "17" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NO" "9" "24" "47" "4" "E-STAT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NO" BY REFERENCE TI-NO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "40" "4" "E-NO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TI-YBK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "40" "4" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE T-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TID-M_PNAME1 " " BY REFERENCE TID-M_IDLST "1"
            "TI-KEY" BY REFERENCE TI-KEY.
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           COPY LIBCPR.
           MOVE 0 TO W-DC.
       M-15.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
      *           READ TID-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TID-M_PNAME1 BY REFERENCE TID-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  TI-PC NOT = 1
               GO TO M-15
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
      ******************     異　動　処　理     ************************
           MOVE TI-NO TO UT-KEY.
      *           READ UKET-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE UT-SKC TO W-SKB.
           MOVE TI-SKB TO UT-SKC.
           IF  TI-SKB = 70
               MOVE ZERO TO UT-SKC
           END-IF
           MOVE TI-IDO TO UT-IDD.
           MOVE TI-SNI TO UT-SNI.
           MOVE TI-FRI TO UT-FDD.
           MOVE TI-FIK TO UT-HKD.
           IF  TI-YBK NOT = ZERO
               MOVE TI-YBK TO UT-SBC
           END-IF
           IF  TI-SKB = 32
               GO TO M-35
           END-IF
           IF  TI-SKB NOT = 60 AND 70
               GO TO M-55
           END-IF
           IF  W-SKB NOT = 32
               GO TO M-55
           END-IF
      **********     割手買い戻し  (銀行･割引ﾌｧｲﾙ 更新)     ************
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           SUBTRACT UT-KIN FROM B-YBZ.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-25.
           MOVE ZERO TO TYB-R.
           MOVE UT-SBC TO TY-YBK.
           MOVE TI-IDO TO TY-IDO.
           MOVE TI-SNI TO TY-SNI.
           MOVE TI-NO  TO TY-NO.
           MOVE 999999 TO TY-MAN.
           MOVE 9999 TO TY-SNM.
           MOVE UT-TCD TO TY-TCD.
           MOVE UT-TSC TO TY-KBN.
           MOVE UT-KIN TO TY-KIN.
           IF  TI-SKB = 70
               MOVE 1 TO TY-FUC
           END-IF
           IF  TI-SKB = 60
               MOVE 9 TO TY-FUC
           END-IF
      *           WRITE TYB-R.
      *//////////////
           CALL "DB_Insert" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-55
           END-IF
           MOVE 1 TO W-EC.
           GO TO M-45.
      ******************     割　引　処　理     ************************
       M-35.
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ADD UT-KIN TO B-YBZ.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-40.
           MOVE ZERO TO TYB-R.
           MOVE UT-SBC TO TY-YBK.
           MOVE TI-IDO TO TY-IDO.
           MOVE TI-SNI TO TY-SNI.
           MOVE TI-NO  TO TY-NO.
           MOVE UT-MKD TO TY-MAN.
           MOVE UT-SNM TO TY-SNM.
           MOVE UT-TCD TO TY-TCD.
           MOVE UT-TSC TO TY-KBN.
           MOVE UT-KIN TO TY-KIN.
      *           WRITE TYB-R.
      *//////////////
           CALL "DB_Insert" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-55
           END-IF
           MOVE 2 TO W-EC.
       M-45.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-NO" E-NO "p" RETURNING RESU.
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
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           MOVE "TYBF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           IF  W-EC = 1
               GO TO M-25
           END-IF
           GO TO M-40.
      ****************     受取手形マスター　更新     ******************
       M-55.
           IF  TI-SKB = 70
               MOVE ZERO TO UT-SBC
           END-IF
      *           REWRITE UKET-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            UKET-M_PNAME1 UKET-M_LNAME UKET-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           IF  TI-SKB = 60
               PERFORM TM-RTN THRU TM-EX
           END-IF
      *
           IF  TI-YBK = ZERO
               GO TO M-60
           END-IF
           MOVE TI-YBK TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-60
           END-IF
           MOVE TI-SNI TO W-NEND.
           MOVE TI-GET TO W-GETD.
           MOVE ZERO TO W-NG.
           MOVE B-NG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGD NOT > W-NG
               GO TO M-60
           END-IF
           MOVE W-NGD TO W-NG.
           MOVE W-NGS TO B-NG.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-60.
      *           DELETE TID-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TID-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-15.
       M-95.
           IF  W-DC = 0
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TID-M_IDLST TID-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TM-RTN.
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE UT-TCD TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TM-900
           END-IF
           IF  T-ENG NOT = ZERO
               GO TO TM-900
           END-IF
           MOVE D-NTNG TO T-ENG.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       TM-900.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       TM-EX.
           EXIT.
