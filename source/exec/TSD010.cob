       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD010.
      **************************************************
      *****     受取・割引・支払手形　決済更新     *****
      **************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-DC               PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NGPD.
             03  W-NGD.
               04  W-NEND     PIC  9(002).
               04  W-GETD     PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-MNGP.
             03  W-MNEN       PIC  9(004).
             03  W-MNENL REDEFINES W-MNEN.
               04  W-MNEN1    PIC  9(002).
               04  W-MNEN2    PIC  9(002).
             03  W-MGET       PIC  9(002).
             03  W-MPEY       PIC  9(002).
           02  W-MNGPL  REDEFINES W-MNGP.
             03  F            PIC  9(002).
             03  W-MNGPS      PIC  9(006).
           02  W-SNM.
             03  W-SNM1       PIC  9(002).
             03  W-SNM2       PIC  9(002).
           02  W-FILE         PIC  X(013).
           02  W-DTC          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LIBANK.
           COPY LIUKET.
           COPY LISHIT.
      *FD  TYB-F
       01  TYB-F_TSD010.
           02  TYB-F_PNAME1   PIC  X(004) VALUE "TYBF".
           02  F              PIC  X(001).
           02  TYB-F_LNAME    PIC  X(012) VALUE "TYB-F_TSD010".
           02  F              PIC  X(001).
           02  TYB-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT     PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TYB-F_RES      USAGE  POINTER.
       01  TYB-R.
           02  YB-YBK         PIC  9(004).
           02  YB-DATE.
             03  YB-NEN       PIC  9(002).
             03  YB-GP        PIC  9(004).
           02  YB-MKD         PIC  9(006).
           02  YB-TCD         PIC  9(004).
           02  YB-TKB         PIC  9(002).
           02  YB-NO          PIC  9(004).
           02  YB-KIN         PIC  9(010).
           02  F              PIC  X(007).
           02  YB-SNI         PIC  9(004).
           02  F              PIC  X(004).
       77  F                  PIC  X(001).
      *FD  TGO-F
       01  TGO-F_TSD010.
           02  TGO-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TGO-F_LNAME    PIC  X(012) VALUE "TGO-F_TSD010".
           02  F              PIC  X(001).
           02  TGO-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TGO-F_SORT     PIC  X(100) VALUE SPACE.
           02  TGO-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TGO-F_RES      USAGE  POINTER.
       01  TGO-R.
           02  TGO-DTC        PIC  9(001).
           02  TGO-TSC        PIC  9(002).
           02  TGO-OKD        PIC  9(006).
           02  TGO-NO         PIC  9(004).
           02  TGO-BCD        PIC  9(004).
           02  TGO-TCD        PIC  9(004).
           02  TGO-KIN        PIC  9(010).
           02  TGO-UFD        PIC  9(006).
           02  TGO-MKD        PIC  9(006).
           02  TGO-SBC        PIC  9(004).
           02  TGO-FDM        PIC  N(024).
           02  F              PIC  X(025).
           02  TGO-SNU        PIC  9(004).
           02  TGO-SNM        PIC  9(004).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　受取・割引・支払手形　決済更新　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "<   H   年   月   日  迄決済   >".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  FILLER.
             03  D-UKET  PIC  N(013) VALUE
                  "［　ＵＫＥＴＭ　更新中　］".
             03  D-SHIT  PIC  N(013) VALUE
                  "［　ＳＨＩＴＭ　更新中　］".
           02  D-NG.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
       01  C-ACP.
           02  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(027) VALUE
                  "***  UKETM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  SHITM REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  TYBF WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  BANKM REWRITE ｴﾗｰ  ***".
             03  E-NO    PIC  9(004).
             03  E-SBC   PIC  9(004).
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張後、ＦＮＣ＋再開".
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
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "404" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "19" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "16" "0" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKET" "bN" "16" "21" "26" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHIT" "bN" "16" "21" "26" "D-UKET" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "Z9" "12" "25" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "30" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "12" "35" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEYD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "41" "1" "A-PEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "300" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "300" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "27" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NO" "9" "24" "50" "4" "E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NO" BY REFERENCE UT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SBC" "9" "24" "50" "4" "E-NO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SBC" BY REFERENCE UT-SBC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-SBC" " " RETURNING RESU.
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
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
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
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-NEND = W-NEN - DATE-YC1.
           MOVE W-GET TO W-GETD.
           MOVE W-PEY TO W-PEYD.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-PEYD < 1 OR > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           MOVE W-PEYD TO W-PEY.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           MOVE W-NGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE CALNM_IDLST CALNM_PNAME1
               GO TO M-10
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" BANK-M_PNAME1 " " BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TGO-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TGO-F_PNAME1 " " BY REFERENCE TGO-F_IDLST "0".
           CALL "SD_Output" USING "D-UKET" D-UKET "p" RETURNING RESU.
       M-20.
      *           READ UKET-M NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  UT-SKC NOT = 20 AND 32
               GO TO M-20
           END-IF
           MOVE ZERO TO W-MNGP.
           MOVE UT-OKD TO W-MNGPS.
           IF  W-MNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-MNEN
           END-IF
           IF  W-MNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-MNEN
           END-IF
           IF  W-MNGP > W-NGP
               GO TO M-20
           END-IF
           MOVE 0 TO W-DTC.
           IF  UT-SKC = 20
               MOVE 1 TO W-DTC
               GO TO M-35
           END-IF
           MOVE 2 TO W-DTC.
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SBC" E-SBC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE BANK-M_IDLST BANK-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               MOVE 0 TO W-DC
               GO TO M-95
           END-IF
           SUBTRACT UT-KIN FROM B-YBZ.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SBC" E-SBC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE BANK-M_IDLST BANK-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               MOVE 0 TO W-DC
               GO TO M-95
           END-IF.
       M-25.
           MOVE ZERO TO TYB-R.
           MOVE UT-SBC TO YB-YBK.
           MOVE W-MNGPS TO YB-DATE.
           COMPUTE YB-NEN = W-MNEN - DATE-YC1.
           MOVE 999999 TO YB-MKD.
           MOVE UT-TCD TO YB-TCD.
           MOVE UT-TSC TO YB-TKB.
           MOVE UT-KEY TO YB-NO.
           MOVE UT-KIN TO YB-KIN.
           MOVE UT-SNM TO YB-SNI.
      *           WRITE TYB-R.
      *//////////////
           CALL "DB_Insert" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           GO TO M-35.
       M-30.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               MOVE "TYBF         " TO W-FILE
               CALL "SD_Output" USING
                "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0"
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           MOVE 0 TO W-DC.
           GO TO M-95.
       M-35.
           MOVE 50 TO UT-SKC.
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
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE BANK-M_IDLST BANK-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               MOVE 0 TO W-DC
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-20.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "SD_Output" USING "D-SHIT" D-SHIT "p" RETURNING RESU.
       M-55.
      *           READ SHIT-M NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SHIT-M_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  ST-SKC NOT = 00
               GO TO M-55
           END-IF
           MOVE ZERO TO W-MNGP.
           MOVE ST-MKD TO W-MNGPS.
           MOVE ST-SNM TO W-MNEN.
           IF  W-MNGP > W-NGP
               GO TO M-55
           END-IF
           MOVE 3 TO W-DTC.
           MOVE 50 TO ST-SKC.
      *           REWRITE SHIT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SHIT-M_PNAME1 SHIT-M_LNAME SHIT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1
               MOVE 0 TO W-DC
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-55.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
       M-95.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           INITIALIZE TGO-R.
           MOVE SPACE TO TGO-FDM.
           MOVE W-DTC TO TGO-DTC.
           IF  W-DTC = 1 OR 2
               MOVE UT-TSC TO TGO-TSC
               MOVE UT-OKD TO TGO-OKD
               MOVE UT-KEY TO TGO-NO
               MOVE UT-BCD TO TGO-BCD
               MOVE UT-TCD TO TGO-TCD
               MOVE UT-KIN TO TGO-KIN
               MOVE UT-UTD TO TGO-UFD
               MOVE UT-MKD TO TGO-MKD
               MOVE UT-SBC TO TGO-SBC
               MOVE UT-FDM TO TGO-FDM
               MOVE UT-SNU TO TGO-SNU
               MOVE UT-SNM TO TGO-SNM
           ELSE
               MOVE ST-TSC TO TGO-TSC
               MOVE W-MNGPS TO TGO-OKD
               MOVE ST-KEY TO TGO-NO
               MOVE ST-BCD TO TGO-BCD
               MOVE ST-TCD TO TGO-TCD
               MOVE ST-KIN TO TGO-KIN
               MOVE ST-FDD TO TGO-UFD
               MOVE ST-MKD TO TGO-MKD
               MOVE ST-SNF TO TGO-SNU
               MOVE ST-SNM TO TGO-SNM
           END-IF
      *           WRITE TGO-R.
      *//////////////
           CALL "DB_Insert" USING
            TGO-F_PNAME1 TGO-F_LNAME TGO-R RETURNING RET.
       S-10.
           EXIT.
