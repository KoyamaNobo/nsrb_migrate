       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKD110.
      *********************************************************
      *    PROGRAM         :  入金累積ファイル　修正入力　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ------                          *
      *        変更　　　  :  95/07/18                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-HNG          PIC  9(006).
           02  W-HNGD  REDEFINES W-HNG.
             03  W-HNEN       PIC  9(004).
             03  W-HNEND REDEFINES W-HNEN.
               04  W-HNEN1    PIC  9(002).
               04  W-HNEN2    PIC  9(002).
             03  W-HGET       PIC  9(002).
           02  W-HNGL  REDEFINES W-HNG.
             03  F            PIC  9(002).
             03  W-HNGS       PIC  9(004).
           02  W-KNG          PIC  9(006).
           02  W-KNGD  REDEFINES W-KNG.
             03  W-KNEN       PIC  9(004).
             03  W-KNEND REDEFINES W-KNEN.
               04  W-KNEN1    PIC  9(002).
               04  W-KNEN2    PIC  9(002).
             03  W-KGET       PIC  9(002).
           02  W-KNGL  REDEFINES W-KNG.
             03  F            PIC  9(002).
             03  W-KNGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-KEY.
             03  W-DNO        PIC  9(006).
             03  W-GNO        PIC  9(001).
           02  W-END          PIC  9(001) VALUE 0.
           02  W-NKNA         PIC  N(006).
           02  W-NSNA         PIC  N(006).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LITM.
           COPY LSNYUR.
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　入金累積ファイル　修正入力　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(008) VALUE "入 金 №".
             03  FILLER  PIC  X(001) VALUE "-".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
           02  FILLER  PIC  X(017) VALUE
                "（*印は修正不可）".
           02  FILLER  PIC  N(004) VALUE "日　　付".
           02  FILLER  PIC  X(010) VALUE "* 得 意 先".
           02  FILLER.
             03  FILLER  PIC  N(004) VALUE "入金区分".
             03  FILLER  PIC  N(004) VALUE "相殺区分".
           02  FILLER  PIC  N(004) VALUE "手形期日".
           02  FILLER  PIC  X(010) VALUE "* 金　　額".
           02  FILLER  PIC  N(004) VALUE "請求年月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-DNO   PIC  9(006).
             03  A-GNO   PIC  9(001).
           02  A-DATE  PIC  9(006).
           02  FILLER.
             03  A-NC    PIC  9(002).
             03  A-NSC   PIC  9(001).
           02  A-TD    PIC  9(006).
           02  A-SS    PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MD.
             03  FILLER.
               04  D-TCD   PIC  9(004).
               04  D-NAME  PIC  N(026).
             03  FILLER.
               04  D-NN    PIC  N(006).
               04  D-NSNA  PIC  N(006).
             03  D-KIN.
               04  01D-KIN PIC ZZZZZZZ9- .
           02  S-NSC   PIC  X(024) VALUE
                "                        ".
           02  S-TD    PIC  X(006) VALUE "      ".
           02  S-SS    PIC  X(004) VALUE "    ".
           02  S-DMM   PIC  X(001) VALUE " ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾋｽﾞｹ ｴﾗｰ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME5   PIC  X(027) VALUE
                  "***  NYURF REWRITE ｴﾗｰ  ***".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "163" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "8" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "4" "0" "18" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "4" "8" "8" " " "02C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "4" "24" "1" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "X" "4" "33" "9" "0202C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "34" "17" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "7" "8" "8" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "9" "6" "10" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "11" "0" "16" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "N" "11" "8" "8" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0206C-MID" "N" "11" "38" "8" "0106C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "13" "8" "8" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "6" "10" "07C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "N" "17" "8" "8" "08C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "22" "30" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "4" "0" "7" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "4" "18" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GNO" "9" "4" "25" "1" "A-DNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GNO" BY REFERENCE W-GNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "7" "18" "6" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE NUR-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "11" "0" "3" "A-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NC" "9" "11" "18" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NC" BY REFERENCE NUR-NC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NSC" "9" "11" "48" "1" "A-NC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NSC" BY REFERENCE NUR-NSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TD" "9" "13" "18" "6" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TD" BY REFERENCE NUR-TGKS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SS" "9" "17" "18" "4" "A-TD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SS" BY REFERENCE NUR-SSS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-SS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "124" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "0" "0" "89" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" " " "9" "0" "56" " " "D-MD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" "9" "9" "18" "4" " " "01D-MD"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCD" BY REFERENCE NUR-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "9" "23" "52" "D-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" " " "11" "0" "24" "01D-MD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NN" "N" "11" "23" "12" " " "02D-MD"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NN" BY REFERENCE W-NKNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NSNA" "N" "11" "50" "12" "D-NN" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NSNA" BY REFERENCE W-NSNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "15" "0" "9" "02D-MD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "ZZZZZZZ9-" "15" "18" "9" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE NUR-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-NSC" "X" "11" "48" "24" "D-MD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-TD" "X" "13" "18" "6" "S-NSC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SS" "X" "17" "18" "4" "S-TD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-DMM" "X" "22" "47" "1" "S-SS" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "94" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "27" "E-ME4" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-HNG W-KNG.
           MOVE D-NHNG TO W-HNGS.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF
           MOVE D-NKNG TO W-KNGS.
           IF  W-KNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-KNEN
           END-IF
           IF  W-KNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-KNEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-10.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 8
               GO TO M-90
           END-IF
           PERFORM REW-RTN THRU REW-EX.
           IF  W-END = 8
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *--------------  入　　力  ---------------------------------------
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-DNO = ZERO
               GO TO ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-GNO "A-GNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-GNO = 0
               GO TO ACP-010
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
       ACP-020.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           IF  NUR-KEY NOT = W-KEY
               GO TO ACP-020
           END-IF
      *
           MOVE NUR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　マスター　なし　" TO T-NAME
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE NUR-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE HKB-NKNA TO W-NKNA.
      *
           IF  NUR-NSC = 0
               GO TO ACP-030
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE NUR-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NSNA
           END-IF
           MOVE HKB-NSNA TO W-NSNA.
       ACP-030.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NC" A-NC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           IF  NUR-NC = "60"
               CALL "SD_Output" USING "A-NSC" A-NSC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "S-NSC" S-NSC "p" RETURNING RESU
           END-IF
           IF  NUR-TGK = ZERO
               CALL "SD_Output" USING "S-TD" S-TD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TD" A-TD "p" RETURNING RESU
           END-IF
           IF  NUR-SS = ZERO
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-SS" A-SS "p" RETURNING RESU
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           MOVE ZERO TO NUR-NEN1.
           IF  NUR-NGP = ZERO
               GO TO ACP-040
           END-IF
           IF  NUR-GET < 1 OR > 12
               GO TO ACP-040
           END-IF
           IF  NUR-PEY < 1 OR > 31
               GO TO ACP-040
           END-IF
      *
           IF  NUR-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO NUR-NEN
           END-IF
           IF  NUR-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO NUR-NEN
           END-IF
           IF  NUR-NG NOT = W-HNG AND W-KNG
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-NC "A-NC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE NUR-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           IF  NUR-NC2 NOT = 0 AND 8 AND 9
               GO TO ACP-060
           END-IF
           IF  NUR-NC = "78" OR "98"
               GO TO ACP-060
           END-IF
           IF  NUR-NC1 NOT = 7
               IF  NUR-NC2 = 9
                   GO TO ACP-060
               END-IF
           END-IF
           MOVE HKB-NKNA TO W-NKNA.
           CALL "SD_Output" USING "D-NN" D-NN "p" RETURNING RESU.
      *
           IF  NUR-NC = "60"
               MOVE ZERO TO NUR-TGK
               CALL "SD_Output" USING "S-TD" S-TD "p" RETURNING RESU
               GO TO ACP-070
           END-IF
           MOVE 0 TO NUR-NSC.
           CALL "SD_Output" USING "S-NSC" S-NSC "p" RETURNING RESU.
           IF  NUR-NC NOT = "30" AND "40"
               MOVE ZERO TO NUR-TGK
               CALL "SD_Output" USING "S-TD" S-TD "p" RETURNING RESU
               GO TO ACP-100
           ELSE
               GO TO ACP-080
           END-IF.
       ACP-070.
           CALL "SD_Accept" USING BY REFERENCE A-NSC "A-NSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-070
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE NUR-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-070
           END-IF
           MOVE HKB-NSNA TO W-NSNA.
           CALL "SD_Output" USING "D-NSNA" D-NSNA "p" RETURNING RESU.
           GO TO ACP-100.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-TD "A-TD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  NUR-TGG < 1 OR > 12
               GO TO ACP-080
           END-IF
           IF  NUR-TGP < 1 OR > 31
               GO TO ACP-080
           END-IF
      *
           MOVE ZERO TO NUR-TGN1.
           IF  NUR-TGN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO NUR-TGN
           END-IF
           IF  NUR-TGN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO NUR-TGN
           END-IF
           IF  NUR-TGK < NUR-DATE
               GO TO ACP-080
           END-IF.
       ACP-100.
           IF  NUR-NC2 > 7
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
               MOVE ZERO TO NUR-SS
               GO TO ACP-200
           END-IF
      *
           CALL "SD_Accept" USING BY REFERENCE A-SS "A-SS" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  NUR-NC = "30" OR "40"
                   GO TO ACP-080
               ELSE
                   IF  NUR-NC = "60"
                       GO TO ACP-070
                   ELSE
                       GO TO ACP-060
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
      *
           MOVE ZERO TO NUR-SSN1.
           IF  NUR-SS = ZERO
               CALL "SD_Output" USING "S-SS" S-SS "p" RETURNING RESU
               GO TO ACP-200
           END-IF
           IF  NUR-SSG < 1 OR > 12
               GO TO ACP-100
           END-IF
           IF  NUR-SSN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO NUR-SSN
           END-IF
           IF  NUR-SSN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO NUR-SSN
           END-IF
           IF  NUR-SS > NUR-NG
               GO TO ACP-100
           END-IF.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  NUR-NC2 < 8
                   GO TO ACP-100
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  NUR-NC1 = 3 OR 4
                   GO TO ACP-080
               ELSE
                   GO TO ACP-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-200
           END-IF.
       ACP-EX.
           EXIT.
      *--------------  ＮＹＵＲＦ　ＲＥＷＲＩＴＥ　---------------------
       REW-RTN.
      *           REWRITE NYUR-R.
      *///////////////
           CALL "DB_Update" USING
            NYUR-F_PNAME1 NYUR-F_LNAME NYUR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
       REW-EX.
           EXIT.
