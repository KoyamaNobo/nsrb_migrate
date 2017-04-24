       IDENTIFICATION DIVISION.
       PROGRAM-ID. JJM510.
      ************************************************
      *****    振込銀行マスター　メンテナンス    *****
      *****         ( SCREEN : SCJM51 )          *****
      ************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       01  HEAD1.
           02  F            PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(016) VALUE SPACE.
           02  F            PIC  N(023) VALUE
                  "＊＊＊　　振込銀行マスター　プルーフリスト　　".
           02  F            PIC  N(003) VALUE   "＊＊＊".
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  F            PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F            PIC  N(006) VALUE   "銀　行　名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(008) VALUE   "本　支　店　名　".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "最終年月".
           02  F            PIC  X(005) VALUE "  :  ".
           02  F            PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F            PIC  N(006) VALUE   "銀　行　名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(008) VALUE   "本　支　店　名　".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "最終年月".
       01  W-P.
           02  W-P1    OCCURS 58.
             03  P-KEY1     PIC  9(007).
             03  F          PIC  X(001).
             03  P-BNA1     PIC  X(015).
             03  F          PIC  X(001).
             03  P-SNA1     PIC  X(015).
             03  F          PIC  X(001).
             03  P-ENG1     PIC 99/99.
             03  F          PIC  X(002).
             03  P-X1       PIC  X(001).
             03  F          PIC  X(002).
             03  P-KEY2     PIC  9(007).
             03  F          PIC  X(001).
             03  P-BNA2     PIC  X(015).
             03  F          PIC  X(001).
             03  P-SNA2     PIC  X(015).
             03  F          PIC  X(001).
             03  P-ENG2     PIC 99/99.
       01  W-DATA1.
           02  W-PAGE       PIC  9(002).
           02  W-PC         PIC  9(001).
           02  W-LD         PIC  9(002).
           02  W-CD         PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-DMM        PIC  9(001).
       01  W-DATA2.
           02  W-BKN1       PIC  X(015).
           02  W-BKN2       PIC  X(015).
       01  W-D.
           02  W-CHK        PIC  9(001).
           02  CNT          PIC  9(001).
           02  W-ACT        PIC  9(001).
           02  W-L          PIC  9(002).
           02  W-SEKEY.
             03  W-SKEY     PIC  9(004).
             03  W-EKEY     PIC  9(004) VALUE 9999.
           02  W-C          PIC  9(002).
           02  W-FILE       PIC  X(013).
           COPY LSTAT.
       01  W-R.
           02  W-KEY.
             03  W-BKC      PIC  9(004).
             03  W-HSC      PIC  9(003).
           02  W-BNA        PIC  X(015).
           02  W-SNA        PIC  X(015).
           02  F            PIC  X(001).
           02  W-ENG        PIC  9(004).
      *
           COPY LIFBKM.
           COPY LSPF.
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-ACT   PIC  9(001).
             03  A-KEY   PIC  9(007).
             03  A-BNA   PIC  X(015).
             03  A-SNA   PIC  X(015).
             03  A-DMM   PIC  9(001).
           02  FILLER.
             03  A-SKEY  PIC  9(004).
             03  A-EKEY  PIC  9(004).
             03  A-CHK   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-PM    PIC  X(048) VALUE
                  "<   銀行コード 0000 ～ 9999 迄打ち出し  ﾘﾀｰﾝ   >".
             03  D-PMC   PIC  X(048) VALUE
                  "                                                ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  FBKM ﾅｼ  ***".
             03  E-ME2   PIC  X(022) VALUE
                  "***  FBKM ﾄｳﾛｸｽﾞﾐ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  FBKM WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  FBKM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(025) VALUE
                  "***  FBKM DELETE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  ｷﾞﾝｺｳﾒｲ ﾆ ｺﾓｼﾞ ｱﾘ  ***".
             03  E-ME7   PIC  X(027) VALUE
                  "***  ﾎﾝｼﾃﾝﾒｲ ﾆ ｺﾓｼﾞ ｱﾘ  ***".
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                      "オーバーフロー、領域を拡張し、ＦＮＣ＋再開".
             03  E-ME78  PIC  N(002) VALUE   "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "39" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "W-L" "14" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "W-L" "17" "7" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BNA" "X" "W-L" "26" "15" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BNA" BY REFERENCE W-BNA "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNA" "X" "W-L" "42" "15" "A-BNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNA" BY REFERENCE W-SNA "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "60" "1" "A-SNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "22" "0" "9" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "22" "29" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "22" "37" "4" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "22" "53" "1" "A-EKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "22" "0" "96" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "22" "14" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PMC" "X" "22" "14" "48" "D-PM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "319" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "319" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "22" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "27" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " " RETURNING RESU.
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
            "E-CL" " " "24" "0" "80" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           CALL "SD_Screen_Output" USING "SCJM51" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT = 9
               GO TO M-95
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU
               GO TO M-65
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE SPACE TO W-BNA W-SNA.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-KEY TO FBK-KEY.
      *           READ FBKM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE FBK-R TO W-R.
           CALL "SD_Output" USING "A-BNA" A-BNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNA" A-SNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-35
           END-IF
           GO TO M-25.
       M-20.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-BNA "A-BNA" "X" "15"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE ZERO TO W-C.
           INSPECT W-BNA TALLYING W-C FOR ALL "ｧ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｩ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｪ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｫ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｬ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｭ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｮ".
           INSPECT W-BNA TALLYING W-C FOR ALL "ｯ".
           IF  W-C NOT = ZERO
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-SNA "A-SNA" "X" "15"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           MOVE ZERO TO W-C.
           INSPECT W-SNA TALLYING W-C FOR ALL "ｧ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｩ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｪ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｫ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｬ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｭ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｮ".
           INSPECT W-SNA TALLYING W-C FOR ALL "ｯ".
           IF  W-C NOT = ZERO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-15
               ELSE
                   GO TO M-30
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
           IF  W-ACT = 2
               GO TO M-50
           END-IF
           IF  W-ACT = 3
               GO TO M-55
           END-IF.
       M-40.
           MOVE W-R TO FBK-R.
      *           WRITE FBK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            FBKM_PNAME1 FBKM_LNAME FBK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           GO TO M-60.
       M-45.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
           MOVE "FBKM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           GO TO M-40.
       M-50.
           MOVE ZERO TO FBK-R.
           MOVE W-R TO FBK-R.
      *           REWRITE FBK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FBKM_PNAME1 FBKM_LNAME FBK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           GO TO M-60.
       M-55.
      *           DELETE FBKM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING FBKM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF.
       M-60.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               CALL "SD_Screen_Output" USING "SCJM51" RETURNING RESU
               MOVE 5 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           GO TO M-15.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "SD_Output" USING "D-PMC" D-PMC "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-65
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-EKEY < W-SKEY
               GO TO M-70
           END-IF.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-70
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-75
           END-IF
           IF  W-CHK = 9
               CALL "SD_Output" USING "D-PMC" D-PMC "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  W-CHK NOT = 1
               GO TO M-75
           END-IF
      *
           MOVE ZERO TO FBK-KEY.
           MOVE W-SKEY TO FBK-BKC.
      *           START FBKM KEY NOT < FBK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            FBKM_PNAME1 "FBK-KEY" " NOT < " FBK-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-65
           END-IF
      *           READ FBKM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-65
           END-IF
           IF  FBK-BKC > W-EKEY
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-65
           END-IF
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO CNT.
           MOVE SPACE TO W-P W-DATA2.
           MOVE ZERO TO W-DATA1.
       M-80.
           PERFORM S-20 THRU S-35.
       M-85.
      *           READ FBKM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  FBK-BKC > W-EKEY
               GO TO M-90
           END-IF
           MOVE 5 TO CHK.
           GO TO M-80.
       M-90.
           PERFORM S-40 THRU S-50.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
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
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-30
           END-IF
           ADD 1 TO W-CD.
           IF  W-CD NOT = 02
               MOVE ZERO TO W-LD
               GO TO S-20
           END-IF
           PERFORM S-40 THRU S-50.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD W-CD.
           GO TO S-20.
       S-30.
           IF  CHK = 5
               IF  FBK-BKN = W-BKN1
                   MOVE SPACE TO W-BKN2
               ELSE
                   MOVE FBK-BKN TO W-BKN2
               END-IF
           END-IF
           IF  W-LD = 1
               MOVE FBK-BKN TO W-BKN2
           END-IF
           IF  W-CD = ZERO
               MOVE ":" TO P-X1(W-LD)
               MOVE FBK-KEY TO P-KEY1(W-LD)
               MOVE W-BKN2 TO P-BNA1(W-LD)
               MOVE FBK-HSN TO P-SNA1(W-LD)
               IF  FBK-ENG NOT = ZERO
                   MOVE FBK-ENG TO P-ENG1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 01
               MOVE FBK-KEY TO P-KEY2(W-LD)
               MOVE W-BKN2 TO P-BNA2(W-LD)
               MOVE FBK-HSN TO P-SNA2(W-LD)
               IF  FBK-ENG NOT = ZERO
                   MOVE FBK-ENG TO P-ENG2(W-LD)
               END-IF
           END-IF
           MOVE FBK-BKN TO W-BKN1.
       S-35.
           EXIT.
       S-40.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-45.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO SP-R
               MOVE W-P1(W-LD) TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-45
           END-IF.
       S-50.
           EXIT.
