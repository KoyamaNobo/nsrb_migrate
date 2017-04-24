       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD520.
      *********************************************************
      *    PROGRAM         :  購買　日計　更新　　　　　　    *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-D.
             03  W-KZ         PIC S9(009).
             03  W-KZZ        PIC S9(008).
             03  W-SI         PIC S9(009).
             03  W-SIZ        PIC S9(008).
             03  W-SH         PIC S9(009).
             03  W-SHZ        PIC S9(008).
             03  W-NS         PIC S9(007)V9(02).
             03  W-NK         PIC S9(008).
             03  W-SS         PIC S9(007)V9(02).
           02  W-KEY.
             03  W-SCD        PIC  9(004).
             03  W-JCD        PIC  9(006).
           02  W-TD.
             03  W-T          PIC S9(006)V9(02).
             03  W-KT         PIC S9(006)V9(02).
             03  W-SD         PIC  9(006).
             03  W-ED         PIC  9(008).
             03  W-EDD   REDEFINES W-ED.
               04  W-ENEN     PIC  9(004).
               04  W-ENENL REDEFINES W-ENEN.
                 05  W-ENEN1  PIC  9(002).
                 05  W-ENEN2  PIC  9(002).
               04  F          PIC  9(004).
             03  W-EDL   REDEFINES W-ED.
               04  F          PIC  9(002).
               04  W-EDS      PIC  9(006).
             03  W-ENGP  REDEFINES W-ED.
               04  W-ENG      PIC  9(006).
               04  W-ENGL  REDEFINES W-ENG.
                 05  F        PIC  9(002).
                 05  W-ENGS   PIC  9(004).
               04  F          PIC  9(002).
             03  W-CD         PIC  9(008).
             03  W-CDD   REDEFINES W-CD.
               04  W-CNEN     PIC  9(004).
               04  W-CNENL REDEFINES W-CNEN.
                 05  W-CNEN1  PIC  9(002).
                 05  W-CNEN2  PIC  9(002).
               04  F          PIC  9(004).
             03  W-CDL   REDEFINES W-CD.
               04  F          PIC  9(002).
               04  W-CDS      PIC  9(006).
           02  W-DC1          PIC  9(001) VALUE 0.
           02  W-DC2          PIC  9(001) VALUE 0.
           02  W-DC3          PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIJM.
           COPY LIJTM.
           COPY LISTM.
      *FD  JK-M
       01  JK-M_KBD520.
           02  JK-M_PNAME1    PIC  X(003) VALUE "JKM".
           02  F              PIC  X(001).
           02  JK-M_LNAME     PIC  X(011) VALUE "JK-M_KBD520".
           02  F              PIC  X(001).
           02  JK-M_KEY1      PIC  X(100) VALUE SPACE.
           02  JK-M_KEY2      PIC  X(100) VALUE SPACE.
           02  JK-M_SORT      PIC  X(100) VALUE SPACE.
           02  JK-M_IDLST     PIC  X(100) VALUE SPACE.
           02  JK-M_RES       USAGE  POINTER.
       01  JK-R.
           02  K-KEY.
             03  K-JCD1       PIC  9(006).
             03  K-JCD2       PIC  9(006).
           02  F              PIC  X(004).
       77  F                  PIC  X(001).
      *FD  JSS-F
       01  JSS-F_KBD520.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KBD520".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_KEY2     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC.
             03  JS-DC1       PIC  9(001).
             03  JS-DC2       PIC  9(001).
           02  JS-DATE        PIC  9(008).
           02  JS-NGP   REDEFINES JS-DATE.
             03  F            PIC  9(002).
             03  JS-NGPS      PIC  9(006).
           02  JS-NGPL  REDEFINES JS-DATE.
             03  JS-NG        PIC  9(006).
             03  JS-NGL   REDEFINES JS-NG.
               04  F          PIC  9(002).
               04  JS-NGS     PIC  9(004).
             03  F            PIC  9(002).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  F              PIC  X(007).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  JS-BSC         PIC  9(001).
           02  JS-BKC         PIC  9(002).
           02  JS-KCO         PIC  X(005).
           02  JS-KHC         PIC  9(001).
           02  F              PIC  X(010).
           02  JS-KEY         PIC  X(007).
           02  JS-PCNT        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HA-F
       01  HA-F_KBD520.
           02  HA-F_PNAME1    PIC  X(003) VALUE "HAF".
           02  F              PIC  X(001).
           02  HA-F_LNAME     PIC  X(011) VALUE "HA-F_KBD520".
           02  F              PIC  X(001).
           02  HA-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HA-F_KEY2      PIC  X(100) VALUE SPACE.
           02  HA-F_SORT      PIC  X(100) VALUE SPACE.
           02  HA-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HA-F_RES       USAGE  POINTER.
       01  HA-R.
           02  HA-DATE        PIC  9(008).
           02  HA-JCD         PIC  9(006).
           02  HA-SS          PIC S9(007)V9(02).
           02  HA-KEY         PIC  X(007).
           02  F              PIC  X(001).
           02  HA-PCNT        PIC  9(001).
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
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　購買　日計更新　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-MSG1.
             03  FILLER  PIC  N(022) VALUE
                  "未印字データ有り　印字済み分のみ処理を続行中".
           02  D-MSG2.
             03  FILLER  PIC  N(021) VALUE
                  "工品未印字データ有り　その他の処理を続行中".
           02  D-MSG3.
             03  FILLER  PIC  N(021) VALUE
                  "工品未変換データ有り　その他の処理を続行中".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  STM ﾅｼ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  STM REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  JM ﾅｼ 10  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  JM ﾅｼ 11  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  JM ﾅｼ 12  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  JM REWRITE ｴﾗｰ 1  ***".
             03  E-ME7   PIC  X(018) VALUE
                  "***  JM ﾅｼ 20  ***".
             03  E-ME8   PIC  X(023) VALUE
                  "***  ｻﾞｲｺ ｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  JM ﾅｼ 30  ***".
             03  E-ME10  PIC  X(018) VALUE
                  "***  JM ﾅｼ 40  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  JM REWRITE ｴﾗｰ 2  ***".
             03  E-ME12  PIC  X(018) VALUE
                  "***  JM ﾅｼ 50  ***".
             03  E-ME13  PIC  X(026) VALUE
                  "***  JM REWRITE ｴﾗｰ 3  ***".
             03  E-ME14  PIC  X(019) VALUE
                  "***  JTM ﾅｼ 10  ***".
             03  E-ME15  PIC  X(019) VALUE
                  "***  JTM ﾅｼ 20  ***".
             03  E-ME16  PIC  X(027) VALUE
                  "***  JTM REWRITE ｴﾗｰ 1  ***".
             03  E-ME17  PIC  X(027) VALUE
                  "***  JTM REWRITE ｴﾗｰ 2  ***".
             03  E-ME18  PIC  X(024) VALUE
                  "***  SM REWRITE ｴﾗｰ  ***".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-SCD   PIC  9(004).
             03  E-JCD1  PIC  9(006).
             03  E-JCD2  PIC  9(006).
             03  E-JCD3  PIC  9(006).
             03  E-JCD4  PIC  9(006).
             03  E-JKEY  PIC  X(007).
             03  E-HKEY  PIC  X(007).
             03  E-STAT  PIC  X(002).
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "238" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "34" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "128" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG1" " " "0" "0" "44" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MSG1" "N" "14" "10" "44" " " "D-MSG1" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG2" " " "0" "0" "42" "D-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MSG2" "N" "16" "10" "42" " " "D-MSG2" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG3" " " "0" "0" "42" "D-MSG2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MSG3" "N" "18" "10" "42" " " "D-MSG3" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "522" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "522" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "18" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "23" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "18" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "18" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "26" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "19" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "19" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "27" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "27" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "24" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SCD" "9" "24" "45" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SCD" BY REFERENCE JS-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD1" "9" "24" "45" "6" "E-SCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD1" BY REFERENCE JS-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD2" "9" "24" "45" "6" "E-JCD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD2" BY REFERENCE HA-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD3" "9" "24" "45" "6" "E-JCD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD3" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD4" "9" "24" "45" "6" "E-JCD3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD4" BY REFERENCE K-JCD2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JKEY" "X" "24" "55" "7" "E-JCD4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JKEY" BY REFERENCE JS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HKEY" "X" "24" "55" "7" "E-JKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HKEY" BY REFERENCE HA-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-HKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-STAT" " " RETURNING RESU.
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
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "I-O" ST-M_PNAME1 " " BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-M_PNAME1 " " BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JK-M_PNAME1 " " BY REFERENCE JK-M_IDLST "1"
            "K-KEY" BY REFERENCE K-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HA-F_PNAME1 " " BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
      *
       M-10.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  JS-PCNT = 0
               IF  W-DC1 = 0
                   MOVE 1 TO W-DC1
                   CALL "SD_Output" USING
                    "D-MSG1" D-MSG1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-10
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-KCO = SPACE OR ZERO
               GO TO M-15
           END-IF
           IF  JS-JCD < 490000 OR > 498999
               GO TO M-15
           END-IF
           IF  JS-KHC = 0
               IF  W-DC2 = 0
                   MOVE 1 TO W-DC2
                   CALL "SD_Output" USING
                    "D-MSG2" D-MSG2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-10
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-KHC = 1
               IF  W-DC3 = 0
                   MOVE 1 TO W-DC3
                   CALL "SD_Output" USING
                    "D-MSG3" D-MSG3 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-10
               ELSE
                   GO TO M-10
               END-IF
           END-IF.
       M-15.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
      *
           MOVE JS-SCD TO ST-KEY.
      *           READ ST-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO W-D.
           MOVE ST-TSK TO W-SI.
           MOVE ST-TSKZ TO W-SIZ.
           MOVE ST-THK TO W-SH.
           MOVE ST-THKZ TO W-SHZ.
           IF  JS-DC1 NOT = 3
               ADD JS-KIN TO W-SI
               ADD JS-SHZ TO W-SIZ
           ELSE
               ADD JS-KIN TO W-SH
               ADD JS-SHZ TO W-SHZ
           END-IF
           MOVE ST-ZKZ TO W-KZ.
           MOVE ST-ZKZZ TO W-KZZ.
           COMPUTE W-KZ = W-KZ + W-SI - W-SH.
           COMPUTE W-KZZ = W-KZZ + W-SIZ - W-SHZ.
           MOVE W-KZ TO ST-KZ.
           MOVE W-KZZ TO ST-KZZ.
           MOVE W-SI TO ST-TSK.
           MOVE W-SIZ TO ST-TSKZ.
           MOVE W-SH TO ST-THK.
           MOVE W-SHZ TO ST-THKZ.
      *           REWRITE ST-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            ST-M_PNAME1 ST-M_LNAME ST-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           PERFORM S-05 THRU S-15.
           IF  JS-DC1 = 3
               GO TO M-10
           END-IF
           IF  JS-JCD = 999000
               GO TO M-10
           END-IF.
      *
       M-20.
           MOVE JS-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE JS-JCD TO JT-KEY.
      *           READ JT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO W-TD.
           MOVE J-ED TO W-EDS.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  JS-DC2 > 2
               GO TO M-40
           END-IF
           IF  JS-DC2 = 2
               GO TO M-35
           END-IF
      *
           IF  W-ED > JS-DATE
               GO TO M-40
           END-IF
           IF  J-MCD NOT = ZERO
               GO TO M-25
           END-IF
           IF  JS-T > ZERO
               MOVE JS-T TO J-ST
               MOVE JS-NGPS TO J-ED
           END-IF
           GO TO M-40.
      *
       M-25.
           MOVE JS-T TO W-KT.
           MOVE ZERO TO W-T.
           MOVE J-MCD TO W-JCD.
           MOVE W-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE J-ST TO W-T.
       M-30.
           MOVE JS-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           COMPUTE W-T = W-T + W-KT.
           IF  W-T > ZERO
               MOVE W-T TO J-ST
               MOVE W-KT TO J-KT
               MOVE JS-NGPS TO J-ED
           END-IF
           GO TO M-40.
      *
       M-35.
           MOVE JS-CD TO W-CDS.
           IF  W-CNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-CNEN
           END-IF
           IF  W-CNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-CNEN
           END-IF
           IF  W-ED > W-CD
               GO TO M-40
           END-IF
           COMPUTE W-T = J-ST + JS-T.
           IF  W-T > ZERO
               MOVE W-T TO J-ST
               MOVE JS-CD TO J-ED
           END-IF.
       M-40.
           IF  JS-DC2 = 2 OR 3
               GO TO M-45
           END-IF
      *
           MOVE JT-SSU TO W-NS.
           ADD JS-SU TO W-NS.
           MOVE W-NS TO JT-SSU.
      *
       M-45.
           MOVE JT-SIK TO W-NK.
           ADD JS-KIN TO W-NK.
           MOVE W-NK TO JT-SIK.
      *           REWRITE J-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *           REWRITE JT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JT-M_PNAME1 JT-M_LNAME JT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           IF  JS-DC2 NOT = 2
               GO TO M-10
           END-IF
           MOVE SPACE TO K-KEY.
           MOVE JS-JCD TO K-JCD1.
      *           START JK-M KEY NOT < K-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JK-M_PNAME1 "K-KEY" " NOT < " K-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF.
       M-50.
      *           READ JK-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JK-M_PNAME1 BY REFERENCE JK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF JS-JCD NOT = K-JCD1
               GO TO M-10
           END-IF
           MOVE K-JCD2 TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD4" E-JCD4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           COMPUTE W-T = J-ST + JS-T.
           IF  W-T > ZERO
               MOVE W-T TO J-ST
           END-IF
      *           REWRITE J-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD4" E-JCD4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JKEY" E-JKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-50.
      *
       M-55.
           MOVE ZERO TO W-D.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  HA-PCNT = 0
               IF  W-DC1 = 0
                   MOVE 1 TO W-DC1
                   CALL "SD_Output" USING
                    "D-MSG1" D-MSG1 "p" RETURNING RESU
                   GO TO M-55
               ELSE
                   GO TO M-55
               END-IF
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE HA-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD2" E-JCD2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE HA-JCD TO JT-KEY.
      *           READ JT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD2" E-JCD2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  J-ZC = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD2" E-JCD2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE JT-HSU TO W-SS.
           ADD HA-SS TO W-SS.
           IF  J-ST NOT = ZERO
               GO TO M-65
           END-IF
           IF  J-MCD = ZERO
               GO TO M-65
           END-IF
           MOVE ZERO TO W-TD.
           MOVE J-MCD TO W-JCD.
           MOVE W-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD3" E-JCD3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-60
           END-IF
           IF  J-ST NOT = ZERO
               MOVE J-ED TO W-SD
               MOVE J-ST TO W-KT
           END-IF.
       M-60.
           MOVE HA-JCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-KT NOT = ZERO
               MOVE W-SD TO J-ED
               MOVE W-KT TO J-ST
           END-IF.
       M-65.
           MOVE W-SS TO JT-HSU.
      *           REWRITE J-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD2" E-JCD2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *           REWRITE JT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JT-M_PNAME1 JT-M_LNAME JT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD2" E-JCD2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HKEY" E-HKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-55.
      *
       M-90.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
      *
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JK-M_IDLST JK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE JS-SCD TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  S-ENG = ZERO
               GO TO S-10
           END-IF
           MOVE ZERO TO W-ED.
           MOVE S-ENG TO W-ENGS.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  JS-NG <= W-ENG
               GO TO S-15
           END-IF.
       S-10.
           MOVE JS-NGS TO S-ENG.
           MOVE ZERO TO S-TNG.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       S-15.
           EXIT.
