       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD210.
      ********************************************************
      *    PROGRAM         :  廃却不良別・数量インプット   ***
      *    PRINTER TYPE    :  JIPS                         ***
      *    SCREEN          :  SCKD21                       ***
      *    DATA WRITTN     :  57/05/20                     ***
      *        変更　　　  :  98/12/07                     ***
      *    COMPILE TYPE    :  COBOL                        ***
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　工品廃却不良　入力リスト　　＊＊＊".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC  Z(002).
       01  HEAD2.
           02  F              PIC  N(004) VALUE "　日　付".
           02  F              PIC  X(009) VALUE "    ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(006) VALUE "不　良　名　".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "場　　所".
           02  F              PIC  X(003) VALUE SPACE.
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NAME         PIC  X(020).
           02  F              PIC  X(001).
           02  P-NHC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-FRN          PIC  N(006).
           02  P-SU           PIC ---,--9.
           02  P-T            PIC ----,--9.99.
           02  P-KIN          PIC ---,---,--9.
           02  F              PIC  X(002).
           02  P-BS           PIC  9(002).
           02  F              PIC  X(001).
           02  P-BSN          PIC  X(006).
       01  W-DATA.
           02  W-R.
             03  W-NHC        PIC  9(002).
             03  W-DATE       PIC  9(008).
             03  W-NGP   REDEFINES W-DATE.
               04  W-NG.
                 05  W-NEN    PIC  9(004).
                 05  W-NENL  REDEFINES W-NEN.
                   06  W-NEN1 PIC  9(002).
                   06  W-NEN2 PIC  9(002).
                 05  W-GET    PIC  9(002).
               04  W-NGL   REDEFINES W-NG.
                 05  F        PIC  9(002).
                 05  W-NGS    PIC  9(004).
               04  W-PEY      PIC  9(002).
             03  W-NGPL  REDEFINES W-DATE.
               04  F          PIC  9(002).
               04  W-NGPS     PIC  9(006).
             03  W-HCD        PIC  X(005).
             03  W-TS         PIC  9(002).
             03  W-KK         PIC S9(005).
             03  W-SU         PIC S9(006)V9(02).
             03  W-TN         PIC  9(006)V9(02).
             03  W-KIN        PIC S9(008).
             03  W-YC         PIC  9(002).
             03  F            PIC  X(001).
             03  W-BS         PIC  9(002).
             03  F            PIC  X(013).
           02  W-D.
             03  WD-DATE      PIC  9(008).
           02  W-DMM          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-KNG          PIC  9(004).
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKKBM.
           COPY LSPF.
      *FD  KNH-F
       01  KNH-F_KHD210.
           02  KNH-F_PNAME1   PIC  X(004) VALUE "KNHF".
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_KHD210".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R.
           02  KNF-NHC        PIC  9(002).
           02  KNF-DATE.
             03  F            PIC  9(002).
             03  KNF-NGPS     PIC  9(006).
           02  KNF-HCD        PIC  X(005).
           02  KNF-TS         PIC  9(002).
           02  KNF-KK         PIC S9(005).
           02  KNF-SU         PIC S9(006)V9(02).
           02  KNF-TN         PIC  9(006)V9(02).
           02  KNF-KIN        PIC S9(008).
           02  KNF-YC         PIC  9(002).
           02  F              PIC  9(001).
           02  KNF-BS         PIC  9(002).
           02  F              PIC  X(002).
           02  KNF-NC         PIC  9(001).
           02  F              PIC  X(009).
           02  KNF-PRC        PIC  9(001).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-DATE  PIC  9(006).
             03  A-HCD   PIC  X(005).
             03  A-NHC   PIC  9(002).
             03  A-SU    PIC S9(004).
             03  A-BS    PIC  9(002).
             03  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-NAME  PIC  X(020).
             03  D-FRN   PIC  N(006).
             03  D-SU    PIC ZZZZ- .
             03  D-BSN   PIC  X(006).
           02  D-PRN   PIC  N(020) VALUE
                "＊＊＊　　廃却不良　入力リスト　　＊＊＊".
           02  FILLER.
             03  D-SME05 PIC  N(017) VALUE
                  "＊＊＊　　他で加硫入力中　　＊＊＊".
             03  D-SME06 PIC  N(017) VALUE
                  "＊＊＊　　他で廃却入力中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(023) VALUE
                  "***  ﾀﾝｶ ｶﾞ ﾊｲｯﾃﾅｲ  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  ｾｲﾋﾝｼｲﾚ ｴﾗｰ  ***".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
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
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "20" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "W-L" "6" "6" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "X" "W-L" "13" "5" "A-DATE" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NHC" "9" "W-L" "40" "2" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NHC" BY REFERENCE W-NHC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-L" "56" "4" "A-NHC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BS" "9" "W-L" "62" "2" "A-SU" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BS" BY REFERENCE W-BS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "74" "1" "A-BS" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "219" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "43" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "X" "W-L" "19" "20" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FRN" "N" "W-L" "43" "12" "D-NAME" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-FRN" BY REFERENCE KKB-FRN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZ-" "W-L" "56" "5" "D-FRN" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BSN" "X" "W-L" "65" "6" "D-SU" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-BSN" BY REFERENCE KKB-KSN2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN" "N" "1" "18" "40" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "15" "0" "136" "D-PRN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME05" "bN" "15" "18" "34" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME06" "bN" "15" "18" "34" "D-SME05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME13" "bN" "15" "18" "34" "D-SME06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME15" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "243" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "243" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "23" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "21" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "N" "24" "15" "44" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME90" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE D-NKNG TO W-KNG.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC15 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC13 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC05 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME05" D-SME05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC06 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME06" D-SME06 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE 1 TO KKB-SC06.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           MOVE ZERO TO W-D.
       M-10.
           CALL "SD_Screen_Output" USING "SCKD21" RETURNING RESU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 21
               GO TO M-10
           END-IF
           MOVE ZERO TO W-R.
           IF  WD-DATE NOT = ZERO
               MOVE WD-DATE TO W-DATE
               CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-03R TO W-NGPS
               CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-20
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-20
           END-IF
           IF  W-NGS NOT = W-KNG
               GO TO M-20
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-60
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  KH-GT1 = ZERO
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  KH-NC = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-NHC "A-NHC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-NHC < 1 OR > 30
               GO TO M-30
           END-IF
           MOVE SPACE TO KKB-KEY.
           MOVE 05 TO KKB-NO.
           MOVE W-NHC TO KKB-FRC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "D-FRN" D-FRN "p" RETURNING RESU.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  W-SU = ZERO
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-BS "A-BS" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           MOVE SPACE TO KKB-KEY.
           MOVE 04 TO KKB-NO.
           MOVE W-BS TO KKB-KS2.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "D-BSN" D-BSN "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-DMM = 9
               GO TO M-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-45
           END-IF
           MOVE ZERO TO KNH-R.
           MOVE KH-GT1 TO W-TN.
           COMPUTE W-KIN = W-SU * W-TN.
           MOVE KH-YC TO W-YC.
           MOVE W-R TO KNH-R.
      *           WRITE KNH-R.
      *//////////////
           CALL "DB_Insert" USING
            KNH-F_PNAME1 KNH-F_LNAME KNH-R RETURNING RET.
           MOVE W-DATE TO WD-DATE.
           GO TO M-15.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           MOVE ZERO TO W-D.
       M-65.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KNF-NHC = ZERO
               GO TO M-65
           END-IF
           IF  KNF-PRC NOT = 0
               GO TO M-65
           END-IF
           MOVE 5 TO W-PC.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-70.
           MOVE KNF-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " **  ﾏｽﾀｰ ﾅｼ  **    " TO KH-NAME
           END-IF
           MOVE SPACE TO W-P.
           IF  WD-DATE NOT = KNF-DATE
               MOVE KNF-NGPS TO P-DATE
           END-IF
           MOVE KNF-HCD TO P-HCD.
           MOVE KH-NAME TO P-NAME.
           MOVE KNF-NHC TO P-NHC.
           MOVE KNF-SU TO P-SU.
           MOVE KNF-TN TO P-T.
           MOVE KNF-KIN TO P-KIN.
           MOVE KNF-BS TO P-BS.
      *
           MOVE SPACE TO KKB-KEY.
           MOVE 05 TO KKB-NO.
           MOVE KNF-NHC TO KKB-FRC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "マスターなし" TO KKB-FRN
           END-IF
           MOVE KKB-FRN TO P-FRN.
      *
           MOVE SPACE TO KKB-KEY.
           MOVE 04 TO KKB-NO.
           MOVE KNF-BS TO KKB-KS2.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ﾏｽﾀ ﾅｼ" TO KKB-KSN2
           END-IF
           MOVE KKB-KSN2 TO P-BSN.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 9 TO KNF-PRC.
      *           REWRITE KNH-R.
      *///////////////
           CALL "DB_Update" USING
            KNH-F_PNAME1 KNH-F_LNAME KNH-R RETURNING RET.
           MOVE KNF-DATE TO WD-DATE.
       M-75.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KNF-NHC = ZERO
               GO TO M-75
           END-IF
           IF  KNF-PRC NOT = 0
               GO TO M-75
           END-IF
           GO TO M-70.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE 0 TO KKB-SC06.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           IF  W-PC NOT = ZERO
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
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
