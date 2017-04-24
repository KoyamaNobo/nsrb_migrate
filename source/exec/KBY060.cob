       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY060.
      *********************************************************
      *    PROGRAM         :  材料棚卸入力　　　　　　　　    *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  SCBY06                          *
      *        変更　　　  :  99/04/26                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　材料棚卸　入力リスト　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　棚　卸　数".
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　棚　卸　数".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  F            PIC  X(001).
             03  P-BSC1       PIC  9(002).
             03  F            PIC  X(001).
             03  P-KEY1       PIC  9(006).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  N(024).
             03  P-TSU1       PIC --,---,--9.99.
             03  F            PIC  X(002).
             03  P-X          PIC  X(001).
             03  F            PIC  X(002).
             03  F            PIC  X(001).
             03  P-BSC2       PIC  9(002).
             03  F            PIC  X(001).
             03  P-KEY2       PIC  9(006).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  N(024).
             03  P-TSU2       PIC --,---,--9.99.
       01  W-DATA.
           02  W-BSC          PIC  9(002).
           02  W-KEY          PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-IC           PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
           02  W-POC          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSPF.
      *FD  JTIF
       01  JTIF_KBY050.
           02  JTIF_PNAME1    PIC  X(004) VALUE "JTIF".
           02  F              PIC  X(001).
           02  JTIF_LNAME     PIC  X(011) VALUE "JTIF_KBY050".
           02  F              PIC  X(001).
           02  JTIF_KEY1      PIC  X(100) VALUE SPACE.
           02  JTIF_SORT      PIC  X(100) VALUE SPACE.
           02  JTIF_IDLST     PIC  X(100) VALUE SPACE.
           02  JTIF_RES       USAGE  POINTER.
       01  JTI-R.
           02  JTI-KEY        PIC  9(006).
           02  JTI-BSC        PIC  9(002).
           02  JTI-TSU        PIC S9(007)V9(02).
           02  F              PIC  X(003).
           02  JTI-PC         PIC  9(001).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　材料棚卸　入力リスト　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "全件=1  未作表分=5  作表しない=9    ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-APT.
           02  FILLER.
             03  A-BSC   PIC  9(002).
             03  A-KEY   PIC  9(006).
             03  A-TSU   PIC S9(007)V9(02).
             03  A-DMM   PIC  9(001).
           02  A-IC    PIC  9(001).
           02  A-CHK   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-NAME  PIC  N(024).
             03  D-TSU   PIC ZZZZZZ9.99- .
       01  C-ER.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ｻﾞｲｺ ｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  JTIF WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(022) VALUE
                  "***  JTIF DATA ﾅｼ  ***".
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
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "342" " " " " RETURNING RESU.
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
            "08C-MID" "X" "15" "10" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "19" "22" "08C-MID" " " RETURNING RESU.
      *C-APT
       CALL "SD_Init" USING 
            "C-APT" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-APT" " " "W-L" "0" "18" " " "C-APT" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BSC" "9" "W-L" "3" "2" " " "01C-APT" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BSC" BY REFERENCE W-BSC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "W-L" "7" "6" "A-BSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE JTI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TSU" "S9" "W-L" "63" "9" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TSU" BY REFERENCE JTI-TSU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "77" "1" "A-TSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IC" "9" "15" "45" "1" "01C-APT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IC" BY REFERENCE W-IC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "20" "36" "1" "A-IC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "59" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "14" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TSU" "ZZZZZZ9.99-" "W-L" "63" "11" "D-NAME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-TSU" BY REFERENCE JTI-TSU "9" "0" RETURNING RESU.
      *C-ER
       CALL "SD_Init" USING 
            "C-ER" " " "0" "0" "180" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ER" " " "24" "0" "180" " " "C-ER" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ER" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "22" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME4" " " RETURNING RESU.
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
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" JTIF_PNAME1 " " BY REFERENCE JTIF_IDLST "0".
           MOVE 20 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               CALL "SD_Screen_Output" USING "SCBY06" RETURNING RESU
               MOVE 6 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
      *
           INITIALIZE JTI-R.
           MOVE W-BSC TO JTI-BSC.
           CALL "SD_Output" USING "A-BSC" A-BSC "p" RETURNING RESU.
           GO TO M-20.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-BSC "A-BSC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-BSC = 99
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-50
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  JTI-KEY = ZERO
               GO TO M-20
           END-IF
      *
           MOVE JTI-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  J-ZC = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TSU "A-TSU" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-TSU" D-TSU "p" RETURNING RESU.
           IF  JTI-TSU = ZERO
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-20
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
      *           WRITE JTI-R.
      *//////////////
           CALL "DB_Insert" USING
            JTIF_PNAME1 JTIF_LNAME JTI-R RETURNING RET.
           GO TO M-10.
       M-50.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JTIF_IDLST JTIF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-IC "A-IC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-IC NOT = 1 AND 5 AND 9
               GO TO M-55
           END-IF
           IF  W-IC = 9
               GO TO M-95
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-CHK = 9
               GO TO M-55
           END-IF
           IF  W-CHK NOT = 1
               GO TO M-60
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" JTIF_PNAME1 " " BY REFERENCE JTIF_IDLST "0".
       M-65.
      *           READ JTIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JTIF_PNAME1 BY REFERENCE JTI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JTIF_IDLST JTIF_PNAME1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-IC = 5
               IF  JTI-PC NOT = 0
                   GO TO M-65
               END-IF
           END-IF
      *
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           PERFORM S-20 THRU S-30.
           MOVE ZERO TO W-LCD W-KEY.
           MOVE 99 TO W-BSC.
       M-70.
           PERFORM S-35 THRU S-40.
           MOVE JTI-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊　マスター　なし　＊　" TO J-NAME
           END-IF
           IF  JTI-PC = 0
               MOVE 1 TO JTI-PC
      *               REWRITE JTI-R
      *///////////////
               CALL "DB_Update" USING
                JTIF_PNAME1 JTIF_LNAME JTI-R RETURNING RET
           END-IF
           IF  W-CD NOT = 0
               GO TO M-75
           END-IF
           MOVE ":" TO P-X(W-LD).
           IF  W-BSC NOT = JTI-BSC
               MOVE JTI-BSC TO W-BSC
               MOVE JTI-KEY TO W-KEY
               MOVE JTI-BSC TO P-BSC1(W-LD)
               MOVE JTI-KEY TO P-KEY1(W-LD)
               MOVE J-NAME TO P-NAME1(W-LD)
           END-IF
           IF  W-KEY NOT = JTI-KEY
               MOVE JTI-KEY TO W-KEY
               MOVE JTI-KEY TO P-KEY1(W-LD)
               MOVE J-NAME TO P-NAME1(W-LD)
           END-IF
           MOVE JTI-TSU TO P-TSU1(W-LD).
           GO TO M-80.
       M-75.
           IF  W-BSC NOT = JTI-BSC
               MOVE JTI-BSC TO W-BSC
               MOVE JTI-KEY TO W-KEY
               MOVE JTI-BSC TO P-BSC2(W-LD)
               MOVE JTI-KEY TO P-KEY2(W-LD)
               MOVE J-NAME TO P-NAME2(W-LD)
           END-IF
           IF  W-KEY NOT = JTI-KEY
               MOVE JTI-KEY TO W-KEY
               MOVE JTI-KEY TO P-KEY2(W-LD)
               MOVE J-NAME TO P-NAME2(W-LD)
           END-IF
           MOVE JTI-TSU TO P-TSU2(W-LD).
       M-80.
      *           READ JTIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JTIF_PNAME1 BY REFERENCE JTI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  W-IC = 5
               IF  JTI-PC NOT = 0
                   GO TO M-80
               END-IF
           END-IF
           GO TO M-70.
       M-85.
           PERFORM S-45 THRU S-55.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JTIF_IDLST JTIF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-25.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO P-NAME1(W-LD) P-NAME2(W-LD)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-40
           END-IF
           MOVE 99 TO W-BSC.
           MOVE ZERO TO W-KEY.
           IF  W-CD = 0
               ADD 1 TO W-CD
               MOVE ZERO TO W-LD
               GO TO S-35
           END-IF
           PERFORM S-45 THRU S-55.
           PERFORM S-20 THRU S-30.
           MOVE ZERO TO W-LCD.
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-50.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-50
               END-IF
           END-IF.
       S-55.
           EXIT.
