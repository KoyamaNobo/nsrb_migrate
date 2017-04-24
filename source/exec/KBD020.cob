       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD020.
      *********************************************************
      *    PROGRAM         :  出庫伝票入力　　　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBD02                          *
      *        変更　　　  :  62/06/04                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　購買　出庫伝票　プルーフリスト　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(060) VALUE SPACE.
           02  F              PIC  X(003) VALUE ":  ".
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(059) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(003) VALUE "伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(001) VALUE "行".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "材　　料　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(005) VALUE " :   ".
           02  F              PIC  N(003) VALUE "伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(001) VALUE "行".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "材　　料　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  P-15K        PIC  X(005).
             03  P-LPD1.
               04  P-LDATE    PIC 99/99/99.
               04  F          PIC  X(002).
             03  P-LPD2  REDEFINES P-LPD1.
               04  F          PIC  X(002).
               04  P-LDNO     PIC  9(006).
               04  P-LV       PIC  X(001).
               04  P-LGNO     PIC  9(001).
             03  F            PIC  X(001).
             03  P-LJCD       PIC  9(006).
             03  F            PIC  X(001).
             03  P-LJNA       PIC  N(024).
             03  P-LSSU       PIC -----,--9.99.
             03  F            PIC  X(001).
             03  P-X          PIC  X(001).
             03  F            PIC  X(001).
             03  P-RPD1.
               04  P-RDATE    PIC 99/99/99.
               04  F          PIC  X(002).
             03  P-RPD2  REDEFINES P-RPD1.
               04  F          PIC  X(002).
               04  P-RDNO     PIC  9(006).
               04  P-RV       PIC  X(001).
               04  P-RGNO     PIC  9(001).
             03  F            PIC  X(001).
             03  P-RJCD       PIC  9(006).
             03  F            PIC  X(001).
             03  P-RJNA       PIC  N(024).
             03  P-RSSU       PIC -----,--9.99.
       01  W-R.
           02  WR-DATE        PIC  9(008).
           02  WR-JCD         PIC  9(006).
           02  WR-SSU         PIC S9(007)V9(02).
           02  WR-KEY.
             03  WR-DNO       PIC  9(006).
             03  WR-GNO       PIC  9(001).
           02  F              PIC  X(001).
           02  WR-PCNT        PIC  9(001).
       01  W-ARD.
           02  W-RD    OCCURS   8  PIC  X(032).
       01  W-DATA.
           02  W-DATE         PIC  9(008) VALUE ZERO.
           02  W-DATED REDEFINES W-DATE.
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
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-LP           PIC  9(002).
           02  W-CP           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-DNOD         PIC  9(006).
           02  W-DC           PIC  9(001).
           02  W-DCD          PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-LIST         PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-NGD          PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LIJM.
           COPY LSPF.
      *FD  HA-F
       01  HA-F_KBD020.
           02  HA-F_PNAME1    PIC  X(003) VALUE "HAF".
           02  F              PIC  X(001).
           02  HA-F_LNAME     PIC  X(011) VALUE "HA-F_KBD020".
           02  F              PIC  X(001).
           02  HA-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HA-F_KEY2      PIC  X(100) VALUE SPACE.
           02  HA-F_SORT      PIC  X(100) VALUE SPACE.
           02  HA-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HA-F_RES       USAGE  POINTER.
       01  HA-R.
           02  F              PIC  9(002).
           02  HA-DATE        PIC  9(006).
           02  HA-JCD         PIC  9(006).
           02  HA-SSU         PIC S9(007)V9(02).
           02  HA-KEY.
             03  HA-DNO       PIC  9(006).
             03  HA-GNO       PIC  9(001).
           02  F              PIC  X(001).
           02  HA-PCNT        PIC  9(001).
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-DNO   PIC  9(006).
           02  A-DATE  PIC  9(006).
           02  FILLER.
             03  A-JCD   PIC  9(006).
             03  A-SSU   PIC S9(007)V9(02).
           02  A-LIST  PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-JNA   PIC  N(024).
             03  D-SSU   PIC ZZZZZZ9.99- .
           02  C-PM.
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　購買　出庫伝票　入力リスト　　＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  X(040) VALUE
                  "全件=1  未作表分=5  作表しない=9    ﾘﾀｰﾝ".
             03  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-SPC.
           02  FILLER.
             03  S-JCD   PIC  X(006) VALUE "      ".
             03  S-JNA   PIC  X(048) VALUE
                  "                                   ".
             03  S-SSU   PIC  X(011) VALUE "           ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ｻﾞｲｺ ｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  DATA ｱﾘ  ***".
             03  E-ME6   PIC  X(018) VALUE
                  "***  KNNOM ﾅｼ  ***".
             03  E-ME7   PIC  X(023) VALUE
                  "***  HAF WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(025) VALUE
                  "***  HAF REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(024) VALUE
                  "***  HAF DALETE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  KBNOM REWRITE ｴﾗｰ  ***".
             03  E-ME72  PIC  N(013) VALUE
                  "日次更新後、入力して下さい".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "51" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "4" "67" "6" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "5" "13" "6" "A-DNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-L" "0" "13" "A-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "W-L" "6" "6" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE WR-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSU" "S9" "W-L" "62" "9" "A-JCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSU" BY REFERENCE WR-SSU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-LIST" "9" "14" "48" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-LIST" BY REFERENCE W-LIST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "46" "1" "A-LIST" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "443" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "59" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNA" "N" "W-L" "13" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-JNA" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSU" "ZZZZZZ9.99-" "W-L" "62" "11" "D-JNA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSU" BY REFERENCE WR-SSU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-PM" " " "0" "0" "384" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM" "N" "3" "10" "46" " " "C-PM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PM" "N" "4" "10" "46" "01C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-PM" "N" "5" "10" "46" "02C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-PM" "N" "6" "10" "46" "03C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-PM" "N" "7" "10" "46" "04C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-PM" "N" "8" "10" "46" "05C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-PM" "N" "9" "10" "46" "06C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-PM" "X" "14" "13" "40" "07C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-PM" "X" "20" "29" "22" "08C-PM" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "65" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SPC" " " "W-L" "0" "65" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-JCD" "X" "W-L" "6" "6" " " "01C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-JNA" "X" "W-L" "13" "48" "S-JCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SSU" "X" "W-L" "62" "11" "S-JNA" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "332" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "332" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "18" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "23" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "25" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "24" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME72" "N" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME72" " " RETURNING RESU.
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
       M-020.
           COPY LIBCPR.
           MOVE D-NBNG TO W-NGD.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 " " BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "03" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE BNO-DNO2 TO W-DNOD.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "I-O" HA-F_PNAME1 "SHARED" BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
           MOVE DATE-05R TO H-DATE.
           MOVE ZERO TO W-PC W-EC.
           CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-500
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO M-040
           END-IF
           IF  W-ACT = 1
               GO TO M-100
           END-IF.
       M-060.
           CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           MOVE SPACE TO HA-KEY.
           MOVE W-DNO TO HA-DNO.
      *           START HA-F KEY NOT < HA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HA-F_PNAME1 "HA-KEY" " NOT < " HA-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           IF  W-DNO NOT = HA-DNO
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           PERFORM S-20 THRU S-30.
           IF  W-ACT = 3
               GO TO M-300
           END-IF.
       M-100.
           MOVE ZERO TO W-R.
           IF  W-ACT = 1
               CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU
               COMPUTE W-DNO = BNO-DNO2 + 1
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU
               IF  W-DATE NOT = ZERO
                   CALL "SD_Output" USING
                    "A-DATE" A-DATE "p" RETURNING RESU
               END-IF
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-040
               ELSE
                   GO TO M-080
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-120
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-120
           END-IF
           IF  W-NGS NOT = W-NGD
               GO TO M-120
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-140.
           IF  W-ACT = 1
               CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU
               MOVE ZERO TO W-ARD
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-DC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-160.
           ADD 1 TO W-DC W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-DC = 9
               GO TO M-300
           END-IF
           MOVE W-RD(W-DC) TO W-R.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT = ADV
               MOVE W-DC TO W-DCD
               MOVE W-L TO W-LD
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           MOVE WR-JCD TO J-KEY.
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
               GO TO M-180
           END-IF
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           IF  J-ZC NOT = ZERO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-180
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-SSU "A-SSU" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           CALL "SD_Output" USING "D-SSU" D-SSU "p" RETURNING RESU.
           IF  WR-SSU = ZERO
               GO TO M-200
           END-IF
      *
           MOVE W-DATE TO WR-DATE.
           MOVE W-DNO TO WR-DNO.
           MOVE W-DC TO WR-GNO.
           MOVE W-R TO W-RD(W-DC).
           GO TO M-160.
       M-220.
           IF  W-DC = 1
               GO TO M-120
           END-IF
           SUBTRACT 1 FROM W-DC W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE W-RD(W-DC) TO W-R.
           GO TO M-200.
       M-240.
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE W-R TO W-RD(W-DCD).
           ADD 1 TO W-DCD W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-DCD NOT = 9
               GO TO M-240
           END-IF
           MOVE W-LD TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-080
               ELSE
                   GO TO M-220
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF
           IF  W-DMM = 9
               MOVE ZERO TO W-R
               IF  W-ACT = 1
                   GO TO M-140
               ELSE
                   CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU
                   GO TO M-080
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-300
           END-IF
           IF  W-ACT = 3
               GO TO M-400
           END-IF
           MOVE ZERO TO W-DC.
       M-320.
           ADD 1 TO W-DC.
           IF  W-DC = 9
               GO TO M-420
           END-IF
           MOVE W-RD(W-DC) TO W-R.
           IF  WR-DATE = ZERO
               IF  W-ACT = 2
                   GO TO M-380
               ELSE
                   GO TO M-420
               END-IF
           END-IF
           IF  W-ACT NOT = 1
               GO TO M-340
           END-IF
      *****************************************************************
           MOVE W-R TO HA-R.
      *           WRITE HA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HA-F_PNAME1 HA-F_LNAME HA-R RETURNING RET.
           IF  RET = 1
               GO TO M-325
           END-IF
           GO TO M-330.
       M-325.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-420.
       M-330.
           MOVE W-DNO TO BNO-DNO2.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-420
           END-IF
           GO TO M-320.
      *****************************************************************
       M-340.
           MOVE WR-KEY TO HA-KEY.
      *           READ HA-F INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-360
           END-IF
           MOVE W-R TO HA-R.
      *           REWRITE HA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HA-F_PNAME1 HA-F_LNAME HA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-420
           END-IF
           GO TO M-320.
       M-360.
           MOVE W-R TO HA-R.
      *           WRITE HA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HA-F_PNAME1 HA-F_LNAME HA-R RETURNING RET.
           IF  RET = 1
               GO TO M-370
           END-IF
           GO TO M-320.
       M-370.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-420.
       M-380.
           MOVE W-DNO TO HA-DNO.
           MOVE W-DC TO HA-GNO.
      *           START HA-F KEY NOT < HA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HA-F_PNAME1 "HA-KEY" " NOT < " HA-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-420
           END-IF
           PERFORM S-35 THRU S-40.
           GO TO M-420.
       M-400.
           MOVE ZERO TO HA-KEY.
           MOVE W-DNO TO HA-DNO.
      *           START HA-F KEY NOT < HA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HA-F_PNAME1 "HA-KEY" " NOT < " HA-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-420
           END-IF
           PERFORM S-35 THRU S-40.
       M-420.
           IF  W-EC = 5
               GO TO M-500
           END-IF
           MOVE ZERO TO W-R.
           IF  W-ACT = 1
               GO TO M-100
           END-IF
           GO TO M-060.
       M-500.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-PM" C-PM "p" RETURNING RESU.
       M-520.
           CALL "SD_Accept" USING BY REFERENCE A-LIST "A-LIST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-520
           END-IF
           IF  W-LIST NOT = 1 AND 5 AND 9
               GO TO M-520
           END-IF.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-520
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF
           IF  W-DMM = 9
               GO TO M-520
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-540
           END-IF
           IF  W-LIST = 9
               GO TO M-980
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HA-F_PNAME1 "SHARED" BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
       M-560.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  W-LIST = 5
               IF  HA-PCNT NOT = 0
                   GO TO M-560
               END-IF
           END-IF
           PERFORM S-85 THRU S-95.
           MOVE ZERO TO W-LP W-CP.
       M-580.
           MOVE HA-DNO TO W-DNO.
           MOVE 0 TO CHK.
       M-600.
           PERFORM S-45 THRU S-65.
           IF  HA-PCNT NOT = 9
               ADD 1 TO HA-PCNT
           END-IF
      *           REWRITE HA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HA-F_PNAME1 HA-F_LNAME HA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-620.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF  W-LIST = 5
               IF  HA-PCNT NOT = 0
                   GO TO M-620
               END-IF
           END-IF
           IF  HA-DNO = W-DNO
               GO TO M-600
           END-IF
           GO TO M-580.
       M-900.
           PERFORM S-70 THRU S-80.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
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
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           CALL "SD_Screen_Output" USING "SCBD02" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
           MOVE ZERO TO W-ARD W-DC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-25.
           ADD 1 TO W-DC W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-DC = 9
               GO TO S-30
           END-IF
           MOVE ZERO TO W-R.
           MOVE HA-R TO W-RD(W-DC) W-R.
           IF  W-DC = 1
               MOVE WR-DATE TO W-DATE
               CALL "SD_Output" USING
                "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           MOVE WR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　材料マスター　なし　　＊＊　" TO J-NAME
           END-IF
           CALL "SD_Output" USING "A-JCD" A-JCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SSU" D-SSU "p" RETURNING RESU.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
           IF  W-DNO = HA-DNO
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           IF  W-DNO NOT = HA-DNO
               GO TO S-40
           END-IF
      *           DELETE HA-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HA-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO S-40
           END-IF
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           IF  CHK NOT = 0
               GO TO S-55
           END-IF
           ADD 1 TO W-LP.
           IF  W-LP < 58
               GO TO S-50
           END-IF
           ADD 1 TO W-CP.
           IF  W-CP NOT = 2
               MOVE ZERO TO W-LP
               GO TO S-45
           END-IF
           PERFORM S-70 THRU S-80.
           PERFORM S-85 THRU S-95.
           MOVE ZERO TO W-LP W-CP.
           GO TO S-45.
       S-50.
           MOVE 5 TO CHK.
           IF  W-CP = 0
               MOVE ":" TO P-X(W-LP)
               MOVE HA-DATE TO P-LDATE(W-LP)
           ELSE
               MOVE HA-DATE TO P-RDATE(W-LP)
           END-IF.
       S-55.
           ADD 1 TO W-LP.
           IF  W-LP < 59
               GO TO S-60
           END-IF
           ADD 1 TO W-CP.
           IF  W-CP NOT = 2
               MOVE ZERO TO W-LP CHK
               GO TO S-45
           END-IF
           PERFORM S-70 THRU S-80.
           PERFORM S-85 THRU S-95.
           MOVE ZERO TO W-LP W-CP CHK.
           GO TO S-45.
       S-60.
           MOVE HA-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　材料マスター　なし　　＊＊　" TO J-NAME
           END-IF
           IF  W-CP = 0
               MOVE HA-DNO TO P-LDNO(W-LP)
               MOVE "-" TO P-LV(W-LP)
               MOVE HA-GNO TO P-LGNO(W-LP)
               MOVE HA-JCD TO P-LJCD(W-LP)
               MOVE J-NAME TO P-LJNA(W-LP)
               MOVE HA-SSU TO P-LSSU(W-LP)
               MOVE ":" TO P-X(W-LP)
           ELSE
               MOVE ":" TO P-X(W-LP)
               MOVE HA-DNO TO P-RDNO(W-LP)
               MOVE "-" TO P-RV(W-LP)
               MOVE HA-GNO TO P-RGNO(W-LP)
               MOVE HA-JCD TO P-RJCD(W-LP)
               MOVE J-NAME TO P-RJNA(W-LP)
               MOVE HA-SSU TO P-RSSU(W-LP)
           END-IF.
       S-65.
           EXIT.
       S-70.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LP.
       S-75.
           ADD 1 TO W-LP.
           IF  W-LP NOT = 59
               IF  P-X(W-LP) = ":"
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LP) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-75
               END-IF
           END-IF.
       S-80.
           EXIT.
       S-85.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LP.
       S-90.
           ADD 1 TO W-LP.
           IF  W-LP NOT = 59
               MOVE W-15K TO P-15K(W-LP)
               MOVE SPACE TO P-LJNA(W-LP) P-RJNA(W-LP)
               GO TO S-90
           END-IF.
       S-95.
           EXIT.
