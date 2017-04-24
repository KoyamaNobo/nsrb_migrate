       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG990.
      *********************************************************
      *    PROGRAM         : 　問合せ用年間ワーク作成         *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  96/12/17                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
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
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
           02  W-KIN          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  SNTR-F
       01  SNTR-F_HMG991.
           02  SNTR-F_PNAME1  PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMG991".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NG      PIC  9(006).
             03  SNTR-NGD  REDEFINES SNTR-NG.
               04  SNTR-NEN   PIC  9(004).
               04  SNTR-GET   PIC  9(002).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(031).
           02  SNTR-SU        PIC S9(005).
           02  SNTR-BT        PIC S9(005).
           02  SNTR-UKIN      PIC S9(008).
           02  F              PIC  X(001).
           02  SNTR-DC        PIC  9(001).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(003).
           02  SNTR-BC.
             03  SNTR-BC1     PIC  9(002).
             03  SNTR-BC2     PIC  9(002).
             03  SNTR-BC3     PIC  9(002).
           02  SNTR-BCD   REDEFINES SNTR-BC.
             03  SNTR-BCD1    PIC  9(003).
             03  F            PIC  9(003).
           02  F              PIC  X(001).
           02  SNTR-TNC       PIC  9(002).
           02  SNTR-FKC       PIC  9(002).
           02  F              PIC  X(019).
           02  SNTR-SKD       PIC  9(008).
           02  SNTR-BMC       PIC  9(002).
           02  SNTR-BMNO      PIC  9(001).
           02  F              PIC  X(002).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TZNTP-M
       01  TZNTP-M_HMG991.
           02  TZNTP-M_PNAME1 PIC  X(006) VALUE "TZNTPM".
           02  F              PIC  X(001).
           02  TZNTP-M_LNAME  PIC  X(014) VALUE "TZNTP-M_HMG991".
           02  F              PIC  X(001).
           02  TZNTP-M_KEY1   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_KEY2   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_SORT   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_IDLST  PIC  X(100) VALUE SPACE.
           02  TZNTP-M_RES    USAGE  POINTER.
       01  TZNTP-R.
           02  TZNTP-KEY.
             03  TZNTP-TCD    PIC  9(004).
             03  TZNTP-IKC    PIC  9(001).
           02  TZNTP-OUD      PIC  X(108).
           02  TZNTP-NUD.
             03  TZNTP-NU05   PIC S9(009).
             03  TZNTP-NU06   PIC S9(009).
             03  TZNTP-NU07   PIC S9(009).
             03  TZNTP-NU08   PIC S9(009).
             03  TZNTP-NU09   PIC S9(009).
             03  TZNTP-NU10   PIC S9(009).
             03  TZNTP-NU11   PIC S9(009).
             03  TZNTP-NU12   PIC S9(009).
             03  TZNTP-NU01   PIC S9(009).
             03  TZNTP-NU02   PIC S9(009).
             03  TZNTP-NU03   PIC S9(009).
             03  TZNTP-NU04   PIC S9(009).
           02  TZNTP-TUD.
             03  TZNTP-OTU    PIC S9(010).
             03  TZNTP-NTU    PIC S9(010).
           02  TZNTP-OAD      PIC  X(108).
           02  TZNTP-NAD.
             03  TZNTP-NA05   PIC S9(009).
             03  TZNTP-NA06   PIC S9(009).
             03  TZNTP-NA07   PIC S9(009).
             03  TZNTP-NA08   PIC S9(009).
             03  TZNTP-NA09   PIC S9(009).
             03  TZNTP-NA10   PIC S9(009).
             03  TZNTP-NA11   PIC S9(009).
             03  TZNTP-NA12   PIC S9(009).
             03  TZNTP-NA01   PIC S9(009).
             03  TZNTP-NA02   PIC S9(009).
             03  TZNTP-NA03   PIC S9(009).
             03  TZNTP-NA04   PIC S9(009).
           02  TZNTP-TAD.
             03  TZNTP-OTA    PIC S9(010).
             03  TZNTP-NTA    PIC S9(010).
           02  TZNTP-NG.
             03  TZNTP-NEN    PIC  9(004).
             03  TZNTP-GET    PIC  9(002).
           02  TZNTP-TNC      PIC  9(002).
           02  TZNTP-BC       PIC  9(001).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID1.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　得意先年間ファイル　更新（問合せ）　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(026) VALUE
                  "***  TZNTPM WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(028) VALUE
                  "***  TZNTPM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(019) VALUE
                  "***  TZNTPM ﾅｼ  ***".
             03  E-ME7   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
             03  E-HCD   PIC  9(006).
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
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "350" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "3" "10" "50" " " "C-MID1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "N" "4" "10" "50" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "N" "5" "10" "50" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "N" "6" "10" "50" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" "N" "7" "10" "50" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID1" "N" "8" "10" "50" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID1" "N" "9" "10" "50" "06C-MID1" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "100" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "100" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "28" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "19" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "16" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "5" "E-ME7" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TZNTP-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "45" "6" "E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE SNTR-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
      *
           MOVE ZERO TO W-DATE.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-GET < 5
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE 5 TO W-GET.
      *
           CALL "DB_F_Open" USING
            "I-O" TZNTP-M_PNAME1 " " BY REFERENCE TZNTP-M_IDLST "1"
            "TZNTP-KEY" BY REFERENCE TZNTP-KEY.
       M-06.
      *           READ TZNTP-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-07
           END-IF
           IF  TZNTP-BC NOT = 0
               GO TO M-06
           END-IF
           MOVE ZERO TO TZNTP-NAD TZNTP-NTA.
      *           REWRITE TZNTP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-06.
       M-07.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
      *
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-NG < W-NG
               GO TO M-10
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           IF  SNTR-SKD = 99999999
               GO TO M-10
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-10
           END-IF
           COMPUTE W-KIN = W-UKIN - W-GKIN.
           IF  ZERO = W-UKIN AND W-KIN
               GO TO M-10
           END-IF
           PERFORM MDR-RTN THRU MDR-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DST-RTN.
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SNTR-FT TO HI-YG
           END-IF
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 0) AND (SNTR-DC NOT = 2)
               IF  HI-YG NOT = ZERO
                   COMPUTE W-GKIN = SNTR-SU * HI-YG
               ELSE
                   COMPUTE W-GKIN = SNTR-SU * SNTR-FT
               END-IF
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-UKIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-UKIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.
       MDR-RTN.
           MOVE ZERO TO TZNTP-R.
           MOVE SNTR-TCD TO TZNTP-TCD.
           IF (SNTR-BCD1 NOT = 322) OR (SNTR-BC3 NOT = 30)
               MOVE 1 TO TZNTP-IKC
           END-IF
           IF  SNTR-BC3 = 30
               MOVE 2 TO TZNTP-IKC
           END-IF
           IF  SNTR-BCD1 = 322
               MOVE 3 TO TZNTP-IKC
           END-IF
      *           READ TZNTP-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MDR-EX
           END-IF
           PERFORM SET-RTN THRU SET-EX.
      *           REWRITE TZNTP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       MDR-EX.
           EXIT.
       SET-RTN.
           IF  SNTR-GET = 5
               ADD W-KIN TO TZNTP-NA05 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 6
               ADD W-KIN TO TZNTP-NA06 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 7
               ADD W-KIN TO TZNTP-NA07 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 8
               ADD W-KIN TO TZNTP-NA08 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 9
               ADD W-KIN TO TZNTP-NA09 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 10
               ADD W-KIN TO TZNTP-NA10 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 11
               ADD W-KIN TO TZNTP-NA11 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 12
               ADD W-KIN TO TZNTP-NA12 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 1
               ADD W-KIN TO TZNTP-NA01 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 2
               ADD W-KIN TO TZNTP-NA02 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 3
               ADD W-KIN TO TZNTP-NA03 TZNTP-NTA
           END-IF
           IF  SNTR-GET = 4
               ADD W-KIN TO TZNTP-NA04 TZNTP-NTA
           END-IF.
       SET-EX.
           EXIT.
