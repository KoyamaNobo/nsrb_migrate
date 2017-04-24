       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG910.
      *********************************************************
      *    PROGRAM         :  月次更新　累積　　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-NS         PIC S9(006)V9(02).
           02  W-SK         PIC S9(008).
           02  W-YS         PIC S9(006)V9(02).
           02  W-YK         PIC S9(008).
           02  W-NG         PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN      PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1   PIC  9(002).
               04  W-NEN2   PIC  9(002).
             03  W-GET      PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F          PIC  9(002).
             03  W-NGS      PIC  9(004).
           02  W-FILE       PIC  X(013).
       01  ERR-STAT         PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT1.
      *FD  KHTMYR
       01  KHTMYR_KHG910.
           02  KHTMYR_PNAME1  PIC  X(006) VALUE "KHTMYR".
           02  F              PIC  X(001).
           02  KHTMYR_LNAME   PIC  X(013) VALUE "KHTMYR_KHG910".
           02  F              PIC  X(001).
           02  KHTMYR_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTMYR_KEY2    PIC  X(100) VALUE SPACE.
           02  KHTMYR_SORT    PIC  X(100) VALUE SPACE.
           02  KHTMYR_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTMYR_RES     USAGE  POINTER.
       01  KHTMY-R.
           02  KHTMY-KEYD.
             03  KHTMY-YC     PIC  9(002).
             03  KHTMY-NC     PIC  9(001).
             03  KHTMY-KEY    PIC  X(005).
           02  KHTMY-KSU      PIC S9(006)V9(02).
           02  KHTMY-HSU      PIC S9(006)V9(02).
           02  KHTMY-ISU      PIC S9(006)V9(02).
           02  KHTMY-KKIN     PIC S9(008).
           02  KHTMY-SSU      PIC S9(006)V9(02).
           02  KHTMY-UKIN     PIC S9(008).
           02  KHTMY-NKIN     PIC S9(007).
           02  KHTMY-GKIN     PIC S9(008).
           02  KHTMY-ZSU      PIC S9(006)V9(02).
           02  KHTMY-ZKIN     PIC S9(008).
           02  KHTMY-AZS      PIC S9(006).
           02  KHTMY-AAS      PIC S9(006).
           02  KHTMY-AUS      PIC S9(006).
           02  KHTMY-ASS      PIC S9(006).
           02  KHTMY-AC       PIC  9(001).
           02  KHTMY-TSY.
             03  KHTMY-TTG    PIC S9(006).
             03  KHTMY-TYG    PIC S9(006).
           02  KHTMY-KIS      PIC  9(001).
           02  KHTMY-KCO      PIC  X(005).
           02  KHTMY-JTS      PIC S9(006)V9(02).
           02  KHTMY-TTS      PIC S9(006)V9(02).
           02  F              PIC  X(018).
           02  KHTMY-NG       PIC  9(006).
       77  F                  PIC  X(001).
      *FD  KHMYR
       01  KHMYR_KHG910.
           02  KHMYR_PNAME1   PIC  X(005) VALUE "KHMYR".
           02  F              PIC  X(001).
           02  KHMYR_LNAME    PIC  X(012) VALUE "KHMYR_KHG910".
           02  F              PIC  X(001).
           02  KHMYR_KEY1     PIC  X(100) VALUE SPACE.
           02  KHMYR_KEY2     PIC  X(100) VALUE SPACE.
           02  KHMYR_SORT     PIC  X(100) VALUE SPACE.
           02  KHMYR_IDLST    PIC  X(100) VALUE SPACE.
           02  KHMYR_RES      USAGE  POINTER.
       01  KHMY-R.
           02  KHMY-KEY.
             03  KHMY-KEY1  PIC  X(002).
             03  KHMY-KEY2  PIC  9(003).
           02  KHMY-NAME    PIC  X(020).
           02  KHMY-YC      PIC  9(002).
           02  KHMY-TGM     PIC  9(004)V9(02).
           02  KHMY-TKN     PIC  9(004)V9(02).
           02  KHMY-TSZ     PIC  9(002)V9(02).
           02  KHMY-MGS     PIC  9(001)V9(03).
           02  KHMY-MKM     PIC  9(001)V9(03).
           02  KHMY-MKH     PIC  9(001)V9(03).
           02  KHMY-MTS     PIC  9(001)V9(03).
           02  KHMY-MKR     PIC  9(002)V9(03).
           02  KHMY-KKH     PIC  9(004)V9(02).
           02  KHMY-SBB     PIC  9(002)V9(02).
           02  KHMY-STS     PIC  9(002)V9(02).
           02  KHMY-SNE     PIC  9(002)V9(02).
           02  KHMY-SKP     PIC  9(002)V9(02).
           02  KHMY-SKY     PIC  9(002)V9(02).
           02  KHMY-SMK     PIC  9(002)V9(02).
           02  KHMY-SPK     PIC  9(002)V9(02).
           02  KHMY-SKG     PIC  9(002)V9(02).
           02  KHMY-SAN     PIC  9(002)V9(02).
           02  KHMY-SET     PIC  9(002)V9(02).
           02  KHMY-SST     PIC  9(003)V9(02).
           02  KHMY-DRH     PIC  9(003)V9(02).
           02  KHMY-KPS     PIC  9(003)V9(02).
           02  KHMY-SKH     PIC  9(002)V9(02).
           02  KHMY-SHY     PIC  9(002)V9(02).
           02  KHMY-T1      PIC  9(006)V9(02).
           02  KHMY-T2      PIC  9(006)V9(02).
           02  KHMY-KIS     PIC  9(001).
           02  KHMY-SYS     PIC  9(003).
           02  KHMY-TRS     PIC  9(002).
           02  KHMY-MS      PIC  9(001).
           02  KHMY-KCO     PIC  X(005).
           02  KHMY-USG     PIC  9(004)V9(02).
           02  F            PIC  X(069).
           02  KHMY-NG      PIC  9(006).
           02  KHMY-ENG     PIC  9(004).
           02  KHMY-ADD     PIC  9(004).
           02  KHMY-COD     PIC  9(006).
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
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　工品　年間累積　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "[      年    月分　　  ]".
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME4   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  KHMYR WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  KHTMYR WRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(005).
             03  E-HCD   PIC  X(005).
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
            "C-MID" " " "0" "0" "262" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "X" "3" "10" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "4" "10" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "10" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "6" "10" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "7" "10" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "8" "10" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "9" "10" "34" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "13" "15" "24" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "13" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "13" "19" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "9" "13" "25" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "60" "5" "E-ME8" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "X" "24" "60" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 " " BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
       M-10.
      *           READ KHT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
       M-15.
           INITIALIZE KHTMY-R.
           MOVE KHT-R TO KHTMY-R.
           MOVE W-NG TO KHTMY-NG.
      *           WRITE KHTMY-R.
      *//////////////
           CALL "DB_Insert" USING
            KHTMYR_PNAME1 KHTMYR_LNAME KHTMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1.
           MOVE "KHTMYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
           GO TO M-15.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" KHMYR_PNAME1 " " BY REFERENCE KHMYR_IDLST "0".
       M-35.
      *           READ KH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF.
       M-40.
           INITIALIZE KHMY-R.
           MOVE KH-R TO KHMY-R.
           MOVE W-NG TO KHMY-NG.
      *           WRITE KHMY-R.
      *//////////////
           CALL "DB_Insert" USING
            KHMYR_PNAME1 KHMYR_LNAME KHMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHMYR_IDLST KHMYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KHMYR_IDLST KHMYR_PNAME1.
           MOVE "KHMYR        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" KHMYR_PNAME1 " " BY REFERENCE KHMYR_IDLST "0".
           GO TO M-40.
       M-45.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHMYR_IDLST KHMYR_PNAME1.
      *
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
