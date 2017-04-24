       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     HMD810.
      **********************************************
      ******    出荷指図　売上未計上リスト    ******
      **********************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA  DIVISION.
       WORKING-STORAGE     SECTION.
       77  WK0128ID             PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1          PIC  X(003).
           02  STN-NO2          PIC  X(003).
       01  W-FID.
           02  W-FID1           PIC  X(006) VALUE "WK0128".
           02  W-FID2           PIC  X(003).
       01  HEAD1.
           02  F                PIC  X(005) VALUE X"1A24212474".
           02  F                PIC  N(002) VALUE "［　".
           02  H-SNEN           PIC  9(002).
           02  F                PIC  X(001) VALUE "/".
           02  H-SGET           PIC  9(002).
           02  F                PIC  X(001) VALUE "/".
           02  H-SPEY           PIC  9(002).
           02  F                PIC  X(004) VALUE  " ～ ".
           02  H-ENEN           PIC  9(002).
           02  F                PIC  X(001) VALUE "/".
           02  H-EGET           PIC  9(002).
           02  F                PIC  X(001) VALUE "/".
           02  H-EPEY           PIC  9(002).
           02  F                PIC  N(002) VALUE "　］".
           02  F                PIC  X(010) VALUE SPACE.
           02  F                PIC  N(023) VALUE
                 "＊＊＊　　出荷指図　売上未計上リスト　　＊＊＊".
           02  F                PIC  X(011) VALUE SPACE.
           02  F                PIC  X(005) VALUE "DATE ".
           02  H-DATE           PIC 99B99B99.
           02  F                PIC  X(007) VALUE "     P.".
           02  H-PAGE           PIC ZZ9.
       01  HEAD2.
           02  F                PIC  X(005) VALUE X"1A24212078".
           02  F                PIC  X(001) VALUE SPACE.
           02  F                PIC  N(004) VALUE "　日　付".
           02  F                PIC  X(005) VALUE SPACE.
           02  F                PIC  X(007) VALUE "ｺｰﾄﾞ   ".
           02  F                PIC  N(008) VALUE
                 "得　意　先　名　".
           02  F                PIC  X(028) VALUE SPACE.
           02  F                PIC  N(008) VALUE
                 "直　送　先　名　".
           02  F                PIC  X(028) VALUE SPACE.
           02  F                PIC  N(002) VALUE "倉庫".
           02  F                PIC  X(003) VALUE SPACE.
           02  F                PIC  N(002) VALUE "個数".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(004) VALUE "指図№　".
       01  HEAD3.
           02  F                PIC  X(010) VALUE SPACE.
           02  F                PIC  N(002) VALUE "伝区".
           02  F                PIC  X(009) VALUE "   ｺｰﾄﾞ  ".
           02  F                PIC  N(006) VALUE
                 "品　　　名　".
           02  F                PIC  X(028) VALUE SPACE.
           02  F                PIC  X(003) VALUE "1  ".
           02  F                PIC  N(002) VALUE "３号".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "２号".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "１号".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "０号".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "　中".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "　大".
           02  F                PIC  X(002) VALUE SPACE.
           02  F                PIC  N(002) VALUE "特大".
           02  F                PIC  X(015) VALUE
                   " 28.0 29.0 30.0".
           02  F                PIC  X(005) VALUE SPACE.
           02  F                PIC  N(002) VALUE "合計".
       01  HEAD4.
           02  F                PIC  X(059) VALUE SPACE.
           02  F                PIC  X(027) VALUE
                   "2 12.5 13.0 13.5 14.0 15.0 ".
           02  F                PIC  X(024) VALUE
                   "16.0 17.0 18.0 19.0 20.0".
       01  HEAD5.
           02  F                PIC  X(059) VALUE SPACE.
           02  F                PIC  X(027) VALUE
                   "3 21.0 21.5 22.0 22.5 23.0 ".
           02  F                PIC  X(019) VALUE
                   "23.5 24.0 24.5 25.0".
       01  HEAD6.
           02  F                PIC  X(059) VALUE SPACE.
           02  F                PIC  X(027) VALUE
                   "4 24.0 24.5 25.0 25.5 26.0 ".
           02  F                PIC  X(014) VALUE
                   "26.5 27.0 27.5".
       01  W-P1.
           02  P-Y              PIC  9(002).
           02  P-X1             PIC  X(001).
           02  P-M              PIC  9(002).
           02  P-X2             PIC  X(001).
           02  P-D              PIC  9(002).
           02  F                PIC  X(002).
           02  P-TCD            PIC  9(004).
           02  P-X3             PIC  X(001).
           02  P-CNO            PIC  9(003).
           02  F                PIC  X(001).
           02  P-TNAME          PIC  N(026).
           02  F                PIC  X(001).
           02  P-CNAME          PIC  N(026).
           02  F                PIC  X(001).
           02  P-W1             PIC  X(001).
           02  P-KRCD           PIC  9(001).
           02  P-W2             PIC  X(001).
           02  F                PIC  X(001).
           02  P-W3             PIC  X(001).
           02  P-KS             PIC ZZ9.
           02  P-W4             PIC  X(001).
           02  F                PIC  X(001).
           02  P-W9             PIC  X(001).
           02  P-SSNO           PIC  9(006).
           02  P-W10            PIC  X(001).
       01  W-P2.
           02  F                PIC  X(012).
           02  P-DK             PIC  9(001).
           02  F                PIC  X(002).
           02  P-HCD            PIC  9(006).
           02  F                PIC  X(001).
           02  P-HNAME          PIC  N(024).
           02  F                PIC  X(001).
           02  P-SIZ            PIC  9(001).
           02  P-SIZ1           PIC -----.
           02  P-SIZ2           PIC -----.
           02  P-SIZ3           PIC -----.
           02  P-SIZ4           PIC -----.
           02  P-SIZ5           PIC -----.
           02  P-SIZ6           PIC -----.
           02  P-SIZ7           PIC -----.
           02  P-SIZ8           PIC -----.
           02  P-SIZ9           PIC -----.
           02  P-SIZ10          PIC -----.
           02  P-KEI            PIC ----,---.
       01  W-P3.
           02  F                PIC  X(059).
           02  P-TEKI           PIC  N(024).
           02  F                PIC  X(009).
           02  P-K              PIC  N(002).
           02  F                PIC  X(003).
           02  P-GKEI           PIC ----,---.
       01  ERR-STAT             PIC  X(002).
       01  W-DATA.
           02  W-SNGP.
             03  W-SNEN         PIC  9(004).
             03  W-SNEND REDEFINES W-SNEN.
               04  W-SNEN1      PIC  9(002).
               04  W-SNEN2      PIC  9(002).
             03  W-SGET         PIC  9(002).
             03  W-SPEY         PIC  9(002).
           02  W-SNGPD REDEFINES W-SNGP.
             03  F              PIC  9(002).
             03  W-SNGS         PIC  9(004).
             03  F              PIC  9(002).
           02  W-ENGP.
             03  W-ENEN         PIC  9(004).
             03  W-ENEND REDEFINES W-ENEN.
               04  W-ENEN1      PIC  9(002).
               04  W-ENEN2      PIC  9(002).
             03  W-EGET         PIC  9(002).
             03  W-EPEY         PIC  9(002).
           02  W-ENGPD REDEFINES W-ENGP.
             03  F              PIC  9(002).
             03  W-ENGPS        PIC  9(006).
           02  W-KPC            PIC  9(001).
           02  W-DMM            PIC  9(001).
           02  W-BCO            PIC  9(001).
           02  W-HYMD.
             03  W-HY           PIC  9(004).
             03  W-HYD   REDEFINES W-HY.
               04  W-HY1        PIC  9(002).
               04  W-HY2        PIC  9(002).
             03  W-HM           PIC  9(002).
             03  W-HD           PIC  9(002).
           02  W-PAGE           PIC  9(003).
           02  W-SSNO           PIC  9(006).
           02  W-DK             PIC  9(001).
           02  W-12.
             03  W-121.
               04  W-1211       PIC S9(004).
               04  W-1212       PIC S9(004).
               04  W-1213       PIC S9(004).
               04  W-1214       PIC S9(004).
               04  W-1215       PIC S9(004).
               04  W-1216       PIC S9(004).
               04  W-1217       PIC S9(004).
               04  W-1218       PIC S9(004).
               04  W-1219       PIC S9(004).
               04  W-12110      PIC S9(004).
             03  W-122          PIC S9(006).
           02  W-GKEI           PIC S9(006).
           COPY  LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LIHSMS.
           COPY LSPF.
      *FD  HSMSW-F
       01  HSMSW-F_HMD810.
           02  HSMSW-F_PNAME1 PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HSMSW-F_LNAME  PIC  X(014) VALUE "HSMSW-F_HMD810".
           02  F              PIC  X(001).
           02  HSMSW-F_KEY1   PIC  X(100) VALUE SPACE.
           02  HSMSW-F_SORT   PIC  X(100) VALUE SPACE.
           02  HSMSW-F_IDLST  PIC  X(100) VALUE SPACE.
           02  HSMSW-F_RES    USAGE  POINTER.
       01  HSMSW-R1.
           02  HSMSW-KEY.
             03  HSMSW-01       PIC  9(006).
             03  HSMSW-01D   REDEFINES HSMSW-01.
               04  HSMSW-011    PIC  9(001).
               04  HSMSW-012    PIC  9(005).
             03  HSMSW-02       PIC  9(001).
           02  HSMSW-03         PIC  9(001).
           02  HSMSW-05.
             03  HSMSW-051      PIC  9(004).
             03  HSMSW-051D  REDEFINES HSMSW-051.
               04  HSMSW-0511   PIC  9(002).
               04  HSMSW-0512   PIC  9(002).
             03  HSMSW-052      PIC  9(002).
             03  HSMSW-053      PIC  9(002).
           02  HSMSW-06.
             03  HSMSW-061      PIC  9(004).
             03  HSMSW-062      PIC  9(003).
           02  HSMSW-07         PIC  9(001).
           02  HSMSW-09.
             03  HSMSW-091      PIC  9(004).
             03  HSMSW-092      PIC  9(002).
           02  HSMSW-10         PIC  9(001).
           02  HSMSW-12.
             03  HSMSW-121.
               04  HSMSW-1211   PIC S9(004).
               04  HSMSW-1212   PIC S9(004).
               04  HSMSW-1213   PIC S9(004).
               04  HSMSW-1214   PIC S9(004).
               04  HSMSW-1215   PIC S9(004).
               04  HSMSW-1216   PIC S9(004).
               04  HSMSW-1217   PIC S9(004).
               04  HSMSW-1218   PIC S9(004).
               04  HSMSW-1219   PIC S9(004).
               04  HSMSW-12110  PIC S9(004).
             03  HSMSW-122      PIC S9(006).
           02  HSMSW-13         PIC  9(001).
           02  HSMSW-14         PIC S9(003).
           02  HSMSW-21         PIC  9(001).
           02  HSMSW-20         PIC  9(002).
           02  HSMSW-16         PIC  9(002).
           02  HSMSW-17         PIC  9(005).
           02  HSMSW-18         PIC  9(008).
           02  FILLER           PIC  X(028).
           02  HSMSW-19         PIC  9(001).
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
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　出荷指図　売上未計上リスト　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(034) VALUE
                "  年   月   日  ～　  年   月   日".
           02  FILLER  PIC  X(034) VALUE
                "部別改頁   しない=0 , する=1   ( )".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  D-NGP.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-KPC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
               03  E-ME1   PIC  X(017) VALUE
                    "***  DATA ﾅｼ  ***".
               03  E-STAT  PIC  X(002).
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE    DIVISION.
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "412" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "20" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "20" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "20" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "20" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "20" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "20" "46" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "20" "46" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "26" "34" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "26" "34" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "39" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "14" "0" "14" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "14" "26" "2" " " "D-NGP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "14" "31" "2" "A-SNEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "14" "36" "2" "A-SGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "14" "46" "2" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "14" "51" "2" "A-ENEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "14" "56" "2" "A-EGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KPC" "9" "16" "58" "1" "D-NGP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KPC" BY REFERENCE W-KPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "56" "1" "A-KPC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "29" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "29" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-SNGP W-ENGP.
           MOVE D-NHNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           MOVE 1 TO W-SPEY.
           ACCEPT W-ENGPS FROM DATE.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
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
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SGET = ZERO
               IF  W-SNEN2 = ZERO
                   MOVE ZERO TO W-SNEN
                   GO TO M-20
               END-IF
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SPEY = ZERO
               IF  W-SGET = ZERO
                   GO TO M-21
               END-IF
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-20
           END-IF.
       M-21.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-21
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       M-22.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-21
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-22
           END-IF
           IF  W-EGET = 99
               IF  W-ENEN2 = 99
                   MOVE 9999 TO W-ENEN
                   GO TO M-23
               END-IF
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-22
           END-IF.
       M-23.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-22
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-23
           END-IF
           IF  W-EPEY = 99
               IF  W-EGET = 99
                   GO TO M-24
               END-IF
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-23
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO M-10
           END-IF.
       M-24.
           CALL "SD_Accept" USING BY REFERENCE A-KPC "A-KPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-23
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-24
           END-IF
           IF  W-KPC > 1
               GO TO M-24
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-24
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HSMSW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSMSW-F_PNAME1 " " BY REFERENCE HSMSW-F_IDLST "0".
       M-30.
      *           READ HSMSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSMSW-F_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSW-F_IDLST HSMSW-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-99
           END-IF
           IF  HSMSW-19 NOT = 0
               GO TO M-30
           END-IF
           IF  HSMSW-061 = 5358 OR 5350 OR 5349
               GO TO M-30
           END-IF
           IF  HSMSW-05 < W-SNGP OR > W-ENGP
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SNEN2 TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-SPEY TO H-SPEY.
           MOVE W-ENEN2 TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           MOVE W-EPEY TO H-EPEY.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
       M-34.
           MOVE HSMSW-011 TO W-BCO.
       M-35.
           MOVE HSMSW-01 TO W-SSNO.
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNAME P-CNAME.
           MOVE "/" TO P-X1 P-X2.
           MOVE "(" TO P-W1 P-W3 P-W9.
           MOVE ")" TO P-W2 P-W4 P-W10.
           MOVE HSMSW-0512 TO P-Y.
           MOVE HSMSW-052 TO P-M.
           MOVE HSMSW-053 TO P-D.
           MOVE HSMSW-061 TO P-TCD.
           MOVE "-" TO P-X3.
           MOVE HSMSW-062 TO P-CNO.
           MOVE HSMSW-061 TO T-KEY
      *           READ T-M UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME.
           MOVE T-NAME TO P-TNAME.
           MOVE HSMSW-06 TO TC-KEY
      *           READ TC-M UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF
           IF  HSMSW-062 < 002
               MOVE SPACE TO TC-NAME
           END-IF
           MOVE TC-NAME TO P-CNAME.
           MOVE HSMSW-07 TO P-KRCD.
           MOVE HSMSW-14 TO P-KS.
           MOVE HSMSW-01 TO P-SSNO.
           PERFORM S-20 THRU S-25.
      *
           MOVE ZERO TO W-GKEI.
       M-40.
           MOVE HSMSW-03 TO W-DK.
           IF  HSMSW-13 = 4 OR 5
               MOVE 4 TO W-DK
           END-IF
           MOVE ZERO TO W-12.
           MOVE HSMSW-12 TO W-12.
           IF  W-DK = 1 OR 2
               COMPUTE W-1211 = -1 * W-1211
               COMPUTE W-1212 = -1 * W-1212
               COMPUTE W-1213 = -1 * W-1213
               COMPUTE W-1214 = -1 * W-1214
               COMPUTE W-1215 = -1 * W-1215
               COMPUTE W-1216 = -1 * W-1216
               COMPUTE W-1217 = -1 * W-1217
               COMPUTE W-1218 = -1 * W-1218
               COMPUTE W-1219 = -1 * W-1219
               COMPUTE W-12110 = -1 * W-12110
               COMPUTE W-122 = -1 * W-122
           END-IF
           MOVE HSMSW-09 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           IF  HSMSW-091 NOT = 9999
               ADD W-122 TO W-GKEI
           END-IF
           PERFORM S-30 THRU S-35.
       M-45.
      *           READ HSMSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSMSW-F_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  HSMSW-19 NOT = 0
               GO TO M-45
           END-IF
           IF  HSMSW-061 = 5358 OR 5350 OR 5349
               GO TO M-45
           END-IF
           IF  HSMSW-05 < W-SNGP OR > W-ENGP
               GO TO M-45
           END-IF
           IF  HSMSW-011 NOT = W-BCO
               GO TO M-50
           END-IF
           IF  HSMSW-01 = W-SSNO
               GO TO M-40
           END-IF
           PERFORM S-40 THRU S-45.
           GO TO M-35.
       M-50.
           PERFORM S-40 THRU S-45.
           IF  W-KPC = 1
               PERFORM S-05 THRU S-15
           END-IF
           GO TO M-34.
       M-80.
           PERFORM S-40 THRU S-45.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSW-F_IDLST HSMSW-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-99.
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
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNAME.
           MOVE W-DK TO P-DK.
           MOVE HSMSW-09 TO P-HCD.
           MOVE HI-NAME TO P-HNAME.
           MOVE HSMSW-10 TO P-SIZ.
           MOVE W-1211 TO P-SIZ1.
           MOVE W-1212 TO P-SIZ2.
           MOVE W-1213 TO P-SIZ3.
           MOVE W-1214 TO P-SIZ4.
           MOVE W-1215 TO P-SIZ5.
           MOVE W-1216 TO P-SIZ6.
           MOVE W-1217 TO P-SIZ7.
           MOVE W-1218 TO P-SIZ8.
           MOVE W-1219 TO P-SIZ9.
           MOVE W-12110 TO P-SIZ10.
           MOVE W-122 TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.
       S-40.
           MOVE W-SSNO TO HSMS-01B.
           MOVE 7 TO HSMS-02B.
      *           READ HSMSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HSMS-15
           END-IF
           MOVE SPACE TO W-P3.
           MOVE SPACE TO P-TEKI.
           MOVE HSMS-15 TO P-TEKI.
           MOVE "　計" TO P-K.
           MOVE W-GKEI TO P-GKEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-10
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-45.
           EXIT.
