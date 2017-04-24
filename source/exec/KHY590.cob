       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY590.
      *********************************************************
      *    PROGRAM         :  工品年間累積ファイル抽出　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/13                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=品名日付入力 , 1=品名日付履物 *
      *                    :  5=得意先品名日付入力            *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID2          PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-HNG          PIC  9(006).
           02  W-HNGD  REDEFINES W-HNG.
             03  W-HNEN       PIC  9(004).
             03  W-HNENL REDEFINES W-HNEN.
               04  W-HNEN1    PIC  9(002).
               04  W-HNEN2    PIC  9(002).
             03  W-HGET       PIC  9(002).
           02  W-HNGL  REDEFINES W-HNG.
             03  F            PIC  9(002).
             03  W-HNGS       PIC  9(004).
           02  W-ONG          PIC  9(006).
           02  W-ONGD  REDEFINES W-ONG.
             03  W-ONEN       PIC  9(004).
             03  W-ONENL REDEFINES W-ONEN.
               04  W-ONEN1    PIC  9(002).
               04  W-ONEN2    PIC  9(002).
             03  W-OGET       PIC  9(002).
           02  W-ONGL  REDEFINES W-ONG.
             03  F            PIC  9(002).
             03  W-ONGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIKHM.
       01  KHTMYR_KHY590.
           02  KHTMYR_PNAME1   PIC  X(006)  VALUE "KHTMYR".
           02  F               PIC  X(001).
           02  KHTMYR_LNAME    PIC  X(013)  VALUE "KHTMYR_KHY590".
           02  F               PIC  X(001).
           02  KHTMYR_KEY1     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_KEY2     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_SORT     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_IDLST    PIC  X(100)  VALUE SPACE.
           02  KHTMYR_RES      USAGE  POINTER.
       01  KHTY-R.
           02  KHTY-YC        PIC  9(002).
           02  KHTY-NC        PIC  9(001).
           02  KHTY-HCD       PIC  X(005).
           02  KHTY-KSU       PIC S9(006)V9(02).
           02  KHTY-HSU       PIC S9(006)V9(02).
           02  KHTY-ISU       PIC S9(006)V9(02).
           02  KHTY-KKIN      PIC S9(008).
           02  KHTY-SSU       PIC S9(006)V9(02).
           02  KHTY-UKIN      PIC S9(008).
           02  KHTY-NKIN      PIC S9(007).
           02  KHTY-GKIN      PIC S9(008).
           02  KHTY-ZSU       PIC S9(006)V9(02).
           02  KHTY-ZKIN      PIC S9(008).
           02  F              PIC  X(077).
           02  KHTY-NG        PIC  9(006).
       77  F                  PIC  X(001).
       01  KHYDF_KHY590.
           02  KHYDF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  KHYDF_LNAME    PIC  X(012)  VALUE "KHYDF_KHY590".
           02  F              PIC  X(001).
           02  KHYDF_KEY1     PIC  X(100)  VALUE SPACE.
           02  KHYDF_KEY2     PIC  X(100)  VALUE SPACE.
           02  KHYDF_SORT     PIC  X(100)  VALUE SPACE.
           02  KHYDF_IDLST    PIC  X(100)  VALUE SPACE.
           02  KHYDF_RES      USAGE  POINTER.
       01  KHYD-R.
           02  KHYD-HCD       PIC  X(005).
           02  KHYD-YC        PIC  9(002).
           02  KHYD-NC        PIC  9(001).
           02  KHYD-ZS        PIC S9(006)V9(02).
           02  KHYD-ZK        PIC S9(008).
           02  KHYD-NS        PIC S9(006)V9(02).
           02  KHYD-NK        PIC S9(008).
           02  KHYD-US        PIC S9(006)V9(02).
           02  KHYD-UK        PIC S9(008).
           02  KHYD-YS        PIC S9(006)V9(02).
           02  KHYD-YK        PIC S9(008).
           02  KHYD-GK        PIC S9(008).
           02  KHYD-NG        PIC  9(006).
           02  KHYD-SNG       PIC  9(006).
           02  KHYD-ENG       PIC  9(006).
           02  F              PIC  X(030).
       77  F                  PIC  X(001).
       01  URIRYR_KHY590.
           02  URIRYR_PNAME1   PIC  X(006)  VALUE "URIRYR".
           02  F               PIC  X(001).
           02  URIRYR_LNAME    PIC  X(013)  VALUE "URIRYR_KHY590".
           02  F               PIC  X(001).
           02  URIRYR_KEY1     PIC  X(100)  VALUE SPACE.
           02  URIRYR_KEY2     PIC  X(100)  VALUE SPACE.
           02  URIRYR_SORT     PIC  X(100)  VALUE SPACE.
           02  URIRYR_IDLST    PIC  X(100)  VALUE SPACE.
           02  URIRYR_RES      USAGE  POINTER.
       01  URIRY-R.
           02  URIRY-DC       PIC  9(001).
           02  URIRY-NG       PIC  9(006).
           02  F              PIC  X(002).
           02  URIRY-TCD      PIC  9(004).
           02  URIRY-HCD      PIC  X(005).
           02  URIRY-SU       PIC S9(006)V9(02).
           02  URIRY-T        PIC S9(006)V9(02).
           02  URIRY-KIN      PIC S9(008).
           02  F              PIC  X(031).
           02  URIRY-JCD      PIC  9(006).
           02  URIRY-GT       PIC  9(006)V9(02).
           02  F              PIC  X(036).
           02  URIRY-BC       PIC  9(001).
           02  F              PIC  X(004).
       77  F                  PIC  X(001).
       01  URIDF_KHY590.
           02  URIDF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  URIDF_LNAME    PIC  X(012)  VALUE "URIDF_KHY590".
           02  F              PIC  X(001).
           02  URIDF_KEY1     PIC  X(100)  VALUE SPACE.
           02  URIDF_KEY2     PIC  X(100)  VALUE SPACE.
           02  URIDF_SORT     PIC  X(100)  VALUE SPACE.
           02  URIDF_IDLST    PIC  X(100)  VALUE SPACE.
           02  URIDF_RES      USAGE  POINTER.
       01  URID-R.
           02  URID-DC        PIC  9(001).
           02  F              PIC  X(008).
           02  URID-TCD       PIC  9(004).
           02  URID-HCD       PIC  X(005).
           02  URID-SU        PIC S9(006)V9(02).
           02  URID-T         PIC S9(006)V9(02).
           02  URID-KIN       PIC S9(008).
           02  F              PIC  X(019).
           02  URID-SNG       PIC  9(006).
           02  URID-ENG       PIC  9(006).
           02  URID-JCD       PIC  9(006).
           02  URID-GT        PIC  9(006)V9(02).
           02  F              PIC  X(036).
           02  URID-BC        PIC  9(001).
           02  F              PIC  X(004).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　工品年間累積ファイル　抽出　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(039) VALUE
                "データ期間　 '  年   月　～　'  年   月".
           02  FILLER  PIC  X(039) VALUE
                "作表期間　   '  年   月　～　'  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ACP.
           02  FILLER.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY     PIC  X(005).
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "450" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "13" "39" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "14" "13" "39" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "24" "22" "09C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "12" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "12" "27" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-HNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "12" "32" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-HGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "12" "43" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ONEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "12" "48" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-OGET "2" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "14" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "14" "27" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "14" "32" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "14" "43" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "14" "48" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "48" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "40" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 5
               CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           MOVE D-SNG TO W-HNGS.
           MOVE D-ENG TO W-ONGS.
           MOVE ZERO TO W-HNEN1.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF.
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF.
           MOVE ZERO TO W-ONEN1.
           IF  W-ONEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ONEN
           END-IF.
           IF  W-ONEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ONEN
           END-IF.
           IF  JS-SIGN = 0 OR 5
               MOVE W-HNG TO W-SNG
               MOVE W-ONG TO W-ENG
               GO TO M-10
           END-IF.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" 
                                         RETURNING RESU.
           IF  JS-SIGN = 1
               GO TO M-40
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT = BTB AND SKP
               GO TO M-15
           END-IF.
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-HNEN > W-SNEN
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-HNG > W-SNG
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT = BTB AND SKP
               GO TO M-25
           END-IF.
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ONEN < W-ENEN
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-ONG < W-ENG
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF.
       M-40.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           IF  JS-SIGN = 5
               GO TO M-65
           END-IF.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO KHYDF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" KHYDF_PNAME1 " " BY REFERENCE KHYDF_IDLST "0".
       M-45.
      *           READ KHTMYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KHTMYR_PNAME1 BY REFERENCE KHTY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           IF  KHTY-NG < W-SNG OR > W-ENG
               GO TO M-45
           END-IF.
           MOVE ZERO TO KHYD-R.
           MOVE KHTY-HCD TO KHYD-HCD.
           MOVE KHTY-YC TO KHYD-YC.
           MOVE KHTY-NC TO KHYD-NC.
           MOVE KHTY-ZSU TO KHYD-ZS.
           MOVE KHTY-ZKIN TO KHYD-ZK.
           COMPUTE KHYD-NS = KHTY-KSU - KHTY-HSU + KHTY-ISU.
           MOVE KHTY-KKIN TO KHYD-NK.
           MOVE KHTY-SSU TO KHYD-US.
           MOVE KHTY-GKIN TO KHYD-GK.
           COMPUTE KHYD-UK = KHTY-UKIN - KHTY-NKIN.
           COMPUTE KHYD-YS = KHYD-ZS + KHYD-NS - KHYD-US.
           IF  KHYD-YS = ZERO
               GO TO M-50
           END-IF.
           MOVE KHTY-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                         RETURNING RESU
           END-IF.
           COMPUTE KHYD-YK = KHYD-YS * KH-GT1.
       M-50.
           MOVE KHTY-NG TO KHYD-NG.
           MOVE W-SNG TO KHYD-SNG.
           MOVE W-ENG TO KHYD-ENG.
      *           WRITE KHYD-R.
      *///////////////
           CALL "DB_Insert" USING
            KHYDF_PNAME1 KHYDF_LNAME KHYD-R RETURNING RET.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
           GO TO M-45.
       M-65.
           MOVE W-FID TO WK0128ID2.
           MOVE WK0128ID2 TO URIDF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" URIDF_PNAME1 " " BY REFERENCE URIDF_IDLST "0".
       M-70.
      *           READ URIRYR AT END 
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           IF  URIRY-NG < W-SNG OR > W-ENG
               GO TO M-70
           END-IF.
           IF  URIRY-HCD = ZERO OR SPACE
               GO TO M-70
           END-IF.
           IF  URIRY-DC = 4 OR 5 OR 9
               GO TO M-70
           END-IF.
           INITIALIZE URID-R.
           MOVE URIRY-R TO URID-R.
           MOVE W-SNG TO URID-SNG.
           MOVE W-ENG TO URID-ENG.
      *           WRITE URID-R.
      *//////////////////////
           CALL "DB_Insert" USING
            URIDF_PNAME1 URIDF_LNAME URID-R RETURNING RET.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
           GO TO M-70.
       M-95.
           IF  JS-SIGN = 0 OR 1
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHYDF_IDLST KHYDF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1
           ELSE
               IF  JS-SIGN = 5
                   CALL "DB_F_Close" USING
                    BY REFERENCE URIRYR_IDLST URIRYR_PNAME1
                   CALL "DB_F_Close" USING
                    BY REFERENCE URIDF_IDLST URIDF_PNAME1
               END-IF
           END-IF.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
