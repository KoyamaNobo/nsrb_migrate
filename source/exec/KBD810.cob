       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD810.
      *********************************************************
      *    PROGRAM         :  発注入庫残ワーク作成            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  随時=0 , 月末=1                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-DATE         PIC  9(008).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-PC           PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SUD   OCCURS  10.
                 05  W-SU     PIC S9(004).
           02  CHK            PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHSHF.
           COPY LIHSHN.
      *FD  HSHZF
       01  HSHZF_KBD810.
           02  HSHZF_PNAME1     PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  HSHZF_LNAME      PIC  X(012) VALUE "HSHZF_KBD810".
           02  F                PIC  X(001).
           02  HSHZF_KEY1       PIC  X(100) VALUE SPACE.
           02  HSHZF_SORT       PIC  X(100) VALUE SPACE.
           02  HSHZF_IDLST      PIC  X(100) VALUE SPACE.
           02  HSHZF_RES        USAGE  POINTER.
       01  HSHZ-R.
           02  HSHZ-SCD         PIC  9(004).
           02  HSHZ-HCD         PIC  9(006).
           02  HSHZ-KEY         PIC  X(008).
           02  HSHZ-RNO   REDEFINES HSHZ-KEY.
             03  HSHZ-RSN       PIC  9(002).
             03  HSHZ-RNG       PIC  9(004).
             03  HSHZ-RND       PIC  9(002).
           02  HSHZ-HDD         PIC  9(008).
           02  HSHZ-HDDD  REDEFINES HSHZ-HDD.
             03  HSHZ-HNEN      PIC  9(004).
             03  HSHZ-HGP       PIC  9(004).
           02  HSHZ-HDDL  REDEFINES HSHZ-HDD.
             03  F              PIC  9(002).
             03  HSHZ-HNGPS     PIC  9(006).
           02  HSHZ-AHSUD.
             03  HSHZ-HSUD  OCCURS   4.                                 ｻｲｽﾞ
               04  HSHZ-AHSU  OCCURS  10.
                 05  HSHZ-HSU   PIC S9(004).                            数量
           02  HSHZ-T           PIC  9(005).
           02  HSHZ-NDD         PIC  9(008).
           02  HSHZ-NDDD  REDEFINES HSHZ-NDD.
             03  HSHZ-NNG.
               04  HSHZ-NNEN    PIC  9(004).
               04  HSHZ-NNENL REDEFINES HSHZ-NNEN.
                 05  HSHZ-NNEN1 PIC  9(002).
                 05  HSHZ-NNEN2 PIC  9(002).
               04  HSHZ-NGET    PIC  9(002).
             03  HSHZ-NPEY      PIC  9(002).
           02  HSHZ-NDDL  REDEFINES HSHZ-NDD.
             03  F              PIC  9(002).
             03  HSHZ-NNGPS     PIC  9(006).
           02  HSHZ-ENGP        PIC  9(006).
           02  F                PIC  X(043).
           02  HSHZ-DATE        PIC  9(008).
       77  F                    PIC  X(001).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　発注入庫残ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(031) VALUE
                "【       年   月   日 現在 　】".
           02  FILLER  PIC  X(026) VALUE
                "品　名  000000   ～ 999999".
           02  FILLER  PIC  X(028) VALUE
                "納　期  00/00/00 ～ 99/99/99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(004).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  FILLER.
             03  A-SHCD1 PIC  9(006).
             03  A-EHCD1 PIC  9(006).
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
           COPY LIBSCR.
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "387" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "12" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "12" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "12" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "12" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "12" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "12" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "12" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "19" "31" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "19" "26" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "16" "19" "28" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "22" "21" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "12" "24" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "12" "31" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "12" "36" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "12" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD1" "9" "14" "27" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD1" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD1" "9" "14" "39" "6" "A-SHCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD1" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "12" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "16" "27" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "16" "30" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "16" "33" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "39" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "42" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "16" "45" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "38" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           IF  JS-SIGN = 0
               ACCEPT W-NGPS FROM DATE
           ELSE
               MOVE DATE-02R TO W-NGPS
           END-IF
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
      *
           PERFORM ACT-RTN THRU ACT-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO HSHZF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HSHZF_PNAME1 " " BY REFERENCE HSHZF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
            "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
            HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
       M-15.
      *           READ HSHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HSH-HDD > W-DATE
               GO TO M-15
           END-IF
           IF  HSH-HCD < W-SHCD
               GO TO M-15
           END-IF
           IF  HSH-HCD > W-EHCD
               GO TO M-90
           END-IF
           IF  HSH-NDD < W-SNGP OR > W-ENGP
               GO TO M-15
           END-IF
           PERFORM NGP-RTN THRU NGP-EX.
           IF  W-NGP NOT = ZERO
               IF  W-NGP < W-DATE
                   GO TO M-15
               END-IF
           END-IF
           MOVE ZERO TO W-ASUD.
           MOVE HSH-AHSUD TO W-ASUD.
      *
           MOVE SPACE TO HSHN-KEY.
           MOVE HSH-KEY TO HSHN-RNO.
      *           START HSHNF KEY NOT < HSHN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHNF_PNAME1 "HSHN-KEY" " NOT < " HSHN-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF.
       M-20.
      *           READ HSHNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  HSH-KEY NOT =  HSHN-RNO
               GO TO M-35
           END-IF
           IF  HSHN-DATE > W-DATE
               GO TO M-35
           END-IF
      *
           MOVE ZERO TO W-S.
       M-25.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO M-20
           END-IF
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           SUBTRACT HSHN-SU(W-S,CNT) FROM W-SU(W-S,CNT).
           GO TO M-30.
       M-35.
           INITIALIZE HSHZ-R.
           MOVE HSH-SCD TO HSHZ-SCD.
           MOVE HSH-HCD TO HSHZ-HCD.
           MOVE HSH-KEY TO HSHZ-KEY.
           MOVE HSH-HDD TO HSHZ-HDD.
           MOVE W-ASUD TO HSHZ-AHSUD.
           MOVE HSH-T TO HSHZ-T.
           MOVE HSH-NDD TO HSHZ-NDD.
           MOVE HSH-ENGP TO HSHZ-ENGP.
           MOVE W-DATE TO HSHZ-DATE.
      *           WRITE HSHZ-R.
      *//////////////
           CALL "DB_Insert" USING
            HSHZF_PNAME1 HSHZF_LNAME HSHZ-R RETURNING RET.
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHZF_IDLST HSHZF_PNAME1.
           IF  CHK = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE "***  ＤＡＴＡ　なし  ***      " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACT-RTN.
           MOVE ZERO TO W-SHCD W-SNGP.
           MOVE 999999 TO W-EHCD.
           MOVE 99999999 TO W-ENGP.
           GO TO ACT-300.
       ACT-005.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-005
           END-IF
           IF  W-NEN < 2003
               GO TO ACT-005
           END-IF.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-005
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-010
           END-IF
           IF  W-GET NOT = 99
               IF  W-GET < 1 OR > 12
                   GO TO ACT-010
               END-IF
           END-IF.
       ACT-015.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-015
           END-IF
           IF  W-PEY NOT = 99
               IF  W-PEY < 1 OR > 31
                   GO TO ACT-015
               END-IF
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD1 "A-SHCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF ESTAT = BTB
               GO TO ACT-015
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO ACT-020
           END-IF.
       ACT-040.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD1 "A-EHCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF ESTAT = BTB
               GO TO ACT-020
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO ACT-040
           END-IF
           IF W-SHCD > W-EHCD
               GO TO ACT-040
           END-IF.
       ACT-180.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-020
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-180
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 = ZERO
               GO TO ACT-200
           END-IF
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       ACT-200.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-180
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-200
           END-IF.
       ACT-220.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-220
           END-IF.
       ACT-240.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACT-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACT-220
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-240
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       ACT-260.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-240
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACT-260
           END-IF
           IF  W-EGET = 99
               IF  W-ENEN2 = 99
                   MOVE 99 TO W-ENEN1
               END-IF
           END-IF.
       ACT-280.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-280
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO ACT-240
           END-IF.
       ACT-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACT-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACT-300
           END-IF
           IF  W-DMM = 9
               GO TO ACT-005
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACT-300
           END-IF
           MOVE W-NGP TO W-DATE.
       ACT-EX.
           EXIT.
       NGP-RTN.
           MOVE ZERO TO W-NGP.
           IF  HSH-ENGP = ZERO
               GO TO NGP-EX
           END-IF
           MOVE HSH-ENGP TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       NGP-EX.
           EXIT.
