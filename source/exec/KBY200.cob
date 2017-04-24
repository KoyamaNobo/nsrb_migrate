       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY200.
      *********************************************************
      *    PROGRAM         :  購買品目別年間累積ワーク　作成  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  99/05/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SNGM         PIC  9(006).
           02  W-ENGM         PIC  9(006).
           02  W-SNG          PIC  9(006).
           02  W-ENG          PIC  9(006).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DATE.
             03  W-SNGD.
               04  W-SNEN     PIC  9(002).
               04  W-SGET     PIC  9(002).
             03  W-ENGD.
               04  W-ENEN     PIC  9(002).
               04  W-EGET     PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-BC.
             03  W-BC1        PIC  9(001).
             03  W-BC2        PIC  9(002).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
      *FD  JSSR-F
       01  JSSR-F_KBY200.
           02  JSSR-F_PNAME1  PIC  X(009) VALUE "JSSRYR".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBY200".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R.
           02  JR-DC.
             03  JR-DC1       PIC  9(001).
             03  JR-DC2       PIC  9(001).
           02  JR-NGP.
             03  JR-NG.
               04  JR-NEN     PIC  9(004).
               04  JR-GET     PIC  9(002).
             03  JR-PEY       PIC  9(002).
           02  JR-SCD         PIC  9(004).
           02  JR-JCD.
             03  JR-JCD12.
               04  JR-JCD1    PIC  9(001).
               04  JR-JCD2    PIC  9(002).
             03  JR-JCD3      PIC  9(003).
           02  JR-SU          PIC S9(007)V9(02).
           02  JR-T           PIC S9(006)V9(02).
           02  JR-KIN         PIC S9(008).
           02  JR-SHZ         PIC S9(007).
           02  JR-SNGP.
             03  JR-SNG.
               04  JR-SNEN    PIC  9(002).
               04  JR-SGET    PIC  9(002).
             03  JR-SPEY      PIC  9(002).
           02  JR-SJCD        PIC  9(006).
           02  JR-NHN         PIC  9(006).
           02  JR-FC          PIC  9(001).
           02  JR-YC          PIC  9(001).
           02  JR-TC          PIC  9(001).
           02  JR-HC          PIC  9(001).
           02  JR-SC          PIC  9(001).
           02  JR-BSC         PIC  9(001).
           02  JR-BKC         PIC  9(002).
           02  F              PIC  X(016).
           02  JR-KEY         PIC  X(007).
           02  JR-CR          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JSY-F
       01  JSY-F_KBY200.
           02  JSY-F_PNAME1   PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSY-F_LNAME    PIC  X(012) VALUE "JSY-F_KBY200".
           02  F              PIC  X(001).
           02  JSY-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSY-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSY-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSY-F_RES      USAGE  POINTER.
       01  JSY-R.
           02  JY-NG          PIC  9(006).
           02  JY-BCD         PIC  9(001).
           02  JY-SEC         PIC  9(001).
           02  JY-HINC        PIC  9(002).
           02  JY-KIN         PIC S9(010).
           02  F              PIC  X(044).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品目別年間累積ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "データ年月  '  年  月  ～  '  年  月".
           02  FILLER  PIC  X(036) VALUE
                "作表年月    '  年  月　～　'  年  月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NGM.
               03  FILLER  PIC  9(002).
               03  FILLER  PIC Z9 .
               03  FILLER  PIC  9(002).
               03  FILLER  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  KBNOM ﾅｼ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  KBNOM REWRITE ｴﾗｰ  ***".
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "416" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "15" "36" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "15" "36" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "22" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "16" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "16" "28" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "16" "32" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "16" "43" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "16" "47" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGM" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGM" "9" "14" "28" "2" " " "D-NGM" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGM" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGM" "Z9" "14" "32" "2" "01D-NGM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGM" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGM" "9" "14" "43" "2" "02D-NGM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGM" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGM" "Z9" "14" "47" "2" "03D-NGM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NGM" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           INITIALIZE W-DATA.
           MOVE D-NBNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NG TO W-ENGM.
           MOVE W-NGS TO W-ENGD.
      *
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               GO TO M-95
           END-IF
           MOVE JR-NG TO W-NG W-SNGM.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           MOVE W-NGS TO W-SNGD.
           CALL "SD_Output" USING "D-NGM" D-NGM "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
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
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF
           MOVE ZERO TO W-NG.
           MOVE W-SNGD TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-SNG.
           IF  W-SNG < W-SNGM OR > W-ENGM
               GO TO M-10
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-25
           END-IF
           MOVE ZERO TO W-NG.
           MOVE W-ENGD TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-ENG.
           IF  W-ENG < W-SNGM OR > W-ENGM OR < W-SNG
               GO TO M-20
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
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JSY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" JSY-F_PNAME1 " " BY REFERENCE JSY-F_IDLST "0".
       M-35.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JR-NG > W-ENG
               GO TO M-35
           END-IF
           IF  JR-NG < W-SNG
               GO TO M-35
           END-IF
           IF  JR-JCD = 999000
               GO TO M-35
           END-IF
           IF  JR-KIN = ZERO
               GO TO M-35
           END-IF
           IF  JR-DC1 = 3
               GO TO M-35
           END-IF
      *
           MOVE JR-JCD12 TO W-BC.
           IF  W-BC2 < 05
               MOVE 00 TO W-BC2
           END-IF
           IF  W-BC2 > 04 AND < 10
               MOVE 05 TO W-BC2
           END-IF
           IF  W-BC2 > 09 AND < 15
               MOVE 10 TO W-BC2
           END-IF
           IF  W-BC2 > 14 AND < 20
               MOVE 15 TO W-BC2
           END-IF
           IF  W-BC2 > 19 AND < 25
               MOVE 20 TO W-BC2
           END-IF
           IF  W-BC2 > 24 AND < 30
               MOVE 25 TO W-BC2
           END-IF
           IF  W-BC2 > 29 AND < 35
               MOVE 30 TO W-BC2
           END-IF
           IF  W-BC2 > 34 AND < 40
               MOVE 35 TO W-BC2
           END-IF
           IF  W-BC2 > 39 AND < 45
               MOVE 40 TO W-BC2
           END-IF
           IF  W-BC2 > 44 AND < 50
               MOVE 45 TO W-BC2
           END-IF
           IF  W-BC2 > 50 AND < 62
               MOVE 60 TO W-BC2
           END-IF
           IF  W-BC2 > 61 AND < 64
               MOVE 62 TO W-BC2
           END-IF
           IF  W-BC2 > 63 AND < 68
               MOVE 64 TO W-BC2
           END-IF
           IF  W-BC2 > 67 AND < 80
               MOVE 68 TO W-BC2
           END-IF
           IF  W-BC2 > 79 AND < 82
               MOVE 80 TO W-BC2
           END-IF
           IF  W-BC2 > 81 AND < 84
               MOVE 82 TO W-BC2
           END-IF
           IF  W-BC2 > 83 AND < 90
               MOVE 84 TO W-BC2
           END-IF
           IF  W-BC2 > 89 AND < 99
               MOVE 90 TO W-BC2
           END-IF
           IF  W-BC2 = 99
               MOVE 99 TO W-BC2
           END-IF
      *
           MOVE ZERO TO JSY-R.
           MOVE JR-NG TO JY-NG.
           MOVE W-BC1 TO JY-BCD.
           MOVE W-BC2 TO JY-HINC.
           MOVE JR-KIN TO JY-KIN.
      *           WRITE JSY-R.
      *//////////////
           CALL "DB_Insert" USING
            JSY-F_PNAME1 JSY-F_LNAME JSY-R RETURNING RET.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO M-35.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSY-F_IDLST JSY-F_PNAME1.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 "SHARED" BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-SNG TO BNO-SNG.
           MOVE W-ENG TO BNO-ENG.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
