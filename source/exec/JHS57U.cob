       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS57U.
      *********************************************************
      *    PROGRAM         :  赤ちゃん本舗　請求データ抽出    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  W-DATA.
           02  WT-D.
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  W-D.
             03  W-MNGP       PIC  9(008).
             03  W-DNO        PIC  9(006).
             03  W-STC        PIC  9(007).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SNGPD  REDEFINES W-SNGP.
             03  W-SNG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-ENGPD  REDEFINES W-ENGP.
             03  W-ENG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-ENGPL  REDEFINES W-ENGP.
             03  F            PIC  9(002).
             03  W-ENGPS      PIC  9(006).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
      *FD  TDNARF
       01  TDNARF_JHS57U.
           02  TDNARF_PNAME1    PIC  X(006) VALUE "TDNARF".
           02  F                PIC  X(001).
           02  TDNARF_LNAME     PIC  X(013) VALUE "TDNARF_JHS57U".
           02  F                PIC  X(001).
           02  TDNARF_KEY1      PIC  X(100) VALUE SPACE.
           02  TDNARF_SORT      PIC  X(100) VALUE SPACE.
           02  TDNARF_IDLST     PIC  X(100) VALUE SPACE.
           02  TDNARF_RES       USAGE  POINTER.
       01  TDNA-R.
           02  TDNA-KEY.
             03  TDNA-STC       PIC  9(007).
             03  TDNA-DNO       PIC  9(007).
             03  TDNA-DGN       PIC  9(002).
           02  TDNA-JAN         PIC  X(013).
           02  TDNA-SU          PIC  9(006).
           02  TDNA-GTN         PIC  9(007).
           02  TDNA-UTN         PIC  9(007).
           02  TDNA-GKIN        PIC  9(010).
           02  TDNA-UKIN        PIC  9(010).
           02  TDNA-DPM         PIC  X(002).
           02  TDNA-CLS         PIC  X(003).
           02  TDNA-SHM         PIC  X(013).
           02  TDNA-MKH         PIC  X(010).
           02  TDNA-MSB         PIC  X(010).
           02  TDNA-TY          PIC  X(002).
           02  TDNA-HCD         PIC  9(006).
           02  TDNA-COR         PIC  N(004).
           02  TDNA-SIZ         PIC  X(004).
           02  TDNA-NSU         PIC  9(006).
           02  TDNA-TSC         PIC  9(001).
           02  F                PIC  X(008).
           02  TDNA-CCD         PIC  9(003).
           02  TDNA-TNA         PIC  N(014).
           02  TDNA-HNO         PIC  9(009).
           02  TDNA-HNGP        PIC  9(006).
           02  TDNA-NNGP        PIC  9(006).
           02  TDNA-THC         PIC  9(006).
           02  TDNA-BI          PIC  X(010).
           02  TDNA-SNGP        PIC  9(008).
           02  TDNA-HNA         PIC  X(006).
           02  TDNA-ZON         PIC  X(004).
           02  TDNA-DC          PIC  9(002).
           02  F                PIC  X(007).
           02  TDNA-DNGP        PIC  9(008).
           02  TDNA-NRC         PIC  9(001).
           02  F                PIC  X(008).
           02  TDNA-PC          PIC  9(001).
           02  TDNA-RC          PIC  9(001).
       77  F                    PIC  X(001).
      *FD  AKSKF
       01  AKSKF_JHS57U.
           02  AKSKF_PNAME1     PIC  X(009) VALUE "WK0064000".
           02  F                PIC  X(001).
           02  AKSKF_LNAME      PIC  X(012) VALUE "AKSKF_JHS57U".
           02  F                PIC  X(001).
           02  AKSKF_KEY1       PIC  X(100) VALUE SPACE.
           02  AKSKF_SORT       PIC  X(100) VALUE SPACE.
           02  AKSKF_IDLST      PIC  X(100) VALUE SPACE.
           02  AKSKF_RES        USAGE  POINTER.
       01  AKSK-R.
           02  AKSK-NGP         PIC  9(008).
           02  AKSK-DNO         PIC  9(006).
           02  AKSK-STC         PIC  9(007).
           02  AKSK-KIN         PIC S9(010).
           02  AKSK-SHZ         PIC S9(007).
           02  F                PIC  X(026).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　赤ちゃん本舗　請求データ抽出　　＊＊＊".
           02  FILLER  PIC  X(046) VALUE
                "｛   '  年   月   日  ～  '  年   月   日   ｝".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
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
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "10" "16" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "22" "43" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "10" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "10" "22" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "10" "27" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "10" "32" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "10" "43" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "10" "48" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "10" "53" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "60" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           ACCEPT W-ENGPS FROM DATE.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EPEY" A-EPEY "p" RETURNING RESU.
           MOVE 20 TO W-SNEN1 W-ENEN1.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO M-10
           END-IF.
       M-11.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO M-11
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-11
           END-IF.
       M-12.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-11.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-12.
           IF  W-SPEY < 1 OR > 31
               GO TO M-12.
       M-13.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-12
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO M-13
           END-IF
           IF  W-SNEN2 > W-ENEN2
               GO TO M-13
           END-IF.
       M-14.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-13
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO M-14
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-14
           END-IF
           IF  W-SNG > W-ENG
               GO TO M-14
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-14
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-15
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TDNARF_PNAME1 " " BY REFERENCE TDNARF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" AKSKF_PNAME1 " " BY REFERENCE AKSKF_IDLST "0".
       M-25.
      *           READ TDNARF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNARF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNA-DNGP < W-SNGP OR > W-ENGP
               GO TO M-25
           END-IF.
       M-30.
           MOVE TDNA-DNGP TO W-MNGP.
           MOVE TDNA-DNO TO W-DNO.
           MOVE TDNA-STC TO W-STC.
           MOVE ZERO TO WT-D.
       M-35.
           IF  TDNA-TSC = 1
               COMPUTE W-KIN = W-KIN + (TDNA-NSU * TDNA-GTN)
           ELSE
               ADD TDNA-GKIN TO W-KIN
           END-IF.
       M-40.
      *           READ TDNARF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNARF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  TDNA-DNGP < W-SNGP OR > W-ENGP
               GO TO M-40
           END-IF
           IF  TDNA-DNGP = W-MNGP
               IF  TDNA-DNO = W-DNO
                   IF  TDNA-STC = W-STC
                       GO TO M-35
                   END-IF
               END-IF
           END-IF
           PERFORM WRI-RTN THRU WRI-EX.
           GO TO M-30.
       M-80.
           PERFORM WRI-RTN THRU WRI-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNARF_IDLST TDNARF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AKSKF_IDLST AKSKF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       WRI-RTN.
           COMPUTE W-SHZ = W-KIN * 0.05.
      *
           INITIALIZE AKSK-R.
           MOVE W-MNGP TO AKSK-NGP.
           MOVE W-DNO TO AKSK-DNO.
           MOVE W-STC TO AKSK-STC.
           MOVE W-KIN TO AKSK-KIN.
           MOVE W-SHZ TO AKSK-SHZ.
      *           WRITE AKSK-R.
      *//////////////
           CALL "DB_Insert" USING
            AKSKF_PNAME1 AKSKF_LNAME AKSK-R RETURNING RET.
       WRI-EX.
           EXIT.
