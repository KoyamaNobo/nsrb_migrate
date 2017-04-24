       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRD350.
      *********************************************************
      *    入金販売変換　(SIWAKE-IW→WK0128___)   　　　　　  *
      *    JS-SIGN  :  日付入力=0 , 日付自動(月末)=1　　　　  *
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
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-HNG          PIC  9(006).
           02  W-HNGD  REDEFINES W-HNG.
             03  W-HNEN       PIC  9(004).
             03  W-HNENL REDEFINES W-HNEN.
               04  W-HNEN1    PIC  9(002).
               04  W-HNEN2    PIC  9(002).
             03  W-HGET       PIC  9(002).
           02  W-HNGSD REDEFINES W-HNG.
             03  F            PIC  9(002).
             03  W-HNGS       PIC  9(004).
           02  W-KNG          PIC  9(006).
           02  W-KNGD  REDEFINES W-KNG.
             03  W-KNEN       PIC  9(004).
             03  W-KNENL REDEFINES W-KNEN.
               04  W-KNEN1    PIC  9(002).
               04  W-KNEN2    PIC  9(002).
             03  W-KGET       PIC  9(002).
           02  W-KNGSD REDEFINES W-KNG.
             03  F            PIC  9(002).
             03  W-KNGS       PIC  9(004).
           02  W-END          PIC  9(001).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY SIWAIW.
           COPY LITM.
      *FD  NYUW-F
       01  NYUW-F_PRD350.
           02  NYUW-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  NYUW-F_LNAME    PIC  X(013)  VALUE "NYUW-F_HMY620".
           02  F               PIC  X(001).
           02  NYUW-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_SORT     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  NYUW-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  N-DATE         PIC  9(008).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC           PIC  9(002).
           02  N-NSC          PIC  9(001).
           02  N-TD           PIC  9(008).
           02  N-SS           PIC  9(006).
           02  N-SSD   REDEFINES N-SS.
             03  N-NEN        PIC  9(004).
             03  N-NEND  REDEFINES N-NEN.
               04  N-NEN1     PIC  9(002).
               04  N-NEN2     PIC  9(002).
             03  N-GET        PIC  9(002).
           02  N-SSL   REDEFINES N-SS.
             03  F            PIC  9(002).
             03  N-SSS        PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  N-FDNO.
             03  N-FNO        PIC  9(006).
             03  N-FGNO       PIC  9(002).
           02  N-SKD          PIC  9(008).
           02  N-DCC          PIC  9(001).
           02  F              PIC  X(016).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
           02  F              PIC  X(042).
           02  N-HHC          PIC  9(001).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　入金販売変換ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "（　'  年  月  日 まで　）".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN     PIC  9(002).
             03  A-GET     PIC  9(002).
             03  A-PEY     PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME7     PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME8     PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME50    PIC  N(020) VALUE
                  "【　　全体のクリアがされていません　　】".
             03  E-TCD     PIC  9(004).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "342" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "18" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "20" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "13" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NEN" "9" "13" "23" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-GET" "9" "13" "27" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PEY" "9" "13" "31" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "37" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "102" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME50" "N" "24" "15" "40" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "40" "4" "E-ME50" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE SDWTCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           MOVE D-NHNG TO W-HNGS.
           MOVE D-NKNG TO W-KNGS.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF
           IF  W-KNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-KNEN
           END-IF
           IF  W-KNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-KNEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  D-NANG < D-NHNG AND < D-NKNG
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME50" E-ME50 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-SIGN = 1
               MOVE W-HNG TO W-NG
               MOVE 99 TO W-PEY
               CALL "SD_Output" USING "A-NEN" A-NEN "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "A-GET" A-GET "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "A-PEY" A-PEY "p" 
                                         RETURNING RESU
               GO TO M-30
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NEN NOT = W-HNEN AND W-KNEN
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-NG NOT = W-HNG AND W-KNG
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF.
       M-30.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 " " BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" NYUW-F_PNAME1 " " BY REFERENCE NYUW-F_IDLST "0".
       M-35.
      *           READ SDW NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SDWHHC NOT = 0 AND 8
               GO TO M-35
           END-IF
           IF  0140 NOT = KRCDMW AND KSCDMW
               GO TO M-35
           END-IF
           IF (KRCDMW > 4999 AND < 6000) OR (KSCDMW > 4999 AND < 6000)
               GO TO M-35
           END-IF
           IF  SDWYMD > W-DATE
               GO TO M-35
           END-IF
           IF  SDWSIN NOT = 1
               GO TO M-35
           END-IF
           IF  SDWTCD = ZERO
               GO TO M-35
           END-IF
           MOVE SDWTCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-TCD" E-TCD "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
               GO TO M-35
           END-IF
           IF  T-BC = 0
               IF  W-HNG NOT = SDWYM
                   GO TO M-35
               END-IF
           END-IF
           IF  T-BC NOT = 0
               IF  W-KNG NOT = SDWYM
                   GO TO M-35
               END-IF
           END-IF
           MOVE ZERO TO NYU-R.
           MOVE SDWYMD TO N-DATE.
           MOVE SDWTCD TO N-TCD.
           MOVE SDWJNO TO N-FNO.
           MOVE SDWLNO TO N-FGNO.
           MOVE SDWNKCD TO N-NC.
           MOVE SDWNSC TO N-NSC.
           MOVE SDWTKD TO N-TD.
           MOVE SDWSKNG TO N-SSS.
           IF  SDWSKNG = ZERO
               GO TO M-40
           END-IF
           IF  N-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO N-NEN
           END-IF
           IF  N-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO N-NEN
           END-IF.
       M-40.
           MOVE T-BC TO N-BC.
           MOVE T-TNC TO N-TC.
           MOVE T-DCC TO N-DCC.
           MOVE SDWSKD TO N-SKD.
           MOVE 1 TO N-ACT.
           IF  KRCDMW = 0140
               COMPUTE N-KIN = KRKINW * -1
           END-IF
           IF  KSCDMW = 0140
               MOVE KSKINW TO N-KIN
           END-IF
           MOVE SDWHHC TO N-HHC.
      *           WRITE NYU-R.
      *///////////////
           CALL "DB_Insert" USING
            NYUW-F_PNAME1 NYUW-F_LNAME NYU-R RETURNING RET.
           GO TO M-35.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
