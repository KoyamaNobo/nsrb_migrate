       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG450.
      *********************************************************
      *    PROGRAM         :  材料受払Ｆ　年間累積            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/10                        *
      *    BASE            :  KBG450                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=月次 , 1=問合せ               *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  W-NG         PIC  9(006).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  F            PIC  9(002).
           02  W-NGD          PIC  9(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
      *FD  JSSR-F
       01  JSSR-F_KBG960.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBG960".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSS-R.
           02  JS-DC.
             03  JS-DC1       PIC  9(001).
             03  JS-DC2       PIC  9(001).
           02  JS-DATE        PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  F              PIC  X(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  F              PIC  X(007).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  F              PIC  X(001).
           02  JS-BKC         PIC  9(002).
           02  F              PIC  X(016).
           02  JS-KEY         PIC  X(007).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  HAR-F
       01  HAR-F_KBG960.
           02  HAR-F_PNAME1   PIC  X(004) VALUE "HARF".
           02  F              PIC  X(001).
           02  HAR-F_LNAME    PIC  X(012) VALUE "HAR-F_KBG960".
           02  F              PIC  X(001).
           02  HAR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HAR-F_SORT     PIC  X(100) VALUE SPACE.
           02  HAR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HAR-F_RES      USAGE  POINTER.
       01  HA-R.
           02  HA-DATE        PIC  9(008).
           02  HA-JCD         PIC  9(006).
           02  HA-SU          PIC S9(007)V9(02).
           02  HA-KEY         PIC  X(007).
           02  F              PIC  X(002).
       77  F                  PIC  X(001).
      *FD  JUHR-F
       01  JUHR-F_KBG960.
           02  JUHR-F_PNAME1  PIC  X(005) VALUE "JUHRF".
           02  F              PIC  X(001).
           02  JUHR-F_LNAME   PIC  X(013) VALUE "JUHR-F_KBG960".
           02  F              PIC  X(001).
           02  JUHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JUHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JUHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JUHR-F_RES     USAGE  POINTER.
       01  JUHR-R.
           02  JUHR-KEY       PIC  9(006).
           02  JUHR-NGP       PIC  9(008).
           02  JUHR-KNSC      PIC  9(001).
           02  JUHR-SU        PIC S9(007)V9(02).
           02  JUHR-TN        PIC S9(006)V9(02).
           02  JUHR-KIN       PIC S9(008).
           02  JUHR-SD        PIC  9(006).
           02  JUHR-DNO       PIC  9(007).
           02  JUHR-YC        PIC  9(001).
           02  JUHR-BKC       PIC  9(002).
           02  JUHR-BKNO      PIC  9(002).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  JUHW-F
       01  JUHW-F_KBG960.
           02  JUHW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JUHW-F_LNAME   PIC  X(013) VALUE "JUHW-F_KBG960".
           02  F              PIC  X(001).
           02  JUHW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JUHW-F_SORT    PIC  X(100) VALUE SPACE.
           02  JUHW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JUHW-F_RES     USAGE  POINTER.
       01  JUHW-R.
           02  JUHW-KEY       PIC  9(006).
           02  JUHW-NGP       PIC  9(008).
           02  JUHW-KNSC      PIC  9(001).
           02  JUHW-SU        PIC S9(007)V9(02).
           02  JUHW-TN        PIC S9(006)V9(02).
           02  JUHW-KIN       PIC S9(008).
           02  JUHW-SD        PIC  9(006).
           02  JUHW-DNO       PIC  9(007).
           02  JUHW-YC        PIC  9(001).
           02  JUHW-BKC       PIC  9(002).
           02  JUHW-BKNO      PIC  9(002).
           02  F              PIC  X(006).
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
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　材料受払累積Ｆ　作成　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　材料受払ワーク　作成　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  JUHRF WRITE ｴﾗｰ  ***".
             03  E-JCD   PIC  9(006).
           COPY LIBSCR.
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "280" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "6" "10" "40" " " "C-MID1"  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "46" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "46" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE J-KEY "6" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE D-NBNG TO W-NGS.
           ADD 2000 TO W-NEN.
           IF  JS-SIGN = 0
               CALL "DB_F_Open" USING
                "EXTEND" JUHR-F_PNAME1 " " BY REFERENCE JUHR-F_IDLST "0"
           ELSE
               CALL "CBLSTNNO" USING STN-NO USER_ID
               MOVE STN-NO2 TO W-FID2
               MOVE W-FID TO WK0064ID
               MOVE WK0064ID TO JUHW-F_PNAME1
               CALL "DB_F_Open" USING
                "EXTEND" JUHW-F_PNAME1 " " BY REFERENCE JUHW-F_IDLST "0"
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
       M-10.
      *           READ JT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  JT-ZC NOT = 0
               GO TO M-10
           END-IF
           IF  JT-ZKS = ZERO
               GO TO M-10
           END-IF
      *
           MOVE JT-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO J-ST
               MOVE 9 TO J-YC
           END-IF.
       M-12.
           IF  JS-SIGN = 1
               MOVE ZERO TO JUHW-R
               MOVE JT-KEY TO JUHW-KEY
               MOVE W-NGP TO JUHW-NGP
               MOVE JT-ZKS TO JUHW-SU
               MOVE J-ST TO JUHW-TN
               MOVE JT-ZKK TO JUHW-KIN
               MOVE J-YC TO JUHW-YC
               MOVE J-BKC TO JUHW-BKC
               MOVE J-BKNO TO JUHW-BKNO
      *               WRITE JUHW-R
      *//////////////
               CALL "DB_Insert" USING
                JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET
               GO TO M-10
           END-IF
           MOVE ZERO TO JUHR-R.
           MOVE JT-KEY TO JUHR-KEY.
           MOVE W-NGP TO JUHR-NGP.
           MOVE JT-ZKS TO JUHR-SU.
           MOVE J-ST TO JUHR-TN.
           MOVE JT-ZKK TO JUHR-KIN.
           MOVE J-YC TO JUHR-YC.
           MOVE J-BKC TO JUHR-BKC.
           MOVE J-BKNO TO JUHR-BKNO.
      *           WRITE JUHR-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHR-F_PNAME1 JUHR-F_LNAME JUHR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JT-M_IDLST JT-M_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-12.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-20.
      *           READ JSSR-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSSR-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  JS-JCD = 999000
               GO TO M-20
           END-IF
           IF  JS-DC1 = 3
               GO TO M-20
           END-IF
           IF  JS-DC2 = 2 OR 3
               GO TO M-20
           END-IF
           MOVE JS-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  J-ZC NOT = ZERO
               GO TO M-20
           END-IF.
       M-22.
           IF JS-SIGN = 1
               MOVE ZERO TO JUHW-R
               MOVE JS-JCD TO JUHW-KEY
               MOVE JS-DATE TO JUHW-NGP
               MOVE 1 TO JUHW-KNSC
               MOVE JS-SU TO JUHW-SU
               MOVE JS-T TO JUHW-TN
               MOVE JS-KIN TO JUHW-KIN
               MOVE JS-CD TO JUHW-SD
               MOVE JS-KEY TO JUHW-DNO
               MOVE J-YC TO JUHW-YC
               MOVE J-BKC TO JUHW-BKC
               MOVE J-BKNO TO JUHW-BKNO
      *               WRITE JUHW-R
      *//////////////
               CALL "DB_Insert" USING
                JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET
               GO TO M-20
           END-IF
           MOVE ZERO TO JUHR-R.
           MOVE JS-JCD TO JUHR-KEY.
           MOVE JS-DATE TO JUHR-NGP.
           MOVE 1 TO JUHR-KNSC.
           MOVE JS-SU TO JUHR-SU.
           MOVE JS-T TO JUHR-TN.
           MOVE JS-KIN TO JUHR-KIN.
           MOVE JS-CD TO JUHR-SD.
           MOVE JS-KEY TO JUHR-DNO.
           MOVE J-YC TO JUHR-YC.
           MOVE J-BKC TO JUHR-BKC.
           MOVE J-BKNO TO JUHR-BKNO.
      *           WRITE JUHR-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHR-F_PNAME1 JUHR-F_LNAME JUHR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-22.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0".
       M-30.
      *           READ HAR-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HAR-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           MOVE HA-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF.
       M-32.
           IF  JS-SIGN = 1
               MOVE ZERO TO JUHW-R
               MOVE HA-JCD TO JUHW-KEY
               MOVE HA-DATE TO JUHW-NGP
               MOVE 2 TO JUHW-KNSC
               MOVE HA-SU TO JUHW-SU
               MOVE HA-KEY TO JUHW-DNO
               MOVE J-YC TO JUHW-YC
               MOVE J-BKC TO JUHW-BKC
               MOVE J-BKNO TO JUHW-BKNO
      *               WRITE JUHW-R
      *//////////////
               CALL "DB_Insert" USING
                JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET
               GO TO M-30
           END-IF
           MOVE ZERO TO JUHR-R.
           MOVE HA-JCD TO JUHR-KEY.
           MOVE HA-DATE TO JUHR-NGP.
           MOVE 2 TO JUHR-KNSC.
           MOVE HA-SU TO JUHR-SU.
           MOVE HA-KEY TO JUHR-DNO.
           MOVE J-YC TO JUHR-YC.
           MOVE J-BKC TO JUHR-BKC.
           MOVE J-BKNO TO JUHR-BKNO.
      *           WRITE JUHR-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHR-F_PNAME1 JUHR-F_LNAME JUHR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HAR-F_IDLST HAR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-32.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HAR-F_IDLST HAR-F_PNAME1.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE JUHR-F_IDLST JUHR-F_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE JUHW-F_IDLST JUHW-F_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JUHR-F_IDLST JUHR-F_PNAME1.
           MOVE "JUHRF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JUHR-F_PNAME1 " " BY REFERENCE JUHR-F_IDLST "0".
       S-10.
           EXIT.
