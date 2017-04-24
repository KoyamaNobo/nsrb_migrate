       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG610.
      *********************************************************
      *    PROGRAM         :  製品仕入ファイル　作成　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-KEY.
             03  W-JCD        PIC  9(006).
             03  W-SCD        PIC  9(004).
             03  W-SC         PIC  9(001).
             03  W-SJCD       PIC  9(006).
             03  W-NGD        PIC  9(006).
             03  W-BKC        PIC  9(002).
           02  W-SU           PIC S9(006).
           02  W-KIN          PIC S9(009).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-SNG          PIC  9(006).
           02  W-FILE         PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSJSSW.
      *FD  SS-F
       01  SS-F_KBG610.
           02  SS-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SS-F_LNAME     PIC  X(011) VALUE "SS-F_KBG610".
           02  F              PIC  X(001).
           02  SS-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SS-F_SORT      PIC  X(100) VALUE SPACE.
           02  SS-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SS-F_RES       USAGE  POINTER.
       01  SS-R.
           02  SS-JCD         PIC  9(006).
           02  SS-SCD         PIC  9(004).
           02  SS-SU          PIC S9(006).
           02  SS-KIN         PIC S9(009).
           02  SS-SC          PIC  9(001).
           02  SS-SJCD        PIC  9(006).
           02  SS-NG          PIC  9(006).
           02  SS-BKC         PIC  9(002).
           02  SS-BKNO        PIC  9(002).
           02  F              PIC  X(016).
           02  SS-SNG         PIC  9(006).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　製品仕入ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3.
               04  FILLER  PIC  X(015) VALUE
                    "***  JM ﾅｼ  ***".
               04  FILLER  PIC  9(006).
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張後、ＦＮＣ＋再開".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-STAT  PIC  X(002).
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
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "329" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "329" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" " " "24" "0" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME3" "X" "24" "15" "15" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME3" "9" "24" "33" "6" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME78" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " " RETURNING RESU.
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
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO SS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SS-F_PNAME1 " " BY REFERENCE SS-F_IDLST "0".
       M-10.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  ZERO = JR-SU AND JR-KIN
               GO TO M-10
           END-IF
           MOVE JR-NG TO W-SNG.
       M-15.
           MOVE JR-JCD TO W-JCD.
           MOVE JR-SCD TO W-SCD.
           MOVE JR-SJCD TO W-SJCD.
           MOVE JR-HC TO W-SC.
           PERFORM S-20 THRU S-25.
           MOVE W-NG TO W-NGD.
           MOVE JR-BKC TO W-BKC.
           MOVE ZERO TO W-SU W-KIN.
       M-20.
           IF  JR-DC = 10 OR 11
               ADD JR-SU TO W-SU
           END-IF
           ADD JR-KIN TO W-KIN.
       M-25.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ZERO = JR-SU AND JR-KIN
               GO TO M-25
           END-IF
           IF (JR-JCD NOT = W-JCD) OR (JR-SCD NOT = W-SCD)
                                   OR (JR-SJCD NOT = W-SJCD)
               GO TO M-30
           END-IF
           PERFORM S-20 THRU S-25.
           IF  W-NGD = W-NG
               GO TO M-20
           END-IF.
       M-30.
           PERFORM S-05 THRU S-15.
           GO TO M-15.
       M-90.
           PERFORM S-05 THRU S-15.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SS-F_IDLST SS-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  ZERO = W-SU AND W-KIN
               GO TO S-15
           END-IF
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO J-BKNO
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
      *
           INITIALIZE SS-R.
           MOVE W-JCD TO SS-JCD.
           MOVE W-SCD TO SS-SCD.
           MOVE W-SU TO SS-SU.
           MOVE W-KIN TO SS-KIN.
           MOVE W-SC TO SS-SC.
           MOVE W-SJCD TO SS-SJCD.
           MOVE W-NGD TO SS-NG.
           MOVE J-BKC TO SS-BKC.
           MOVE J-BKNO TO SS-BKNO.
           MOVE W-SNG TO SS-SNG.
      *           WRITE SS-R.
      *//////////////
           CALL "DB_Insert" USING
            SS-F_PNAME1 SS-F_LNAME SS-R RETURNING RET.
       S-15.
           EXIT.
       S-20.
           IF  JR-SNG = ZERO
               MOVE JR-NG TO W-NG
               GO TO S-25
           END-IF
           MOVE JR-SNG TO W-NGS.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       S-25.
           EXIT.
