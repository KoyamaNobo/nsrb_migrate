       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG930.
      **********************************
      *****     工品　月次更新     *****
      **********************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-JZS          PIC S9(007).
           02  W-HZS          PIC S9(006)V9(02).
           02  W-HZK          PIC S9(008).
           02  W-NG           PIC  9(004).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHTM.
      *FD  KNHR-F
       01  KNHR-F_KHG930.
           02  KNHR-F_PNAME1  PIC  X(005) VALUE "KNHRF".
           02  F              PIC  X(001).
           02  KNHR-F_LNAME   PIC  X(013) VALUE "KNHR-F_KHG930".
           02  F              PIC  X(001).
           02  KNHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  KNHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHR-F_RES     USAGE  POINTER.
       01  KNHR-R             PIC  X(064).
       77  F                  PIC  X(001).
      *FD  URIR-F
       01  URIR-F_KHG930.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHG930".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R             PIC  X(128).
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
                "＊＊＊　　工品他　月次　更新・クリア　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(025) VALUE
                  "***  KJM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  KHTM REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME6   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME7   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-KHM   PIC  X(005).
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
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "117" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "117" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "27" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KHM" "X" "24" "50" "5" "E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KHM" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE D-NKNG TO W-NG.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING "I-O SEQUENTIAL" KHT-M_PNAME1 
            " " BY REFERENCE KHT-M_IDLST "2"
            "KHT-KEY" BY REFERENCE KHT-KEY "KHT-KEYD" BY REFERENCE
            KHT-KEYD.
       M-30.
      *           READ KHT-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KHM" E-KHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           COMPUTE W-HZS =
                      KHT-KSU + KHT-ZSU - KHT-HSU + KHT-ISU - KHT-SSU.
           IF (KHT-YC = 00 OR 99)
               MOVE ZERO TO W-HZS
           END-IF
           MOVE W-HZS TO KHT-ZSU KHT-TTS.
           COMPUTE W-HZK = KHT-ZSU * KH-GT1.
           MOVE W-HZK TO KHT-ZKIN.
           MOVE ZERO TO KHT-KKIN KHT-KSU KHT-HSU KHT-ISU
                        KHT-SSU KHT-UKIN KHT-NKIN KHT-GKIN.
           MOVE KHT-AAS TO KHT-AZS.
           MOVE ZERO TO KHT-AUS KHT-ASS KHT-AC.
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KHM" E-KHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               GO TO M-95
           END-IF
           GO TO M-30.
       M-80.
      *
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           MOVE D-KTNG2 TO D-KTNG1.
           MOVE ZERO TO D-KTNG2.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
      *
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
