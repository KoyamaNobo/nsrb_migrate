       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTN91U.
      *********************************************************
      *    受注入出荷累積Ｆ　生成  （親コード変更時）         *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-MSG              PIC  X(030).
       77  W-KEY              PIC  9(006).
       01  W-DATA.
           02  W-END          PIC  9(001) VALUE ZERO.
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIHIM.
      *FD  JNSR
       01  JNSR_JTN91U.
           02  JNSR_PNAME1    PIC  X(005) VALUE "JNSR1".
           02  F              PIC  X(001).
           02  JNSR_LNAME     PIC  X(011) VALUE "JNSR_JTN91U".
           02  F              PIC  X(001).
           02  JNSR_KEY1      PIC  X(100) VALUE SPACE.
           02  JNSR_SORT      PIC  X(100) VALUE SPACE.
           02  JNSR_IDLST     PIC  X(100) VALUE SPACE.
           02  JNSR_RES       USAGE  POINTER.
       01  JNSR-R.
           02  JNSR-KEY1.
             03  JNSR-01              PIC  9(06)  COMP-3.
             03  JNSR-02              PIC  9(08)  COMP-3.
             03  JNSR-03              PIC  9(02).
             03  JNSR-04              PIC  9(06)  COMP-3.
             03  JNSR-05              PIC  9(01).
           02  JNSR-06                PIC  9(01).
           02  JNSR-07                PIC  9(01).
           02  JNSR-08.
             03  JNSR-081             PIC S9(04)  COMP-3    OCCURS  10.
           02  JNSR-09                PIC  9(01).
           02  JNSR-10                PIC  9(01).
           02  JNSR-11.
             03  JNSR-111             PIC  9(04).
             03  JNSR-112             PIC  9(03).
           02  JNSR-12                PIC  9(06)  COMP-3.
           02  JNSR-13                PIC  9(01).
           02  JNSR-14                PIC  9(01).
           02  JNSR-KEY2.
             03  JNSR-15.
               04  JNSR-151           PIC  9(06)  COMP-3.
               04  JNSR-152           PIC  9(01).
             03  JNSR-16              PIC  9(08)  COMP-3.
             03  JNSR-17              PIC  9(01).
             03  JNSR-18.
               04  JNSR-181           PIC  9(06)  COMP-3.
               04  JNSR-182           PIC  9(01).
           02  JNSR-KEY3.
             03  JNSR-19              PIC  9(04).
             03  JNSR-20              PIC  9(08)  COMP-3.
             03  JNSR-21              PIC  9(01).
             03  JNSR-22.
               04  JNSR-221           PIC  9(06)  COMP-3.
               04  JNSR-222           PIC  9(01).
           02  JNSR-23                PIC  N(09).
           02  JNSR-24                PIC  N(23).
           02  FILLER                 PIC  X(08).
           02  JNSR-90                PIC  9(01).
           02  JNSR-91                PIC  9(01).
           02  JNSR-92                PIC  9(02).
       77  F                          PIC  X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　受注入出荷累積Ｆ　変換　　＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-FILE  PIC  X(013).
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
            "C-MID" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "12" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "20" "42" "22" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "59" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "43" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-FILE" "X" "24" "46" "13" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-FILE" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "1"
            "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
       M-15.
      *           READ JNSR NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNSR_PNAME1 BY REFERENCE JNSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE JNSR-01 TO W-KEY.
           MOVE W-KEY TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-MHCD HI-HCD
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  HIM ﾅｼ  ***" TO W-MSG
               MOVE JNSR-01 TO W-FILE
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
               GO TO M-90
           END-IF
           IF  HI-MHCD NOT = HI-HCD
               MOVE HI-MHCD TO JNSR-01
           END-IF
      *           REWRITE JNSR-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNSR_PNAME1 JNSR_LNAME JNSR-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  REWRITE ｴﾗｰ  ***" TO W-MSG
               MOVE "JNSR" TO W-FILE
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
