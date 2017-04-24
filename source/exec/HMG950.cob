       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG950.
      ************************************************************
      *    PROGRAM         :  履物品名統計ファイル　年間累積　　 *
      *    PRINTER TYPE    :  ****                               *
      *    SCREEN          :  ******                             *
      *        変更　　　  :  62/05/22                           *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENL  REDEFINES W-NEND.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGL  REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NG.
               04  W-NEN      PIC  9(002).
               04  W-GET      PIC  9(002).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHHTF.
       01  HHTYR_HMG950.
           02  HHTYR_PNAME1    PIC  X(005) VALUE "HHTYR".
           02  F               PIC  X(001).
           02  HHTYR_LNAME     PIC  X(012) VALUE "HHTYR_HMG950".
           02  F               PIC  X(001).
           02  HHTYR_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTYR_SORT      PIC  X(100) VALUE SPACE.
           02  HHTYR_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTYR_RES       USAGE  POINTER.
       01  HHTYR-R.
           02  HHTYR-MHCD     PIC  9(006).
           02  HHTYR-HCD      PIC  9(006).
           02  HHTYR-SIZ      PIC  9(001).
           02  HHTYR-AZSU.
             03  HHTYR-ZSUD  OCCURS  10.
               04  HHTYR-ZSU  PIC S9(006) COMP-3.
           02  HHTYR-ANSU.
             03  HHTYR-NSUD  OCCURS  10.
               04  HHTYR-NSU  PIC S9(006) COMP-3.
           02  HHTYR-AUSU.
             03  HHTYR-USUD  OCCURS  10.
               04  HHTYR-USU  PIC S9(006) COMP-3.
           02  HHTYR-AASS.
             03  HHTYR-ASSD  OCCURS  10.
               04  HHTYR-ASS  PIC S9(004) COMP-3.
           02  HHTYR-ATZS.
             03  HHTYR-TZSD  OCCURS  10.
               04  HHTYR-TZS  PIC S9(006) COMP-3.
           02  HHTYR-ATSU.
             03  HHTYR-TSUD  OCCURS  10.
               04  HHTYR-TSU  PIC S9(006) COMP-3.
           02  HHTYR-BC1      PIC  9(002).
           02  HHTYR-BC2      PIC  9(002).
           02  HHTYR-BC3      PIC  9(002).
           02  F              PIC  X(001).
           02  HHTYR-NG       PIC  9(006).
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
                "＊＊＊　　品名統計マスター　年間累積　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME4     PIC  X(025) VALUE
                  "***  HHTYR WRITE ｴﾗｰ  ***".
           COPY LIBSCR.
           COPY LSSEM.
       PROCEDURE DIVISION.
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
           "C-ERR" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "25" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NG.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
             HHT-KEY2.
           CALL "DB_F_Open" USING
            "EXTEND" HHTYR_PNAME1 " " BY REFERENCE HHTYR_IDLST "0".
       M-15.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
       M-20.
           MOVE ZERO TO HHTYR-R.
           MOVE HHT-R TO HHTYR-R.
           MOVE W-NGD TO HHTYR-NG.
      *           WRITE HHTYR-R.
      *///////////////
           CALL "DB_Insert" USING
            HHTYR_PNAME1 HHTYR_LNAME HHTYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                     RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                     RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYR_IDLST HHTYR_PNAME1.
           MOVE "HHTYR        " TO W-FILE.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HHTYR_PNAME1 BY REFERENCE HHTYR_IDLST "0".
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYR_IDLST HHTYR_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
