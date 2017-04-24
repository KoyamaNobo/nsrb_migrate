       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG980.
      *********************************************************
      *    仕訳インプットワーク　更新             　　　　　  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NG           PIC  9(006).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENL  REDEFINES W-NEND.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGL   REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY SIWAIW.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　仕訳インプットワーク　更新　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(031) VALUE
                  "***  SIWAKE-IW REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(030) VALUE
                  "***  SIWAKE-IW DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(008).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
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
            "C-ERR" " " "0" "0" "69" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "69" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "31" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "30" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "49" "8" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE SDW-KEY "8" "0" RETURNING RESU.
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
      *
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE W-NGD TO W-NG.
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GETD
               SUBTRACT 1 FROM W-NEND
           END-IF
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GETD
               SUBTRACT 1 FROM W-NEND
           END-IF
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GETD
               SUBTRACT 1 FROM W-NEND
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" SDW_PNAME1 " " BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
       M-10.
      *           READ SDW NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  SDWYM >= W-NG
               GO TO M-10
           END-IF
           IF  SDWYM > W-NGD
               GO TO M-15
           END-IF
      *
      *           DELETE SDW INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SDW_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-15.
           IF  SDWHHC NOT = 0
               GO TO M-10
           END-IF
           MOVE 5 TO SDWHHC.
      *           REWRITE SDW-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SDW_PNAME1 SDW_LNAME SDW-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
