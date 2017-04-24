       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS81I.
      *********************************************************
      *    PROGRAM         :  ワークマン店名ファイル　メンテ  *
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
       01  W-DATA.
           02  W-CRT.
             03  W-ACT       PIC  9(001).
             03  W-TNC       PIC  9(004).
             03  W-NAME      PIC  N(026).
             03  W-BSC       PIC  9(001).
             03  W-DMM       PIC  9(001).
      *
           02  W-L            PIC  9(002).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           02  W-C3           PIC  9(002).
           02  W-CHK          PIC  X(001).
           02  W-END          PIC  9(001).
           COPY LSTAT.
      *
      *FD  WTNAF
       01  WTNAF_JHS81I.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JHS81I".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TNC     PIC  9(004).
           02  WTNA-NAME      PIC  N(026).
           02  WTNA-BSC       PIC  9(001).
           02  F              PIC  X(007).
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
           02  FILLER  PIC  N(010) VALUE
                "ワークマン店名　入力".
           02  FILLER  PIC  X(043) VALUE
                "登録=1 修正=2 削除=3 問合せ=5 終了=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "店番".
             03  FILLER  PIC  N(002) VALUE "店名".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "場所".
             03  FILLER  PIC  X(015) VALUE
                  "0=伊勢崎,1=竜王".
           02  FILLER  PIC  X(028) VALUE
                "確認（OK=1,NO=9）--->   ﾘﾀｰﾝ".
           02  FILLER.
             03  FILLER PIC X(18) VALUE "UND (07,06) (,14)_".
             03  FILLER PIC X(18) VALUE "UND (07,17) (,73)_".
           02  FILLER   PIC X(18) VALUE "UND (09,17) (,22)_".
       01  C-MID2.
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "№".
             03  FILLER  PIC  N(003) VALUE "店　名".
             03  FILLER  PIC  N(001) VALUE "№".
             03  FILLER  PIC  N(003) VALUE "店　名".
             03  FILLER  PIC  N(001) VALUE "№".
             03  FILLER  PIC  N(003) VALUE "店　名".
           02  FILLER  PIC  X(030) VALUE
                "確認 (NEXT=1,NO=9) --->   ﾘﾀｰﾝ".
           02  FILLER        PIC X(37)   
                     VALUE "OVE (05,02) (,80)_ UND (05,02) (,80)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,02) (22)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,28) (22)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,54) (22)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,80) (22)_".
           02  FILLER        PIC X(18)   VALUE "UND (22,02) (,80)_".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-TNC   PIC  9(004).
             03  A-NAME  PIC  N(026).
           02  A-BSC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-TNC   PIC  9(004).
             03  D-BSC   PIC  X(001).
             03  D-NAME  PIC  N(010).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME9   PIC  N(005) VALUE
                  "キャンセル".
             03  E-ME11  PIC  X(026) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  REWRITE ｴﾗｰ   ***".
             03  E-ME13  PIC  X(027) VALUE
                  "***  DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(004).
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
            "C-MID" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "30" "20" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "19" "43" "01C-MID" " " RETURNING RESU.
      *C-MID1.
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" " " "7" "0" "8" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID1" "N" "7" "6" "4" " " "01C-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID1" "N" "7" "17" "4" "0101C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" " " "9" "0" "19" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID1" "N" "9" "17" "4" " " "02C-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID1" "X" "9" "25" "15" "0102C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "X" "23" "43" "28" "02C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" " " "7" "0" "54" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID1" "A" "7" "6" "18" " " "04C-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID1" "A" "7" "17" "18" "0104C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" "A" "9" "17" "18" "04C-MID1" " " RETURNING RESU.
      *C-MID2.
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" " " "5" "0" "24" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID2" "N" "5" "4" "2" " " "01C-MID2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID2" "N" "5" "8" "6" "0101C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-MID2" "N" "5" "30" "2" "0201C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-MID2" "N" "5" "34" "6" "0301C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0501C-MID2" "N" "5" "56" "2" "0401C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0601C-MID2" "N" "5" "60" "6" "0501C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" "X" "23" "41" "30" "01C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID2" "A" "5" "2" "37" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID2" "A" "5" "2" "17" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID2" "A" "5" "28" "17" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID2" "A" "5" "54" "17" "05C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID2" "A" "5" "80" "17" "06C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID2" "A" "22" "2" "18" "07C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "7" "0" "56" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNC" "9" "7" "11" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNC" BY REFERENCE W-TNC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "7" "22" "52" "A-TNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BSC" "9" "9" "22" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BSC" BY REFERENCE W-BSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "A-BSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "25" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNC" "9" "W-L" "W-C1" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNC" BY REFERENCE WTNA-TNC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BSC" "X" "W-L" "W-C2" "1" "D-TNC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BSC" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "W-C3" "20" "D-BSC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE WTNA-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "129" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "129" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "N" "24" "15" "10" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "27" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "4" "E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE WTNA-TNC "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           MOVE ZERO TO W-CRT.
      *
           CALL "DB_F_Open" USING
            "I-O" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-900
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3 AND 5
               GO TO M-040
           END-IF
           IF  W-ACT = 5
               GO TO M-200
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-TNC "A-TNC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-TNC = ZERO
               GO TO M-060
           END-IF
      *
           MOVE W-TNC TO WTNA-KEY.
      *           READ WTNAF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-080
           END-IF
           MOVE WTNA-NAME TO W-NAME.
           MOVE WTNA-BSC TO W-BSC.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BSC" A-BSC "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           GO TO M-100.
       M-080.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "52"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF.
       M-110.
           CALL "SD_Accept" USING BY REFERENCE A-BSC "A-BSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-110
           END-IF
           IF  W-BSC NOT = 0 AND 1 AND 9
               GO TO M-110
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-110
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-120
           END-IF
      *
           IF  W-ACT NOT = 1
               GO TO M-140
           END-IF
           INITIALIZE WTNA-R.
           MOVE W-TNC TO WTNA-KEY.
           MOVE W-NAME TO WTNA-NAME.
           MOVE W-BSC TO WTNA-BSC.
      *           WRITE WTNA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            WTNAF_PNAME1 WTNAF_LNAME WTNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-180.
       M-140.
           IF  W-ACT NOT = 2
               GO TO M-160
           END-IF
           MOVE W-NAME TO WTNA-NAME.
           MOVE W-BSC TO WTNA-BSC.
      *           REWRITE WTNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            WTNAF_PNAME1 WTNAF_LNAME WTNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-180.
       M-160.
      *           DELETE WTNAF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING WTNAF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF.
       M-180.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           GO TO M-060.
       M-200.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           MOVE 0 TO W-END.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 3 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 7 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 8 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
      *
      *           READ WTNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" WTNAF_PNAME1 BY REFERENCE WTNA-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-240
           END-IF
           IF  WTNA-BSC = 0
               MOVE SPACE TO W-CHK
           ELSE
               MOVE "*" TO W-CHK
           END-IF.
       M-220.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 23
               CALL "SD_Output" USING "D-TNC" D-TNC "p" RETURNING RESU
               CALL "SD_Output" USING "D-BSC" D-BSC "p" RETURNING RESU
               CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU
               GO TO M-280
           END-IF
           IF  W-C1 = 3
               MOVE 5 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 29 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 33 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 34 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               GO TO M-220
           ELSE 
               IF  W-C1 = 29
                   MOVE 5 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   MOVE 55 TO W-C1
                   CALL "SD_Arg_Match_Col" USING
                    "W-C1" "2" W-C1 RETURNING RESU
                   MOVE 59 TO W-C2
                   CALL "SD_Arg_Match_Col" USING
                    "W-C2" "2" W-C2 RETURNING RESU
                   MOVE 60 TO W-C3
                   CALL "SD_Arg_Match_Col" USING
                    "W-C3" "2" W-C3 RETURNING RESU
                   GO TO M-220
               END-IF
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-DMM = 9
               GO TO M-260
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-240
           END-IF
           IF  W-END = 1
               GO TO M-260
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 3 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 7 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 8 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           GO TO M-220.
       M-260.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           MOVE ZERO TO W-CRT.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           GO TO M-040.
       M-280.
      *           READ WTNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" WTNAF_PNAME1 BY REFERENCE WTNA-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-240
           END-IF
           IF  WTNA-BSC = 0
               MOVE SPACE TO W-CHK
           ELSE
               IF  WTNA-BSC = 1
                   MOVE "*" TO W-CHK
               ELSE
                   IF  WTNA-BSC = 9
                       MOVE "#" TO W-CHK
                   END-IF
               END-IF
           END-IF
           GO TO M-220.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
