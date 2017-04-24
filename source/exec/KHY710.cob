       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY710.
      *********************************************************
      *    PROGRAM         :  工品年間販売計画入力            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKY75                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NEN          PIC  9(004).
           02  W-NENL  REDEFINES W-NEN.
             03  W-NEN1       PIC  9(002).
             03  W-NEN2       PIC  9(002).
           02  W-MEI.
             03  WB-AKIN.
               04  WB-KIND  OCCURS  12.
                 05  WB-KIN   PIC S9(009).
             03  WH-AKIN.
               04  WH-KIND  OCCURS  12.
                 05  WH-KIN   PIC S9(009).
             03  WB-KKIN      PIC S9(009).
             03  WH-KKIN      PIC S9(009).
             03  WB-SKIN      PIC S9(009).
             03  WH-SKIN      PIC S9(009).
             03  WB-TKIN      PIC S9(009).
             03  WH-TKIN      PIC S9(009).
           02  W-INV.
             03  WB-C         PIC  9(001).
             03  WH-C         PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
      *FD  KBHKF
       01  KBHKF_KHY710.
           02  KBHKF_PNAME1   PIC  X(005) VALUE "KBHKF".
           02  F              PIC  X(001).
           02  KBHKF_LNAME    PIC  X(012) VALUE "KBHKF_KHY710".
           02  F              PIC  X(001).
           02  KBHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  KBHKF_KEY2     PIC  X(100) VALUE SPACE.
           02  KBHKF_SORT     PIC  X(100) VALUE SPACE.
           02  KBHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  KBHKF_RES      USAGE  POINTER.
       01  KBHK-R.
           02  KBHK-KEY.
             03  KBHK-NEN     PIC  9(004).
             03  KBHK-BMN     PIC  9(002).
           02  KBHK-AKIN.
             03  KBHK-KIND  OCCURS  12.
               04  KBHK-KIN   PIC S9(009).
           02  F              PIC  X(014).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　工品販売計画　入力　　＊＊＊".
       01  C-ACP.
           02  A-NEN   PIC  9(002).
           02  FILLER.
             03  AB-KIN   PIC S9(009).
             03  AH-KIN   PIC S9(009).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  DB-KIN   PIC ZZZZZZZZZ- .
             03  DH-KIN   PIC ZZZZZZZZZ- .
           02  FILLER.
             03  DB-KKIN  PIC ZZZ,ZZZ,ZZZ- .
             03  DH-KKIN  PIC ZZZ,ZZZ,ZZZ- .
           02  FILLER.
             03  DB-SKIN  PIC ZZZ,ZZZ,ZZZ- .
             03  DH-SKIN  PIC ZZZ,ZZZ,ZZZ- .
           02  FILLER.
             03  DB-TKIN  PIC ZZZ,ZZZ,ZZZ- .
             03  DH-TKIN  PIC ZZZ,ZZZ,ZZZ- .
       01  C-ERR.
           02  FILLER.
             03  E-KEY.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(002).
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  ﾐﾄｳﾛｸ  ***".
             03  E-ME3   PIC  X(015) VALUE
                  "***  ｷｬﾝｾﾙ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(020) VALUE
                  "***  DETETE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "38" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "25" "38" " " "C-MID"  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "21" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "1" "9" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "W-L" "0" "18" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "AB-KIN" "S9" "W-L" "17" "9" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AB-KIN" BY REFERENCE WB-KIN(1) "9" "1" BY REFERENCE CNT 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "AH-KIN" "S9" "W-L" "53" "9" "AB-KIN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "AH-KIN" BY REFERENCE WH-KIN(1) "9" "1" BY REFERENCE CNT 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "73" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "92" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "20" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DB-KIN" "ZZZZZZZZZ-" "W-L" "17" "10" " " "01C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DB-KIN" BY REFERENCE WB-KIN(1) "9" "1" BY REFERENCE CNT 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DH-KIN" "ZZZZZZZZZ-" "W-L" "53" "10" "DB-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DH-KIN" BY REFERENCE WH-KIN(1) "9" "1" BY REFERENCE CNT 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "11" "0" "24" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DB-KKIN" "ZZZ,ZZZ,ZZZ-" "11" "15" "12" " " "02C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DB-KKIN" BY REFERENCE WB-KKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DH-KKIN" "ZZZ,ZZZ,ZZZ-" "11" "51" "12" "DB-KKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DH-KKIN" BY REFERENCE WH-KKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "18" "0" "24" "02C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DB-SKIN" "ZZZ,ZZZ,ZZZ-" "18" "15" "12" " " "03C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DB-SKIN" BY REFERENCE WB-SKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DH-SKIN" "ZZZ,ZZZ,ZZZ-" "18" "51" "12" "DB-SKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DH-SKIN" BY REFERENCE WH-SKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "19" "0" "24" "03C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DB-TKIN" "ZZZ,ZZZ,ZZZ-" "19" "15" "12" " " "04C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DB-TKIN" BY REFERENCE WB-TKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DH-TKIN" "ZZZ,ZZZ,ZZZ-" "19" "51" "12" "DB-TKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DH-TKIN" BY REFERENCE WH-TKIN "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "114" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "114" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" " " "24" "0" "6" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-KEY" "9" "24" "40" "4" " " "E-KEY"  RETURNING RESU.
       CALL "SD_From" USING 
            "01E-KEY" BY REFERENCE KBHK-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-KEY" "9" "24" "45" "2" "01E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02E-KEY" BY REFERENCE KBHK-BMN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "15" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "21" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "20" "E-ME12" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKY75" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "I-O" KBHKF_PNAME1 "SHARED" BY REFERENCE KBHKF_IDLST "1"
            "KBHK-KEY" BY REFERENCE KBHK-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
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
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKY75" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           MOVE ZERO TO W-MEI W-INV.
      *
           MOVE W-NEN TO KBHK-NEN.
           MOVE 32 TO KBHK-BMN.
      *           READ KBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO WB-C
               GO TO M-15
           END-IF
           MOVE KBHK-AKIN TO WB-AKIN.
           MOVE 2 TO WB-C.
       M-15.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 33 TO KBHK-BMN.
      *           READ KBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO WH-C
               GO TO M-20
           END-IF
           MOVE KBHK-AKIN TO WH-AKIN.
           MOVE 2 TO WH-C.
       M-20.
           MOVE ZERO TO CNT.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 13
               GO TO M-30
           END-IF
           IF  CNT = 7
               CALL "SD_Output" USING
                "DB-KKIN" DB-KKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "DH-KKIN" DH-KKIN "p" RETURNING RESU
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           CALL "SD_Output" USING "DB-KIN" DB-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DH-KIN" DH-KIN "p" RETURNING RESU.
           ADD WB-KIN(CNT) TO WB-TKIN.
           ADD WH-KIN(CNT) TO WH-TKIN.
           IF  CNT < 7
               ADD WB-KIN(CNT) TO WB-KKIN
               ADD WH-KIN(CNT) TO WH-KKIN
           ELSE
               ADD WB-KIN(CNT) TO WB-SKIN
               ADD WH-KIN(CNT) TO WH-SKIN
           END-IF
           GO TO M-25.
       M-30.
           CALL "SD_Output" USING "DB-SKIN" DB-SKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DH-SKIN" DH-SKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DB-TKIN" DB-TKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DH-TKIN" DH-TKIN "p" RETURNING RESU.
      *
           MOVE 0 TO W-C.
       M-35.
           MOVE ZERO TO CNT.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-40.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 13
               GO TO M-55
           END-IF
           IF  CNT = 7
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  W-C = 0
                   COMPUTE WB-KKIN = WB-KIN(01) + WB-KIN(02)
                                   + WB-KIN(03) + WB-KIN(04)
                                   + WB-KIN(05) + WB-KIN(06)
                   CALL "SD_Output" USING
                    "DB-KKIN" DB-KKIN "p" RETURNING RESU
               ELSE
                   COMPUTE WH-KKIN = WH-KIN(01) + WH-KIN(02)
                                   + WH-KIN(03) + WH-KIN(04)
                                   + WH-KIN(05) + WH-KIN(06)
                   CALL "SD_Output" USING
                    "DH-KKIN" DH-KKIN "p" RETURNING RESU
               END-IF
           END-IF.
       M-45.
           IF  W-C = 0
               CALL "SD_Accept" USING BY REFERENCE AB-KIN "AB-KIN" 
                "S9" "9" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE AH-KIN "AH-KIN"
                "S9" "9" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-C = 0
               CALL "SD_Output" USING "DB-KIN" DB-KIN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DH-KIN" DH-KIN "p" RETURNING RESU
           END-IF
           GO TO M-40.
       M-50.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               IF  W-C = 0
                   GO TO M-10
               ELSE
                   MOVE 0 TO W-C
                   MOVE 12 TO CNT
                   MOVE 17 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   GO TO M-45
               END-IF
           END-IF
           IF  CNT = 6
               SUBTRACT 1 FROM W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           GO TO M-45.
       M-55.
           IF  W-C = 0
               COMPUTE WB-SKIN = WB-KIN(07) + WB-KIN(08)
                               + WB-KIN(09) + WB-KIN(10)
                               + WB-KIN(11) + WB-KIN(12)
               COMPUTE WB-TKIN = WB-KKIN + WB-SKIN
               CALL "SD_Output" USING
                "DB-SKIN" DB-SKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "DB-TKIN" DB-TKIN "p" RETURNING RESU
               MOVE 1 TO W-C
               GO TO M-35
           ELSE
               COMPUTE WH-SKIN = WH-KIN(07) + WH-KIN(08)
                               + WH-KIN(09) + WH-KIN(10)
                               + WH-KIN(11) + WH-KIN(12)
               COMPUTE WH-TKIN = WH-KKIN + WH-SKIN
               CALL "SD_Output" USING
                "DH-SKIN" DH-SKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "DH-TKIN" DH-TKIN "p" RETURNING RESU
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCKY75" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  ESTAT = BTB
               MOVE 1 TO W-C
               MOVE 12 TO CNT
               MOVE 17 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-60
           END-IF
      *
           IF  WB-C NOT = 1
               GO TO M-65
           END-IF
           MOVE ZERO TO KBHK-R.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 32 TO KBHK-BMN.
           MOVE WB-AKIN TO KBHK-AKIN.
      *           WRITE KBHK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            KBHKF_PNAME1 KBHKF_LNAME KBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-70.
       M-65.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 32 TO KBHK-BMN.
      *           READ KBHKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE WB-AKIN TO KBHK-AKIN.
      *           REWRITE KBHK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBHKF_PNAME1 KBHKF_LNAME KBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-70.
           IF  WH-C NOT = 1
               GO TO M-75
           END-IF
           MOVE ZERO TO KBHK-R.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 33 TO KBHK-BMN.
           MOVE WH-AKIN TO KBHK-AKIN.
      *           WRITE KBHK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            KBHKF_PNAME1 KBHKF_LNAME KBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-95.
       M-75.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 33 TO KBHK-BMN.
      *           READ KBHKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE WH-AKIN TO KBHK-AKIN.
      *           REWRITE KBHK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBHKF_PNAME1 KBHKF_LNAME KBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KBHKF_IDLST KBHKF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
