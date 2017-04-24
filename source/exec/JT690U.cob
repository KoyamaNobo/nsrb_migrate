       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT690U.
      *********************************************************
      *    PROGRAM         :  発送明細ファイル　作成          *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *    DATE      　　  :  96/06/17                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-RTEKI        PIC  N(024).
           02  W-RTKAD   REDEFINES  W-RTEKI.
             03  W-RTKAD  OCCURS  24.
               04  W-RTK      PIC  N(001).
           02  W-STEKI        PIC  N(023).
           02  W-STEKID  REDEFINES  W-STEKI.
             03  W-STKD  OCCURS  23.
               04  W-STK      PIC  N(001).
           02  W-WTEKI        PIC  N(023).
           02  W-WTEKID  REDEFINES  W-WTEKI.
             03  W-WTKD  OCCURS  23.
               04  W-WTK      PIC  N(001).
           02  W-SHTST        PIC  N(009).
           02  W-STEKID  REDEFINES  W-SHTST.
             03  W-SHTD  OCCURS   9.
               04  W-SHT      PIC  N(001).
           02  CNT1           PIC  9(002).
           02  CNT2           PIC  9(002).
           02  CNTD           PIC  9(002).
           02  W-DNO          PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-TCCD         PIC  9(007).
           02  W-SOC          PIC  9(001).
      *
           COPY LITM.
           COPY L-JSTR.
           COPY LIHSMS.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　発送明細接続ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(033) VALUE
                    "***  ﾆｼﾞｭｳ ｴﾗｰ  (NO.       )  ***".
               04  FILLER  PIC  X(007).
             03  E-ME2.
               04  FILLER  PIC  X(033) VALUE
                    "***  WRITE ｴﾗｰ  (NO.       )  ***".
               04  FILLER  PIC  X(007).
             03  E-ME3.
               04  FILLER  PIC  X(034) VALUE
                    "***  DELETE ｴﾗｰ  (NO.       )  ***".
               04  FILLER  PIC  X(007).
             03  E-ME4.
               04  FILLER  PIC  X(032) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  (NO.       )  ***".
               04  FILLER  PIC  X(007).
               04  FILLER  PIC  9(004).
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(024) VALUE
                   "オーバーフロー、領域を拡張後、「ＦＮＣ＋ＰＦ５」".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
             03  E-STAT  PIC  X(002).
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
            "01C-MID" "N" "3" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "46" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "321" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "321" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" " " "24" "0" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME1" "X" "24" "15" "33" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME1" "X" "24" "35" "7" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME1" BY REFERENCE JSTR-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" " " "24" "0" "40" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME2" "X" "24" "15" "33" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME2" "X" "24" "35" "7" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" " " "24" "0" "41" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME3" "X" "24" "15" "34" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME3" "X" "24" "35" "7" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" " " "24" "0" "43" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME4" "X" "24" "15" "32" " " "E-ME4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME4" "X" "24" "34" "7" "01E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME4" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME4" "9" "24" "50" "4" "02E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME4" BY REFERENCE HSMS-061 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "61" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "48" "01E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-CL" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 " " BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
       M-20.
      *           READ JSTR NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JSTR-17 NOT = 1
               GO TO M-20
           END-IF
           IF  JSTR-03     = 3
               GO TO M-20
           END-IF
           IF  ZERO = JSTR-1211(01) AND JSTR-1211(02) AND JSTR-1211(03)
                 AND JSTR-1211(04) AND JSTR-1211(05) AND JSTR-1211(06)
                 AND JSTR-1211(07) AND JSTR-1211(08) AND JSTR-1211(09)
                 AND JSTR-1211(10)
               GO TO M-20
           END-IF.
       M-25.
           MOVE JSTR-01 TO W-DNO.
           MOVE JSTR-05 TO W-NGP.
           MOVE JSTR-06 TO W-TCCD.
           MOVE JSTR-07 TO W-SOC.
           MOVE ALL "　" TO W-RTEKI W-STEKI W-SHTST.
           IF  ALL "　" = JSTR-14D AND JSTR-15
               GO TO M-30
           END-IF
           PERFORM S-05 THRU S-55.
       M-30.
           MOVE JSTR-KEY TO HSMS-KEY.
      *           READ HSMSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
      *           DELETE HSMSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSMSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-40
           END-IF.
       M-35.
           PERFORM S-60 THRU S-70.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF.
       M-40.
      *           READ JSTR NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  JSTR-17 NOT = 1
               GO TO M-40
           END-IF
           IF  JSTR-03     = 3
               GO TO M-40
           END-IF
           IF  ZERO = JSTR-1211(01) AND JSTR-1211(02) AND JSTR-1211(03)
                 AND JSTR-1211(04) AND JSTR-1211(05) AND JSTR-1211(06)
                 AND JSTR-1211(07) AND JSTR-1211(08) AND JSTR-1211(09)
                 AND JSTR-1211(10)
               GO TO M-40
           END-IF
           IF  JSTR-01 = W-DNO
               GO TO M-30
           END-IF
      *
           MOVE W-DNO TO HSMS-01.
           MOVE 7 TO HSMS-02.
      *           READ HSMSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
      *           DELETE HSMSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSMSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-45.
           PERFORM S-75 THRU S-85.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           GO TO M-25.
       M-85.
           MOVE W-DNO TO HSMS-01.
           MOVE 7 TO HSMS-02.
      *           READ HSMSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
      *           DELETE HSMSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSMSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-90.
           PERFORM S-75 THRU S-85.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE JSTR-15 TO W-STEKI.
           MOVE JSTR-14D TO W-SHTST.
           IF  JSTR-14D = ALL "　"
               MOVE W-STEKI TO W-RTEKI
               GO TO S-55
           END-IF
           MOVE 10 TO CNT1.
           MOVE 25 TO CNT2.
       S-10.
           SUBTRACT 1 FROM CNT1.
           IF  CNT1 = ZERO
               GO TO S-15
           END-IF
           IF  W-SHT(CNT1) NOT = "　"
               SUBTRACT 1 FROM CNT2
               MOVE W-SHT(CNT1) TO W-RTK(CNT2)
           END-IF
           GO TO S-10.
       S-15.
           IF JSTR-15 = ALL "　"
               GO TO S-55
           END-IF
           MOVE CNT2 TO CNTD.
           MOVE ZERO TO CNT1 CNT2.
       S-20.
           ADD 1 TO CNT1.
           IF  CNT1 = 24
               GO TO S-25
           END-IF
           IF  W-STK(CNT1) NOT = "　"
               MOVE CNT1 TO CNT2
           END-IF
           GO TO S-20.
       S-25.
           IF  CNTD <= CNT2
               GO TO S-35
           END-IF
           MOVE ZERO TO CNT1.
       S-30.
           ADD 1 TO CNT1.
           IF  CNT1 <= CNT2
               MOVE W-STK(CNT1) TO W-RTK(CNT1)
               GO TO S-30
           END-IF
           GO TO S-55.
       S-35.
           MOVE ZERO TO CNT1 CNT2.
       S-40.
           ADD 1 TO CNT1.
           IF  CNT1 = 24
               GO TO S-45
           END-IF
           IF  W-STK(CNT1) NOT = "　"
               ADD 1 TO CNT2
               MOVE W-STK(CNT1) TO W-WTK(CNT2)
           END-IF
           GO TO S-40.
       S-45.
           MOVE W-WTEKI TO W-STEKI.
           IF  CNTD <= CNT2
               COMPUTE CNT2 = CNTD - 1
           END-IF
           MOVE ZERO TO CNT1.
       S-50.
           ADD 1 TO CNT1.
           IF  CNT1 <= CNT2
               MOVE W-STK(CNT1) TO W-RTK(CNT1)
               GO TO S-50
           END-IF.
       S-55.
           EXIT.
       S-60.
           INITIALIZE HSMS-R1.
           MOVE JSTR-KEY TO HSMS-KEY.
           MOVE JSTR-03 TO HSMS-03.
           IF  HSMS-03 = 5
               MOVE 1 TO HSMS-03
           ELSE
               IF  HSMS-03 = 6
                   MOVE 2 TO HSMS-03
               END-IF
           END-IF
           MOVE JSTR-05 TO HSMS-05.
           MOVE JSTR-06 TO HSMS-06.
           MOVE JSTR-07 TO HSMS-07.
           MOVE JSTR-09 TO HSMS-09.
           MOVE JSTR-10 TO HSMS-10.
           MOVE JSTR-1211(01) TO HSMS-1211(01).
           MOVE JSTR-1211(02) TO HSMS-1211(02).
           MOVE JSTR-1211(03) TO HSMS-1211(03).
           MOVE JSTR-1211(04) TO HSMS-1211(04).
           MOVE JSTR-1211(05) TO HSMS-1211(05).
           MOVE JSTR-1211(06) TO HSMS-1211(06).
           MOVE JSTR-1211(07) TO HSMS-1211(07).
           MOVE JSTR-1211(08) TO HSMS-1211(08).
           MOVE JSTR-1211(09) TO HSMS-1211(09).
           MOVE JSTR-1211(10) TO HSMS-1211(10).
           MOVE JSTR-122 TO HSMS-122.
           MOVE JSTR-13 TO HSMS-13.
           MOVE JSTR-15A TO HSMS-14.
           IF  HSMS-062 = 001
               MOVE ZERO TO HSMS-062
           END-IF
           MOVE JSTR-20 TO HSMS-22.
           MOVE HSMS-061 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE T-TNC TO HSMS-20.
           MOVE JSTR-14 TO HSMS-24.
      *           WRITE HSMS-R1 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-65
           END-IF
           GO TO S-70.
       S-65.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO S-70
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE "HSMSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           GO TO S-60.
       S-70.
           EXIT.
       S-75.
           MOVE SPACE TO HSMS-R2.
           MOVE ALL "　" TO HSMS-15.
           MOVE 0 TO HSMS-03B HSMS-23B HSMS-24B HSMS-26B
                     HSMS-25B HSMS-19B.
           MOVE W-DNO TO HSMS-01B.
           MOVE 7 TO HSMS-02B.
           MOVE W-NGP TO HSMS-05B.
           MOVE W-TCCD TO HSMS-06B.
           MOVE W-SOC TO HSMS-07B.
           MOVE W-RTEKI TO HSMS-15.
      *           WRITE HSMS-R2 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-80
           END-IF
           GO TO S-85.
       S-80.
           IF ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO S-85
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE "HSMSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           GO TO S-75.
       S-85.
           EXIT.
