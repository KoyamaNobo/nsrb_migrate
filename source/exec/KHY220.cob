       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY220.
      *********************************************************
      *    PROGRAM         :  工品仕掛品棚卸　入力    　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKY22                          *
      *        変更　　　  :  62/04/10                        *
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
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　工品仕掛品棚卸　入力リスト　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  F            PIC  X(001).
             03  P-KC1        PIC  9(001).
             03  F            PIC  X(002).
             03  P-HCD1       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  X(020).
             03  P-SU1        PIC ----,--9.99.
             03  F            PIC  X(002).
             03  P-X1         PIC  X(001).
             03  F            PIC  X(003).
             03  P-KC2        PIC  9(001).
             03  F            PIC  X(002).
             03  P-HCD2       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  X(020).
             03  P-SU2        PIC ----,--9.99.
             03  F            PIC  X(002).
             03  P-X2         PIC  X(001).
             03  F            PIC  X(003).
             03  P-KC3        PIC  9(001).
             03  F            PIC  X(002).
             03  P-HCD3       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME3      PIC  X(020).
             03  P-SU3        PIC ----,--9.99.
       01  W-DATA.
           02  W-KC           PIC  9(001).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-POC          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIKHM.
           COPY LSPF.
      *FD  KHTN-F
       01  KHTN-F_KHY220.
           02  KHTN-F_PNAME1  PIC  X(005) VALUE "KHTNF".
           02  F              PIC  X(001).
           02  KHTN-F_LNAME   PIC  X(013) VALUE "KHTN-F_KHY220".
           02  F              PIC  X(001).
           02  KHTN-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTN-F_SORT    PIC  X(100) VALUE SPACE.
           02  KHTN-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTN-F_RES     USAGE  POINTER.
       01  KHTN-R.
           02  KHTN-KC        PIC  9(001).
           02  KHTN-HCD       PIC  X(005).
           02  KHTN-SU        PIC S9(006)V9(02).
           02  KHTN-YC        PIC  9(002).
           02  KHTN-NC        PIC  9(001).
           02  F              PIC  X(003).
           02  KHTN-PC        PIC  9(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　工品仕掛品　棚卸入力リスト　　＊＊＊".
           02  FILLER  PIC  X(050) VALUE
                "[   追加分作表=1  全件作表=5  作表しない=9       ]".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-KC    PIC  9(001).
             03  A-HCD   PIC  X(005).
             03  A-SU    PIC S9(006)V9(02).
             03  A-DMM   PIC  9(001).
           02  A-PC    PIC  9(001).
           02  A-DMMD  PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-NAME  PIC  X(020).
             03  D-SU    PIC ZZZZZ9.99- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  KH-M ﾅｼ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  KHTNF WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  KHTNF REWRITE ｴﾗｰ  ***".
             03  E-REC   PIC  X(017).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "118" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "17" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "8" "14" "50" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "20" "29" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "15" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "13" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KC" "9" "W-L" "18" "1" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KC" BY REFERENCE W-KC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "X" "W-L" "21" "5" "A-KC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE KHTN-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-L" "48" "8" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE KHTN-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "61" "1" "A-SU" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PC" "9" "8" "59" "1" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMMD" "9" "20" "46" "1" "A-PC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMMD" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "30" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "30" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "X" "W-L" "27" "20" " " "01C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZ9.99-" "W-L" "48" "10" "D-NAME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE KHTN-SU "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "86" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "86" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-REC" "X" "24" "50" "17" "E-ME3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-REC" BY REFERENCE KHTN-R "21" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" KHTN-F_PNAME1 " " BY REFERENCE KHTN-F_IDLST "0".
           MOVE ZERO TO W-DATA.
           CALL "SD_Screen_Output" USING "SCKY22" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 22
               CALL "SD_Screen_Output" USING "SCKY22" RETURNING RESU
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           IF  W-KC NOT = 0
               CALL "SD_Output" USING "A-KC" A-KC "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KC "A-KC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-KC = 0
               GO TO M-15
           END-IF.
       M-20.
           INITIALIZE KHTN-R.
           MOVE W-KC TO KHTN-KC.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-50
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE KHTN-HCD TO KH-HCD.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           MOVE KH-YC TO KHTN-YC.
           MOVE KH-NC TO KHTN-NC.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  KHTN-SU = ZERO
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               GO TO M-20
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
      *
      *           WRITE KHTN-R.
      *//////////////
           CALL "DB_Insert" USING
            KHTN-F_PNAME1 KHTN-F_LNAME KHTN-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-10.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-PC "A-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-PC NOT = 1 AND 5 AND 9
               GO TO M-55
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMMD "A-DMMD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = 9
               GO TO M-55
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-60
           END-IF
           IF  W-PC = 9
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" KHTN-F_PNAME1 " " BY REFERENCE KHTN-F_IDLST "0".
       M-65.
      *           READ KHTN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTN-F_PNAME1 BY REFERENCE KHTN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1
               GO TO M-95
           END-IF
           IF  W-PC = 1
               IF  KHTN-PC NOT = 0
                   GO TO M-65
               END-IF
           END-IF
           MOVE SPACE TO W-P.
       M-70.
           MOVE KHTN-KC TO W-KC.
           MOVE 0 TO CHK.
       M-75.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           IF  KHTN-PC = 9
               GO TO M-80
           END-IF
           MOVE 9 TO KHTN-PC.
      *           REWRITE KHTN-R.
      *///////////////
           CALL "DB_Update" USING
            KHTN-F_PNAME1 KHTN-F_LNAME KHTN-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-REC" E-REC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       M-80.
      *           READ KHTN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTN-F_PNAME1 BY REFERENCE KHTN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-PC = 1
               IF  KHTN-PC NOT = 0
                   GO TO M-80
               END-IF
           END-IF
           IF  KHTN-KC NOT = W-KC
               GO TO M-70
           END-IF
           GO TO M-75.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1.
           PERFORM S-40 THRU S-50.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-25
           END-IF
           ADD 1 TO W-CD.
           IF  W-CD NOT = 3
               MOVE ZERO TO W-LD CHK
               GO TO S-20
           END-IF
           PERFORM S-40 THRU S-50.
           MOVE ZERO TO W-LD W-CD CHK.
           MOVE SPACE TO W-P.
           GO TO S-20.
       S-25.
           EXIT.
       S-30.
           MOVE KHTN-HCD TO KH-HCD.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ﾏｽﾀｰ ﾐﾄｳﾛｸ          " TO KH-NAME
           END-IF
           IF  W-CD = 0
               MOVE KHTN-HCD TO P-HCD1(W-LD)
               MOVE KH-NAME TO P-NAME1(W-LD)
               MOVE KHTN-SU TO P-SU1(W-LD)
               MOVE ":" TO P-X1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE KHTN-KC TO P-KC1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE KHTN-HCD TO P-HCD2(W-LD)
               MOVE KH-NAME TO P-NAME2(W-LD)
               MOVE KHTN-SU TO P-SU2(W-LD)
               MOVE ":" TO P-X2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE KHTN-KC TO P-KC2(W-LD)
               END-IF
           END-IF
           IF  W-CD = 2
               MOVE KHTN-HCD TO P-HCD3(W-LD)
               MOVE KH-NAME TO P-NAME3(W-LD)
               MOVE KHTN-SU TO P-SU3(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE KHTN-KC TO P-KC3(W-LD)
               END-IF
           END-IF.
       S-35.
           EXIT.
       S-40.
           IF  W-POC = 0
               MOVE 9 TO W-POC
               CALL "PR_Open" RETURNING RESP
               ACCEPT H-DATE FROM DATE
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-45.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               IF  P-X1(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-45
               END-IF
           END-IF.
       S-50.
           EXIT.
