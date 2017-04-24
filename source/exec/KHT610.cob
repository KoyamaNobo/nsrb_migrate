       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHT610.
      *********************************************************
      *    PROGRAM         :  用途区分日付別　売上実績問合せ  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  62/04/06                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-YC           PIC  9(002).
           02  W-PEY          PIC  9(002).
           02  W-KIN          PIC S9(008).
           02  W-TKIN         PIC S9(008).
           02  W-L            PIC  9(002).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKKBM.
      *FD  URIR-F
       01  URIR-F_KHT610.
           02  URIR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHT610".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  UR-DC          PIC  9(001).
           02  F              PIC  X(006).
           02  UR-PEY         PIC  9(002).
           02  F              PIC  X(004).
           02  UR-HCD         PIC  X(005).
           02  UR-SU          PIC S9(006)V9(02).
           02  F              PIC  X(008).
           02  UR-KIN         PIC S9(008).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(084).
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
       01  C-ACP.
           02  A-YC    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-YCN   PIC  N(016).
           02  D-DATA.
             03  01D-DATA  PIC ZZ .
             03  02D-DATA  PIC ----,---,--9 .
           02  D-TOTAL.
             03  01D-TOTAL PIC ----,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC" "9" "4" "19" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YC" BY REFERENCE W-YC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "56" "1" "A-YC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "58" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YCN" "N" "4" "33" "32" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-YCN" BY REFERENCE KKB-YCN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "14" "D-YCN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "ZZ" "W-L" "W-C1" "2" " " "D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "----,---,--9" "W-L" "W-C2" "12" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOTAL" " " "20" "0" "12" "D-DATA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TOTAL" "----,---,--9" "20" "45" "12" " " "D-TOTAL"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TOTAL" BY REFERENCE W-TKIN "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "107" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "107" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "0" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "0" "41" "40" "01E-CL" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
       M-10.
           CALL "SD_Screen_Output" USING "SCKT61" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-YC "A-YC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = ADV
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE W-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-YCN" D-YCN "p" RETURNING RESU.
           GO TO M-25.
       M-20.
      *           READ KKB-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KKB-M_PNAME1 BY REFERENCE KKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST
                "1" "KKB-KEY" BY REFERENCE KKB-KEY
               GO TO M-20
           END-IF
           IF  KKB-NO NOT = 01
               GO TO M-20
           END-IF
           MOVE KKB-YC TO W-YC.
           CALL "SD_Output" USING "A-YC" A-YC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YCN" D-YCN "p" RETURNING RESU.
       M-25.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 25 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 28 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           MOVE ZERO TO W-TKIN.
       M-30.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  UR-YC NOT = W-YC
               GO TO M-30
           END-IF
           IF  UR-KIN = ZERO
               GO TO M-30
           END-IF
           IF  UR-DC = 5 OR 9
               GO TO M-30
           END-IF.
       M-35.
           MOVE UR-PEY TO W-PEY.
           MOVE ZERO TO W-KIN.
       M-40.
           IF  UR-DC = 8
               SUBTRACT UR-KIN FROM W-KIN W-TKIN
           ELSE
               ADD UR-KIN TO W-KIN W-TKIN
           END-IF.
       M-45.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  UR-KIN = ZERO
               GO TO M-45
           END-IF
           IF  UR-DC = 5 OR 9
               GO TO M-45
           END-IF
           IF  UR-YC NOT = W-YC
               GO TO M-50
           END-IF
           IF  UR-PEY = W-PEY
               GO TO M-40
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-35.
       M-50.
           PERFORM S-05 THRU S-10.
       M-55.
           CALL "SD_Output" USING "D-TOTAL" D-TOTAL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               MOVE 6 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               ADD 17 TO W-C1 W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
       S-10.
           EXIT.
