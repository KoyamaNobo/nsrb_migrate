       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG900.
      *********************************************************
      *    PROGRAM         :  入出庫振替単価チェック  　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/17                        *
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
           02  W-AHCD.
             03  W-HCDD  OCCURS  80.
               04  W-HCD      PIC  9(006).
           02  W-HCDW         PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-END          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHIM.
      *FD  SNTR-F
       01  SNTR-F_HMG900.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMG900".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE      PIC  9(008).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(051).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(046).
           02  SNTR-UNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  UTR-F
       01  UTR-F_HMG900.
           02  UTR-F_PNAME1   PIC  X(004) VALUE "UTRF".
           02  F              PIC  X(001).
           02  UTR-F_LNAME    PIC  X(012) VALUE "UTR-F_HMG900".
           02  F              PIC  X(001).
           02  UTR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  UTR-F_SORT     PIC  X(100) VALUE SPACE.
           02  UTR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  UTR-F_RES      USAGE  POINTER.
       01  UTR-R.
           02  UTR-NO         PIC  9(007).
           02  UTR-DATE       PIC  9(008).
           02  UTR-NGPD  REDEFINES UTR-DATE.
             03  UTR-NG       PIC  9(006).
             03  F            PIC  9(002).
           02  UTR-HCD        PIC  9(006).
           02  UTR-SIZ        PIC  9(001).
           02  UTR-SUD.
             03  UTR-SU       PIC S9(004)  OCCURS  10.
           02  UTR-SUT        PIC S9(005).
           02  UTR-BKIN       PIC S9(008).
           02  UTR-FKIN       PIC S9(008).
           02  UTR-NRC        PIC  9(001).
           02  UTR-SSC        PIC  9(001).
           02  UTR-HPC        PIC  9(001).
           02  UTR-SKC        PIC  9(001).
           02  UTR-BC.
             03  UTR-BC1      PIC  9(002).
             03  UTR-BC2      PIC  9(002).
             03  UTR-BC3      PIC  9(002).
           02  F              PIC  X(034).
           02  UTR-PRC        PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　振替単価　未決定　チェック　　＊＊＊".
       01  C-DSP.
           02  D-HCD   PIC  9(006).
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-HCD   PIC  9(006).
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
           "C-MID" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "15" "46" " " "C-MID" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-HCD" "9" "W-L" "W-C" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-HCD" BY REFERENCE W-HCD(1) "6" "1" BY REFERENCE CNT 6
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "72" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "22" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "35" "6" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE W-HCDW "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 0 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-UNC = 1
               GO TO M-10
           END-IF
           IF  SNTR-FT NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE SNTR-HCD TO W-HCDW.
           PERFORM S-05 THRU S-10.
           IF  CNT = 81
               CALL "DB_F_Close" USING
                BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1
               GO TO M-30
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           ADD 1 TO CNT.
           ADD 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 11 OR 21 OR 31 OR 41 OR 51 OR 61 OR 71
               ADD 2 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 7 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  CNT = 81
               GO TO M-25
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" UTR-F_PNAME1 " " BY REFERENCE UTR-F_IDLST "0".
       M-20.
      *           READ UTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UTR-F_PNAME1 BY REFERENCE UTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           MOVE UTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE UTR-HCD TO W-HCDW
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  HI-FT NOT = 1
               GO TO M-20
           END-IF
           MOVE UTR-HCD TO W-HCDW.
           PERFORM S-05 THRU S-10.
           IF  CNT = 81
               GO TO M-25
           END-IF
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTR-F_IDLST UTR-F_PNAME1.
       M-30.
           IF  CNT NOT = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                BY REFERENCE ESTAT RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD 1 TO CNT.
           ADD 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 11 OR 21 OR 31 OR 41 OR 51 OR 61 OR 71
               ADD 2 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 7 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  CNT = 81
               GO TO S-10
           END-IF
           MOVE W-HCDW TO W-HCD(CNT).
           CALL "SD_Output" USING "D-HCD" D-HCD "p" RETURNING RESU.
       S-10.
           EXIT.
