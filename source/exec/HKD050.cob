       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKD050.
      *********************************************************
      *    PROGRAM         :  請求明細ファイル　入金日　セット*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-TBL          PIC  X(024) VALUE
                "312831303130313130313031".
           02  W-GN    REDEFINES W-TBL.
             03  W-GN1   OCCURS  12  PIC  9(002).
           02  W-NGP1.
             03  W-NEN1       PIC  9(004).
             03  W-GET1       PIC  9(002).
             03  W-PEY1       PIC  9(002).
           02  W-NGP2.
             03  W-NEN2       PIC  9(004).
             03  W-GET2       PIC  9(002).
             03  W-PEY2       PIC  9(002).
           02  W-SIT.
             03  W-SI1   OCCURS  12  PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-SI           PIC  9(003).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
      *FD  SM-F
       01  SM-F_HKD050.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKD050".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  S-R.
           02  S-TCD          PIC  9(004).
           02  S-NGP.
             03  S-NG         PIC  9(006).
             03  S-PEY        PIC  9(002).
           02  S-ZS           PIC S9(009).
           02  S-ZSZ          PIC S9(007).
           02  S-UR           PIC S9(009).
           02  S-URZ          PIC S9(007).
           02  S-TS           PIC S9(007).
           02  S-TSZ          PIC S9(005).
           02  S-NK           PIC S9(009).
           02  S-NKZ          PIC S9(007).
           02  S-DATE         PIC  9(006).
           02  S-SI           PIC  9(003).
           02  S-SU           PIC  9(001).
           02  F              PIC  X(019).
           02  S-DC           PIC  9(001).
       77  F                  PIC  X(001).
      *FD  NYU-F
       01  NYU-F_HKD050.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HKD050".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  NYU-DATE       PIC  9(008).
           02  NYU-NGP   REDEFINES NYU-DATE.
             03  NYU-NG.
               04  NYU-NEN    PIC  9(004).
               04  NYU-GET    PIC  9(002).
             03  NYU-PEY      PIC  9(002).
           02  NYU-NGPL  REDEFINES NYU-DATE.
             03  F            PIC  9(002).
             03  NYU-NGPS     PIC  9(006).
           02  NYU-TCD        PIC  9(004).
           02  NYU-KIN        PIC S9(008).
           02  NYU-NC.
             03  NYU-NC1      PIC  9(001).
             03  NYU-NC2      PIC  9(001).
           02  F              PIC  9(001).
           02  NYU-TGK        PIC  9(008).
           02  NYU-TGKD  REDEFINES NYU-TGK.
             03  F            PIC  9(002).
             03  NYU-TGKS     PIC  9(006).
           02  NYU-SS.
             03  NYU-SSN      PIC  9(004).
             03  NYU-SSG      PIC  9(002).
           02  NYU-SSL   REDEFINES NYU-SS.
             03  F            PIC  9(002).
             03  NYU-SSS      PIC  9(004).
           02  NYU-BC         PIC  9(001).
           02  NYU-TC         PIC  9(002).
           02  F              PIC  X(003).
           02  NYU-KEY        PIC  X(007).
           02  F              PIC  X(052).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　請求明細Ｆ　入金日　変換　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" NYU-F_PNAME1 "SHARED" BY REFERENCE NYU-F_IDLST "1"
            "NYU-KEY" BY REFERENCE NYU-KEY.
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       M-10.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF
           IF  NYU-SS = ZERO
               GO TO M-10
           END-IF
           IF  NYU-NC2 = 8
               GO TO M-10
           END-IF.
       M-15.
      *           READ SM-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  S-DC NOT = 0
               GO TO M-15
           END-IF
           IF  NYU-TCD NOT = S-TCD
               GO TO M-15
           END-IF
           IF  NYU-SS NOT = S-NG
               GO TO M-15
           END-IF
           MOVE NYU-NGPS TO S-DATE.
           MOVE ZERO TO W-C W-SIT.
           IF  NYU-TGK = ZERO
               GO TO M-25
           END-IF
           IF  NYU-NC1 NOT = 3 AND 4
               GO TO M-25
           END-IF.
       M-20.
           PERFORM S-05 THRU S-15.
           ADD 1 TO W-C.
           MOVE W-SI TO W-SI1(W-C).
       M-25.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  NYU-SS = ZERO
               GO TO M-25
           END-IF
           IF  NYU-NC2 = 8
               GO TO M-25
           END-IF.
       M-30.
           IF  NYU-TCD NOT = S-TCD
               GO TO M-35
           END-IF
           IF  NYU-SS NOT = S-NG
               GO TO M-35
           END-IF
           IF  NYU-NC1 = 3 OR 4
               GO TO M-20
           END-IF
           GO TO M-25.
       M-35.
           PERFORM S-20 THRU S-35.
       M-50.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
           GO TO M-15.
       M-55.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
           GO TO M-10.
       M-60.
           PERFORM S-20 THRU S-35.
       M-65.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE S-NGP TO W-NGP1.
           COMPUTE W-SI = W-GN1(W-GET1) - W-PEY1.
           MOVE NYU-TGK TO W-NGP2.
           ADD W-PEY2 TO W-SI.
           IF  W-GET1 = 12
               MOVE ZERO TO W-GET1
           END-IF
           ADD 1 TO W-GET1.
           IF  W-GET1 = W-GET2
               GO TO S-15
           END-IF
           IF  W-GET2 = 1
               MOVE 13 TO W-GET2
           END-IF
           SUBTRACT 1 FROM W-GET2.
           MOVE W-GET1 TO CNT.
       S-10.
           ADD W-GN1(CNT) TO W-SI.
           IF  CNT = W-GET2
               GO TO S-15
           END-IF
           IF  CNT = 12
               MOVE ZERO TO CNT
           END-IF
           ADD 1 TO CNT.
           GO TO S-10.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO CNT W-SI.
           IF  W-C = ZERO
               GO TO S-30
           END-IF.
       S-25.
           IF  CNT NOT = W-C
               ADD 1 TO CNT
               ADD W-SI1(CNT) TO W-SI
               GO TO S-25
           END-IF
           COMPUTE W-SI ROUNDED = W-SI / W-C.
       S-30.
           MOVE W-SI TO S-SI.
           MOVE W-C TO S-SU.
      *           REWRITE S-R.
      *///////////////
           CALL "DB_Update" USING
            SM-F_PNAME1 SM-F_LNAME S-R RETURNING RET.
       S-35.
           EXIT.
