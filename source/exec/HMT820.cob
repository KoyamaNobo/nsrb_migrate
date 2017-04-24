       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HMT820.
      **************************************************************
      *    PROGRAM         :  品名別売上前年対比ワーク作成         *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ------                               *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  色あり(6ｹﾀ)=0 , 色なし(4ｹﾀ)=1        *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0512".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-HCDD  REDEFINES  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  W-HCD2       PIC  9(002).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  W-NG.
             03  W-N          PIC  9(004).
             03  W-ND    REDEFINES W-N.
               04  W-N1       PIC  9(002).
               04  W-N2       PIC  9(002).
             03  W-G          PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-YMD.
             03  W-YM1   OCCURS  12.
               04  W-Y1       PIC  9(004).
               04  W-YD1   REDEFINES W-Y1.
                 05  W-Y11    PIC  9(002).
                 05  W-Y12    PIC  9(002).
               04  W-M1       PIC  9(002).
             03  W-YM2   OCCURS  12.
               04  W-Y2       PIC  9(004).
               04  W-YD2   REDEFINES W-Y2.
                 05  W-Y21    PIC  9(002).
                 05  W-Y22    PIC  9(002).
               04  W-M2       PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
      *FD  SSRYF
       01  SSRYF_HMT820.
           02  SSRYF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SSRYF_LNAME    PIC  X(012) VALUE "SSRYF_HMT820".
           02  F              PIC  X(001).
           02  SSRYF_KEY1     PIC  X(100) VALUE SPACE.
           02  SSRYF_SORT     PIC  X(100) VALUE SPACE.
           02  SSRYF_IDLST    PIC  X(100) VALUE SPACE.
           02  SSRYF_RES      USAGE  POINTER.
       01  SSRY-R.
           02  F              PIC  9(004).
           02  SSRY-HCD       PIC  9(006).
           02  SSRY-HCDD  REDEFINES  SSRY-HCD.
             03  SSRY-HCD1    PIC  9(004).
             03  SSRY-HCD2    PIC  9(002).
           02  SSRY-SU        PIC S9(007).
           02  SSRY-KIN       PIC S9(010).
           02  F              PIC  X(014).
           02  SSRY-BC.
             03  SSRY-BC1     PIC  9(002).
             03  SSRY-BC2     PIC  9(002).
             03  SSRY-BC3     PIC  9(002).
           02  SSRY-BMC       PIC  9(002).
           02  SSRY-BMNO      PIC  9(001).
           02  F              PIC  X(002).
           02  SSRY-NG        PIC  9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  HY-F
       01  HY-F_HMT820.
           02  HY-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HY-F_LNAME     PIC  X(011) VALUE "HY-F_HMT820".
           02  F              PIC  X(001).
           02  HY-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HY-F_SORT      PIC  X(100) VALUE SPACE.
           02  HY-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HY-F_RES       USAGE  POINTER.
       01  HY-R.
           02  HY-HCD         PIC  9(006).
           02  HY-HCDD  REDEFINES  HY-HCD.
             03  HY-HCD1      PIC  9(004).
             03  HY-HCD2      PIC  9(002).
           02  HY-ZAD.
             03  HY-ZD    OCCURS  12.
               04  HY-ZSU     PIC S9(007).
               04  HY-ZKN     PIC S9(010).
           02  HY-KAD.
             03  HY-KD    OCCURS  12.
               04  HY-KSU     PIC S9(007).
               04  HY-KKN     PIC S9(010).
           02  HY-BC.
             03  HY-BC1       PIC  9(002).
             03  HY-BC2       PIC  9(002).
             03  HY-BC3       PIC  9(002).
           02  HY-BMC         PIC  9(002).
           02  HY-BMNO        PIC  9(001).
           02  F              PIC  X(089).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品名月別前年対比ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                   "***  ﾃﾞｰﾀ ﾅｼ  *** ".
             03  E-ME2   PIC  X(018) VALUE
                   "***  ﾃﾞｰﾀ ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
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
           "C-ERR" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "96" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-N2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-N
           END-IF
           IF  W-N2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-N
           END-IF
           IF  W-G < 5
               SUBTRACT 1 FROM W-N
           END-IF
           MOVE 4 TO W-G.
           MOVE ZERO TO CNT1.
           MOVE 1 TO CNT2.
       M-10.
           IF  CNT1 = ZERO
               MOVE W-NG TO W-YM2(CNT2)
           ELSE
               MOVE W-YM2(CNT1) TO W-YM2(CNT2)
           END-IF
           ADD 1 TO W-M2(CNT2).
           IF  W-M2(CNT2) = 13
               ADD 1 TO W-Y2(CNT2)
               MOVE 1 TO W-M2(CNT2)
           END-IF
           MOVE W-YM2(CNT2) TO W-YM1(CNT2).
           SUBTRACT 1 FROM W-Y1(CNT2).
           ADD 1 TO CNT1 CNT2.
           IF  CNT1 NOT = 12
               GO TO M-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO SSRYF_PNAME1.
           MOVE W-FID2 TO WK0512ID.
           MOVE WK0512ID TO HY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSRYF_PNAME1 " " BY REFERENCE SSRYF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HY-F_PNAME1 " " BY REFERENCE HY-F_IDLST "0".
       M-15.
      *           READ SSRYF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SSRYF_PNAME1 BY REFERENCE SSRY-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-YM1(1) > SSRY-NG
               GO TO M-15
           END-IF
           IF  ZERO = SSRY-SU AND SSRY-KIN
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO W-HCD.
           IF  JS-SIGN = 0
               MOVE SSRY-HCD TO W-HCD
           ELSE
               MOVE SSRY-HCD1 TO W-HCD1
           END-IF
           MOVE ZERO TO HY-R.
           MOVE W-HCD TO HY-HCD.
           MOVE SSRY-BC TO HY-BC.
           MOVE SSRY-BMC TO HY-BMC.
           MOVE SSRY-BMNO TO HY-BMNO.
       M-25.
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT1.
           IF  CNT1 > 12
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SSRY-NG = W-YM1(CNT1)
               ADD SSRY-SU TO HY-ZSU(CNT1)
               ADD SSRY-KIN TO HY-ZKN(CNT1)
               GO TO M-35
           END-IF
           IF  SSRY-NG = W-YM2(CNT1)
               ADD SSRY-SU TO HY-KSU(CNT1)
               ADD SSRY-KIN TO HY-KKN(CNT1)
               GO TO M-35
           END-IF
           GO TO M-30.
       M-35.
      *           READ SSRYF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SSRYF_PNAME1 BY REFERENCE SSRY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  W-YM1(1) > SSRY-NG
               GO TO M-35
           END-IF
           IF  ZERO = SSRY-SU AND SSRY-KIN
               GO TO M-35
           END-IF
           IF  JS-SIGN = 0
               IF  SSRY-HCD = W-HCD
                   GO TO M-25
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  SSRY-HCD1 = W-HCD1
                   GO TO M-25
               END-IF
           END-IF.
       M-40.
      *           WRITE HY-R.
      *//////////////
           CALL "DB_Insert" USING
            HY-F_PNAME1 HY-F_LNAME HY-R RETURNING RET.
           GO TO M-20.
       M-80.
      *           WRITE HY-R.
      *//////////////
           CALL "DB_Insert" USING
            HY-F_PNAME1 HY-F_LNAME HY-R RETURNING RET.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SSRYF_IDLST SSRYF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HY-F_IDLST HY-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
