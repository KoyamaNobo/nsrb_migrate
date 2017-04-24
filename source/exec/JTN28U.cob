       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTN28U.
       AUTHOR.          ________.
      *********************************************************
      *    PROGRAM         :  有効在庫データ　作成 (生協)     *
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  03/02/21                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-ME               PIC  X(040).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-HCD          PIC  X(006).
           02  W-HNA          PIC  N(024).
           02  W-KBN          PIC  N(003).
           02  W-KC           PIC  9(001).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SUD   OCCURS  10.
                 05  W-SU     PIC S9(006).
           02  W-SUT          PIC S9(006).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-TC           PIC  9(001).
           02  W-HCC          PIC  9(001).
           02  W-KBC          PIC  9(001).
           02  W-AZC.
             03  W-ZCD   OCCURS   4.
               04  W-ZC       PIC  9(001).
           02  W-AGSUD.
             03  W-AGSU   OCCURS   4.
               04  W-GSUD   OCCURS  10.
                 05  W-GSU    PIC S9(006).
      *FD  JT-YZAI
       01  JT-YZAI_JTN28U.
           02  JT-YZAI_PNAME1      PIC  X(009) VALUE SPACE.
           02  F                   PIC  X(001).
           02  JT-YZAI_LNAME       PIC  X(014) VALUE "JT-YZAI_JTN28U".
           02  F                   PIC  X(001).
           02  JT-YZAI_KEY1        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST       PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES         USAGE  POINTER.
       01  YZAI-R.
           02  YZAI-HCD            PIC  9(006).
           02  YZAI-HNA            PIC  N(024).
           02  YZAI-KBN            PIC  N(003).
           02  YZAI-SIZ            PIC  9(001).
           02  YZAI-ASU.
             03  YZAI-SUD       OCCURS  10.
               04   YZAI-SU        PIC S9(006).
           02  YZAI-TSU            PIC S9(006).
           02  YZAI-KC             PIC  9(001).
       77  F                       PIC  X(001).
      *FD  YZDF
       01  YZDF_JTN28U.
           02  YZDF_PNAME1         PIC  X(005) VALUE "SYZDF".
           02  F                   PIC  X(001).
           02  YZDF_LNAME          PIC  X(011) VALUE "YZDF_JTN28U".
           02  F                   PIC  X(001).
           02  YZDF_KEY1           PIC  X(100) VALUE SPACE.
           02  YZDF_SORT           PIC  X(100) VALUE SPACE.
           02  YZDF_IDLST          PIC  X(100) VALUE SPACE.
           02  YZDF_RES            USAGE  POINTER.
       01  YZD-R.
           02  YZD-HCD             PIC  X(006).
           02  YZD-HNA             PIC  N(024).
           02  YZD-KBN             PIC  N(003).
           02  YZD-SIZ             PIC  9(001).
           02  YZD-ASU.
             03  YZD-SUD   OCCURS  10.
               04  YZD-SU          PIC S9(006).
           02  YZD-TSU             PIC S9(006).
       77  F                       PIC  X(001).
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
           02  DSP-MID.
             03  FILLER  PIC  X(22) VALUE
                  "                      ".
             03  FILLER  PIC  X(20) VALUE
                 "有効在庫データ　作成".
       01  C-ERR.
           02  E-ME    PIC  X(040).
           COPY LSSEM.
       PROCEDURE   DIVISION.
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
            "C-MID" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID" " " "0" "0" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID" "RX" "1" "27" "22" " " "DSP-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID" "X" "1" "28" "20" "01DSP-MID" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "40" " " "C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-ME "40" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           INITIALIZE W-DATA.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-YZAI_PNAME1 " " BY REFERENCE JT-YZAI_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" YZDF_PNAME1 " " BY REFERENCE YZDF_IDLST "0".
       M-10.
      *           READ JT-YZAI AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           IF  ZERO = YZAI-SU(01) AND YZAI-SU(02) AND YZAI-SU(03)
                 AND YZAI-SU(04) AND YZAI-SU(05) AND YZAI-SU(06)
                 AND YZAI-SU(07) AND YZAI-SU(08) AND YZAI-SU(09)
                 AND YZAI-SU(10)
               GO TO M-10
           END-IF.
       M-15.
           MOVE YZAI-HCD TO W-HCD.
           MOVE YZAI-HNA TO W-HNA.
           MOVE ZERO TO W-AGSUD W-TC W-HCC.
       M-20.
           MOVE YZAI-KC TO W-KC.
           MOVE YZAI-KBN TO W-KBN.
           ADD 1 TO W-TC.
           MOVE ZERO TO W-ASUD W-SUT W-AZC.
       M-25.
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT > 10
               ADD YZAI-TSU TO W-SUT
               GO TO M-40
           END-IF
           MOVE YZAI-SU(CNT) TO W-SU(YZAI-SIZ,CNT).
           IF  YZAI-KC = 3
               SUBTRACT YZAI-SU(CNT) FROM W-GSU(YZAI-SIZ,CNT)
           ELSE
               ADD YZAI-SU(CNT) TO W-GSU(YZAI-SIZ,CNT)
           END-IF
           IF  W-ZC(YZAI-SIZ) = 0
               IF  YZAI-SU(CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(YZAI-SIZ)
               END-IF
           END-IF
           GO TO M-30.
       M-40.
      *           READ JT-YZAI AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  ZERO = YZAI-SU(01) AND YZAI-SU(02) AND YZAI-SU(03)
                 AND YZAI-SU(04) AND YZAI-SU(05) AND YZAI-SU(06)
                 AND YZAI-SU(07) AND YZAI-SU(08) AND YZAI-SU(09)
                 AND YZAI-SU(10)
               GO TO M-40
           END-IF
           IF  YZAI-HCD NOT = W-HCD
               GO TO M-45
           END-IF
           IF  YZAI-KC = W-KC
               GO TO M-25
           END-IF
           PERFORM S-05 THRU S-15.
           GO TO M-20.
       M-45.
           PERFORM S-05 THRU S-15.
           IF  W-TC > 1
               PERFORM S-20 THRU S-45
           END-IF
           GO TO M-15.
       M-80.
           PERFORM S-05 THRU S-15.
           IF  W-TC > 1
               PERFORM S-20 THRU S-45
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE YZDF_IDLST YZDF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE 0 TO W-S W-KBC.
       S-10.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO S-15
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO S-10
           END-IF
      *
           MOVE SPACE TO YZD-R.
           INITIALIZE YZD-R.
           IF  W-HCC = 0
               MOVE 1 TO W-HCC
               MOVE W-HCD TO YZD-HCD
               MOVE W-HNA TO YZD-HNA
           END-IF
           IF  W-KBC = 0
               MOVE 1 TO W-KBC
               MOVE W-KBN TO  YZD-KBN
           END-IF
           MOVE W-S TO YZD-SIZ.
           MOVE W-ASU(W-S) TO YZD-ASU.
           IF  W-S = 1
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 4
               MOVE W-SUT TO YZD-TSU
           END-IF
      *           WRITE YZD-R.
      *//////////////
           CALL "DB_Insert" USING
            YZDF_PNAME1 YZDF_LNAME YZD-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               MOVE SPACE TO W-ME
               MOVE "***  WRITE ｴﾗｰ  ***" TO W-ME
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-15
           END-IF
           GO TO S-10.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-SUT W-AZC W-S.
       S-25.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO S-35
           END-IF
           MOVE ZERO TO CNT.
       S-30.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO S-25
           END-IF
           ADD W-GSU(W-S,CNT) TO W-SUT.
           IF  W-ZC(W-S) = 0
               IF  W-GSU(W-S,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC(W-S)
               END-IF
           END-IF
           GO TO S-30.
       S-35.
           MOVE 0 TO W-S W-KBC.
       S-40.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO S-45
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO S-40
           END-IF
      *
           MOVE SPACE TO YZD-R.
           INITIALIZE YZD-R.
           IF  W-KBC = 0
               MOVE 1 TO W-KBC
               MOVE "合　計" TO  YZD-KBN
           END-IF
           MOVE W-S TO YZD-SIZ.
           MOVE W-AGSU(W-S) TO YZD-ASU.
           IF  W-S = 1
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE W-SUT TO YZD-TSU
               END-IF
           END-IF
           IF  W-S = 4
               MOVE W-SUT TO YZD-TSU
           END-IF
      *           WRITE YZD-R.
      *//////////////
           CALL "DB_Insert" USING
            YZDF_PNAME1 YZDF_LNAME YZD-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               MOVE SPACE TO W-ME
               MOVE "***  WRITE ｴﾗｰ  ***" TO W-ME
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-30
           END-IF
           GO TO S-40.
       S-45.
            EXIT.
