       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KBM030.
      **************************************
      ******    仕入先シール　作成    ******
      **************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  W-POC                     PIC 9(001)    VALUE 0.
       01  W-15K                     PIC X(005)    VALUE X"1A24212078".
       01  W-P1.
           02  P-15K                 PIC X(005).
           02  P-UM1                 PIC N(002).
           02  P-UB1                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM2                 PIC N(002).
           02  P-UB2                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM3                 PIC N(002).
           02  P-UB3                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM4                 PIC N(002).
           02  P-UB4                 PIC X(008).
       01  W-P2.
           02  P-JU1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU4                 PIC N(020).
       01  W-P3.
           02  P-JS1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS4                 PIC N(020).
       01  W-P4.
           02  F                     PIC X(012).
           02  P-JC1                 PIC N(012).
           02  F                     PIC X(017).
           02  P-JC2                 PIC N(012).
           02  F                     PIC X(017).
           02  P-JC3                 PIC N(012).
           02  F                     PIC X(017).
           02  P-JC4                 PIC N(012).
       01  W-P5.
           02  F                     PIC X(001).
           02  P-NA1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA4                 PIC N(020).
       01  W-P6.
           02  F                     PIC X(001).
           02  P-NA5                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA6                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA7                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA8                 PIC N(020).
       01  W-DATA.
           02  W-TPC                 PIC 9(001).
           02  W-DMM                 PIC 9(001).
           02  W-EC                  PIC 9(001).
           02  W-C                   PIC 9(002).
           02  W-ZC                  PIC 9(002).
           02  W-L                   PIC 9(002).
           02  CNT                   PIC 9(002).
           02  CNTD                  PIC 9(002).
           02  CNTE                  PIC 9(002).
           02  CNTF                  PIC 9(002).
           02  CNTG                  PIC 9(002).
           02  CNTH                  PIC 9(002).
           02  W-OLD                 PIC 9(004).
           02  W-AD.
               03  W-D    OCCURS  16.
                   04  W-KEY         PIC 9(004).
                   04  W-NAME        PIC N(024).
                   04  W-JSU         PIC N(024).
                   04  W-JSS         PIC N(012).
                   04  W-UNO         PIC X(008).
           02  W-NMD                 PIC N(027).
           02  W-NAD             REDEFINES  W-NMD.
               03  W-NA              PIC N(001)    OCCURS  27.
           02  W-NUR                 PIC N(020).
           02  W-NUD             REDEFINES  W-NUR.
               03  W-NU              PIC N(001)    OCCURS  20.
           02  W-NSR                 PIC N(020).
           02  W-NSD             REDEFINES  W-NSR.
               03  W-NS              PIC N(001)    OCCURS  20.
           02  W-JUR                 PIC N(024).
           02  W-JUD             REDEFINES  W-JUR.
               03  W-JU              PIC N(001)    OCCURS  24.
           02  W-ADRU                PIC N(020).
           02  W-ADDU            REDEFINES  W-ADRU.
               03  W-ADU             PIC N(001)    OCCURS  20.
           02  W-ADRN                PIC N(020).
           02  W-ADDN            REDEFINES  W-ADRN.
               03  W-ADN             PIC N(001)    OCCURS  20.
           02  W-END                 PIC 9(001) VALUE 0.
       01  ERR-STAT                  PIC X(002).
           COPY LSTAT.
      *
           COPY LISM.
      *FD  SP-F
       77  SP-R                      PIC X(204).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　郵便　宛名シール　作成　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　　　（　仕入先　）　　　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(038)    VALUE
                   "[  TEST PRINT  ｽﾙ=9  ｼﾅｲ=1     ﾘﾀｰﾝ  ]".
       01  C-MID1.
           02  FILLER  PIC N(019)    VALUE
                 "＊＊＊　　仕入先シール　作成　　＊＊＊".
           02  FILLER  PIC X(017)    VALUE
                   "同上=F5 , 終了=F9".
           02  FILLER.
               03  FILLER  PIC X(016)    VALUE
                     "ｺｰﾄﾞ  仕  入  先".
               03  FILLER  PIC X(008)    VALUE
                     "確認=F10".
           02  FILLER  PIC X(022)    VALUE
                     "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC      PIC 9(001).
           02  A-KEY      PIC 9(004).
           02  A-DMM      PIC 9(001).
       01  C-DSP.
           02  D-NAME     PIC N(024).
           02  D-CLEAR.
               03  FILLER  PIC X(004)    VALUE  "    ".
               03  FILLER  PIC X(048)    VALUE
                    "                                                ".
       01  C-ERR.
           02  FILLER.
               03  E-ME1   PIC X(018)   VALUE
                     "***  ｼｲﾚｻｷ ﾅｼ  ***".
               03  E-ME98  PIC X(005)   VALUE X"1B4A05".
               03  E-ME99  PIC X(005)   VALUE X"1B4205".
               03  E-STAT  PIC X(002).
               03  E-CL    PIC X(050)   VALUE
                   "                                                  ".
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "374" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "12" "38" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "1" "12" "38" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "2" "58" "17" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" " " "3" "0" "24" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID1" "X" "3" "8" "16" " " "03C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID1" "X" "3" "68" "8" "0103C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "X" "23" "40" "22" "03C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "15" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "W-L" "8" "4" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY(1) "4" "1" BY REFERENCE W-C 132
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "57" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "14" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME(1) "48" "1" 
            BY REFERENCE W-C 132 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CLEAR" "X" "W-L" "0" "11" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CLEAR" "X" "W-L" "8" "4" " " "D-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CLEAR" "X" "W-L" "14" "48" "01D-CLEAR" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "80" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-TPC = 1
               GO  TO  M-15
           END-IF
           IF  W-TPC NOT = 9
               GO  TO  M-10
           END-IF
           MOVE  SPACE       TO  W-P1    W-P2    W-P3    W-P4    W-P5
                                 W-P6    W-NSR.
           MOVE  W-15K       TO  P-15K.
           MOVE  "〒　"    TO  P-UM1     P-UM2     P-UM3     P-UM4.
           MOVE  "XXXXXXXX"  TO  P-UB1     P-UB2     P-UB3     P-UB4.
           MOVE  ALL "Ｎ"  TO  P-JU1     P-JU2     P-JU3     P-JU4
                                 P-JS1     P-JS2     P-JS3     P-JS4
                                 P-JC1     P-JC2     P-JC3     P-JC4
                                 P-NA1     P-NA2     P-NA3     P-NA4.
           PERFORM  S-110  THRU  S-120.
           MOVE  W-NSR      TO  P-NA5  P-NA6  P-NA7  P-NA8.
           PERFORM  S-05  THRU  S-10.
           GO  TO  M-10.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           MOVE  ZERO  TO  W-C  W-EC.
           PERFORM  S-15  THRU  S-25.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-20.
           ADD 1 TO W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-C > 16
               GO TO M-50
           END-IF
           IF  W-EC NOT = ZERO
               GO TO M-40
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-35
           END-IF
           IF  ESTAT = PF5
               IF  W-OLD NOT = ZERO
                   CALL "SD_Output" USING
                    "A-KEY" A-KEY "p" RETURNING RESU
                   GO  TO  M-27
               END-IF
           END-IF
           IF  ESTAT = ADV
               MOVE 1 TO W-EC
               GO  TO  M-40
           END-IF
           IF  ESTAT = PF9
               IF  W-C = 1
                   GO  TO  M-95
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-25
           END-IF.
       M-27.
           MOVE W-KEY(W-C) TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           MOVE S-NAME TO W-NAME(W-C).
           MOVE S-JSU TO W-JSU(W-C).
           MOVE S-JSS TO W-JSS(W-C).
           MOVE S-UNO TO W-UNO(W-C).
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE W-KEY(W-C) TO W-OLD.
           GO  TO  M-20.
       M-35.
           SUBTRACT 1 FROM W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-C = ZERO
               GO TO M-15
           END-IF
           IF  W-KEY(W-C) = ZERO
               GO TO M-35
           END-IF
           GO TO M-25.
       M-40.
           MOVE ZERO TO W-KEY(W-C).
           MOVE SPACE TO W-NAME(W-C) W-JSU(W-C) W-JSS(W-C).
           MOVE SPACE TO W-UNO(W-C).
           CALL "SD_Output" USING "D-CLEAR" D-CLEAR "p" RETURNING RESU.
           GO TO M-20.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-50
           END-IF
           IF  W-DMM = 9
               MOVE ZERO TO W-C W-EC
               MOVE 3 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-50
           END-IF
      *
           MOVE  ZERO  TO  W-C CNT.
       M-60.
           ADD 1 TO W-C.
           IF  W-C > 16
               GO TO M-80
           END-IF
           IF  W-KEY(W-C) = ZERO
               GO TO M-80
           END-IF
           PERFORM  S-30  THRU  S-40.
           PERFORM  S-90  THRU  S-105.
           IF  CNT = ZERO
               MOVE  SPACE  TO  W-P1  W-P2  W-P3  W-P4  W-P5  W-P6
               MOVE  W-15K  TO  P-15K
           END-IF
           ADD  1  TO  CNT.
           IF  CNT = 1
               MOVE  "〒　"    TO  P-UM1
               MOVE  W-UNO(W-C)  TO  P-UB1
               MOVE  W-ADRU      TO  P-JU1
               MOVE  W-ADRN      TO  P-JS1
               MOVE  W-JSS(W-C)  TO  P-JC1
               MOVE  W-NUR       TO  P-NA1
               MOVE  W-NSR       TO  P-NA5
               GO  TO  M-60
           END-IF
           IF  CNT = 2
               MOVE  "〒　"    TO  P-UM2
               MOVE  W-UNO(W-C)  TO  P-UB2
               MOVE  W-ADRU      TO  P-JU2
               MOVE  W-ADRN      TO  P-JS2
               MOVE  W-JSS(W-C)  TO  P-JC2
               MOVE  W-NUR       TO  P-NA2
               MOVE  W-NSR       TO  P-NA6
               GO  TO  M-60
           END-IF
           IF  CNT = 3
               MOVE  "〒　"    TO  P-UM3
               MOVE  W-UNO(W-C)  TO  P-UB3
               MOVE  W-ADRU      TO  P-JU3
               MOVE  W-ADRN      TO  P-JS3
               MOVE  W-JSS(W-C)  TO  P-JC3
               MOVE  W-NUR       TO  P-NA3
               MOVE  W-NSR       TO  P-NA7
               GO  TO  M-60
           END-IF
           MOVE  "〒　"    TO  P-UM4.
           MOVE  W-UNO(W-C)  TO  P-UB4.
           MOVE  W-ADRU      TO  P-JU4.
           MOVE  W-ADRN      TO  P-JS4.
           MOVE  W-JSS(W-C)  TO  P-JC4.
           MOVE  W-NUR       TO  P-NA4.
           MOVE  W-NSR       TO  P-NA8.
           PERFORM  S-05  THRU  S-10.
           MOVE  ZERO  TO  CNT.
           GO TO M-60.
       M-80.
           IF  CNT NOT = ZERO
               PERFORM S-05 THRU S-10
           END-IF
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P1   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P2   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P3   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P4   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P5   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P6   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-10.
           EXIT.
       S-15.
           MOVE  ZERO  TO  W-ZC.
       S-20.
           ADD  1  TO  W-ZC.
           IF  W-ZC > 16
               MOVE  ZERO   TO  W-KEY(W-ZC)
               MOVE  SPACE  TO  W-NAME(W-ZC)  W-JSU(W-ZC)  W-JSS(W-ZC)
               MOVE  SPACE  TO  W-UNO(W-ZC)
               GO  TO  S-20
           END-IF.
       S-25.
           EXIT.
       S-30.
           MOVE  SPACE        TO  W-NMD  W-NUR  W-NSR.
           MOVE  W-NAME(W-C)  TO  W-NMD.
           MOVE  25  TO  CNTD.
       S-35.
           SUBTRACT  1  FROM  CNTD.
           IF  CNTD NOT = ZERO
               IF  W-NA(CNTD) = SPACE
                   GO  TO  S-35
               END-IF
           END-IF
           ADD  3  TO  CNTD.
           MOVE  "様"  TO  W-NA(CNTD).
           IF  CNTD < 21
               MOVE  W-NMD  TO  W-NSR
               GO  TO  S-40
           END-IF
           IF  CNTD > 20
               MOVE  6  TO  CNTE
               PERFORM  S-45  THRU  S-50
           END-IF.
       S-40.
           EXIT.
       S-45.
           ADD  1  TO  CNTE.
           IF  CNTE < 28
               IF  W-NA(CNTE) NOT = SPACE
                   GO  TO  S-45
               END-IF
               IF  CNTE > 20
                   MOVE  ZERO  TO  CNTF
                   PERFORM  S-75  THRU  S-85
                   GO  TO  S-50
               END-IF
               IF  CNTE < 21
                   PERFORM  S-55  THRU  S-70
               END-IF
           END-IF.
       S-50.
           EXIT.
       S-55.
           MOVE  ZERO  TO  CNTF.
       S-60.
           ADD  1  TO  CNTF.
           IF  CNTF < CNTE
               MOVE  W-NA(CNTF)  TO  W-NU(CNTF)
               GO  TO  S-60
           END-IF
           MOVE  CNTE  TO  CNTG.
           MOVE  ZERO  TO  CNTH.
       S-65.
           ADD  1  TO  CNTG  CNTH.
           IF  CNTG < 28
               MOVE  W-NA(CNTG)  TO  W-NS(CNTH)
               GO  TO  S-65
           END-IF.
       S-70.
           EXIT.
       S-75.
           ADD  1  TO  CNTF.
           IF  CNTF < 8
               MOVE  W-NA(CNTF)  TO  W-NU(CNTF)
               GO  TO  S-75
           END-IF
           MOVE  1  TO  CNTG.
           MOVE  W-NA(CNTF)  TO  W-NS(CNTG).
       S-80.
           ADD  1  TO  CNTF  CNTG.
           IF  CNTF < 28
               MOVE  W-NA(CNTF)  TO  W-NS(CNTG)
               GO  TO  S-80
           END-IF.
       S-85.
           EXIT.
       S-90.
           MOVE  SPACE       TO  W-JUR  W-ADRU  W-ADRN.
           MOVE  W-JSU(W-C)  TO  W-JUR.
           IF  SPACE = W-JU(21) AND W-JU(22) AND W-JU(23) AND W-JU(24)
               MOVE  W-JUR  TO  W-ADRU
               GO  TO  S-105
           END-IF
           MOVE  ZERO  TO  CNTE.
       S-95.
           ADD  1  TO  CNTE.
           IF  CNTE > 20
               MOVE  W-JU(21)  TO  W-ADN(01)
               MOVE  W-JU(22)  TO  W-ADN(02)
               MOVE  W-JU(23)  TO  W-ADN(03)
               MOVE  W-JU(24)  TO  W-ADN(04)
               GO  TO  S-105
           END-IF
           MOVE  W-JU(CNTE)  TO  W-ADU(CNTE).
           IF  W-JU(CNTE) NOT = SPACE
               GO  TO  S-95
           END-IF
           IF  CNTE < 4
               GO  TO  S-95
           END-IF
           IF  CNTE = 4
               MOVE  W-JU(CNTE)  TO  W-ADU(CNTE)
           END-IF
           MOVE  ZERO  TO  CNTF.
       S-100.
           ADD  1  TO  CNTE  CNTF.
           IF  CNTE < 25
               MOVE  W-JU(CNTE)  TO  W-ADN(CNTF)
               GO  TO  S-100
           END-IF.
       S-105.
           EXIT.
       S-110.
           MOVE  ZERO  TO  CNT.
       S-115.
           ADD  1  TO  CNT.
           IF  CNT < 20
               MOVE  "Ｎ"  TO  W-NS(CNT)
               GO  TO  S-115
           END-IF
           MOVE  "様"  TO  W-NS(CNT).
       S-120.
           EXIT.
