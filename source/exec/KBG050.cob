       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG050.
      *********************************************************
      *    PROGRAM         :  日付別支払明細表　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/08                        *
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
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                 "＊＊＊　　買掛金　支払明細表　　＊＊＊".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(042) VALUE
                " 日  付         現　金       振　込       ".
           02  F              PIC  X(045) VALUE
                "小切手       小　計       手　形     売掛相殺".
           02  F              PIC  X(039) VALUE
                "   その他相殺     　合　計     内消費税".
       01  HEAD5.
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(007) VALUE "【材料仕入分】".
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  N(007) VALUE "【製品仕入分】".
           02  F              PIC  X(037) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(042) VALUE
                " 日  付         現金他       手　形       ".
           02  F              PIC  X(045) VALUE
                "相　殺       合　計                    現金他".
           02  F              PIC  X(039) VALUE
                "       手  形     　相　殺       合　計".
       01  W-P.
           02  P-DATE         PIC 99/99/99B.
           02  P-DATED REDEFINES P-DATE  PIC  X(009).
           02  P-KIN.
             03  P-KM    OCCURS   9  PIC -----,---,---.
           02  P-KIND  REDEFINES P-KIN.
             03  P-RD    OCCURS   9.
               04  F          PIC  X(006).
               04  P-R        PIC ---9.9.
               04  P-X        PIC  X(001).
       01  W-D.
           02  W-DATE         PIC  9(006).
           02  W-KIN.
             03  W-KM    OCCURS   9  PIC S9(009).
       01  WT-D.
           02  WT-KM     OCCURS   9  PIC S9(009).
       01  W-R                PIC S9(003)V9(01).
       01  CHK                PIC  9(001).
       01  CNT                PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LSJSSW.
           COPY LSPF.
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　買掛金　支払明細表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "10" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-DC1 NOT = 3
               GO TO M-10
           END-IF.
       M-15.
           MOVE DATE-05R TO H-DATE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WT-D.
       M-20.
           MOVE ZERO TO W-D.
           MOVE JR-NGPS TO W-DATE.
       M-25.
           ADD JR-KIN TO W-KM(8).
           ADD JR-SHZ TO W-KM(8) W-KM(9).
           IF  JR-SC < 4
               MOVE JR-SC TO CHK
               ADD JR-KIN TO W-KM(4)
               ADD JR-SHZ TO W-KM(4)
           END-IF
           IF  JR-SC > 3
               COMPUTE CHK = JR-SC + 1
           END-IF
           ADD JR-KIN TO W-KM(CHK).
           ADD JR-SHZ TO W-KM(CHK).
       M-30.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  JR-DC1 NOT = 3
               GO TO M-30
           END-IF
           IF  JR-NGPS = W-DATE
               GO TO M-25
           END-IF.
       M-35.
           PERFORM S-05 THRU S-15.
           GO TO M-20.
       M-40.
           PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R W-P.
           MOVE ZERO TO CNT.
           MOVE "[ 合 計 ]" TO P-DATED.
       M-45.
           ADD 1 TO CNT.
           IF  CNT NOT = 10
               MOVE WT-KM(CNT) TO P-KM(CNT)
               GO TO M-45
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  WT-KM(8) = ZERO
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P.
           MOVE "%" TO P-X(1) P-X(2) P-X(3) P-X(4) P-X(5) P-X(6)
                                            P-X(7) P-X(8) P-X(9).
           IF  WT-KM(1) = ZERO
               MOVE 0 TO P-R(1)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(1) * 100) / WT-KM(8)
               MOVE W-R TO P-R(1)
           END-IF
           IF  WT-KM(2) = ZERO
               MOVE 0 TO P-R(2)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(2) * 100) / WT-KM(8)
               MOVE W-R TO P-R(2)
           END-IF
           IF  WT-KM(3) = ZERO
               MOVE 0 TO P-R(3)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(3) * 100) / WT-KM(8)
               MOVE W-R TO P-R(3)
           END-IF
           IF  WT-KM(4) = ZERO
               MOVE 0 TO P-R(4)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(4) * 100) / WT-KM(8)
               MOVE W-R TO P-R(4)
           END-IF
           IF  WT-KM(5) = ZERO
               MOVE 0 TO P-R(5)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(5) * 100) / WT-KM(8)
               MOVE W-R TO P-R(5)
           END-IF
           IF  WT-KM(6) = ZERO
               MOVE 0 TO P-R(6)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(6) * 100) / WT-KM(8)
               MOVE W-R TO P-R(6)
           END-IF
           IF  WT-KM(7) = ZERO
               MOVE 0 TO P-R(7)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(7) * 100) / WT-KM(8)
               MOVE W-R TO P-R(7)
           END-IF
           MOVE 100 TO P-R(8).
           IF  WT-KM(9) = ZERO
               MOVE 0 TO P-R(9)
           ELSE
               COMPUTE W-R ROUNDED = (WT-KM(9) * 100) / WT-KM(8)
               MOVE W-R TO P-R(9)
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           CALL "PR_LineFeed" USING "6" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WT-D.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-50.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-DC1 NOT = 3
               GO TO M-50
           END-IF.
       M-55.
           MOVE ZERO TO W-D.
           MOVE JR-NGPS TO W-DATE.
       M-60.
           IF  JR-SCD < 5000
               ADD JR-KIN TO W-KM(4)
               ADD JR-SHZ TO W-KM(4)
               IF  JR-SC = 1 OR 2 OR 3
                   ADD JR-KIN TO W-KM(1)
                   ADD JR-SHZ TO W-KM(1)
               ELSE
                   IF  JR-SC = 4
                       ADD JR-KIN TO W-KM(2)
                       ADD JR-SHZ TO W-KM(2)
                   ELSE
                       ADD JR-KIN TO W-KM(3)
                       ADD JR-SHZ TO W-KM(3)
                   END-IF
               END-IF
           END-IF
           IF  JR-SCD NOT < 5000
               ADD JR-KIN TO W-KM(9)
               ADD JR-SHZ TO W-KM(9)
               IF  JR-SC = 1 OR 2 OR 3
                   ADD JR-KIN TO W-KM(6)
                   ADD JR-SHZ TO W-KM(6)
               ELSE
                   IF  JR-SC = 4
                       ADD JR-KIN TO W-KM(7)
                       ADD JR-SHZ TO W-KM(7)
                   ELSE
                       ADD JR-KIN TO W-KM(8)
                       ADD JR-SHZ TO W-KM(8)
                   END-IF
               END-IF
           END-IF.
       M-65.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  JR-DC1 NOT = 3
               GO TO M-65
           END-IF
           IF  JR-NGPS = W-DATE
               GO TO M-60
           END-IF
           PERFORM S-05 THRU S-15.
           GO TO M-55.
       M-70.
           PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R W-P.
           MOVE ZERO TO CNT.
           MOVE "[ 合 計 ]" TO P-DATED.
       M-75.
           ADD 1 TO CNT.
           IF  CNT NOT = 10
               MOVE WT-KM(CNT) TO P-KM(CNT)
               GO TO M-75
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  ZERO = W-KM(1) AND W-KM(2) AND W-KM(3) AND W-KM(4) AND
                 W-KM(5) AND W-KM(6) AND W-KM(7) AND W-KM(8) AND W-KM(9)
               GO TO S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE ZERO TO CNT.
           MOVE W-DATE TO P-DATE.
       S-10.
           ADD 1 TO CNT.
           IF  CNT NOT = 10
               MOVE W-KM(CNT) TO P-KM(CNT)
               ADD W-KM(CNT) TO WT-KM(CNT)
               GO TO S-10
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
