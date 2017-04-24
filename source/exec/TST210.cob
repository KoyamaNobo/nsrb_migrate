       IDENTIFICATION DIVISION.
       PROGRAM-ID. TST210.
      ********************************************
      *****     銀行別　割引手形　問合せ     *****
      *****        ( SCREEN : SCTT21)        *****
      ********************************************
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 78-03-29.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-D.
           02  W-YBK          PIC  9(004).
           02  W-IDO          PIC  9(006).
           02  W-ZAN          PIC S9(010).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-PEYD         PIC  9(002).
           02  W-TNA.
             03  W-TNA1       PIC  N(015).
             03  W-TNA2       PIC  N(011).
           02  W-URA          PIC  X(005).
           02  W-FYT          PIC  N(003).
           02  W-OC           PIC  X(001).
           02  W-MAND         PIC  X(008).
           02  W-MAN   REDEFINES W-MAND  PIC 99/99/99.
           02  W-MANM  REDEFINES W-MAND  PIC  N(004).
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIBANK.
           COPY LIUKET.
      *FD  YRIT-F
       01  YRIT-F_TST210.
           02  YRIT-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  YRIT-F_LNAME   PIC  X(013) VALUE "YRIT-F_TST210".
           02  F              PIC  X(001).
           02  YRIT-F_KEY1    PIC  X(100) VALUE SPACE.
           02  YRIT-F_SORT    PIC  X(100) VALUE SPACE.
           02  YRIT-F_IDLST   PIC  X(100) VALUE SPACE.
           02  YRIT-F_RES     USAGE  POINTER.
       01  YRIT-R.
           02  Y-YBK          PIC  9(004).
           02  Y-IDO.
             03  Y-IDO1       PIC  9(002).
             03  Y-IDO2       PIC  9(002).
             03  Y-IDO3       PIC  9(002).
           02  Y-MAN          PIC  9(006).
           02  Y-TCD          PIC  9(004).
           02  Y-KBN          PIC  9(002).
           02  Y-NO           PIC  9(004).
           02  Y-KIN          PIC  9(010).
           02  Y-FUC          PIC  9(001).
           02  F              PIC  X(027).
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
       01  C-ACP.
           02  A-YBK   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DATE.
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  Z(002).
           02  D-BK.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  N(008).
           02  FILLER.
             03  D-D1.
               04  FILLER  PIC  Z(002).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(015).
               04  FILLER  PIC  X(001).
               04  FILLER  PIC  X(008).
               04  FILLER  PIC ZZZZ,ZZZ,ZZZ .
             03  D-ZAN   PIC ZZZZ,ZZZ,ZZZ .
           02  FILLER.
             03  D-D2.
               04  FILLER  PIC  N(011).
               04  FILLER  PIC  X(005).
           02  FILLER.
             03  D-NM    PIC  X(038) VALUE
                  "[  NEXT PAGE = ﾘﾀｰﾝ  END = BSKIP     ]".
             03  D-EM    PIC  X(017) VALUE
                  "[  END DATA     ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  UKETM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YBK" "9" "3" "13" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YBK" BY REFERENCE W-YBK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "55" "1" "A-YBK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "187" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" " " "2" "0" "4" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATE" "Z" "2" "64" "2" " " "D-DATE"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATE" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATE" "Z" "2" "68" "2" "01D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATE" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BK" " " "3" "0" "32" "D-DATE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BK" "N" "3" "19" "16" " " "D-BK"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BK" BY REFERENCE B-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-BK" "N" "3" "37" "16" "01D-BK" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-BK" BY REFERENCE B-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L1" "0" "69" "D-BK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D1" " " "W-L1" "0" "57" " " "03C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D1" "Z" "W-L1" "3" "2" " " "D-D1"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D1" BY REFERENCE W-PEYD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D1" "9" "W-L1" "6" "4" "01D-D1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-D1" BY REFERENCE Y-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-D1" "N" "W-L1" "11" "30" "02D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-D1" BY REFERENCE W-TNA1 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-D1" "X" "W-L1" "42" "1" "03D-D1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "04D-D1" BY REFERENCE W-OC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-D1" "X" "W-L1" "44" "8" "04D-D1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "05D-D1" BY REFERENCE W-MAND "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-D1" "ZZZZ,ZZZ,ZZZ" "W-L1" "53" "12" "05D-D1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-D1" BY REFERENCE Y-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZAN" "ZZZZ,ZZZ,ZZZ" "W-L1" "66" "12" "D-D1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZAN" BY REFERENCE W-ZAN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L2" "0" "27" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D2" " " "W-L2" "0" "27" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D2" "N" "W-L2" "14" "22" " " "D-D2"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D2" BY REFERENCE W-TNA2 "22" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D2" "X" "W-L2" "36" "5" "01D-D2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-D2" BY REFERENCE W-URA "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "23" "0" "55" "04C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "21" "38" " " "05C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "42" "17" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "113" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "113" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-SNG.
           MOVE D-NTNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           COMPUTE W-NEN = W-SNEN - DATE-YC1.
           MOVE W-SGET TO W-GET.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO YRIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
       M-10.
           CALL "SD_Screen_Output" USING "SCTT21" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-YBK "A-YBK" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-10
           END-IF
           MOVE W-YBK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-BK" D-BK "p" RETURNING RESU.
           IF  B-YBC = ZERO
               GO TO M-10
           END-IF
           MOVE B-ZYZ TO W-ZAN.
           IF  W-ZAN = ZERO
               MOVE 4 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
           ELSE
               MOVE 5 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               CALL "SD_Output" USING "D-ZAN" D-ZAN "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" YRIT-F_PNAME1 " " BY REFERENCE YRIT-F_IDLST "0".
       M-15.
      *           READ YRIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" YRIT-F_PNAME1 BY REFERENCE YRIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE YRIT-F_IDLST YRIT-F_PNAME1
               GO TO M-10
           END-IF
           IF  W-YBK NOT = Y-YBK
               GO TO M-15
           END-IF
           MOVE ZERO TO W-PEY.
       M-20.
           MOVE Y-IDO3 TO W-PEY W-PEYD.
       M-25.
           MOVE SPACE TO W-TNA.
           MOVE Y-NO TO UT-KEY.
      *           READ UKET-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　ＵＫＥＴＭ　無し　　＊＊＊" TO W-TNA
               GO TO M-30
           END-IF
           IF  UT-FDM NOT = SPACE
               MOVE UT-FDM TO W-TNA
               MOVE "(ｳﾗﾃ)" TO W-URA
               GO TO M-30
           END-IF
           MOVE SPACE TO W-URA.
           MOVE UT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　　得意先　無し　　＊＊" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
       M-30.
           IF  Y-KBN = 11
               MOVE "ﾔ" TO W-OC
           END-IF
           IF  Y-KBN = 12
               MOVE "ﾀ" TO W-OC
           END-IF
           IF  Y-FUC = 1
               MOVE "＜戻し＞" TO W-MANM
               SUBTRACT Y-KIN FROM W-ZAN
               GO TO M-35
           END-IF
           IF  Y-FUC = 9
               MOVE "＜不渡＞" TO W-MANM
               SUBTRACT Y-KIN FROM W-ZAN
               GO TO M-35
           END-IF
           IF  Y-MAN = 999999
               MOVE SPACE TO W-MAND
               SUBTRACT Y-KIN FROM W-ZAN
           ELSE
               MOVE Y-MAN TO W-MAN
               ADD Y-KIN TO W-ZAN
           END-IF.
       M-35.
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 < 21
               CALL "SD_Output" USING "D-D1" D-D1 "p" RETURNING RESU
               CALL "SD_Output" USING "D-D2" D-D2 "p" RETURNING RESU
               GO TO M-45
           END-IF.
       M-40.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE YRIT-F_IDLST YRIT-F_PNAME1
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-40
           END-IF
           CALL "SD_Screen_Output" USING "SCTT21" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BK" D-BK "p" RETURNING RESU.
           MOVE 5 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE W-PEY TO W-PEYD.
           CALL "SD_Output" USING "D-D1" D-D1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-D2" D-D2 "p" RETURNING RESU.
       M-45.
      *           READ YRIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" YRIT-F_PNAME1 BY REFERENCE YRIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  W-YBK NOT = Y-YBK
               GO TO M-50
           END-IF
           IF  Y-IDO3 NOT = W-PEY
               CALL "SD_Output" USING "D-ZAN" D-ZAN "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE ZERO TO W-PEYD.
           GO TO M-25.
       M-50.
           CALL "SD_Output" USING "D-ZAN" D-ZAN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE YRIT-F_IDLST YRIT-F_PNAME1.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
