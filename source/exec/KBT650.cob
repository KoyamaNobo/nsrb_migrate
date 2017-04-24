       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT650.
      *********************************************************
      *    PROGRAM         :  工品材料仕入明細問合せ          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  全件=0,工品=1                   *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN          PIC  9(001).
       01  W-DATA.
           02  W-SIGN       PIC  N(004).
           02  W-NG.
             03  W-NEN      PIC  9(002).
             03  W-GET      PIC  9(002).
           02  W-JCD        PIC  9(006).
           02  W-DMM        PIC  9(001).
           02  W-END        PIC  9(001).
           02  W-T          PIC S9(006)V9(02).
           02  W-L1         PIC  9(002).
           02  W-L2         PIC  9(002).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
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
           02  FILLER.
             03  FILLER  PIC  N(004).
             03  FILLER  PIC  N(020) VALUE
                  "＊＊＊　　材料仕入明細　問合せ　　＊＊＊".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC Z9 .
             03  FILLER  PIC  N(001) VALUE "月".
           02  FILLER  PIC  X(020) VALUE
                "ｺｰﾄﾞ  材　　料　　名".
           02  FILLER  PIC  X(029) VALUE
                "数　量     単　価      金　額".
           02  FILLER  PIC  X(035) VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=F10 , 終了=F9     ".
       01  C-ACP.
           02  A-JCD   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MEI.
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC --,---,--9.99 .
               04  FILLER  PIC ----,--9.99 .
               04  FILLER  PIC ----,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-JCD   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "140" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" " " "1" "0" "56" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID" "N" "1" "1" "8" " " "01C-MID"  RETURNING RESU.
       CALL "SD_From" USING 
            "0101C-MID" BY REFERENCE W-SIGN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201C-MID" "N" "1" "21" "40" "0101C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-MID" "9" "1" "72" "2" "0201C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0301C-MID" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-MID" "N" "1" "74" "2" "0301C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0501C-MID" "Z9" "1" "76" "2" "0401C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0501C-MID" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0601C-MID" "N" "1" "78" "2" "0501C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "2" "3" "20" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "3" "51" "29" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "31" "35" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "4" "2" "6" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "A-JCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "90" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "90" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "0" "54" " " "D-MEI"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MEI" "9" "W-L1" "2" "6" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MEI" BY REFERENCE J-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEI" "N" "W-L1" "9" "48" "0101D-MEI" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MEI" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" " " "W-L2" "0" "36" "01D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEI" "--,---,--9.99" "W-L2" "44" "13" " " "02D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MEI" BY REFERENCE JT-SSU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MEI" "----,--9.99" "W-L2" "57" "11" "0102D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-MEI" BY REFERENCE W-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MEI" "----,---,--9" "W-L2" "68" "12" "0202D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-MEI" BY REFERENCE JT-SIK "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "48" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "48" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE JT-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-JCD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SIGN = 0
               MOVE SPACE TO W-SIGN
           ELSE
               MOVE "【工品】" TO W-SIGN
           END-IF
           COPY LIBCPR.
           MOVE D-NKNG TO W-NG.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
       M-06.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 3 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-08
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           MOVE SPACE TO JT-KEY.
           MOVE W-JCD TO JT-KEY.
      *           START JT-M KEY NOT < JT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JT-M_PNAME1 "JT-KEY" " NOT < " JT-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JT-M_IDLST JT-M_PNAME1
               GO TO M-08
           END-IF
           MOVE 0 TO W-END.
       M-10.
      *           READ JT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JT-M_IDLST JT-M_PNAME1
               GO TO M-08
           END-IF
           IF  JS-SIGN = 1
               IF  JT-BC NOT = 4
                   GO TO M-10
               END-IF
           END-IF
           IF  ZERO = JT-SSU AND JT-SIK
               GO TO M-10
           END-IF.
       M-15.
           MOVE JT-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "＊＊　材料マスタなし　＊＊" TO J-NAME
           END-IF
           MOVE ZERO TO W-T.
           IF  ZERO NOT = JT-SSU AND JT-SIK
               COMPUTE W-T = (JT-SIK / JT-SSU) + 0.009
           END-IF.
       M-17.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 > 21
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
       M-20.
      *           READ JT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-25
           END-IF
           IF  JS-SIGN = 1
               IF  JT-BC > 4
                   GO TO M-20
               END-IF
           END-IF
           IF  ZERO = JT-SSU AND JT-SIK
               GO TO M-20
           END-IF
           GO TO M-15.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE JT-M_IDLST JT-M_PNAME1
               GO TO M-90
           END-IF
           IF (ESTAT = ADV) OR (W-END = 1)
               CALL "DB_F_Close" USING
                BY REFERENCE JT-M_IDLST JT-M_PNAME1
               GO TO M-06
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-25
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 3 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           GO TO M-17.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
