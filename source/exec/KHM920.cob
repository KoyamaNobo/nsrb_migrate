       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KHM920.
      ************************************
      ******    用途区分　問合せ    ******
      ************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-ACT               PIC  9(001).
           02  W-KEYD.
             03  F                 PIC  X(002) VALUE "01".
             03  W-KEY             PIC  9(002).
             03  F                 PIC  X(003) VALUE SPACE.
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           COPY LSTAT.
      *
           COPY LIKKBM.
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
           02  FILLER  PIC  N(018)    VALUE
                "＊＊＊　　用途区分　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "区分".
             03  FILLER  PIC  N(016) VALUE
                  "用　　途　　名　　　　　　　　　".
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  A-KEY   PIC  9(002).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  FILLER.
               03  D-KEY    PIC 9(002).
               03  D-NAME   PIC N(016).
       01  C-ERR.
           02  FILLER.
               03  E-STAT   PIC X(010).
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE           DIVISION.
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
            "C-MID" " " "0" "0" "106" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "22" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "3" "0" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "RN" "3" "21" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202C-MID" "RN" "3" "27" "32" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "23" "30" "34" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "4" "22" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "23" "67" "1" "A-KEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "34" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "34" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEY" "9" "W-L" "22" "2" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KEY" BY REFERENCE KKB-YC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "27" "32" "D-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE KKB-YCN "32" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "20" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO  TO  M-99
           END-IF
           IF  ESTAT = HTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-99
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE  W-KEYD  TO KKB-KEY.
       M-20.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
      *           START KKB-M KEY NOT < KKB-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            KKB-M_PNAME1 "KKB-KEY" " NOT < " KKB-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO  TO  M-15
           END-IF.
       M-27.
      *           READ KKB-M NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KKB-M_PNAME1 BY REFERENCE KKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO  TO  M-15
           END-IF
           IF  KKB-NO NOT = "01"
               GO TO M-15
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       M-35.
      *           READ KKB-M NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KKB-M_PNAME1 BY REFERENCE KKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  M-25
           END-IF
           IF  KKB-NO NOT = "01"
               GO TO M-35
           END-IF
           GO  TO  M-30.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
