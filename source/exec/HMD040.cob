       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HMD040.
       AUTHOR.             H.KAMASAKA    1996-07-03.
      ******************************************
      ******    発送明細ファイル　削除    ******
      ******************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  W-DATA.
           02  W-DMM               PIC  9(001).
           02  W-DATE.
               03  W-Y             PIC  9(004).
               03  W-YD    REDEFINES W-Y.
                   04  W-Y1        PIC  9(002).
                   04  W-Y2        PIC  9(002).
               03  W-M             PIC  9(002).
               03  W-D             PIC  9(002).
           02  W-NGP   REDEFINES W-DATE.
               03  F               PIC  9(002).
               03  W-NGPS          PIC  9(006).
           02  W-KEY               PIC  9(006).
       01  ERR-STAT                PIC  X(002).
           COPY LSTAT.
      *
           COPY  LIBFDD.
           COPY  LIHSMS.
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
           02  FILLER  PIC  N(021)    VALUE
                 "＊＊＊　　発送明細ファイル　削除　　＊＊＊".
           02  FILLER  PIC  X(022)    VALUE
                   "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM    PIC  9(001).
       01  C-ERR.
           02  FILLER.
               03  E-STAT   PIC X(002).
               03  E-ME1    PIC X(020)   VALUE
                     "***  DELETE ｴﾗｰ  ***".
               03  E-ME98   PIC  X(005) VALUE X"1B4A05".
               03  E-ME99   PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
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
            "C-MID" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "18" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "23" "40" "22" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "32" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
           "E-STAT" BY REFERENCE ERR-STAT "2" "0"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "20" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-DMM = 9
               GO  TO  M-99
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-10
           END-IF
      *
           COPY LIBCPR.
           MOVE  ZERO  TO  W-DATE.
           ACCEPT  W-NGPS  FROM  DATE.
           IF  W-Y2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-Y
           END-IF
           IF  W-Y2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-Y
           END-IF
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           MOVE  ZERO  TO  W-KEY.
           SUBTRACT  1  FROM  W-M.
           IF  W-M = ZERO
               SUBTRACT  1  FROM  W-Y
               MOVE  12  TO  W-M
           END-IF
           SUBTRACT  1  FROM  W-M.
           IF  W-M = ZERO
               SUBTRACT  1  FROM  W-Y
               MOVE  12  TO  W-M
           END-IF.
       M-15.
      *           READ  HSMSF  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               GO  TO  M-99
           END-IF
           IF  W-KEY = HSMS-01
               GO  TO  M-20
           END-IF
           IF  HSMS-02 NOT = 7
               IF  HSMS-19 NOT = ZERO
                   MOVE  HSMS-01  TO  W-KEY
                   GO  TO  M-20
               END-IF
           END-IF
           IF  HSMS-02 NOT = 7
               IF  W-DATE > HSMS-05
                   MOVE  HSMS-01  TO  W-KEY
                   GO  TO  M-20
               END-IF
           END-IF
           GO  TO  M-15.
       M-20.
      *           DELETE  HSMSF  INVALID  KEY
      *///////////////
           CALL "DB_Delete" USING HSMSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               GO  TO  M-99
           END-IF
           GO  TO  M-15.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
