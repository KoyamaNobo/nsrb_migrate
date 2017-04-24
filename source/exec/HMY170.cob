       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY170.
      **************************************************************
      *    PROGRAM         :  得意先年間販売ファイル　集計         *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *        変更　　　  :  95/09/13                             *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  前月=0  ,  当月(月次作表)=1          *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-R.
           02  WR-KEY.
             03  WR-TCD       PIC  9(004).
             03  WR-IKC       PIC  9(001).
           02  WR-OU05        PIC S9(009).
           02  WR-OU06        PIC S9(009).
           02  WR-OU07        PIC S9(009).
           02  WR-OU08        PIC S9(009).
           02  WR-OU09        PIC S9(009).
           02  WR-OU10        PIC S9(009).
           02  WR-OU11        PIC S9(009).
           02  WR-OU12        PIC S9(009).
           02  WR-OU01        PIC S9(009).
           02  WR-OU02        PIC S9(009).
           02  WR-OU03        PIC S9(009).
           02  WR-OU04        PIC S9(009).
           02  WR-NU05        PIC S9(009).
           02  WR-NU06        PIC S9(009).
           02  WR-NU07        PIC S9(009).
           02  WR-NU08        PIC S9(009).
           02  WR-NU09        PIC S9(009).
           02  WR-NU10        PIC S9(009).
           02  WR-NU11        PIC S9(009).
           02  WR-NU12        PIC S9(009).
           02  WR-NU01        PIC S9(009).
           02  WR-NU02        PIC S9(009).
           02  WR-NU03        PIC S9(009).
           02  WR-NU04        PIC S9(009).
           02  WR-OUT         PIC S9(010).
           02  WR-NUT         PIC S9(010).
           02  WR-OA05        PIC S9(009).
           02  WR-OA06        PIC S9(009).
           02  WR-OA07        PIC S9(009).
           02  WR-OA08        PIC S9(009).
           02  WR-OA09        PIC S9(009).
           02  WR-OA10        PIC S9(009).
           02  WR-OA11        PIC S9(009).
           02  WR-OA12        PIC S9(009).
           02  WR-OA01        PIC S9(009).
           02  WR-OA02        PIC S9(009).
           02  WR-OA03        PIC S9(009).
           02  WR-OA04        PIC S9(009).
           02  WR-NA05        PIC S9(009).
           02  WR-NA06        PIC S9(009).
           02  WR-NA07        PIC S9(009).
           02  WR-NA08        PIC S9(009).
           02  WR-NA09        PIC S9(009).
           02  WR-NA10        PIC S9(009).
           02  WR-NA11        PIC S9(009).
           02  WR-NA12        PIC S9(009).
           02  WR-NA01        PIC S9(009).
           02  WR-NA02        PIC S9(009).
           02  WR-NA03        PIC S9(009).
           02  WR-NA04        PIC S9(009).
           02  WR-OAT         PIC S9(010).
           02  WR-NAT         PIC S9(010).
           02  WR-NG          PIC  9(006).
           02  WR-TNC         PIC  9(002).
           02  F              PIC  X(026).
           02  WR-SEN         PIC  9(001).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
       01  TZNT-M_HMY170.
           02  TZNT-M_PNAME1    PIC  X(005) VALUE "TZNTM".
           02  F                PIC  X(001).
           02  TZNT-M_LNAME     PIC  X(013) VALUE "TZNT-M_HMY170".
           02  F                PIC  X(001).
           02  TZNT-M_KEY1      PIC  X(100) VALUE SPACE.
           02  TZNT-M_KEY2      PIC  X(100) VALUE SPACE.
           02  TZNT-M_SORT      PIC  X(100) VALUE SPACE.
           02  TZNT-M_IDLST     PIC  X(100) VALUE SPACE.
           02  TZNT-M_RES       USAGE  POINTER.
       01  TZNT-R.
           02  TZNT-KEY.
             03  TZNT-TCD     PIC  9(004).
             03  TZNT-IKC     PIC  9(001).
           02  TZNT-OU05      PIC S9(009).
           02  TZNT-OU06      PIC S9(009).
           02  TZNT-OU07      PIC S9(009).
           02  TZNT-OU08      PIC S9(009).
           02  TZNT-OU09      PIC S9(009).
           02  TZNT-OU10      PIC S9(009).
           02  TZNT-OU11      PIC S9(009).
           02  TZNT-OU12      PIC S9(009).
           02  TZNT-OU01      PIC S9(009).
           02  TZNT-OU02      PIC S9(009).
           02  TZNT-OU03      PIC S9(009).
           02  TZNT-OU04      PIC S9(009).
           02  TZNT-NU05      PIC S9(009).
           02  TZNT-NU06      PIC S9(009).
           02  TZNT-NU07      PIC S9(009).
           02  TZNT-NU08      PIC S9(009).
           02  TZNT-NU09      PIC S9(009).
           02  TZNT-NU10      PIC S9(009).
           02  TZNT-NU11      PIC S9(009).
           02  TZNT-NU12      PIC S9(009).
           02  TZNT-NU01      PIC S9(009).
           02  TZNT-NU02      PIC S9(009).
           02  TZNT-NU03      PIC S9(009).
           02  TZNT-NU04      PIC S9(009).
           02  TZNT-OUT       PIC S9(010).
           02  TZNT-NUT       PIC S9(010).
           02  TZNT-OA05      PIC S9(009).
           02  TZNT-OA06      PIC S9(009).
           02  TZNT-OA07      PIC S9(009).
           02  TZNT-OA08      PIC S9(009).
           02  TZNT-OA09      PIC S9(009).
           02  TZNT-OA10      PIC S9(009).
           02  TZNT-OA11      PIC S9(009).
           02  TZNT-OA12      PIC S9(009).
           02  TZNT-OA01      PIC S9(009).
           02  TZNT-OA02      PIC S9(009).
           02  TZNT-OA03      PIC S9(009).
           02  TZNT-OA04      PIC S9(009).
           02  TZNT-NA05      PIC S9(009).
           02  TZNT-NA06      PIC S9(009).
           02  TZNT-NA07      PIC S9(009).
           02  TZNT-NA08      PIC S9(009).
           02  TZNT-NA09      PIC S9(009).
           02  TZNT-NA10      PIC S9(009).
           02  TZNT-NA11      PIC S9(009).
           02  TZNT-NA12      PIC S9(009).
           02  TZNT-NA01      PIC S9(009).
           02  TZNT-NA02      PIC S9(009).
           02  TZNT-NA03      PIC S9(009).
           02  TZNT-NA04      PIC S9(009).
           02  TZNT-OAT       PIC S9(010).
           02  TZNT-NAT       PIC S9(010).
           02  TZNT-NG        PIC  9(006).
           02  TZNT-TNC       PIC  9(002).
           02  F              PIC  X(027).
       77  F                  PIC  X(001).
       01  TZNTP-M_HMY170.
           02  TZNTP-M_PNAME1    PIC  X(006) VALUE "TZNTPM".
           02  F                 PIC  X(001).
           02  TZNTP-M_LNAME     PIC  X(014) VALUE "TZNTP-M_HMY170".
           02  F                 PIC  X(001).
           02  TZNTP-M_KEY1      PIC  X(100) VALUE SPACE.
           02  TZNTP-M_KEY2      PIC  X(100) VALUE SPACE.
           02  TZNTP-M_SORT      PIC  X(100) VALUE SPACE.
           02  TZNTP-M_IDLST     PIC  X(100) VALUE SPACE.
           02  TZNTP-M_RES       USAGE  POINTER.
       01  TZNTP-R.
           02  TZNTP-KEY.
             03  TZNTP-TCD    PIC  9(004).
             03  TZNTP-IKC    PIC  9(001).
           02  F              PIC  X(507).
       77  F                  PIC  X(001).
       01  WTZNT-F_HMY170.
           02  WTZNT-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F                 PIC  X(001).
           02  WTZNT-F_LNAME     PIC  X(014) VALUE "WTZNT-F_HMY170".
           02  F                 PIC  X(001).
           02  WTZNT-F_KEY1      PIC  X(100) VALUE SPACE.
           02  WTZNT-F_KEY2      PIC  X(100) VALUE SPACE.
           02  WTZNT-F_SORT      PIC  X(100) VALUE SPACE.
           02  WTZNT-F_IDLST     PIC  X(100) VALUE SPACE.
           02  WTZNT-F_RES       USAGE  POINTER.
       01  WTZNT-R.
           02  WTZNT-KEY.
             03  WTZNT-TCD    PIC  9(004).
             03  WTZNT-IKC    PIC  9(001).
           02  WTZNT-OU05     PIC S9(009).
           02  WTZNT-OU06     PIC S9(009).
           02  WTZNT-OU07     PIC S9(009).
           02  WTZNT-OU08     PIC S9(009).
           02  WTZNT-OU09     PIC S9(009).
           02  WTZNT-OU10     PIC S9(009).
           02  WTZNT-OU11     PIC S9(009).
           02  WTZNT-OU12     PIC S9(009).
           02  WTZNT-OU01     PIC S9(009).
           02  WTZNT-OU02     PIC S9(009).
           02  WTZNT-OU03     PIC S9(009).
           02  WTZNT-OU04     PIC S9(009).
           02  WTZNT-NU05     PIC S9(009).
           02  WTZNT-NU06     PIC S9(009).
           02  WTZNT-NU07     PIC S9(009).
           02  WTZNT-NU08     PIC S9(009).
           02  WTZNT-NU09     PIC S9(009).
           02  WTZNT-NU10     PIC S9(009).
           02  WTZNT-NU11     PIC S9(009).
           02  WTZNT-NU12     PIC S9(009).
           02  WTZNT-NU01     PIC S9(009).
           02  WTZNT-NU02     PIC S9(009).
           02  WTZNT-NU03     PIC S9(009).
           02  WTZNT-NU04     PIC S9(009).
           02  WTZNT-OUT      PIC S9(010).
           02  WTZNT-NUT      PIC S9(010).
           02  WTZNT-OA05     PIC S9(009).
           02  WTZNT-OA06     PIC S9(009).
           02  WTZNT-OA07     PIC S9(009).
           02  WTZNT-OA08     PIC S9(009).
           02  WTZNT-OA09     PIC S9(009).
           02  WTZNT-OA10     PIC S9(009).
           02  WTZNT-OA11     PIC S9(009).
           02  WTZNT-OA12     PIC S9(009).
           02  WTZNT-OA01     PIC S9(009).
           02  WTZNT-OA02     PIC S9(009).
           02  WTZNT-OA03     PIC S9(009).
           02  WTZNT-OA04     PIC S9(009).
           02  WTZNT-NA05     PIC S9(009).
           02  WTZNT-NA06     PIC S9(009).
           02  WTZNT-NA07     PIC S9(009).
           02  WTZNT-NA08     PIC S9(009).
           02  WTZNT-NA09     PIC S9(009).
           02  WTZNT-NA10     PIC S9(009).
           02  WTZNT-NA11     PIC S9(009).
           02  WTZNT-NA12     PIC S9(009).
           02  WTZNT-NA01     PIC S9(009).
           02  WTZNT-NA02     PIC S9(009).
           02  WTZNT-NA03     PIC S9(009).
           02  WTZNT-NA04     PIC S9(009).
           02  WTZNT-OAT      PIC S9(010).
           02  WTZNT-NAT      PIC S9(010).
           02  WTZNT-NG       PIC  9(006).
           02  WTZNT-TNC      PIC  9(002).
           02  F              PIC  X(026).
           02  WTZNT-SEN      PIC  9(001).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　得意先年間販売ファイル　集計　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(048) VALUE
                  "全体=0 , 一般ワーク=1 , 教育=2 , ヴィヴ=3  ...  ".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-GETP    PIC  N(007) VALUE
                "（月次作表用）".
       01  C-ACP.
           02  A-SEN     PIC  9(001).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "406" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "10" "48" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "23" "25" "22" "08C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-GETP" "N" "7" "26" "14" " " "C-DSP" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SEN" "9" "12" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "42" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF.
           IF  W-SEN > 3
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-06
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF.
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode"  USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-07
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "A-SEN" A-SEN "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" 
                                              RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "A-DMM" A-DMM "p" 
                                              RETURNING RESU
               CALL "DB_F_Open" USING
                "INPUT SEQUENTIAL" TZNTP-M_PNAME1 "SHARED" BY REFERENCE
                TZNTP-M_IDLST "1" "TZNTP-KEY" BY REFERENCE TZNTP-KEY
               GO TO M-15
           END-IF.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TZNT-M_PNAME1 
            "SHARED" BY REFERENCE TZNT-M_IDLST "1"
            "TZNT-KEY" BY REFERENCE TZNT-KEY.
       M-10.
      *           READ TZNT-M WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "C3_Set_Jrcode"  USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-SEN = 1
               IF  TZNT-IKC NOT = 1 AND 3
                   GO TO M-10
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  TZNT-IKC NOT = 2
                   GO TO M-10
               END-IF
           END-IF.
           IF  W-SEN = 3
               IF  TZNT-IKC NOT = 3
                   GO TO M-10
               END-IF
           END-IF.
           MOVE ZERO TO W-R.
           MOVE TZNT-R TO W-R.
           GO TO M-20.
       M-15.
      *           READ TZNTP-M WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "C3_Set_Jrcode"  USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-SEN = 1
               IF  TZNTP-IKC NOT = 1 AND 3
                   GO TO M-15
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  TZNTP-IKC NOT = 2
                   GO TO M-15
               END-IF
           END-IF.
           IF  W-SEN = 3
               IF  TZNTP-IKC NOT = 3
                   GO TO M-15
               END-IF
           END-IF.
           MOVE ZERO TO W-R.
           MOVE TZNTP-R TO W-R.
       M-20.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO WTZNT-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" WTZNT-F_PNAME1 " " BY REFERENCE WTZNT-F_IDLST "0".
       M-25.
           MOVE ZERO TO WTZNT-R.
           MOVE W-R TO WTZNT-R.
       M-30.
           IF  JS-SIGN = 1
               GO TO M-35
           END-IF.
      *           READ TZNT-M WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  W-SEN = 1
               IF  TZNT-IKC NOT = 1 AND 3
                   GO TO M-30
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  TZNT-IKC NOT = 2
                   GO TO M-30
               END-IF
           END-IF.
           IF  W-SEN = 3
               IF  TZNT-IKC NOT = 3
                   GO TO M-30
               END-IF
           END-IF.
           MOVE ZERO TO W-R.
           MOVE TZNT-R TO W-R.
           GO TO M-40.
       M-35.
      *           READ TZNTP-M WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  W-SEN = 1
               IF  TZNTP-IKC NOT = 1 AND 3
                   GO TO M-35
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  TZNTP-IKC NOT = 2
                   GO TO M-35
               END-IF
           END-IF.
           IF  W-SEN = 3
               IF  TZNTP-IKC NOT = 3
                   GO TO M-35
               END-IF
           END-IF.
           MOVE ZERO TO W-R.
           MOVE TZNTP-R TO W-R.
       M-40.
           IF  WR-TCD = WTZNT-TCD
               ADD WR-OU05 TO WTZNT-OU05
               ADD WR-OU06 TO WTZNT-OU06
               ADD WR-OU07 TO WTZNT-OU07
               ADD WR-OU08 TO WTZNT-OU08
               ADD WR-OU09 TO WTZNT-OU09
               ADD WR-OU10 TO WTZNT-OU10
               ADD WR-OU11 TO WTZNT-OU11
               ADD WR-OU12 TO WTZNT-OU12
               ADD WR-OU01 TO WTZNT-OU01
               ADD WR-OU02 TO WTZNT-OU02
               ADD WR-OU03 TO WTZNT-OU03
               ADD WR-OU04 TO WTZNT-OU04
               ADD WR-NU05 TO WTZNT-NU05
               ADD WR-NU06 TO WTZNT-NU06
               ADD WR-NU07 TO WTZNT-NU07
               ADD WR-NU08 TO WTZNT-NU08
               ADD WR-NU09 TO WTZNT-NU09
               ADD WR-NU10 TO WTZNT-NU10
               ADD WR-NU11 TO WTZNT-NU11
               ADD WR-NU12 TO WTZNT-NU12
               ADD WR-NU01 TO WTZNT-NU01
               ADD WR-NU02 TO WTZNT-NU02
               ADD WR-NU03 TO WTZNT-NU03
               ADD WR-NU04 TO WTZNT-NU04
               ADD WR-OUT  TO WTZNT-OUT
               ADD WR-NUT  TO WTZNT-NUT
               ADD WR-OA05 TO WTZNT-OA05
               ADD WR-OA06 TO WTZNT-OA06
               ADD WR-OA07 TO WTZNT-OA07
               ADD WR-OA08 TO WTZNT-OA08
               ADD WR-OA09 TO WTZNT-OA09
               ADD WR-OA10 TO WTZNT-OA10
               ADD WR-OA11 TO WTZNT-OA11
               ADD WR-OA12 TO WTZNT-OA12
               ADD WR-OA01 TO WTZNT-OA01
               ADD WR-OA02 TO WTZNT-OA02
               ADD WR-OA03 TO WTZNT-OA03
               ADD WR-OA04 TO WTZNT-OA04
               ADD WR-NA05 TO WTZNT-NA05
               ADD WR-NA06 TO WTZNT-NA06
               ADD WR-NA07 TO WTZNT-NA07
               ADD WR-NA08 TO WTZNT-NA08
               ADD WR-NA09 TO WTZNT-NA09
               ADD WR-NA10 TO WTZNT-NA10
               ADD WR-NA11 TO WTZNT-NA11
               ADD WR-NA12 TO WTZNT-NA12
               ADD WR-NA01 TO WTZNT-NA01
               ADD WR-NA02 TO WTZNT-NA02
               ADD WR-NA03 TO WTZNT-NA03
               ADD WR-NA04 TO WTZNT-NA04
               ADD WR-OAT  TO WTZNT-OAT
               ADD WR-NAT  TO WTZNT-NAT
               GO TO M-30
           END-IF
           MOVE W-SEN TO WTZNT-SEN.
      *           WRITE WTZNT-R.
      *//////////////////////
           CALL "DB_Insert" USING
            WTZNT-F_PNAME1 WTZNT-F_LNAME WTZNT-R RETURNING RET.
           GO TO M-25.
       M-90.
           MOVE W-SEN TO WTZNT-SEN.
      *           WRITE WTZNT-R.
      *//////////////////////
           CALL "DB_Insert" USING
            WTZNT-F_PNAME1 WTZNT-F_LNAME WTZNT-R RETURNING RET.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE WTZNT-F_IDLST WTZNT-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
