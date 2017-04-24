       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG930.
      *******************************************************
      *    PROGRAM        :  購買月次累積                   *
      *    PRINTER TYPE   :  ****                           *
      *    SCREEN         :  ******                         *
      *    COMPILE TYPE   :  COBOL                          *
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIJM.
           COPY LIJTM.
           COPY LISTM.
      *FD  JSSR-F
       01  JSSR-F_KBG930.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBG930".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R.
           02  JS-DK          PIC  9(002).
           02  JS-SD          PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SSU         PIC S9(007)V9(02).
           02  JS-TN          PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  F              PIC  X(007).
           02  JS-CD          PIC  9(006).
           02  F              PIC  X(013).
           02  JS-YOC         PIC  9(001).
           02  JS-TNC         PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  F              PIC  X(027).
       77  F                  PIC  X(001).
      *FD  HAR-F
       01  HAR-F_KBG930.
           02  HAR-F_PNAME1   PIC  X(004) VALUE "HARF".
           02  F              PIC  X(001).
           02  HAR-F_LNAME    PIC  X(012) VALUE "HAR-F_KBG930".
           02  F              PIC  X(001).
           02  HAR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HAR-F_SORT     PIC  X(100) VALUE SPACE.
           02  HAR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HAR-F_RES      USAGE  POINTER.
       01  HAR-R              PIC  X(032).
       77  F                  PIC  X(001).
      *FD  SMYR
       01  SMYR_KBG930.
           02  SMYR_PNAME1    PIC  X(004) VALUE "SMYR".
           02  F              PIC  X(001).
           02  SMYR_LNAME     PIC  X(011) VALUE "SMYR_KBG930".
           02  F              PIC  X(001).
           02  SMYR_KEY1      PIC  X(100) VALUE SPACE.
           02  SMYR_SORT      PIC  X(100) VALUE SPACE.
           02  SMYR_IDLST     PIC  X(100) VALUE SPACE.
           02  SMYR_RES       USAGE  POINTER.
       01  SMY-R.
           02  SMY-KEY.                                                 ＫＥＹ
             03  SMY-TCD      PIC  9(004).                              仕入先CD
           02  SMY-NAME       PIC  N(024).                              仕入先名
           02  SMY-JSU        PIC  N(024).                              住所(上)
           02  SMY-JSS        PIC  N(012).                              住所(下)
           02  SMY-UNO        PIC  X(008).
           02  SMY-TEL        PIC  X(014).
           02  SMY-FAX        PIC  X(014).
           02  SMY-FKC        PIC  9(002).                              府県ｺｰﾄﾞ
           02  SMY-SZC        PIC  9(001).                              消費税C
           02  SMY-TTS        PIC  9(002).                              支手ｻｲﾄ
           02  SMY-THN        PIC  9(002).                              支手 日
           02  SMY-TMC        PIC  9(001).                              支手複数
           02  SMY-SKR        PIC  9(001).                              送金料C
           02  SMY-PC         PIC  9(002).
           02  SMY-BKC        PIC  9(002).
           02  F              PIC  X(070).
           02  SMY-NG         PIC  9(006).
           02  SMY-ENG        PIC  9(004).
           02  SMY-KKC        PIC  9(001).                              買掛区分
           02  SMY-SFC        PIC  9(001).                              振込区分
           02  SMY-TGC        PIC  9(001).                              手形区分
       77  F                  PIC  X(001).
      *FD  JMYR
       01  JMYR_KBG930.
           02  JMYR_PNAME1    PIC  X(004) VALUE "JMYR".
           02  F              PIC  X(001).
           02  JMYR_LNAME     PIC  X(011) VALUE "JMYR_KBG930".
           02  F              PIC  X(001).
           02  JMYR_KEY1      PIC  X(100) VALUE SPACE.
           02  JMYR_SORT      PIC  X(100) VALUE SPACE.
           02  JMYR_IDLST     PIC  X(100) VALUE SPACE.
           02  JMYR_RES       USAGE  POINTER.
       01  JMY-R.
           02  JMY-JCD        PIC  9(006).                              材料ｺｰﾄﾞ
           02  JMY-NAME       PIC  N(024).                              材料名
           02  JMY-YC         PIC  9(001).                              用途区分
           02  JMY-ZC         PIC  9(001).                              在庫区分
           02  JMY-SC         PIC  9(001).                              製品区分
           02  JMY-TC1        PIC  9(001).                              単位区分
           02  JMY-ST         PIC S9(006)V9(02).                        最終単価
           02  JMY-TC2        PIC  9(001).                              単位区分
           02  JMY-YT         PIC S9(006)V9(02).                        予定単価
           02  JMY-MCD        PIC  9(006).                              加工前CD
           02  JMY-KT         PIC  9(006)V9(02).                        加工単価
           02  FILLER         PIC  X(022).                              FILLER
           02  JMY-BKC        PIC  9(002).                              部門管理
           02  F              PIC  X(003).                              FILLER
           02  JMY-NG         PIC  9(006).
           02  JMY-ED         PIC  9(006).                              最終日付
       77  F                  PIC  X(001).
      *FD  STY-F
       01  STY-F_KBG930.
           02  STY-F_PNAME1   PIC  X(004) VALUE "STYF".
           02  F              PIC  X(001).
           02  STY-F_LNAME    PIC  X(012) VALUE "STY-F_KBG930".
           02  F              PIC  X(001).
           02  STY-F_KEY1     PIC  X(100) VALUE SPACE.
           02  STY-F_SORT     PIC  X(100) VALUE SPACE.
           02  STY-F_IDLST    PIC  X(100) VALUE SPACE.
           02  STY-F_RES      USAGE  POINTER.
       01  STY-R.
           02  STY-KEY.
             03  STY-KEY1     PIC  9(001).
             03  STY-KEY2     PIC  9(003).
           02  STY-ZKZ        PIC S9(009).                              I.120829
           02  STY-ZKZZ       PIC S9(008).                              I.120829
           02  STY-KZ         PIC S9(009).                              I.120829
           02  STY-KZZ        PIC S9(008).                              I.120829
           02  STY-TSK        PIC S9(009).
           02  STY-TSKZ       PIC S9(008).
           02  STY-THK        PIC S9(009).
           02  STY-THKZ       PIC S9(008).
           02  STY-PC         PIC  9(004).
           02  F              PIC  X(003).
           02  STY-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  JTY-F
       01  JTY-F_KBG930.
           02  JTY-F_PNAME1   PIC  X(004) VALUE "JTYF".
           02  F              PIC  X(001).
           02  JTY-F_LNAME    PIC  X(012) VALUE "JTY-F_KBG930".
           02  F              PIC  X(001).
           02  JTY-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JTY-F_SORT     PIC  X(100) VALUE SPACE.
           02  JTY-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JTY-F_RES      USAGE  POINTER.
       01  JTY-R.
           02  JTY-KEY.
             03  JTY-KEYD.
               04  JTY-BC     PIC  9(001).
               04  JTY-RC     PIC  9(002).
             03  JTY-JC       PIC  9(003).
           02  JTY-TSU        PIC S9(007)V9(02).
           02  JTY-SSU        PIC S9(007)V9(02).
           02  JTY-SIK        PIC S9(008).
           02  JTY-HSU        PIC S9(007)V9(02).
           02  JTY-ZKS        PIC S9(007)V9(02).
           02  JTY-ZKK        PIC S9(008).
           02  JTY-YC         PIC  9(001).
           02  JTY-ZC         PIC  9(001).
           02  JTY-SC         PIC  9(001).
           02  JTY-CSU        PIC S9(007)V9(02).
           02  F              PIC  X(009).
           02  JTY-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  JSSRYR
       01  JSSRYR_KBG930.
           02  JSSRYR_PNAME1  PIC  X(006) VALUE "JSSRYR".
           02  F              PIC  X(001).
           02  JSSRYR_LNAME   PIC  X(013) VALUE "JSSRYR_KBG930".
           02  F              PIC  X(001).
           02  JSSRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSRYR_SORT    PIC  X(100) VALUE SPACE.
           02  JSSRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSRYR_RES     USAGE  POINTER.
       01  JSSRYR-R           PIC  X(102).
       77  F                  PIC  X(001).
      *FD  HARYR
       01  HARYR_KBG930.
           02  HARYR_PNAME1   PIC  X(005) VALUE "HARYR".
           02  F              PIC  X(001).
           02  HARYR_LNAME    PIC  X(012) VALUE "HARYR_KBG930".
           02  F              PIC  X(001).
           02  HARYR_KEY1     PIC  X(100) VALUE SPACE.
           02  HARYR_SORT     PIC  X(100) VALUE SPACE.
           02  HARYR_IDLST    PIC  X(100) VALUE SPACE.
           02  HARYR_RES      USAGE  POINTER.
       01  HARYR-R            PIC  X(032).
       77  F                  PIC  X(001).
      *
      *FD  JSS-F
       01  JSS-F_KBG930.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KBG930".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  F              PIC  X(094).
           02  JSS-KEY        PIC  X(007).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  HA-F
       01  HA-F_KBG930.
           02  HA-F_PNAME1    PIC  X(003) VALUE "HAF".
           02  F              PIC  X(001).
           02  HA-F_LNAME     PIC  X(011) VALUE "HA-F_KBG930".
           02  F              PIC  X(001).
           02  HA-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HA-F_SORT      PIC  X(100) VALUE SPACE.
           02  HA-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HA-F_RES       USAGE  POINTER.
       01  HA-R.
           02  F              PIC  X(023).
           02  HA-KEY         PIC  X(007).
           02  F              PIC  X(002).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　購買　月次累積　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                  "［　  年  月分　］".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9 .
       01  E-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  SMYR WRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(024) VALUE
                  "***  JMYR WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  STYF WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(024) VALUE
                  "***  JTYF WRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  JSSRYR WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  HARYR WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  N(008) VALUE
                  "未更新データあり".
           COPY LSSEM.
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "278" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "18" "18" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "16" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "33" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "12" "22" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "26" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
      *E-ERR
       CALL "SD_Init" USING 
            "E-ERR" " " "0" "0" "163" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ERR" " " "24" "0" "163" " " "E-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "24" " " "01E-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "24" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "24" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "N" "24" "15" "16" "E-ME6" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           INITIALIZE W-DATA.
           COPY LIBCPR.
           MOVE D-NBNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           PERFORM SM-RTN THRU SM-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           PERFORM JM-RTN THRU JM-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           PERFORM STM-RTN THRU STM-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           PERFORM JTM-RTN THRU JTM-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           PERFORM JS-RTN THRU JS-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           PERFORM HA-RTN THRU HA-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           CALL "DB_F_Open" USING
            "INPUT SEQUENTIAL" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "1"
            "JSS-KEY" BY REFERENCE JSS-KEY.
      *           READ JSS-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-010
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           GO TO CHK-020.
       CHK-010.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" HA-F_PNAME1 
            " " BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
      *           READ HA-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HA-F_PNAME1 BY REFERENCE HA-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-030
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
       CHK-020.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO CHK-EX.
       CHK-030.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
       CHK-EX.
           EXIT.
      *
       SM-RTN.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" SMYR_PNAME1 " " BY REFERENCE SMYR_IDLST "0".
       SM-020.
      *           READ S-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SM-060
           END-IF.
       SM-040.
           INITIALIZE SMY-R.
           MOVE S-R TO SMY-R.
           MOVE W-NG TO SMY-NG.
      *           WRITE SMY-R.
      *//////////////
           CALL "DB_Insert" USING
            SMYR_PNAME1 SMYR_LNAME SMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SM-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SMYR_IDLST SMYR_PNAME1
               MOVE "SMYR         " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" SMYR_PNAME1 " " BY REFERENCE SMYR_IDLST "0"
               GO TO SM-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       SM-060.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SMYR_IDLST SMYR_PNAME1.
       SM-EX.
           EXIT.
      *
       JM-RTN.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" JMYR_PNAME1 " " BY REFERENCE JMYR_IDLST "0".
       JM-020.
      *           READ J-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" J-M_PNAME1 BY REFERENCE J-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO JM-060
           END-IF.
       JM-040.
           INITIALIZE JMY-R.
           MOVE J-R TO JMY-R.
           MOVE W-NG TO JMY-NG.
      *           WRITE JMY-R.
      *//////////////
           CALL "DB_Insert" USING
            JMYR_PNAME1 JMYR_LNAME JMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO JM-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JMYR_IDLST JMYR_PNAME1
               MOVE "JMYR         " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" JMYR_PNAME1 " " BY REFERENCE JMYR_IDLST "0"
               GO TO JM-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       JM-060.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JMYR_IDLST JMYR_PNAME1.
       JM-EX.
           EXIT.
      *
       STM-RTN.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" ST-M_PNAME1 
            " " BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" STY-F_PNAME1 " " BY REFERENCE STY-F_IDLST "0".
       STM-020.
      *           READ ST-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ST-M_PNAME1 BY REFERENCE ST-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO STM-060
           END-IF.
       STM-040.
           INITIALIZE STY-R.
           MOVE ST-R TO STY-R.
           MOVE W-NG TO STY-NG.
      *           WRITE STY-R.
      *//////////////
           CALL "DB_Insert" USING
            STY-F_PNAME1 STY-F_LNAME STY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO STM-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE STY-F_IDLST STY-F_PNAME1
               MOVE "STYF         " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" STY-F_PNAME1 " " BY REFERENCE STY-F_IDLST "0"
               GO TO STM-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       STM-060.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STY-F_IDLST STY-F_PNAME1.
       STM-EX.
           EXIT.
      *
       JTM-RTN.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JT-M_PNAME1 
           " " BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" JTY-F_PNAME1 " " BY REFERENCE JTY-F_IDLST "0".
       JTM-020.
      *           READ JT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO JTM-060
           END-IF.
       JTM-040.
           INITIALIZE JTY-R.
           MOVE JT-R TO JTY-R.
           MOVE W-NG TO JTY-NG.
      *           WRITE JTY-R.
      *//////////////
           CALL "DB_Insert" USING
            JTY-F_PNAME1 JTY-F_LNAME JTY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO JTM-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JTY-F_IDLST JTY-F_PNAME1
               MOVE "JTYF         " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" JTY-F_PNAME1 " " BY REFERENCE JTY-F_IDLST "0"
               GO TO JTM-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       JTM-060.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JTY-F_IDLST JTY-F_PNAME1.
       JTM-EX.
           EXIT.
      *
       JS-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JSSRYR_PNAME1 " " BY REFERENCE JSSRYR_IDLST "0".
       JS-020.
      *           READ JSSR-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO JS-060
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       JS-040.
           MOVE ZERO TO JSSRYR-R.
           MOVE JSSR-R TO JSSRYR-R.
      *           WRITE JSSRYR-R.
      *//////////////
           CALL "DB_Insert" USING
            JSSRYR_PNAME1 JSSRYR_LNAME JSSRYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO JS-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSSRYR_IDLST JSSRYR_PNAME1
               MOVE "JSSRYR       " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" JSSRYR_PNAME1 " " BY REFERENCE JSSRYR_IDLST "0"
               GO TO JS-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       JS-060.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSRYR_IDLST JSSRYR_PNAME1.
       JS-EX.
           EXIT.
      *
       HA-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" HARYR_PNAME1 " " BY REFERENCE HARYR_IDLST "0".
       HA-020.
      *           READ HAR-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HAR-F_PNAME1 BY REFERENCE HAR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO HA-060
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       HA-040.
           MOVE ZERO TO HARYR-R.
           MOVE HAR-R TO HARYR-R.
      *           WRITE HARYR-R.
      *//////////////
           CALL "DB_Insert" USING
            HARYR_PNAME1 HARYR_LNAME HARYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO HA-020
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HARYR_IDLST HARYR_PNAME1
               MOVE "HARYR        " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" HARYR_PNAME1 " " BY REFERENCE HARYR_IDLST "0"
               GO TO HA-040
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       HA-060.
           CALL "DB_F_Close" USING
            BY REFERENCE HAR-F_IDLST HAR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HARYR_IDLST HARYR_PNAME1.
       HA-EX.
           EXIT.
