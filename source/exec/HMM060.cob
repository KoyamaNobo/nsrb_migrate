       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HMM060.
      ****************************************
      ******    品名コード　問合せ      ******
      ****************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-SEN               PIC  9(001).
           02  W-SBC3              PIC  9(002).
           02  W-EBC3              PIC  9(002) VALUE 99.
           02  W-SBMNO             PIC  9(001).
           02  W-EBMNO             PIC  9(001) VALUE 9.
           02  W-SBC1              PIC  9(002).
           02  W-EBC1              PIC  9(002) VALUE 99.
           02  W-DMM               PIC  9(001).
      *
           02  W-ACT               PIC  9(001).
           02  W-KEY               PIC  9(006).
           02  W-L                 PIC  9(002).
           COPY LSTAT.
      *
           COPY LIHIM.
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
           02  FILLER  PIC  N(019)    VALUE
                "＊＊＊　　品名コード　問合せ　　＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(001) VALUE "部".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(020) VALUE
                  "品　　　　　名　　　　　　　　　　　　　".
             03  FILLER  PIC  N(003) VALUE "分類③".
             03  FILLER  PIC  N(001) VALUE "門".
             03  FILLER  PIC  N(001) VALUE "①".
             03  FILLER  PIC  N(003) VALUE "登録日".
           02  FILLER  PIC  X(030)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=F10 , 終了=F9".
       01  C-MID2.
           02  FILLER  PIC  X(034) VALUE
                "廃止分   非表示=0 , 表示=1 ... [ ]".
           02  FILLER.
             03  FILLER  PIC  X(019) VALUE
                  "分類③   00  ～  99".
             03  FILLER  PIC  X(031) VALUE
                  "(カジュ=10,ワーク=20,教  育=30)".
           02  FILLER.
             03  FILLER  PIC  X(019) VALUE
                  "部門№    0  ～   9".
             03  FILLER  PIC  X(038) VALUE
                  "(国内=1,上海=2,仕入=3,ワーク=4,教育=5)".
           02  FILLER  PIC  X(019) VALUE
                "分類①   00  ～  99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBC1  PIC  9(002).
             03  A-EBC1  PIC  9(002).
           02  A-DMM   PIC  9(001).
      *
           02  A-KEY   PIC  9(006).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  FILLER.
               03  D-KEY    PIC 9(006).
               03  D-NAME   PIC N(024).
               03  D-BC3    PIC 9(002).
               03  D-BMC    PIC 9(002).
               03  D-BC1    PIC 9(003).
               03  D-SNG    PIC 99/99 .
       01  C-ERR.
           02  FILLER.
               03  E-STAT  PIC X(010).
               03  E-ME98  PIC X(005) VALUE X"1B4A05".
               03  E-ME99  PIC X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "RN" "1" "21" "38" " " "C-MID" RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "92" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "RN" "2" "63" "2" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" " " "3" "0" "60" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" "RX" "3" "5" "4" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "RN" "3" "11" "40" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "RN" "3" "56" "6" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID1" "RN" "3" "63" "2" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID1" "RN" "3" "66" "2" "06C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID1" "RN" "3" "70" "6" "07C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID1" "X" "23" "30" "30" "08C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
           "C-MID2" " " "0" "0" "182" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID2" "X" "12" "21" "34" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID2" " " "14" "0" "50" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID2" "X" "14" "21" "19" " " "02C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID2" "X" "14" "43" "31" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID2" " " "16" "0" "57" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID2" "X" "16" "21" "19" " " "05C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID2" "X" "16" "43" "38" "06C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID2" "X" "18" "21" "19" "05C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID2" "X" "23" "31" "22" "08C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "12" "53" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "4" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC3" "9" "14" "30" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC3" "9" "14" "38" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "16" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBMNO" "9" "16" "31" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBMNO" "9" "16" "39" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-ACP" " " "18" "0" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC1" "9" "18" "30" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC1" BY REFERENCE W-SBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC1" "9" "18" "38" "2" "A-SBC1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC1" BY REFERENCE W-EBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "48" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-KEY" "9" "4" "4" "6" "A-DMM" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ACT" "9" "23" "67" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "W-L" "0" "66" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-KEY" "9" "W-L" "4" "6" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-KEY" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "W-L" "11" "48" "D-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-BC3" "9" "W-L" "60" "2" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-BC3" BY REFERENCE HI-BC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-BMC" "9" "W-L" "63" "2" "D-BC3" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-BMC" BY REFERENCE HI-BMC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-BC1" "9" "W-L" "66" "3" "D-BMC" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-BC1" BY REFERENCE HI-BCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SNG" "99/99" "W-L" "70" "5" "D-BC1" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-SNG" BY REFERENCE HI-SNG "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "20" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "10" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-00.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
       M-01.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-01
           END-IF
           IF  W-SEN > 1
               GO TO M-01
           END-IF.
       M-02.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-01
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-02
           END-IF.
       M-03.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-02
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-03
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO M-03
           END-IF.
       M-04.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-03
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-04
           END-IF.
       M-05.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-04
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-05
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO M-05
           END-IF.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-SBC1 "A-SBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-05
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-EBC1 "A-EBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-06
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF
           IF  W-SBC1 > W-EBC1
               GO TO M-07
           END-IF.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-07
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-08
           END-IF
           IF  W-DMM = 9
               GO TO M-01
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-08
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO  TO  M-95
           END-IF
           IF  ESTAT = HTB
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF
           MOVE  W-KEY  TO  HI-KEY.
       M-20.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
      *           START  HI-M  KEY NOT < HI-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO  TO  M-15
           END-IF.
       M-26.
      *           READ  HI-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO  TO  M-15
           END-IF
           IF  W-SEN = 0
               IF  HI-ENG NOT = ZERO
                   GO TO M-26
               END-IF
           END-IF
           IF  HI-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-26
           END-IF
           IF  HI-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-26
           END-IF
           IF  HI-BC1 < W-SBC1 OR > W-EBC1
               GO TO M-26
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BC1" D-BC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BMC" D-BMC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BC3" D-BC3 "p" RETURNING RESU.
           IF HI-SNG NOT = ZERO
               CALL "SD_Output" USING "D-SNG" D-SNG "p" RETURNING RESU
           END-IF.
       M-35.
      *           READ  HI-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               MOVE  ZERO  TO  HI-KEY
               GO  TO  M-15
           END-IF
           IF  W-SEN = 0
               IF  HI-ENG NOT = ZERO
                   GO TO M-35
               END-IF
           END-IF
           IF  HI-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-35
           END-IF
           IF  HI-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-35
           END-IF
           IF  HI-BC1 < W-SBC1 OR > W-EBC1
               GO TO M-35
           END-IF
           GO  TO  M-30.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
