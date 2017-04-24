       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         TSE110.
       AUTHOR.             KAMASAKA     1995-10-25.
      **********************************************
      ******    割引ファイル　メンテナンス    ******
      ******   ( TYBF : TM : BANKM : UKETM )  ******
      **********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  W-R.
           02  W-BCD               PIC  9(004).
           02  W-WKD.
               03  W-YWKD          PIC  9(002).
               03  W-MWKD          PIC  9(002).
               03  W-DWKD          PIC  9(002).
           02  W-MKD.
               03  W-YMKD          PIC  9(002).
               03  W-MMKD          PIC  9(002).
               03  W-DMKD          PIC  9(002).
           02  W-TCD               PIC  9(004).
           02  W-TSC               PIC  9(002).
           02  W-TNO               PIC  X(004).
           02  W-KIN               PIC  9(010).
           02  W-FWC               PIC  9(001).
           02  FILLER              PIC  X(006).
           02  W-SRN.
               03  W-SWKD          PIC  9(004).
               03  W-SMKD          PIC  9(004).
       01  W-DATA.
           02  W-ACT               PIC  9(001).
           02  W-YBC               PIC  9(001).
           02  W-TSCN              PIC  N(003).
           02  W-DMM               PIC  9(001).
           02  W-NEN               PIC  9(004).
           02  W-NENL  REDEFINES W-NEN.
             03  W-NEN1            PIC  9(002).
             03  W-NEN2            PIC  9(002).
       01  ERR-STAT                PIC  X(002).
           COPY LSTAT.
      *
      *FD  TYB-F
       01  TYB-F_TSE110.
           02  TYB-F_PNAME1        PIC  X(004) VALUE "TYBF".
           02  F                   PIC  X(001).
           02  TYB-F_LNAME         PIC  X(012) VALUE "TYB-F_TSE110".
           02  F                   PIC  X(001).
           02  TYB-F_KEY1          PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT          PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST         PIC  X(100) VALUE SPACE.
           02  TYB-F_RES           USAGE  POINTER.
       01  TYB-R.
           02  TY-BCD              PIC  9(004).
           02  TY-WKD              PIC  9(006).
           02  TY-MKD              PIC  9(006).
           02  TY-TCD              PIC  9(004).
           02  TY-TSC              PIC  9(002).
           02  TY-TNO              PIC  X(004).
           02  TY-KIN              PIC  9(010).
           02  TY-FWC              PIC  9(001).
           02  FILLER              PIC  X(006).
           02  TY-SRN.
               03  TY-SWKD         PIC  9(004).
               03  TY-SMKD         PIC  9(004).
       77  F                       PIC  X(001).
      *
           COPY  LIBFDD.
           COPY  LITM.
           COPY  LIUKET.
           COPY  LIBANK.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(023)    VALUE
                   "＊＊＊　　割引ファイル　メンテナンス　　＊＊＊".
           02  FILLER  PIC X(034)    VALUE
                   "登録=1 修正=2 削除=3 終了=9   ﾘﾀｰﾝ".
           02  FILLER  PIC X(030)    VALUE
                   "割引区分       (割引=1,決済=2)".
           02  FILLER  PIC X(008)    VALUE
                   "手 形 №".
           02  FILLER  PIC N(004)    VALUE
                   "手形区分".
           02  FILLER  PIC X(008)    VALUE
                   "取 引 先".
           02  FILLER  PIC N(004)    VALUE
                   "金　　額".
           02  FILLER  PIC N(004)    VALUE
                   "割引銀行".
           02  FILLER  PIC N(005)    VALUE
                   "割引決済日".
           02  FILLER  PIC X(008)    VALUE
                   "満 期 日".
           02  FILLER  PIC X(039)    VALUE
                   "不渡区分       (正規=0,買戻し=1,不渡=9)".
           02  FILLER  PIC X(021)    VALUE
                   "確認 OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ACT     PIC 9(001).
           02  A-YBC     PIC 9(001).
           02  A-TNO     PIC 9(004).
           02  A-BCD     PIC 9(004).
           02  A-WKD     PIC 9(006).
           02  A-MKD     PIC 9(006).
           02  A-FWC     PIC 9(001).
           02  A-DMM     PIC 9(001).
       01  C-DSP.
           02  D-TSC.
               03  FILLER  PIC 9(002).
           02  D-TSCN.
               03  FILLER  PIC N(003).
           02  D-TCD.
               03  FILLER  PIC 9(004).
           02  D-NAME.
               03  FILLER  PIC N(026).
           02  D-KIN.
               03  FILLER  PIC Z,ZZZ,ZZZ,ZZ9 .
           02  D-BCD.
               03  FILLER  PIC 9(004).
           02  D-BNA.
               03  FILLER  PIC N(008).
           02  D-SNA.
               03  FILLER  PIC N(008).
           02  D-WKD.
               03  FILLER  PIC 9(006).
           02  D-MKD.
               03  FILLER  PIC 9(006).
           02  D-FWC.
               03  FILLER  PIC 9(001).
       01  C-ERR.
           02  FILLER.
               03  E-STAT  PIC X(002).
               03  E-ME1   PIC X(017)  VALUE
                     "***  TYBF ﾅｼ  ***".
               03  E-ME2   PIC X(018)  VALUE
                     "***  UKETM ﾅｼ  ***".
               03  E-ME3   PIC X(015)  VALUE
                     "***  TM ﾅｼ  ***".
               03  E-ME4   PIC X(018)  VALUE
                     "***  BANKM ﾅｼ  ***".
               03  E-ME5   PIC X(017)  VALUE
                     "***  ﾄｳﾛｸｽﾞﾐ  ***".
               03  E-ME6   PIC X(015)  VALUE
                     "***  ｷｬﾝｾﾙ  ***".
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
               03  E-CL.
                   04  FILLER  PIC X(040)    VALUE
                         "                                        ".
                   04  FILLER  PIC X(040)    VALUE
                         "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "228" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "8" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "14" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "8" "30" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "7" "8" "8" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "8" "8" "8" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "9" "8" "8" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "10" "8" "8" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "N" "12" "8" "8" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "N" "13" "8" "10" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "14" "8" "8" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "15" "8" "39" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-MID" "X" "20" "24" "21" "11C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "43" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YBC" "9" "5" "19" "1" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YBC" BY REFERENCE W-YBC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNO" "9" "7" "19" "4" "A-YBC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNO" BY REFERENCE W-TNO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BCD" "9" "12" "19" "4" "A-TNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BCD" BY REFERENCE W-BCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-WKD" "9" "13" "19" "6" "A-BCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-WKD" BY REFERENCE W-WKD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MKD" "9" "14" "19" "6" "A-WKD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MKD" BY REFERENCE W-MKD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FWC" "9" "15" "19" "1" "A-MKD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FWC" BY REFERENCE W-FWC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" "A-FWC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "126" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TSC" " " "0" "0" "2" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TSC" "9" "8" "19" "2" " " "D-TSC" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TSC" BY REFERENCE W-TSC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TSCN" " " "0" "0" "6" "D-TSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TSCN" "N" "8" "24" "6" " " "D-TSCN" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TSCN" BY REFERENCE W-TSCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" " " "0" "0" "4" "D-TSCN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TCD" "9" "9" "19" "4" " " "D-TCD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" " " "0" "0" "52" "D-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NAME" "N" "9" "24" "52" " " "D-NAME" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "0" "0" "13" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "Z,ZZZ,ZZZ,ZZ9" "10" "19" "13" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCD" " " "0" "0" "4" "D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BCD" "9" "12" "19" "4" " " "D-BCD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BCD" BY REFERENCE W-BCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BNA" " " "0" "0" "16" "D-BCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BNA" "N" "12" "24" "16" " " "D-BNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BNA" BY REFERENCE B-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" " " "0" "0" "16" "D-BNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SNA" "N" "12" "41" "16" " " "D-SNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SNA" BY REFERENCE B-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WKD" " " "0" "0" "6" "D-SNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-WKD" "9" "13" "19" "6" " " "D-WKD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-WKD" BY REFERENCE W-WKD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MKD" " " "0" "0" "6" "D-WKD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MKD" "9" "14" "19" "6" " " "D-MKD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MKD" BY REFERENCE W-MKD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FWC" " " "0" "0" "1" "D-MKD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-FWC" "9" "15" "19" "1" " " "D-FWC" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-FWC" BY REFERENCE W-FWC "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "192" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "192" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "15" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "15" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
       M-10.
           MOVE  ZERO  TO  W-DATA.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-ACT = 9
               GO  TO  M-99
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-20.
           MOVE  ZERO  TO  W-R.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-YBC "A-YBC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-20
           END-IF
           IF  W-YBC NOT = 1 AND 2
               GO  TO  M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TNO "A-TNO" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-25
           END-IF
           MOVE  W-TNO  TO  UT-KEY.
      *           READ  UKET-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-25
           END-IF
           MOVE  UT-TSC  TO  W-TSC.
           IF  UT-TSC = 10
               MOVE    "小切手"  TO  W-TSCN
           END-IF
           IF  UT-TSC = 11
               MOVE    "約　手"  TO  W-TSCN
           END-IF
           IF  UT-TSC = 12
               MOVE    "為　手"  TO  W-TSCN
           END-IF
           MOVE  UT-TCD  TO  W-TCD.
           MOVE  UT-KIN  TO  W-KIN.
           MOVE  UT-TCD  TO  T-KEY.
      *           READ  T-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-TSC" D-TSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TSCN" D-TSCN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
       M-30.
      *           READ  TYB-F  AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TYB-F_PNAME1 BY REFERENCE TYB-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  M-35
           END-IF
           IF  TY-TNO NOT = W-TNO
               GO  TO  M-30
           END-IF
           IF  W-YBC = 1
               IF  TY-MKD = 999999
                   GO  TO  M-30
               END-IF
           END-IF
           IF  W-YBC = 2
               IF  TY-MKD NOT = 999999
                   GO  TO  M-30
               END-IF
           END-IF
           MOVE  TY-BCD   TO  W-BCD.
           MOVE  TY-WKD   TO  W-WKD.
           MOVE  TY-MKD   TO  W-MKD.
           MOVE  TY-FWC  TO  W-FWC.
           CALL "SD_Output" USING "D-BCD" D-BCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-WKD" D-WKD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MKD" D-MKD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-FWC" D-FWC "p" RETURNING RESU.
           MOVE  TY-BCD  TO  B-KEY.
      *           READ  BANK-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-25
           END-IF
           IF  W-ACT = 2
               GO  TO  M-40
           END-IF
           IF  W-ACT = 3
               GO  TO  M-60
           END-IF.
       M-35.
           IF  W-ACT NOT = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-25
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-BCD "A-BCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               GO  TO  M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-40
           END-IF
           MOVE  W-BCD  TO  B-KEY.
      *           READ  BANK-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-40
           END-IF
           CALL "SD_Output" USING "D-BCD" D-BCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  B-YBC NOT = 1
               GO  TO  M-40
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-WKD "A-WKD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-45
           END-IF
           IF  UT-UTD > W-WKD
               GO  TO  M-45
           END-IF
           CALL "SD_Output" USING "D-WKD" D-WKD "p" RETURNING RESU.
           IF  W-MWKD < 1  OR  > 12
               GO  TO  M-45
           END-IF
           IF  W-DWKD < 1  OR  > 31
               GO  TO  M-45
           END-IF
           MOVE ZERO TO W-NEN.
           MOVE W-YWKD TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO W-SWKD.
       M-50.
           IF  W-YBC = 2
               MOVE  "999999"  TO  W-MKD
               CALL "SD_Output" USING "D-MKD" D-MKD "p" RETURNING RESU
               GO  TO  M-55
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-MKD "A-MKD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-50
           END-IF
           IF  UT-UTD > W-MKD
               GO  TO  M-50
           END-IF
           CALL "SD_Output" USING "D-MKD" D-MKD "p" RETURNING RESU.
           IF  W-YBC = 1
               IF  W-WKD > W-MKD
                   GO  TO  M-45
               END-IF
           END-IF
           IF  W-MMKD < 1  OR  > 12
               GO  TO  M-50
           END-IF
           IF  W-DMKD < 1  OR  > 31
               GO  TO  M-50
           END-IF
           MOVE ZERO TO W-NEN.
           MOVE W-YMKD TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO W-SMKD.
       M-55.
           IF  W-YBC = 1
               MOVE  ZERO  TO  W-FWC
               CALL "SD_Output" USING "D-FWC" D-FWC "p" RETURNING RESU
               GO  TO  M-60
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-FWC "A-FWC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YBC = 2
                   GO  TO  M-45
               ELSE
                   GO  TO  M-50
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-55
           END-IF
           IF  W-FWC NOT = ZERO AND 1 AND 9
               GO  TO  M-55
           END-IF
           CALL "SD_Output" USING "D-FWC" D-FWC "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   CALL "DB_F_Close" USING
                    BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
                   GO  TO  M-25
               ELSE
                   IF  W-YBC = 2
                       GO  TO  M-55
                   ELSE
                       GO  TO  M-50
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-60
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-15
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-60
           END-IF
           IF  W-ACT = 2
               GO  TO  M-70
           END-IF
           IF  W-ACT = 3
               GO  TO  M-75
           END-IF.
       M-65.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           CALL "DB_F_Open" USING
            "EXTEND" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           MOVE   ZERO  TO  TYB-R.
           MOVE   W-R   TO  TYB-R.
      *           WRITE  TYB-R.
      *//////////////
           CALL "DB_Insert" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           GO  TO  M-15.
       M-70.
           MOVE     ZERO  TO  TYB-R.
           MOVE     W-R   TO  TYB-R.
      *           REWRITE  TYB-R.
      *///////////////
           CALL "DB_Update" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           GO  TO  M-15.
       M-75.
           MOVE     X"FF"  TO  TYB-R.
      *           REWRITE  TYB-R.
      *///////////////
           CALL "DB_Update" USING
            TYB-F_PNAME1 TYB-F_LNAME TYB-R RETURNING RET.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           GO  TO  M-15.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_Close".
           STOP  RUN.
