       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD160.
      **************************************************
      *****     購買　仕入・支払ファイル　作成     *****
      **************************************************
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
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　購買　消費税・支払変換　リスト　　＊＊＊".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(065) VALUE SPACE.
           02  F              PIC  X(007) VALUE "I----  ".
           02  F              PIC  N(003) VALUE "仕　入".
           02  F              PIC  X(020) VALUE "  ---I I----------  ".
           02  F              PIC  N(003) VALUE "支　払".
           02  F              PIC  X(012) VALUE "  ---------I".
       01  HEAD3.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(008) VALUE "   ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "仕　　入　　先　　名".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(008) VALUE "伝票№　行　伝区".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "材料".
           02  F              PIC  X(010) VALUE "ｺｰﾄﾞ      ".
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "支払区分".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "支払金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　支払消費税".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(002).
           02  P-DC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-JCD          PIC  9(006).
           02  P-SHZ          PIC ----,---,--9.
           02  F              PIC  X(004).
           02  P-SC           PIC  9(001).
           02  P-HKIN         PIC ---,---,---,--9.
           02  P-HSHZ         PIC ----,---,--9.
       01  W-DATA.
           02  W-TD.
             03  WT-SHZ       PIC S9(008).
             03  WT-HKIN      PIC S9(010).
             03  WT-HSHZ      PIC S9(008).
           02  W-AD.
             03  WA-SHZ       PIC S9(009).
             03  WA-HKIN      PIC S9(011).
             03  WA-HSHZ      PIC S9(009).
           02  W-DATE         PIC  9(008).
           02  W-NGPD  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SCD          PIC  9(004).
           02  W-DC           PIC  9(002).
           02  W-KEY.
             03  W-DNO        PIC  9(006).
             03  W-GNO        PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-DMM          PIC  9(001).
           02  W-FILE         PIC  X(013).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIKBNO.
           COPY LSPF.
      *FD  TJSS-F
       01  TJSS-F_TSD160.
           02  TJSS-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TJSS-F_LNAME   PIC  X(013) VALUE "TJSS-F_TSD160".
           02  F              PIC  X(001).
           02  TJSS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  TJSS-F_SORT    PIC  X(100) VALUE SPACE.
           02  TJSS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  TJSS-F_RES     USAGE  POINTER.
       01  TJSS-RD.
           02  TJSS-R.
             03  TJS-DC       PIC  9(002).
             03  TJS-DATE     PIC  9(008).
             03  TJS-SCD      PIC  9(004).
             03  TJS-JCD      PIC  9(006).
             03  F            PIC  X(017).
             03  TJS-KIN      PIC S9(008).
             03  TJS-SHZ      PIC S9(007).
             03  F            PIC  X(019).
             03  TJS-YC       PIC  9(001).
             03  F            PIC  X(002).
             03  TJS-SC       PIC  9(001).
             03  F            PIC  X(027).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *FD  JSS-F
       01  JSS-F_TSD160.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_TSD160".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  F              PIC  X(017).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  F              PIC  X(019).
           02  JS-YC          PIC  9(001).
           02  F              PIC  X(002).
           02  JS-SC          PIC  9(001).
           02  F              PIC  X(013).
           02  JS-DP.
             03  JS-DPP       PIC  9(004).
             03  JS-DPG       PIC  9(002).
           02  JS-KEY.
             03  JS-DNO       PIC  9(006).
             03  JS-GNO       PIC  9(001).
           02  JS-PCNT        PIC  9(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　購買　仕入・支払ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(018) VALUE
                  "***  KBNOM ﾅｼ  ***".
             03  E-ME2   PIC  X(024) VALUE
                  "***  JSSF WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  KBNOM REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(007).
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張し、ＦＮＣ＋再開".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "227" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "227" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "24" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "55" "7" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE JS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
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
           COPY LIBCPR.
           MOVE DATE-04R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TJSS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TJSS-F_PNAME1 " " BY REFERENCE TJSS-F_IDLST "0".
       M-15.
      *           READ TJSS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TJSS-F_PNAME1 BY REFERENCE TJSS-RD " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TJSS-F_IDLST TJSS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 " " BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "02" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TJSS-F_IDLST TJSS-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-20.
           MOVE TJS-DATE TO W-DATE.
           MOVE ZERO TO W-AD.
       M-25.
           MOVE TJS-SCD TO W-SCD.
           MOVE TJS-DC TO W-DC.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊＊　マスター　なし　＊＊　" TO S-NAME
           END-IF
           COMPUTE W-DNO = BNO-DNO2 + 1.
           MOVE ZERO TO W-TD W-GNO CHK.
       M-40.
           ADD 1 TO W-GNO.
           IF  W-GNO = 9
               COMPUTE W-DNO = BNO-DNO2 + 1
               MOVE 1 TO W-GNO
           END-IF.
       M-45.
           MOVE ZERO TO JSS-R.
           MOVE TJSS-R TO JSS-R.
           MOVE 999999 TO JS-DP.
           MOVE W-KEY TO JS-KEY.
           MOVE 1 TO JS-PCNT.
      *           WRITE JSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           GO TO M-55.
       M-50.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           MOVE "JSSF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           GO TO M-45.
       M-55.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-NGPS TO P-DATE
               MOVE W-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
           END-IF
           MOVE W-DNO TO P-DNO.
           MOVE "-" TO P-V.
           MOVE W-GNO TO P-GNO.
           MOVE W-DC TO P-DC.
           IF  W-DC NOT = 30
               MOVE TJS-JCD TO P-JCD
               MOVE TJS-SHZ TO P-SHZ
               ADD TJS-SHZ TO WT-SHZ
           ELSE
               MOVE TJS-SC TO P-SC
               MOVE TJS-KIN TO P-HKIN
               MOVE TJS-SHZ TO P-HSHZ
               ADD TJS-KIN TO WT-HKIN
               ADD TJS-SHZ TO WT-HSHZ
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-NGPS TO P-DATE
               MOVE W-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-GNO NOT = 1
               GO TO M-60
           END-IF
           MOVE W-DNO TO BNO-DNO2.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-60.
      *           READ TJSS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TJSS-F_PNAME1 BY REFERENCE TJSS-RD " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  TJS-DATE NOT = W-DATE
               GO TO M-70
           END-IF
           IF  TJS-SCD NOT = W-SCD
               GO TO M-65
           END-IF
           IF  TJS-DC NOT = W-DC
               GO TO M-65
           END-IF
           GO TO M-40.
       M-65.
           PERFORM S-20 THRU S-30.
           GO TO M-25.
       M-70.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-20.
       M-80.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TJSS-F_IDLST TJSS-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  W-GNO = 1
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA.
           MOVE "　　　　　　　　　　［　ＴＯＴＡＬ　］　" TO P-SNA.
           IF  WT-SHZ NOT = ZERO
               MOVE WT-SHZ TO P-SHZ
           END-IF
           IF  WT-HKIN NOT = ZERO
               MOVE WT-HKIN TO P-HKIN
           END-IF
           IF  WT-HSHZ NOT = ZERO
               MOVE WT-HSHZ TO P-HSHZ
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           ADD WT-SHZ TO WA-SHZ.
           ADD WT-HKIN TO WA-HKIN.
           ADD WT-HSHZ TO WA-HSHZ.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA.
           MOVE "　　　　｛　　日　付　合　計　　｝　　　" TO P-SNA.
           IF  WA-SHZ NOT = ZERO
               MOVE WA-SHZ TO P-SHZ
           END-IF
           IF  WA-HKIN NOT = ZERO
               MOVE WA-HKIN TO P-HKIN
           END-IF
           IF  WA-HSHZ NOT = ZERO
               MOVE WA-HSHZ TO P-HSHZ
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-40.
           EXIT.
