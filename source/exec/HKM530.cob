       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKM530.
      ******************************************************************
      *    PROGRAM         :  担当者別得意先　名簿
      *    PRINTER TYPE    :  JIPS
      *    SCREEN          :  ******
      *        変更　　　  :  62/03/26
      *    COMPILE TYPE    :  COBOL
      *    JS-SIGN         :  0:担当順 , 1:都道府県順
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
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
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  W-YS           PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(009) VALUE
                "得　意　先　名　簿".
           02  F              PIC  X(002) VALUE SPACE.
           02  W-YE           PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99B.
           02  F              PIC  N(002) VALUE "現在".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  H-PAGE         PIC  Z(002).
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(126) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  W-T            PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-JS           PIC  N(020).
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
       01  HEAD4.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "郵便番号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(008) VALUE "住　　　　　所　".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "電話番号".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "ＦＡＸ番号　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(002) VALUE X"1AC2".
       01  W-P.
           02  F              PIC  X(001).
           02  P-YS           PIC  X(002).
           02  P-T1           PIC  X(002).
           02  F              PIC  X(002).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-T2           PIC  X(002).
           02  F              PIC  X(001).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(001).
           02  P-T3           PIC  X(002).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-T4           PIC  X(002).
           02  F              PIC  X(001).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-T5           PIC  X(002).
           02  F              PIC  X(001).
           02  P-JS           PIC  N(020).
           02  F              PIC  X(001).
           02  P-T6           PIC  X(002).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  P-T7           PIC  X(002).
           02  F              PIC  X(001).
           02  P-FAX          PIC  X(014).
           02  F              PIC  X(001).
           02  P-T8           PIC  X(002).
           02  P-YE           PIC  X(002).
       01  W-D.
           02  W-UC           PIC  9(001).
           02  W-TNC.
             03  W-STNC       PIC  9(002).
             03  W-ETNC       PIC  9(002).
           02  W-TNCD         PIC  9(002).
           02  W-FKCD         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-PRC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LSPF.
      *FD  T-M
       01  T-M_HKM530.
           02  T-M_PNAME1     PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  T-M_LNAME      PIC  X(010) VALUE "T-M_HKM530".
           02  F              PIC  X(001).
           02  T-M_KEY1       PIC  X(100) VALUE SPACE.
           02  T-M_SORT       PIC  X(100) VALUE SPACE.
           02  T-M_IDLST      PIC  X(100) VALUE SPACE.
           02  T-M_RES        USAGE  POINTER.
       01  T-R.
           02  T-NTCD         PIC  9(004).
           02  T-KEY.
             03  T-TCD        PIC  9(004).
           02  T-NAME         PIC  N(026).
           02  T-JSU          PIC  N(020).
           02  T-JSS          PIC  N(020).
           02  T-UNO          PIC  X(008).
           02  T-TEL          PIC  X(014).
           02  T-FAX          PIC  X(014).
           02  T-FKC          PIC  9(002).
           02  F              PIC  X(003).
           02  T-TNC          PIC  9(002).
           02  F              PIC  X(321).
           02  T-SNG          PIC  9(004).
           02  T-ENG          PIC  9(004).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "***                           ***".
           02  FILLER  PIC  X(033) VALUE
                "***     得意先 名簿　作成     ***".
           02  FILLER  PIC  X(033) VALUE
                "***                           ***".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(035) VALUE
                "<  停止分 PRINT  ｽﾙ=5  ｼﾅｲ=0   0  >".
           02  FILLER  PIC  X(047) VALUE
                "<  担当者ｺｰﾄﾞ    ﾖﾘ    迄打出し  >    終了=ｆ･9".
           02  FILLER  PIC  X(028) VALUE
                "[  確認  OK=1 NO=9   ﾘﾀｰﾝ  ]".
       01  C-ACP.
           02  A-UC    PIC  9(001).
           02  FILLER.
             03  A-STNC  PIC  9(002).
             03  A-ETNC  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "341" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "X" "3" "10" "33" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "4" "10" "33" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "5" "10" "33" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "X" "6" "10" "33" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "X" "7" "10" "33" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "8" "10" "33" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "X" "9" "10" "33" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "12" "9" "35" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "14" "9" "47" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "20" "13" "28" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-UC" "9" "12" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-UC" BY REFERENCE W-UC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "4" "A-UC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-STNC" "9" "14" "23" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETNC" "9" "14" "29" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "33" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "79" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "79" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-00.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO T-M_PNAME1.
           MOVE 0 TO W-PRC.
       M-05.
           CALL "SD_Accept" USING BY REFERENCE A-UC "A-UC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-05
           END-IF
           IF  W-UC NOT = 0 AND 5
               GO TO M-05
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-05
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-STNC > W-ETNC
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-05
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 " " BY REFERENCE T-M_IDLST "0".
       M-25.
      *           READ T-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO TO M-10
           END-IF
           IF  T-KEY = 9999
               GO TO M-25
           END-IF
           IF  W-UC = 0
               IF  T-ENG NOT = ZERO
                   GO TO M-25
               END-IF
           END-IF
           IF  T-TNC < W-STNC
               GO TO M-25
           END-IF
           IF  JS-SIGN = 0
               IF  T-TNC > W-ETNC
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   CALL "DB_F_Close" USING
                    BY REFERENCE T-M_IDLST T-M_PNAME1
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  T-TNC > W-ETNC
                   GO TO M-25
               END-IF
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM S-10 THRU S-15.
           IF  JS-SIGN = 1
               MOVE T-FKC TO W-FKCD
           ELSE
               MOVE T-TNC TO W-TNCD
           END-IF
           MOVE 0 TO CHK.
       M-30.
           IF  JS-SIGN = 0
               IF  T-TNC = W-TNCD
                   GO TO M-35
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  T-FKC = W-FKCD
                   GO TO M-35
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
           IF  JS-SIGN = 1
               GO TO M-34
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
       M-34.
           MOVE 0 TO CHK.
           IF  JS-SIGN = 1
               MOVE T-FKC TO W-FKCD
           ELSE
               MOVE T-TNC TO W-TNCD
           END-IF.
       M-35.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE 0 TO CHK
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           IF  T-JSS = SPACE
               MOVE SPACE TO H-JS
           ELSE
               MOVE T-JSU TO H-JS
           END-IF
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE W-YS TO P-YS.
           MOVE W-YE TO P-YE.
           MOVE W-T TO P-T1 P-T2 P-T3 P-T4 P-T5 P-T6 P-T7 P-T8.
           MOVE ALL "　" TO P-NAME P-JS.
           IF  JS-SIGN = 0
               IF  CHK = 0
                   MOVE 1 TO CHK
                   MOVE W-TNCD TO P-TNC
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE T-TNC TO P-TNC
           END-IF
           MOVE T-KEY TO P-KEY.
           MOVE T-NAME TO P-NAME.
           IF  T-JSS = SPACE
               MOVE T-JSU TO P-JS
           ELSE
               MOVE T-JSS TO P-JS
           END-IF
           MOVE T-UNO TO P-UNO.
           MOVE T-TEL TO P-TEL.
           MOVE T-FAX TO P-FAX.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-40.
      *           READ T-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  T-KEY = 9999
               GO TO M-40
           END-IF
           IF  W-UC = 0
               IF  T-ENG NOT = ZERO
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  T-TNC < W-STNC OR > W-ETNC
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  T-TNC > W-ETNC
                   GO TO M-45
               END-IF
           END-IF
           GO TO M-30.
       M-45.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-50.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               CALL "PR_Close" RETURNING RESP
               GO TO M-10
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-50.
       M-95.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO H-JS.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO H-JS.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE W-YS TO P-YS.
           MOVE W-YE TO P-YE.
           MOVE W-T TO P-T1 P-T2 P-T3 P-T4 P-T5 P-T6 P-T7 P-T8.
           MOVE ALL "　" TO P-NAME P-JS.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
