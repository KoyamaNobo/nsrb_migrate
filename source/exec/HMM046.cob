       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM045.
      *********************************************************
      *    PROGRAM         :  履物　品名コード表              *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/14                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD00.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(051) VALUE SPACE.
       01  HEAD01.
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(051) VALUE SPACE.
       01  HEAD03.
           02  F              PIC  X(008) VALUE X"1A26222166222176".
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "履物品名コード一覧表".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  HEAD04.
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(051) VALUE SPACE.
       01  HEAD08.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(095) VALUE SPACE.
           02  F              PIC  N(009) VALUE "日　進　ゴ　ム　㈱".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  HEAD09.
           02  F              PIC  X(103) VALUE SPACE.
           02  F              PIC  N(003) VALUE "作成日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(001) VALUE "’".
           02  H-NGP          PIC  N(008).
           02  F              PIC  X(004) VALUE SPACE.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  W-400K         PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(009) VALUE "履物　品名コード表".
           02  F              PIC  X(017) VALUE SPACE.
           02  W-200K         PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  N(005) VALUE "ＤＡＴＥ　".
           02  H-DATE         PIC  N(008).
           02  F              PIC  N(005) VALUE "　　　Ｐ　".
           02  H-PAGE         PIC  N(003).
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(132) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
       01  HEAD3.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(001) VALUE SPACE.
           02  W-YS           PIC  X(002) VALUE X"1AC0".
           02  W-T            PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "コード".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "開始".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-HSD          PIC  N(002).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  W-YE           PIC  X(002) VALUE X"1AC1".
       01  W-P.
           02  P-20K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-YS           PIC  X(002).
           02  P-T1           PIC  X(002).
           02  F              PIC  X(001).
           02  P-HCD          PIC  N(006).
           02  F              PIC  X(001).
           02  P-T2           PIC  X(002).
           02  F              PIC  X(001).
           02  P-400K         PIC  X(008).
           02  P-NAME         PIC  N(024).
           02  P-200K         PIC  X(008).
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-T3           PIC  X(002).
           02  P-SNG          PIC  N(006).
           02  F              PIC  X(001).
           02  P-T4           PIC  X(002).
           02  P-ENG          PIC  N(006).
           02  F              PIC  X(001).
           02  P-T5           PIC  X(002).
           02  P-YE           PIC  X(002).
       01  W-PD.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(098) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(002) VALUE X"1AC1".
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-MC           PIC  9(001).
           02  W-PHC          PIC  9(001).
           02  W-PHCD         PIC  9(001).
           02  W-PNC          PIC  9(001).
           02  W-BCS          PIC  9(004).
           02  W-BCE          PIC  9(004) VALUE 9999.
           02  W-DMM          PIC  9(001).
           02  W-BC           PIC  9(001).
           02  W-KEY          PIC  9(004).
           02  W-KEY1         PIC  9(004).
           02  W-KEY2         PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-HCDD         PIC  9(006).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-C            PIC  9(001).
           02  W-LC           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NGP          PIC 99/99/99.
           02  W-PND          PIC ZZ9.
           02  W-SNG          PIC B99/99.
           02  W-SNGD  REDEFINES W-SNG  PIC  X(006).
           02  W-ENG          PIC B99/99.
           02  W-ENGD  REDEFINES W-ENG  PIC  X(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　履物　品名コード表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(029) VALUE
                "大見出し PRINT  ｽﾙ=5 ｼﾅｲ=1   ".
           02  FILLER  PIC  X(027) VALUE
                "廃止分 PRINT  ｽﾙ=5 ｼﾅｲ=1   ".
           02  FILLER  PIC  X(038) VALUE
                "品名ｺｰﾄﾞ  0000 ～ 9999 まで　終了=ｆ･9".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-MC    PIC  9(001).
           02  A-PHC   PIC  9(001).
           02  FILLER.
             03  A-BCS   PIC  9(004).
             03  A-BCE   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "382" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "12" "14" "29" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "14" "15" "27" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "18" "17" "38" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "22" "18" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-MC" "9" "12" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-MC" BY REFERENCE W-MC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-PHC" "9" "14" "41" "1" "A-MC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-PHC" BY REFERENCE W-PHC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "18" "0" "8" "A-PHC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-BCS" "9" "18" "27" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-BCS" BY REFERENCE W-BCS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-BCE" "9" "18" "35" "4" "A-BCS" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-BCE" BY REFERENCE W-BCE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "35" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-POC.
           MOVE ZERO TO CHK.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-MC "A-MC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-MC NOT = 1 AND 5
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-PHC "A-PHC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               IF  W-POC = ZERO
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-PHC NOT = 1 AND 5
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-BCS "A-BCS" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-BCE "A-BCE" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-BCS > W-BCE
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE ZERO TO HI-KEY.
           MOVE W-BCS TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE HI-HCD1 TO W-KEY2
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-20
           END-IF.
       M-35.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE HI-HCD1 TO W-KEY2
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-20
           END-IF
           IF  HI-OL NOT = SPACE
               GO TO M-35
           END-IF
           IF  W-PHC = 1
               IF  HI-ENG NOT = ZERO
                   GO TO M-35
               END-IF
           END-IF
           IF  HI-HCD1 < W-BCS
               GO TO M-35
           END-IF
           IF  HI-HCD1 > W-BCE
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               MOVE HI-HCD1 TO W-KEY2
               GO TO M-20
           END-IF
           IF  W-POC NOT = ZERO
               GO TO M-40
           END-IF
           MOVE 5 TO W-POC.
           IF  W-PHC = 1
               MOVE SPACE TO H-HSD
           ELSE
               MOVE "廃止" TO H-HSD
           END-IF
           MOVE DATE-02R TO W-NGP.
           MOVE W-NGP TO H-DATE H-NGP.
           CALL "PR_Open" RETURNING RESP.
           IF  W-MC = 5
               MOVE SPACE TO SP-R
               MOVE HEAD00 TO SP-R
               CALL "PR_LineFeed" USING "13" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD01 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD03 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD04 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD08 TO SP-R
               CALL "PR_LineFeed" USING "35" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD09 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "10" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
       M-40.
           MOVE W-PHC TO W-PHCD.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
               GO TO M-45
           END-IF
           MOVE HI-HCD1 TO W-KEY.
           COMPUTE W-KEY1 = W-KEY + 1.
           IF  W-KEY2 = W-KEY
               GO TO M-50
           END-IF
           IF  W-KEY2 NOT = W-KEY1
               GO TO M-63
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-45.
           MOVE HI-HCD1 TO W-KEY.
       M-50.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HCD P-NAME P-SNG P-ENG.
           MOVE W-YS TO P-YS.
           MOVE W-YE TO P-YE.
           MOVE W-T TO P-T1 P-T2 P-T3 P-T4 P-T5.
           MOVE W-20K TO P-20K.
           MOVE W-15K TO P-15K.
           MOVE W-400K TO P-400K.
           MOVE W-200K TO P-200K.
           MOVE HI-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE SPACE TO W-SNGD W-ENGD.
           IF  HI-SNG NOT = ZERO
               MOVE HI-SNG TO W-SNG
           END-IF
           IF  W-PHC = 5
               IF  HI-ENG NOT = ZERO
                   MOVE HI-ENG TO W-ENG
               END-IF
           END-IF
           MOVE W-SNG TO P-SNG.
           MOVE W-ENG TO P-ENG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HI-HCD TO W-HCD.
           MOVE HI-HCD1 TO W-KEY.
       M-55.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  HI-OL NOT = SPACE
               GO TO M-55
           END-IF
           IF  W-PHC = 1
               IF  HI-ENG NOT = ZERO
                   GO TO M-55
               END-IF
           END-IF
           IF  HI-HCD1 < W-BCS
               GO TO M-55
           END-IF
           IF  HI-HCD1 > W-BCE
               GO TO M-70
           END-IF.
       M-60.
           COMPUTE W-KEY1 = W-KEY + 1.
           IF  HI-HCD1 = W-KEY
               GO TO M-50
           END-IF
           IF  HI-HCD1 NOT = W-KEY1
                GO TO M-63
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-45.
       M-63.
           MOVE 5 TO W-LC.
       M-65.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           SUBTRACT 1 FROM W-LC.
           IF  W-LC = 0
               GO TO M-45
           END-IF
           GO TO M-65.
       M-70.
           MOVE HI-HCD1 TO W-KEY2.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           GO TO M-20.
       M-90.
           IF  CHK NOT = 5
               GO TO M-95
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               CALL "PR_Close" RETURNING RESP
               GO TO M-95
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-90.
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
           MOVE W-PAGE TO W-PND.
           MOVE W-PND TO H-PAGE.
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
