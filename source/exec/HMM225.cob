       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM225.
      *******************************************************************
      *    PROGRAM         :  直送先　名簿                              *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *    COMPILE TYPE    :  COBOL                                     *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  W-YS           PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(009) VALUE
                "直　送　先　名　簿".
           02  F              PIC  X(002) VALUE SPACE.
           02  W-YE           PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(014) VALUE SPACE.
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
           02  F              PIC  X(109) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC1".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  W-T            PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(010) VALUE SPACE.
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
       01  HEAD4.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC0".
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(002) VALUE X"1AC2".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "直　　送　　先　　名".
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
           02  F              PIC  X(002) VALUE X"1AC1".
           02  F              PIC  X(002) VALUE X"1AC2".
       01  W-P.
           02  F              PIC  X(001).
           02  P-YS           PIC  X(002).
           02  P-T1           PIC  X(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-T2           PIC  X(002).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-T3           PIC  X(002).
           02  F              PIC  X(001).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-T4           PIC  X(002).
           02  F              PIC  X(001).
           02  P-JS           PIC  N(020).
           02  F              PIC  X(001).
           02  P-T5           PIC  X(002).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  P-T6           PIC  X(002).
           02  P-YE           PIC  X(002).
       01  W-D.
           02  W-UC           PIC  9(001).
           02  W-TCDD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004) VALUE 9999.
           02  W-PNO          PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  CHK            PIC  9(001).
           02  W-PRC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LSPF.
      *FD  TC-F
       01  TC-F_HMM225.
           02  TC-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TC-F_LNAME     PIC  X(011) VALUE "TC-F_HMM225".
           02  F              PIC  X(001).
           02  TC-F_KEY1      PIC  X(100) VALUE SPACE.
           02  TC-F_SORT      PIC  X(100) VALUE SPACE.
           02  TC-F_IDLST     PIC  X(100) VALUE SPACE.
           02  TC-F_RES       USAGE  POINTER.
       01  TC-R.
           02  TC-TCD         PIC  9(004).
           02  TC-CCD         PIC  9(003).
           02  TC-NAME        PIC  N(026).
           02  TC-JSU         PIC  N(020).
           02  TC-JSS         PIC  N(020).
           02  TC-UNO         PIC  X(008).
           02  TC-TEL         PIC  X(014).
           02  F              PIC  X(023).
           02  TC-PNO         PIC  9(002).
           02  F              PIC  X(006).
           02  F              PIC  X(064).
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
                "***     直送先 名簿　作成     ***".
           02  FILLER  PIC  X(033) VALUE
                "***                           ***".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(051) VALUE
                "<  得意先ｺｰﾄﾞ 0000 ﾖﾘ 9999 迄打出し  >    終了=ｆ･9".
           02  FILLER  PIC  X(028) VALUE
                "[  確認  OK=1 NO=9   ﾘﾀｰﾝ  ]".
       01  C-ACP.
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
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
            "C-MID" " " "0" "0" "310" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "X" "3" "10" "33" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "4" "10" "33" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "10" "33" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "6" "10" "33" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "7" "10" "33" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "8" "10" "33" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "9" "10" "33" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "9" "51" "07C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "13" "28" "08C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "8" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "14" "23" "4" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "14" "31" "4" "A-STCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "33" "1" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TC-F_PNAME1.
           MOVE 0 TO W-PRC.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
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
           IF  W-STCD > W-ETCD
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
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TC-F_PNAME1 " " BY REFERENCE TC-F_IDLST "0".
       M-25.
      *           READ TC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TC-F_PNAME1 BY REFERENCE TC-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TC-F_IDLST TC-F_PNAME1
               GO TO M-10
           END-IF
           IF  TC-TCD = 9999
               GO TO M-25
           END-IF
           IF  TC-TCD < W-STCD
               GO TO M-25
           END-IF
           IF  TC-TCD > W-ETCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TC-F_IDLST TC-F_PNAME1
               GO TO M-10
           END-IF
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM S-10 THRU S-15.
           MOVE TC-TCD TO W-TCD.
           MOVE TC-PNO TO W-PNO.
           MOVE 0 TO CHK.
       M-30.
           IF  TC-TCD = W-TCD
               GO TO M-35
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
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
           MOVE 0 TO CHK.
           MOVE TC-TCD TO W-TCD.
           MOVE TC-PNO TO W-PNO.
       M-35.
           IF  TC-PNO = W-PNO
               GO TO M-37
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
           MOVE TC-PNO TO W-PNO.
       M-37.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE 0 TO CHK
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           IF  TC-JSS = SPACE
               MOVE SPACE TO H-JS
           ELSE
               MOVE TC-JSU TO H-JS
           END-IF
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE W-YS TO P-YS.
           MOVE W-YE TO P-YE.
           MOVE W-T TO P-T1 P-T2 P-T3 P-T4 P-T5 P-T6.
           MOVE ALL "　" TO P-NAME P-JS.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-TCD TO P-TCD
           END-IF
           MOVE TC-CCD TO P-CCD.
           MOVE TC-NAME TO P-NAME.
           IF  TC-JSS = SPACE
               MOVE TC-JSU TO P-JS
           ELSE
               MOVE TC-JSS TO P-JS
           END-IF
           MOVE TC-UNO TO P-UNO.
           MOVE TC-TEL TO P-TEL.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-40.
      *           READ TC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TC-F_PNAME1 BY REFERENCE TC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  TC-TCD = 9999
               GO TO M-40
           END-IF
           IF  TC-TCD > W-ETCD
               GO TO M-45
           END-IF
           GO TO M-30.
       M-45.
           CALL "DB_F_Close" USING BY REFERENCE TC-F_IDLST TC-F_PNAME1.
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
           MOVE W-T TO P-T1 P-T2 P-T3 P-T4 P-T5 P-T6.
           MOVE ALL "　" TO P-NAME P-JS.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
