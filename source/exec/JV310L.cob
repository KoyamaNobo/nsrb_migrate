       IDENTIFICATION DIVISION.
       PROGRAM-ID. JV310L.
      *****************************************************
      *****     荷札・送り状　個数変更参考リスト     ******
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(026) VALUE
               "＊＊＊　　荷札・送り状　個数変更参考リスト　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日付　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "送り状№".
           02  F              PIC  X(010) VALUE "   ｺｰﾄﾞ   ".
           02  F              PIC  N(008) VALUE "直　送　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "荷札№　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "足数".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "入数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "摘　　　要　".
           02  F              PIC  X(027) VALUE SPACE.
       01  W-P.
           02  P-DATE         PIC  99/99/99.
           02  F              PIC  X(001).
           02  P-OKN          PIC  9(006).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  P-V            PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).                              39
           02  F              PIC  X(001).
           02  P-NFN          PIC  9(006).
           02  P-SUT          PIC  ZZ,ZZ9.
           02  P-ISU          PIC  ZZ,ZZ9.
           02  F              PIC  X(002).
           02  P-TEKI         PIC  N(024).                              36
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-OKND         PIC  9(006).
           02  W-MEI.
             03  W-DATE       PIC  9(006).
             03  W-KURA       PIC  9(001).
             03  W-OKN        PIC  9(006).
             03  W-NFN        PIC  9(006).
             03  W-CSC.
               04  W-TCD      PIC  9(004).
               04  W-CCD      PIC  9(003).
             03  W-KSU        PIC  9(005).
             03  W-SUT        PIC  9(005).
             03  W-ISU        PIC  9(003).
             03  W-ISUD       PIC  9(003).
             03  W-TEKI       PIC  N(023).
             03  W-TEKD  REDEFINES W-TEKI.
               04  W-TEK   OCCURS  23.
                 05  W-TE     PIC  N(001).
           02  W-SU           PIC  9(005).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  W-ADD          PIC  N(002).
           02  W-ADDM REDEFINES W-ADD.
             03  W-ADD1       PIC  N(001).
             03  W-ADD2       PIC  N(001).
       01  W-DMM              PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITCM.
           COPY LIHIM.
           COPY L-JNIF.
           COPY LOKJF.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　荷札・送り状　個数変更参考リスト　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(021) VALUE
                  "***  ｵｸﾘｼﾞｮｳ ﾅｼ  ***".
             03  E-ME3   PIC  X(020) VALUE
                  "***  ﾋﾝﾒｲﾏｽﾀ ﾅｼ  ***".
             03  E-ME4   PIC  X(021) VALUE
                  "***  ﾁｮｸｿｳﾏｽﾀ ﾅｼ  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  ﾃｷﾖｳ ﾅｼ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-NFN   PIC  9(006).
             03  E-OKN   PIC  9(006).
             03  E-HCD   PIC  9(006).
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
            "C-MID" " " "0" "0" "358" " " " " RETURNING RESU.
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
            "08C-MID" "X" "20" "25" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "142" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "142" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "20" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "21" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NFN" "9" "24" "40" "6" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NFN" BY REFERENCE W-NFN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-OKN" "9" "24" "40" "6" "E-NFN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-OKN" BY REFERENCE JNIF1-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "40" "6" "E-OKN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE JNIF1-03 "6" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO W-DATA.
           MOVE SPACE TO W-TEKI W-ADD.
           CALL "DB_F_Open" USING
            "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-15.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF1-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-10 NOT = 0                                         印字
               GO TO M-15
           END-IF.
       M-20.
           MOVE JNIF1-14 TO OKJF-KEY.                                   送り状№
      *           READ OKJF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKN" E-OKN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-07 = 6                                             倉
               IF  OKJF-07 NOT = 1                                      個数
                   GO TO M-30
               END-IF
           END-IF
           MOVE JNIF1-14 TO W-OKN.                                      送り状№
       M-25.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF1-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-10 NOT = 0                                         印字
               GO TO M-25
           END-IF
           IF  JNIF1-14 = W-OKN                                         送り状№
               GO TO M-25
           END-IF
           GO TO M-20.
       M-30.
           IF  JNIF1-02 > 6
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO W-MEI.
           MOVE SPACE TO W-TEKI.
           MOVE OKJF-03 TO W-DATE.
           MOVE JNIF1-01 TO W-NFN.
           MOVE OKJF-01 TO W-OKN.
           MOVE OKJF-05 TO W-CSC.
           MOVE OKJF-07 TO W-KSU.
           MOVE JNIF1-03 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE HI-ISU TO W-ISUD.
       M-35.
           PERFORM SUK-RTN THRU SUK-EX.
           ADD W-SU TO W-SUT.
       M-40.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF1-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-10 NOT = 0                                         印字
               GO TO M-40
           END-IF
           IF  JNIF1-02 > 6
               GO TO M-45
           END-IF
           IF  JNIF1-14 NOT = W-OKN
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NFN" E-NFN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JNIF1-01 NOT = W-NFN
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NFN" E-NFN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE JNIF1-03 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE HI-ISU TO W-ISU.
           IF  W-ISUD NOT = W-ISU
               MOVE 777 TO W-ISUD
           END-IF
           GO TO M-35.
       M-45.
           PERFORM SEH-RTN THRU SEH-EX.
           IF  CNT1 > 22
               GO TO M-50
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
       M-50.
      *           READ JNIF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF1-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JNIF1-10 NOT = 0                                          印字
               GO TO M-50
           END-IF
           IF  JNIF1-14 NOT = W-OKN
               GO TO M-20
           END-IF
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
           END-IF.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       PRI-RTN.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               CALL "DB_F_Open" USING
                "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
               ACCEPT H-DATE FROM DATE
               PERFORM S-10 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-TEKI.
           IF  W-OKN = W-OKND
               GO TO PRI-010
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE W-DATE TO P-DATE.
           MOVE W-OKN  TO P-OKN W-OKND.
           MOVE W-TCD TO P-TCD.
           MOVE "-" TO P-V.
           MOVE W-CCD TO P-CCD.
           MOVE W-CSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊　直送先マスタなし　　" TO TC-NAME
           END-IF
           MOVE TC-NAME TO P-NAME.
       PRI-010.
           MOVE W-NFN TO P-NFN.
           MOVE W-SUT TO P-SUT.
           IF  W-ISUD NOT = 777
               MOVE W-ISUD TO P-ISU
           END-IF
           MOVE W-TEKI TO P-TEKI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-DATE TO P-DATE
               MOVE W-OKN  TO P-OKN
               MOVE W-TCD TO P-TCD
               MOVE "-" TO P-V
               MOVE W-CCD TO P-CCD
               MOVE TC-NAME TO P-NAME
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI-EX.
           EXIT.
       SUK-RTN.
           MOVE ZERO TO CNT1 W-SU.
       SUK-010.
           ADD 1 TO CNT1.
           IF  CNT1 NOT = 28
               ADD JNIF1-091(CNT1) TO W-SU
               GO TO SUK-010
           END-IF.
       SUK-EX.
           EXIT.
       SEH-RTN.
           MOVE JNIF2-03 TO W-TEKI.
           MOVE ZERO TO CNT.
       SEH-010.
           ADD 1 TO CNT1.
           IF  CNT1 > 22
               GO TO SEH-EX
           END-IF
           COMPUTE CNT2 = CNT1 + 1.
           MOVE W-TE(CNT1) TO W-ADD1.
           MOVE W-TE(CNT2) TO W-ADD2.
           IF  W-ADD = "一緒" OR "１ケ"
               GO TO SEH-EX
           END-IF
           GO TO SEH-010.
       SEH-EX.
           EXIT.
