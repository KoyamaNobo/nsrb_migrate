       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS21L.
      *********************************************************
      *    PROGRAM         :  受注受信リスト（ワークマン）    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=発行 , 1=再発行               *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　ワークマン　受注受信リスト　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "社店".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票番号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "社　　名".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(004) VALUE "店　　名".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注日　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "納品日　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "分類".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "取引先　".
           02  F              PIC  X(031) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(008) VALUE "( ｺｰﾄﾞ )".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(004) VALUE "サイズ　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　原単価".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "原価金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売単価".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売価金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "商　品　名　".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(003) VALUE "ｹｰｽ".
           02  F              PIC  X(001) VALUE SPACE.
       01  HEADV.
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(036) VALUE
                "------------------------------------".
       01  HEADE.
           02  F              PIC  X(050) VALUE
                "==================================================".
           02  F              PIC  X(050) VALUE
                "==================================================".
           02  F              PIC  X(036) VALUE
                "====================================".
       01  W-P1.
           02  P-SCD          PIC  9(002).
           02  P-X            PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-DNOD         PIC  9(007).
           02  F              PIC  X(001).
           02  P-SNA          PIC  X(020).
           02  P-TNAD         PIC  N(010).
           02  P-TNA          PIC  X(020).
           02  F              PIC  X(001).
           02  P-HNGP         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-NNGP         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-BCD          PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-SHC          PIC  9(001).
           02  F              PIC  X(002).
           02  P-DPC          PIC  X(002).
           02  F              PIC  X(001).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(001).
           02  P-R1           PIC  X(002).
           02  P-UBCD         PIC  X(015).
           02  P-R2           PIC  X(002).
           02  P-HCCD         PIC  X(010).
           02  F              PIC  X(001).
       01  W-P2.
           02  F              PIC  X(009).
           02  P-F            PIC  X(001).
           02  P-HCD          PIC  X(006).
           02  P-R            PIC  X(001).
           02  P-HNA          PIC  N(016).
           02  P-SIZ          PIC  N(004).
           02  P-SU           PIC ZZ,ZZ9.
           02  P-GTN          PIC ZZZ,ZZ9.
           02  P-GKIN         PIC Z,ZZZ,ZZ9.
           02  P-UTN          PIC ZZZ,ZZ9.
           02  P-UKIN         PIC Z,ZZZ,ZZ9.
           02  F              PIC  X(001).
           02  P-SHN          PIC  X(025).
           02  F              PIC  X(001).
           02  P-JAN          PIC  X(007).
           02  P-KSU          PIC  Z(004).
           02  F              PIC  X(001).
           02  P-GCN          PIC  9(006).
           02  F              PIC  X(001).
           02  P-TKCD         PIC  X(005).
       01  W-DATA.
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-D.
             03  W-PAGE       PIC  9(003).
             03  W-STC        PIC  9(009).
             03  W-DNO        PIC  9(009).
             03  W-NNGP       PIC  9(006).
             03  W-HCC        PIC  9(001).
             03  CHK          PIC  9(001).
             03  W-HCD        PIC  X(006).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "          SS   S   M   L  LL28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5    ----".
           COPY LSTAT.
      *
           COPY LITDNW.
           COPY LICODE.
           COPY LIHIM2.
           COPY LWTNAF.
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
           02  C-CL    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　ワークマン　受注受信リスト　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNWF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNWF REWRITE ｴﾗｰ  ***".
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "46" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "89" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "89" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "27" "E-ME10" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-D.
      *
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
       M-15.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-SIGN = 0
               IF  TDNW1-PC NOT = 0
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNW1-PC = 0
                   GO TO M-15
               END-IF
           END-IF
      *
           MOVE W-MSIZ TO W-ASIZD.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-010 THRU MID-EX.
       M-20.
           IF  TDNW1-DGN NOT = 0
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE TDNW1-STC TO W-STC.
           MOVE TDNW1-DNO TO W-DNO.
           MOVE TDNW1-NNGP TO W-NNGP.
           MOVE TDNW1-HCC TO W-HCC.
           MOVE ZERO TO WT-D CHK.
      *
           MOVE TDNW1-TCD TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO WTNA-NAME
               MOVE "＊店名なし＊　　　　" TO WTNA-NAME
           END-IF
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNAD.
           MOVE TDNW1-SNA TO P-SNA.
           MOVE TDNW1-TNA TO P-TNA.
           MOVE TDNW1-SCD TO P-SCD.
           MOVE "-" TO P-X.
           MOVE TDNW1-TCD TO P-TCD.
           MOVE TDNW1-BCD TO P-BCD.
           MOVE "-" TO P-V.
           MOVE TDNW1-SHC TO P-SHC.
           MOVE TDNW1-DPC TO P-DPC.
           MOVE TDNW1-DNOD TO P-DNOD.
           MOVE TDNW1-THC TO P-THC.
           MOVE TDNW1-HNGP TO P-HNGP.
           MOVE TDNW1-NNGP TO P-NNGP.
           MOVE WTNA-NAME TO P-TNAD.
      *
           IF  TDNW1-UBC = "1"
               MOVE "D:" TO P-R1
               MOVE "ﾃｲﾊﾞﾝｺﾞﾝﾄﾞﾗ" TO P-UBCD
           END-IF
           IF  TDNW1-UBC = "2"
               MOVE "D:" TO P-R1
               MOVE "ｷﾝｲﾂ" TO P-UBCD
           END-IF
           IF  TDNW1-UBC = "3"
               MOVE "D:" TO P-R1
               MOVE "ｱｸｼｮﾝｴﾝﾄﾞ" TO P-UBCD
           END-IF
           IF  TDNW1-HCC = 1
               MOVE "E:" TO P-R2
               MOVE "*ｶﾞｲﾁｭｳ*" TO P-HCCD
           END-IF
           IF  TDNW1-HCC = 2
               MOVE "E:" TO P-R2
               MOVE "*ﾅｲﾁｭｳ*" TO P-HCCD
           END-IF
      *
           PERFORM WRI-RTN THRU WRI-EX.
           MOVE "000000" TO W-HCD.
       M-25.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-85
           END-IF
           IF  JS-SIGN = 0
               IF  TDNW1-PC NOT = 0
                   GO TO M-25
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNW1-PC = 0
                   GO TO M-25
               END-IF
           END-IF
           IF (TDNW1-STC NOT = W-STC) OR (TDNW1-DNO NOT = W-DNO)
               GO TO M-45
           END-IF
           IF  TDNW1-DGN = ZERO
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA P-SIZ.
           MOVE "(" TO P-F.
           MOVE ")" TO P-R.
           MOVE SPACE TO CODE-KEY.
           MOVE 9850 TO CODE-TCD.
           MOVE TDNW2-WCO TO CODE-WCO.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-SIZ CODE-SNO
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-SIZ CODE-SNO
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF
           IF (CODE-TCD NOT = 9850) OR (TDNW2-WCO NOT = CODE-WCO)
               MOVE ZERO TO CODE-SIZ CODE-SNO
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF
           IF  CODE-HCD = W-HCD
               MOVE W-HCD TO P-HCD
           ELSE
               MOVE CODE-HCD TO W-HCD
               MOVE W-HCD TO P-HCD
           END-IF.
       M-30.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME HI-SMS
               MOVE "＊品名なし＊　　　　" TO HI-SMS
           END-IF
           IF  HI-SMS NOT = SPACE
               MOVE HI-SMS TO P-HNA
           ELSE
               MOVE HI-NAME TO P-HNA
           END-IF
           MOVE "＊＊＊＊" TO P-SIZ.
           IF (CODE-SIZ NOT = 0) AND (CODE-SNO NOT = ZERO)
               MOVE W-SIZ(CODE-SIZ,CODE-SNO) TO P-SIZ
           END-IF
           IF  W-HCC = 1
               IF  TDNW2-GCN NOT = ZERO
                   MOVE TDNW2-GCN TO P-GCN
               END-IF
           END-IF
           IF  TDNW2-TKC = ZERO
               MOVE "ﾂｲｶﾌｶ" TO P-TKCD
           END-IF
      *
           MOVE TDNW2-SHN TO P-SHN.
           IF  TDNW2-JAN NOT = SPACE AND ZERO
               MOVE TDNW2-JAN TO P-JAN
           ELSE
               MOVE TDNW2-HCD TO P-JAN
           END-IF
           MOVE TDNW2-KSU TO P-KSU.
           MOVE TDNW2-SU TO P-SU.
           MOVE TDNW2-GTN TO P-GTN.
           MOVE TDNW2-GKIN TO P-GKIN.
           MOVE TDNW2-UTN TO P-UTN.
           MOVE TDNW2-UKIN TO P-UKIN.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  JS-SIGN = 1
               GO TO M-40
           END-IF
           MOVE 1 TO TDNW1-PC.
      *           REWRITE TDNW-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-40.
           ADD TDNW2-SU TO WT-SU.
           ADD TDNW2-GKIN TO WT-GKIN.
           ADD TDNW2-UKIN TO WT-UKIN.
           GO TO M-25.
       M-45.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-20.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.
       WRI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  CHK NOT = 0
               GO TO WRI-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO WRI-EX
           END-IF
           MOVE 1 TO TDNW1-PC.
      *           REWRITE TDNW-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           MOVE 1 TO CHK.
       WRI-EX.
           EXIT.
       KEI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA P-SIZ.
           MOVE "（計）　" TO P-SIZ.
           MOVE WT-SU TO P-SU.
           MOVE WT-GKIN TO P-GKIN.
           MOVE WT-UKIN TO P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-END = 1
               MOVE HEADV TO SP-R
           ELSE
               IF (TDNW1-STC = W-STC) AND (TDNW1-NNGP = W-NNGP)
                   MOVE HEADV TO SP-R
               ELSE
                   MOVE HEADE TO SP-R
               END-IF
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
