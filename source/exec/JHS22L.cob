       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS22L.
      *********************************************************
      *    PROGRAM         : ナフコ　受注受信リスト           *
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
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　ナフコ　受注受信リスト　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "社名".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(002) VALUE "店名".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(002) VALUE "社店".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "分類".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票番号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "取引先　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注日　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "納品日　".
           02  F              PIC  X(054) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(008) VALUE "( ｺｰﾄﾞ )".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品名・規格　".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(007) VALUE "JANｺｰﾄﾞ".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "色　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "柄　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｻｲｽﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単位".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　原単価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "原価金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売単価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売価金額".
           02  F              PIC  X(005) VALUE X"1A24212078".
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
           02  P-SNA          PIC  X(015).
           02  F              PIC  X(001).
           02  P-TNA          PIC  X(015).
           02  F              PIC  X(001).
           02  P-SCD          PIC  9(002).
           02  P-TCD          PIC  9(003).
           02  F              PIC  X(003).
           02  P-BCD          PIC  9(002).
           02  F              PIC  X(002).
           02  P-DPC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-DNOD         PIC  9(007).
           02  F              PIC  X(002).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HNGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-NNGP         PIC 99/99/99.
           02  F              PIC  X(003).
           02  P-A            PIC  X(002).
           02  P-AR           PIC  X(007).
           02  F              PIC  X(041).
       01  W-P2.
           02  F              PIC  X(008).
           02  P-FR           PIC  X(001).
           02  P-HCD          PIC  X(006).
           02  P-RE           PIC  X(001).
           02  F              PIC  X(001).
           02  P-SHN          PIC  X(025).
           02  F              PIC  X(001).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(001).
           02  P-HSC          PIC  X(008).
           02  F              PIC  X(001).
           02  P-COR          PIC  X(006).
           02  F              PIC  X(001).
           02  P-GAR          PIC  X(006).
           02  F              PIC  X(001).
           02  P-TM           PIC  N(003).
           02  P-MD    REDEFINES  P-TM.
             03  P-SIZ        PIC  X(005).
             03  F            PIC  X(001).
           02  P-TNI          PIC  X(003).
           02  P-SU           PIC ZZZ,ZZ9.
           02  P-GTN          PIC ZZZZ,ZZ9.
           02  P-GKIN         PIC ZZZZ,ZZZ,ZZ9.
           02  P-UTN          PIC ZZZZ,ZZ9.
           02  P-UKIN         PIC ZZZZ,ZZZ,ZZ9.
       01  W-P3.
           02  F              PIC  X(003).
           02  P-D            PIC  X(002).
           02  P-DUR          PIC  X(026).
           02  P-D1           PIC  X(001).
           02  P-DSHR         PIC  X(014).
           02  P-D2           PIC  X(001).
           02  P-DSMR         PIC  X(007).
           02  F              PIC  X(001).
           02  P-E            PIC  X(002).
           02  P-ER           PIC  X(005).
           02  F              PIC  X(001).
           02  P-F            PIC  X(002).
           02  P-FUR          PIC  X(007).
           02  P-F1           PIC  X(001).
           02  P-FSR          PIC  X(015).
           02  F              PIC  X(001).
           02  P-L            PIC  X(002).
           02  P-LUR          PIC  X(020).
           02  P-L1           PIC  X(001).
           02  P-LCR          PIC  X(016).
           02  P-L2           PIC  X(001).
           02  P-LSR          PIC  X(007).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-D.
             03  W-PAGE       PIC  9(003).
             03  W-STC        PIC  9(009).
             03  W-DNO        PIC  X(009).
             03  W-NNGP       PIC  9(006).
             03  W-DC         PIC  9(001).
             03  CHK          PIC  9(001).
             03  W-HCD        PIC  X(006).
           COPY LSTAT.
      *
           COPY LITDNN.
           COPY LICODE.
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　ナフコ　受注受信リスト　　＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  X(26) VALUE
                "全件 = 0 , 客注のみ = 1   ".
           02  FILLER  PIC  X(22) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(01).
           02  A-DMM   PIC  9(01).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNNF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNNF REWRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "15" "42" " " "C-MID" RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" "X" "12" "23" "26" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID1" "X" "22" "35" "22" "01C-MID1" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SEN" "9" "12" "48" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "52" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
           MOVE 0 TO W-SEN.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               PERFORM ACP-RTN THRU ACP-EX
           END-IF
           IF  COMPLETION_CODE = 255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE ZERO TO W-D.
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
       M-10.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-SIGN = 0
               IF  TDNN1-PC NOT = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNN1-PC = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  W-SEN = 1
               IF  TDNN1-DGN NOT = 00
                   GO TO M-10
               ELSE
                   IF  TDNN1-DSMR = SPACE
                       GO TO M-10
                   END-IF
               END-IF
           END-IF
      *
           ACCEPT H-DATE FROM DATE.
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
       M-15.
           IF  TDNN1-DGN NOT = 0
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE TDNN1-STC TO W-STC.
           MOVE TDNN1-DNO TO W-DNO.
           MOVE TDNN1-NNGP TO W-NNGP.
           MOVE ZERO TO WT-D CHK.
           MOVE SPACE TO W-P1 W-P3.
      *
           MOVE TDNN1-SNA TO P-SNA.
           MOVE TDNN1-TNA TO P-TNA.
           MOVE TDNN1-SCD TO P-SCD.
           MOVE TDNN1-TCD TO P-TCD.
           MOVE TDNN1-BCD TO P-BCD.
           MOVE TDNN1-DPC TO P-DPC.
           MOVE TDNN1-DNOD TO P-DNOD.
           MOVE TDNN1-THC TO P-THC.
           MOVE TDNN1-HNGP TO P-HNGP.
           MOVE TDNN1-NNGP TO P-NNGP.
           IF  TDNN1-AR NOT = SPACE
               MOVE "A:" TO P-A
               MOVE TDNN1-AR TO P-AR
           END-IF
      *
           IF  SPACE = TDNN1-DUR AND TDNN1-DSHR AND TDNN1-DSMR AND
                      TDNN1-ER AND TDNN1-FUR AND TDNN1-FSR AND
                      TDNN1-LUR AND TDNN1-LCR AND TDNN1-LSR
               MOVE 0 TO W-DC
           ELSE
               MOVE 1 TO W-DC
               MOVE "," TO P-D1 P-D2 P-F1 P-L1 P-L2
               MOVE "D:" TO P-D
               MOVE TDNN1-DUR TO P-DUR
               MOVE TDNN1-DSHR TO P-DSHR
               MOVE TDNN1-DSMR TO P-DSMR
               MOVE "E:" TO P-E
               MOVE TDNN1-ER TO P-ER
               MOVE "F:" TO P-F
               MOVE TDNN1-FUR TO P-FUR
               MOVE TDNN1-FSR TO P-FSR
               MOVE "L:" TO P-L
               MOVE TDNN1-LUR TO P-LUR
               MOVE TDNN1-LCR TO P-LCR
               MOVE TDNN1-LSR TO P-LSR
           END-IF
      *
           PERFORM WRI-RTN THRU WRI-EX.
           MOVE "000000" TO W-HCD.
       M-20.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-85
           END-IF
           IF  JS-SIGN = 0
               IF  TDNN1-PC NOT = 0
                   GO TO M-20
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNN1-PC = 0
                   GO TO M-20
               END-IF
           END-IF
           IF (TDNN1-STC NOT = W-STC) OR (TDNN1-DNO NOT = W-DNO)
               GO TO M-40
           END-IF
           IF  TDNN1-DGN = ZERO
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
           MOVE "(" TO P-FR.
           MOVE ")" TO P-RE.
           MOVE SPACE TO CODE-KEY.
           MOVE ZERO TO CODE-TCD.
           MOVE TDNN2-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF.
       M-25.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF
           IF (CODE-TCD NOT = 0000) OR (TDNN2-JAN NOT = CODE-JAN)
               MOVE "000000" TO W-HCD
               MOVE "------" TO P-HCD
               GO TO M-30
           END-IF
           IF  CODE-HCD = W-HCD
               MOVE "  ↑  " TO P-HCD
           ELSE
               MOVE CODE-HCD TO W-HCD
               MOVE W-HCD TO P-HCD
           END-IF.
       M-30.
           MOVE TDNN2-SHN TO P-SHN.
           MOVE TDNN2-JAN TO P-JAN.
           MOVE TDNN2-HSC TO P-HSC.
           MOVE TDNN2-COR TO P-COR.
           MOVE TDNN2-GAR TO P-GAR.
           MOVE TDNN2-SIZ TO P-SIZ.
           MOVE TDNN2-TNI TO P-TNI.
           MOVE TDNN2-SU TO P-SU.
           MOVE TDNN2-GTN TO P-GTN.
           MOVE TDNN2-GKIN TO P-GKIN.
           MOVE TDNN2-UTN TO P-UTN.
           MOVE TDNN2-UKIN TO P-UKIN.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  TDNN2-KKK NOT = SPACE
               MOVE SPACE TO W-P2
               MOVE TDNN2-KKK TO P-SHN
               MOVE W-P2 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
      *
           IF  JS-SIGN = 1
               GO TO M-35
           END-IF
           MOVE 1 TO TDNN1-PC.
      *           REWRITE TDNN-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
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
       M-35.
           ADD TDNN2-SU TO WT-SU.
           ADD TDNN2-GKIN TO WT-GKIN.
           ADD TDNN2-UKIN TO WT-UKIN.
           GO TO M-20.
       M-40.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-SEN NOT = 1
               GO TO M-15
           END-IF
           IF  TDNN1-DGN NOT = 0
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-50.
       M-45.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  TDNN1-PC NOT = 0
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNN1-PC = 0
                   GO TO M-45
               END-IF
           END-IF.
       M-50.
           IF  TDNN1-DGN NOT = 00
               GO TO M-45
           END-IF
           IF  TDNN1-DSMR = SPACE
               GO TO M-45
           END-IF
           GO TO M-15.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SEN > 1
               GO TO ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO ACP-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-010
           END-IF.
       ACP-EX.
           EXIT.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.
       WRI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
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
           MOVE 1 TO TDNN1-PC.
      *           REWRITE TDNN-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
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
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE "（計）" TO P-TM.
           MOVE WT-SU TO P-SU.
           MOVE WT-GKIN TO P-GKIN.
           MOVE WT-UKIN TO P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  W-DC = 1
               MOVE SPACE TO SP-R
               MOVE W-P3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
      *
           IF  W-END = 1
               MOVE HEADV TO SP-R
           ELSE
               IF (TDNN1-STC = W-STC) AND (TDNN1-NNGP = W-NNGP)
                   MOVE HEADV TO SP-R
               ELSE
                   MOVE HEADE TO SP-R
               END-IF
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
