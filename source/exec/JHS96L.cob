       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS96L.
      ******************************************
      *****    品名別ＪＡＮコード　リスト  *****
      ******************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　品名別ＪＡＮコード　リスト　　＊＊＊　".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(005)  VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007)  VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(004) VALUE "サイズ　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ＩＴＦ　".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(008) VALUE "トラスコ中山名称".
           02  F              PIC  X(008) VALUE SPACE.
       01  W-P.
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZN         PIC  N(004).
           02  F              PIC  X(001).
           02  P-ITF          PIC  X(016).
           02  F              PIC  X(001).
           02  P-NNAME        PIC  X(020).
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-BC           PIC  9(002).
           02  W-SED.
             03  W-SHCD       PIC  9(006).
             03  W-EHCD       PIC  9(006).
           02  W-PAGE         PIC  9(003).
       01  W-SET.
           02  W-ADSM.
             03  F            PIC  X(040) VALUE
                  " SET    SS  S   M   L   LL  XL  XXL     ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5     SET".
           02  W-ADSD.
             03  W-ADS   OCCURS   5.
               04  W-DSD   OCCURS  10.
                 05  W-DS     PIC  X(004).
           COPY LSTAT.
      *
           COPY LICODE.
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品名別ＪＡＮコード　リスト　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(012) VALUE "分　 類　　 ".
             03  FILLER  PIC  X(001) VALUE "(".
             03  FILLER  PIC  N(002) VALUE "全件".
             03  FILLER  PIC  X(006) VALUE "=00 , ".
             03  FILLER  PIC  N(002) VALUE "一般".
             03  FILLER  PIC  X(006) VALUE "=10 , ".
             03  FILLER  PIC  N(003) VALUE "ワーク".
             03  FILLER  PIC  X(006) VALUE "=20 , ".
             03  FILLER  PIC  N(002) VALUE "教育".
             03  FILLER  PIC  X(004) VALUE "=30)".
           02  FILLER  PIC  X(026) VALUE
                "品名ｺｰﾄﾞ  000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-BC    PIC  9(002).
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME9   PIC  X(020) VALUE
                  "***  キャンセル  ***".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "147" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "12" "0" "53" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "12" "15" "12" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "12" "30" "1" "0102C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "N" "12" "31" "4" "0202C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-MID" "X" "12" "35" "6" "0302C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-MID" "N" "12" "41" "4" "0402C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-MID" "X" "12" "45" "6" "0502C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-MID" "N" "12" "51" "6" "0602C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-MID" "X" "12" "57" "6" "0702C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-MID" "N" "12" "63" "4" "0802C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-MID" "X" "12" "67" "4" "0902C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "14" "25" "26" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "48" "22" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BC" "9" "12" "25" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BC" BY REFERENCE W-BC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "12" "A-BC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "14" "35" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "14" "45" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "71" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "71" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "20" "E-ME5" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 999999 TO W-EHCD.
           PERFORM INP-RTN THRU INP-EX.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           MOVE W-ADSM TO W-ADSD.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE SPACE TO CODE-KEY2.
           MOVE W-SHCD TO CODE-HCD20.
      *           START CODEF KEY NOT < CODE-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY2" " NOT < " CODE-KEY2 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CODEF_IDLST CODEF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-10.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CODEF_IDLST CODEF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO M-10
           END-IF
           IF  CODE-HCD20 > W-EHCD
               CALL "DB_F_Close" USING
                BY REFERENCE CODEF_IDLST CODEF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-BC = 00
               GO TO M-15
           END-IF
           MOVE CODE-HCD20 TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF
           IF  HI-BC3 NOT = W-BC
               GO TO M-10
           END-IF.
       M-15.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-020 THRU MID-EX.
           MOVE ZERO TO W-HCD.
       M-20.
           PERFORM PRI-RTN THRU PRI-EX.
       M-25.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO M-25
           END-IF
           IF  CODE-HCD20 > W-EHCD
               GO TO M-90
           END-IF
           IF  W-BC = 00
               GO TO M-20
           END-IF
           MOVE CODE-HCD20 TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HI-BC3 NOT = W-BC
               GO TO M-25
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       INP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-BC "A-BC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO INP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-RTN
           END-IF
           IF  W-BC NOT = 00 AND 10 AND 20 AND 30
               GO TO INP-RTN
           END-IF.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-010
           END-IF.
       INP-020.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-020
           END-IF
      *
           IF  W-SHCD > W-EHCD
               GO TO INP-020
           END-IF.
       INP-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-080
           END-IF
           IF  W-DMM = 9
               GO TO INP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO INP-080
           END-IF.
       INP-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.
       PRI-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA P-SIZN.
       PRI-020.
           MOVE CODE-JAN TO P-JAN.
           MOVE CODE-ITF TO P-ITF.
           MOVE CODE-NAME TO P-NNAME.
           IF  CODE-HCD20 = W-HCD
               GO TO PRI-040
           END-IF
           MOVE CODE-HCD20 TO W-HCD.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　品名　なし" TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
       PRI-040.
           MOVE CODE-SIZ TO W-S.
           MOVE CODE-SNO TO CNT.
           IF  1 NOT = W-S AND CNT
               IF  HI-S(W-S,CNT) = 0
                   MOVE "　なし　" TO P-SIZN
                   GO TO PRI-060
               END-IF
           END-IF
           IF (ZERO NOT = HI-SS(2)) OR (ZERO NOT = HI-SS(3))
                                    OR (ZERO NOT = HI-SS(4))
               ADD 1 TO W-S
           END-IF
           MOVE W-DS(W-S,CNT) TO P-SIZN.
       PRI-060.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 64
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI-EX.
           EXIT.
