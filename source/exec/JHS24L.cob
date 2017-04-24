       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS22L.
      *********************************************************
      *    PROGRAM         :  統一伝票リスト（赤ちゃん本舗）  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=発行 , 1=再発行               *
      *    W-JS            :  1=入力 , 2=全体(訂正)  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-JS               PIC  9(001).
       77  W-INV              PIC  9(001) VALUE 0.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  N(004).
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(026) VALUE
               "＊＊＊　　赤ちゃん本舗　統一伝票入力リスト　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "直送".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "社店".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "店　　名".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注№　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　発注日".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　納品日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "　センター納品日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "仕入先　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注者　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｿﾞｰﾝ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　指図日".
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(007) VALUE "JANｺｰﾄﾞ".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "商品名　".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "品　　名".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｻｲｽﾞ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  N(004) VALUE "　訂正数".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　原単価".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "原価金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売単価".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売価金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ﾒｰｶｰ".
           02  F              PIC  N(002) VALUE "品番".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "明細備考".
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
           02  P-TCD          PIC  9(004).
           02  P-V            PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-STC          PIC  9(007).
           02  F              PIC  X(001).
           02  P-NHSN         PIC  N(016).
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(007).
           02  F              PIC  X(001).
           02  P-HNO          PIC  9(009).
           02  F              PIC  X(002).
           02  P-HNGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-NNGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-NGPS         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HNA          PIC  X(006).
           02  F              PIC  X(002).
           02  P-ZON          PIC  X(004).
           02  F              PIC  X(002).
           02  P-BI           PIC  X(010).
           02  F              PIC  X(005).
           02  P-DNGPS        PIC 99/99/99.
       01  W-P2.
           02  F              PIC  X(006).
           02  P-DGN          PIC  9(002).
           02  F              PIC  X(001).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(001).
           02  P-SHM          PIC  X(013).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(020).
           02  P-SIZ          PIC  X(004).
           02  P-SU           PIC ZZ,ZZ9.
           02  P-NSU          PIC ZZ,ZZ9.
           02  P-GTN          PIC ZZZ,ZZ9.
           02  P-GKIN         PIC ZZZZ,ZZ9.
           02  P-UTN          PIC ZZZ,ZZ9.
           02  P-UKIN         PIC ZZZZ,ZZ9.
           02  F              PIC  X(001).
           02  P-MKH          PIC  X(007).
           02  F              PIC  X(001).
           02  P-MSB          PIC  X(007).
       01  W-DATA.
           02  W-TSD.
             03  W-TSC        PIC  9(001).
             03  W-TKIN.
               04  W-GKIN     PIC  9(009).
               04  W-UKIN     PIC  9(009).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-NSU       PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-D.
             03  W-STC        PIC  9(007).
             03  W-DNO        PIC  9(007).
             03  W-PAGE       PIC  9(003).
             03  CHK          PIC  9(001).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           02  W-SC           PIC  9(001).
           02  W-SCD          PIC  9(001).
           02  W-AHNA         PIC  N(024).
           02  W-AHNAD REDEFINES W-AHNA.
             03  W-HNAD  OCCURS  24.
               04  W-HNA      PIC  N(001).
           02  W-ANA          PIC  N(020).
           02  W-ANAD  REDEFINES W-ANA.
             03  W-NAD  OCCURS  20.
               04  W-NA       PIC  N(001).
           COPY LSTAT.
      *
           COPY LITDNA.
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　赤ちゃん本舗　統一伝票入力リスト　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNAF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNAF REWRITE ｴﾗｰ  ***".
             03  E-TDNA  PIC  X(016).
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
            "C-MID" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "52" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "105" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "105" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "27" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TDNA" "X" "24" "44" "16" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TDNA" BY REFERENCE TDNA-KEY "16" "0" RETURNING RESU.
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
           ACCEPT W-JS FROM ARGUMENT-VALUE.
           IF  W-JS > 2 OR < 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-D.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
       M-10.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNAF_IDLST TDNAF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-JS NOT = 2
               IF  W-JS NOT = TDNA-NRC
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  TDNA-PC NOT = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNA-PC = 0
                   GO TO M-10
               END-IF
           END-IF
      *
           ACCEPT H-DATE FROM DATE.
           IF  W-JS = 1
               MOVE "【入力】" TO H-MID
           END-IF
           IF  W-JS = 2
               MOVE "【訂正】" TO H-MID
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
       M-15.
           MOVE TDNA-STC TO W-STC.
           MOVE TDNA-DNO TO W-DNO.
           MOVE ZERO TO WT-D CHK W-TSD.
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NHSN.
           MOVE 0077 TO P-TCD.
           MOVE "-" TO P-V.
           MOVE TDNA-CCD TO P-CCD.
           MOVE TDNA-STC TO P-STC.
           MOVE TDNA-TNA TO P-NHSN.
           MOVE TDNA-DNO TO P-DNO.
           MOVE TDNA-HNO TO P-HNO.
           MOVE TDNA-HNGP TO P-HNGP.
           MOVE TDNA-NNGP TO P-NNGP.
           MOVE TDNA-NGPS TO P-NGPS.
           MOVE TDNA-THC TO P-THC.
           MOVE TDNA-HNA TO P-HNA.
           MOVE TDNA-ZON TO P-ZON.
           MOVE TDNA-BI TO P-BI.
           IF  TDNA-DNGPS NOT = ZERO
               MOVE TDNA-DNGPS TO P-DNGPS
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-20.
           PERFORM HNM-RTN THRU HNM-EX.
           MOVE ZERO TO W-TKIN.
           IF  TDNA-TSC = 0
               MOVE TDNA-GKIN TO W-GKIN
               MOVE TDNA-UKIN TO W-UKIN
           ELSE
               COMPUTE W-GKIN = TDNA-GTN * TDNA-NSU
               COMPUTE W-UKIN = TDNA-UTN * TDNA-NSU
               IF  W-TSC = 0
                   MOVE 1 TO W-TSC
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           MOVE TDNA-DGN TO P-DGN.
           MOVE TDNA-JAN TO P-JAN.
           MOVE TDNA-SHM TO P-SHM.
           IF  TDNA-HCD NOT = ZERO
               MOVE TDNA-HCD TO P-HCD
           END-IF
           MOVE W-ANA TO P-NAME.
           MOVE TDNA-SIZ TO P-SIZ.
           MOVE TDNA-SU TO P-SU.
           IF  TDNA-TSC NOT = 0
               MOVE TDNA-NSU TO P-NSU
           END-IF
           MOVE TDNA-GTN TO P-GTN.
           MOVE W-GKIN TO P-GKIN.
           MOVE TDNA-UTN TO P-UTN.
           MOVE W-UKIN TO P-UKIN.
           MOVE TDNA-MKH TO P-MKH.
           MOVE TDNA-MSB TO P-MSB.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD TDNA-SU TO WT-SU.
           IF  TDNA-TSC NOT = 0
               ADD TDNA-NSU TO WT-NSU
           ELSE
               ADD TDNA-SU TO WT-NSU
           END-IF
           ADD W-GKIN TO WT-GKIN.
           ADD W-UKIN TO WT-UKIN.
      *
           IF  JS-SIGN = 1
               GO TO M-25
           END-IF
           MOVE 1 TO TDNA-PC.
      *           REWRITE TDNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
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
       M-25.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-45
           END-IF
           IF  W-JS NOT = 2
               IF  W-JS NOT = TDNA-NRC
                   GO TO M-25
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  TDNA-PC NOT = 0
                   GO TO M-25
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TDNA-PC = 0
                   GO TO M-25
               END-IF
           END-IF
           IF (TDNA-STC = W-STC) AND (TDNA-DNO = W-DNO)
               GO TO M-20
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-15.
       M-45.
           PERFORM KEI-RTN THRU KEI-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
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
       HNM-RTN.
           MOVE TDNA-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　品名なし　" TO HI-NAME
           END-IF.
       HNM-010.
           MOVE SPACE TO W-AHNA.
           MOVE HI-NAME TO W-AHNA.
           IF  SPACE = W-HNA(21) AND W-HNA(22) AND
                      W-HNA(23) AND W-HNA(24)
               MOVE W-AHNA TO W-ANA
               GO TO HNM-EX
           END-IF
           IF  SPACE = W-HNA(22) AND W-HNA(23) AND W-HNA(24)
               MOVE 1 TO W-SC
           ELSE
               IF  SPACE = W-HNA(23) AND W-HNA(24)
                   MOVE 2 TO W-SC
               ELSE
                   IF  SPACE = W-HNA(24)
                       MOVE 3 TO W-SC
                   ELSE
                       MOVE 4 TO W-SC
                   END-IF
               END-IF
           END-IF
           MOVE ZERO TO CNT CNTD W-SCD.
       HNM-020.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO HNM-EX
           END-IF
           IF  W-SC > W-SCD
               IF  W-HNA(CNT) = SPACE
                   ADD 1 TO W-SCD
                   GO TO HNM-020
               END-IF
           END-IF
           ADD 1 TO CNTD.
           IF  CNTD < 21
               MOVE W-HNA(CNT) TO W-NA(CNTD)
               GO TO HNM-020
           END-IF.
       HNM-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO W-ANA P-NAME.
           MOVE "（" TO W-NA(18).
           MOVE "計" TO W-NA(19).
           MOVE "）" TO W-NA(20).
           MOVE W-ANA TO P-NAME.
           IF  W-TSC = 0
               MOVE WT-SU TO P-SU
           ELSE
               MOVE WT-NSU TO P-NSU
           END-IF
           MOVE WT-GKIN TO P-GKIN.
           MOVE WT-UKIN TO P-UKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  W-END = 1
               MOVE HEADE TO SP-R
           ELSE
               IF  TDNA-STC = W-STC
                   MOVE HEADV TO SP-R
               ELSE
                   MOVE HEADE TO SP-R
               END-IF
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
