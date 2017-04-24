       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG320.
      *********************************************************
      *    PROGRAM         :  買掛金　台帳　　　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    FORM            :  FKB320                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(047) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(006) VALUE "買掛金台帳　".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(042) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(004) VALUE "仕入先　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(001) VALUE SPACE.
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(027) VALUE SPACE.
           02  P-NEN          PIC  9(002).
           02  F              PIC  N(002) VALUE "年度".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  P-PAGE         PIC  Z(004).
       01  HEAD3.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　付　".
           02  F              PIC  X(005) VALUE " ｺｰﾄﾞ".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(004) VALUE "修正日　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単位".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD8.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(001) VALUE "*".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単位".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ｍ　".
           02  F              PIC  X(002) VALUE "=1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "足　".
           02  F              PIC  X(002) VALUE "=2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "個　".
           02  F              PIC  X(002) VALUE "=3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "㎏　".
           02  F              PIC  X(002) VALUE "=4".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(004) VALUE "前頁繰越".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "仕入頁計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "支払頁計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "次頁繰越".
           02  F              PIC  X(002) VALUE SPACE.
       01  W-PM.
           02  F              PIC  X(013).
           02  P-GET          PIC Z9.
           02  P-PEY          PIC Z9.
           02  F              PIC  X(002).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-TGET         PIC  Z(002).
           02  P-TPEY         PIC  Z(002).
           02  F              PIC  X(001).
           02  P-SSU          PIC -------999.
           02  F              PIC  X(001).
           02  P-TC           PIC  9(001).
           02  F              PIC  X(001).
           02  P-TN           PIC ------999.
           02  P-KIN          PIC --------9.
           02  P-SHZ          PIC -------9.
       01  W-PT.
           02  F              PIC  X(019).
           02  P-T1           PIC  N(004).
           02  P-S1           PIC  X(002).
           02  P-T2           PIC  N(004).
           02  P-S2           PIC  X(002).
           02  P-T3           PIC  N(004).
           02  P-S3           PIC  X(002).
           02  P-T4           PIC  N(004).
           02  P-S4           PIC  X(002).
           02  F              PIC  X(009).
           02  P-DM           PIC  N(004).
           02  F              PIC  X(001).
           02  P-ZKK          PIC ---------9.
           02  P-NPK          PIC ---------9.
           02  P-SPK          PIC ---------9.
           02  P-GKK          PIC ---------9.
       01  W-TD.
           02  W-ZKK          PIC S9(010).
           02  W-NPK          PIC S9(010).
           02  W-SPK          PIC S9(010).
           02  W-GKK          PIC S9(010).
           02  W-ZKKZ         PIC S9(008).
           02  W-NPKZ         PIC S9(008).
           02  W-SPKZ         PIC S9(008).
           02  W-GKKZ         PIC S9(008).
       01  W-DATA.
           02  W-PAGE         PIC  9(004).
           02  W-KIN          PIC S9(008).
           02  W-SHZ          PIC S9(007).
           02  W-TPC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SCD          PIC  9(004).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-KZ           PIC S9(010).
           02  W-KZZ          PIC S9(008).
           02  W-DATE         PIC  X(002).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-GC           PIC  9(002).
           02  W-GCD          PIC  9(002).
           02  W-JNA          PIC  N(024).
           02  W-SESCD.
             03  W-SSCD       PIC  9(004).
             03  W-ESCD       PIC  9(004) VALUE 9999.
           02  W-END          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LISTM.
           COPY LIJM.
           COPY LSJSSW.
      *FD  SP-F
       77  SP-R               PIC  X(170).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　買　掛　金　台　帳　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　（白　紙）　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "<<  専用用紙をセット  >>".
           02  FILLER  PIC  X(036) VALUE
                "[  ﾃｽﾄ ﾌﾟﾘﾝﾄ  ｽﾙ=9 ｼﾅｲ=0 ... ﾘﾀｰﾝ  ]".
           02  FILLER  PIC  X(024) VALUE
                "仕入先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  FILLER.
             03  A-SSCD  PIC  9(004).
             03  A-ESCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  STM ﾅｼ  ***".
             03  E-SCD   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FKB320" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "372" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "17" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "11" "36" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "17" "17" "24" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "20" "18" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "14" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "17" "0" "8" "A-TPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSCD" "9" "17" "29" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSCD" BY REFERENCE W-SSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESCD" "9" "17" "37" "4" "A-SSCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESCD" BY REFERENCE W-ESCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "127" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "127" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "10" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "10" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SCD" "9" "24" "36" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SCD" BY REFERENCE JR-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE 0 TO W-END CHK.
           CALL "PR_Open" RETURNING RESP.
           MOVE 24 TO W-GC.
       M-10.
           PERFORM TST-RTN THRU TST-EX.
           IF  W-END NOT = 0
               CALL "PR_Close" RETURNING RESP
               GO TO M-95
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SSCD "A-SSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ESCD "A-ESCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SSCD > W-ESCD
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               CALL "PR_Close" RETURNING RESP
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           IF  CHK = 5
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-30.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "PR_Close" RETURNING RESP
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-SCD < W-SSCD
               GO TO M-30
           END-IF
           IF  JR-SCD > W-ESCD
               CALL "PR_Close" RETURNING RESP
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-BSC = 1
               GO TO M-30
           END-IF
           MOVE JR-DATE TO W-NGP.
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           MOVE 0 TO CHK.
       M-35.
           MOVE ZERO TO W-TD W-PAGE W-DC.
           MOVE JR-SCD TO W-SCD.
           MOVE JR-SCD TO ST-KEY.
      *           READ ST-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO ST-ZKZ ST-ZKZZ
           END-IF
           MOVE ST-ZKZ TO W-KZ.
           MOVE ST-ZKZZ TO W-KZZ.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊　仕入先マスター　無し　＊" TO S-NAME
           END-IF
           MOVE SPACE TO P-SNA.
           MOVE W-SCD TO P-SCD.
           MOVE S-NAME TO P-SNA.
           MOVE W-NEN2 TO P-NEN.
       M-40.
           PERFORM MEI-RTN THRU MEI-EX.
       M-45.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  JR-BSC = 1
               GO TO M-45
           END-IF
           IF  JR-SCD > W-ESCD
               GO TO M-85
           END-IF
           IF  W-SCD = JR-SCD
               GO TO M-40
           END-IF
      *
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-35.
       M-85.
           PERFORM TOT-RTN THRU TOT-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       HED-RTN.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO P-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-GC.
       HED-EX.
           EXIT.
       MEI-RTN.
           IF  W-DC = 0
               MOVE 5 TO W-DC
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO W-PM.
           MOVE SPACE TO P-JNA.
           MOVE JR-GET TO P-GET.
           MOVE JR-PEY TO P-PEY.
           IF  JR-DC1 = 3
               GO TO MEI-040
           END-IF
           MOVE JR-JCD TO P-JCD J-KEY.
           IF  JR-SJCD NOT = ZERO
               MOVE JR-SJCD TO P-JCD J-KEY
           END-IF
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＪＭ　なし　＊＊　" TO J-NAME
           END-IF
           MOVE J-NAME TO P-JNA.
           MOVE JR-SGET TO P-TGET.
           MOVE JR-SPEY TO P-TPEY.
           IF  JR-SUD NOT = ZERO
               MOVE JR-SUD TO P-SSU
           END-IF
           MOVE JR-TC TO P-TC.
           IF  JR-TD NOT = ZERO
               MOVE JR-TD TO P-TN
           END-IF
           MOVE JR-KIN TO P-KIN.
           IF  JR-SHZ NOT = ZERO
               MOVE JR-SHZ TO P-SHZ
           END-IF
           ADD JR-KIN TO W-ZKK W-NPK.
           ADD JR-SHZ TO W-ZKKZ W-NPKZ.
           GO TO MEI-060.
       MEI-040.
           MOVE ALL "　" TO W-JNA.
           IF  JR-SC = 1
               MOVE "　　　　　［　現金支払　］　　　" TO W-JNA
           END-IF
           IF  JR-SC = 2
               MOVE "　　　　　［　振込支払　］　　　" TO W-JNA
           END-IF
           IF  JR-SC = 3
               MOVE "　　　　　［　小切手支払　］　　" TO W-JNA
           END-IF
           IF  JR-SC = 4
               MOVE "　　　　　［　手形支払　］　　　" TO W-JNA
           END-IF
           IF  JR-SC = 5
               MOVE "　　　　　［　売掛相殺　］　　　" TO W-JNA
           END-IF
           IF  JR-SC = 6
               MOVE "　　　　　［　その他相殺　］　　" TO W-JNA
           END-IF
           MOVE W-JNA TO P-JNA.
           COMPUTE W-KIN = -1 * JR-KIN.
           COMPUTE W-SHZ = -1 * JR-SHZ.
           MOVE W-KIN TO P-KIN.
           IF  W-SHZ NOT = ZERO
               MOVE W-SHZ TO P-SHZ
           END-IF
           SUBTRACT JR-KIN FROM W-ZKK.
           SUBTRACT JR-SHZ FROM W-ZKKZ.
           ADD JR-KIN TO W-SPK.
           ADD JR-SHZ TO W-SPKZ.
       MEI-060.
           MOVE SPACE TO SP-R.
           MOVE W-PM TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-GC.
           IF  W-GC = 24
               MOVE 0 TO W-DC
               PERFORM TOT-RTN THRU TOT-EX
           END-IF.
       MEI-EX.
           EXIT.
       TOT-RTN.
           COMPUTE W-GCD = 26 - W-GC.
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_LineFeed" USING W-GCD RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-PT.
           MOVE SPACE TO P-T1 P-T2 P-T3 P-T4 P-DM.
           MOVE "　　本　" TO P-T1.
           MOVE "=5" TO P-S1.
           MOVE "　　枚　" TO P-T2.
           MOVE "=6" TO P-S2.
           MOVE "　セット" TO P-T3.
           MOVE "=7" TO P-S3.
           MOVE "　　反　" TO P-T4.
           MOVE "=8" TO P-S4.
           MOVE "仕　　入" TO P-DM.
           MOVE W-KZ TO P-ZKK.
           MOVE W-NPK TO P-NPK.
           MOVE W-SPK TO P-SPK.
           COMPUTE W-GKK = W-KZ + W-NPK - W-SPK.
           MOVE W-GKK TO P-GKK.
           MOVE SPACE TO SP-R.
           MOVE W-PT TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-PT.
           MOVE SPACE TO P-T1 P-T2 P-T3 P-T4 P-DM.
           MOVE "消費税額" TO P-DM.
           MOVE W-KZZ TO P-ZKK.
           MOVE W-NPKZ TO P-NPK.
           MOVE W-SPKZ TO P-SPK.
           COMPUTE W-GKKZ = W-KZZ + W-NPKZ - W-SPKZ.
           MOVE W-GKKZ TO P-GKK.
           MOVE SPACE TO SP-R.
           MOVE W-PT TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           MOVE W-GKK TO W-KZ.
           MOVE W-GKKZ TO W-KZZ.
           MOVE ZERO TO W-TD.
       TOT-EX.
           EXIT.
       TST-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO TST-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TST-RTN
           END-IF
           IF  W-TPC = 0
               GO TO TST-EX
           END-IF
           IF  W-TPC NOT = 9
               GO TO TST-RTN
           END-IF
      *
           IF  W-GC NOT = 24
               GO TO TST-020
           END-IF
           IF  CHK NOT = 0
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           ELSE
               MOVE 9 TO P-TC
               MOVE 99 TO P-NEN P-GET P-PEY P-TGET P-TPEY
               MOVE 9999 TO P-SCD P-PAGE
               MOVE 999999 TO P-JCD
               MOVE 9999999 TO P-SHZ
               MOVE 9999999 TO P-SHZ
               MOVE 99999999 TO P-TN P-KIN
               MOVE 999999999 TO P-SSU
               MOVE ALL "Ｎ" TO P-SNA P-JNA
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 5 TO CHK.
           MOVE ZERO TO W-GC.
       TST-020.
           MOVE SPACE TO SP-R.
           MOVE W-PM TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-GC.
           GO TO TST-RTN.
       TST-EX.
           EXIT.
