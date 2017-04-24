       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR100.
      ********************************
      *****     領収書　発行     *****
      ********************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIN           PIC  9(001).
       77  W-FNM            PIC  X(016).
       77  W-FNM1           PIC  X(016) VALUE "RSF             ".
       77  W-FNM2           PIC  X(016) VALUE "RSRF            ".
       77  W-15K            PIC  X(005) VALUE X"1A24212078".
       77  W-20K            PIC  X(005) VALUE X"1A24212474".
       77  W-40K            PIC  X(008) VALUE X"1A26212068222176".
       77  W-10K            PIC  X(008) VALUE X"1A26212068212078".
       01  W-PA.
           02  W-P1.
             03  P-20K1     PIC  X(005).
             03  F          PIC  X(051).
             03  P-SHM      PIC  N(005).
             03  F          PIC  X(008).
             03  P-RSN      PIC  9(006).
           02  W-P2.
             03  P-15K2     PIC  X(005).
             03  P-40K      PIC  X(008).
             03  F          PIC  X(007).
             03  P-TNA      PIC  N(018).
             03  F          PIC  X(017).
             03  P-10K      PIC  X(008).
           02  W-P3.
             03  P-20K3     PIC  X(005).
             03  F          PIC  X(012).
             03  P-RSK      PIC  N(013).
             03  F          PIC  X(032).
             03  P-INS      PIC  Z(005).
             03  P-SO    REDEFINES P-INS.
               04  F        PIC  X(001).
               04  P-SSM    PIC  N(002).
             03  F          PIC  X(001).
           02  W-P5.
             03  P-15K5     PIC  X(005).
             03  F          PIC  X(014).
             03  P-SW4      PIC  N(002).
             03  F          PIC  X(001).
             03  P-UN       PIC  Z(002).
             03  F          PIC  X(003).
             03  P-UG       PIC Z9.
             03  F          PIC  X(003).
             03  P-UP       PIC Z9.
             03  F          PIC  X(050).
      *----
       01  W-PB.
           02  W-P6.
             03  P-15K6     PIC  X(005).
             03  F          PIC  X(061).
             03  P-SW5      PIC  N(002).
             03  F          PIC  X(001).
             03  P-HN       PIC  Z(002).
             03  F          PIC  X(003).
             03  P-HG       PIC Z9.
             03  F          PIC  X(003).
             03  P-HP       PIC Z9.
             03  F          PIC  X(001).
           02  W-P7.
             03  F          PIC  X(007).
             03  P-UNM      PIC  N(002).
             03  P-UNO      PIC  X(008).
             03  F          PIC  X(060).
           02  W-P8.
             03  F          PIC  X(009).
             03  P-JSU      PIC  N(020).
             03  F          PIC  X(039).
           02  W-P9.
             03  F          PIC  X(009).
             03  P-JSS      PIC  N(020).
             03  F          PIC  X(039).
           02  W-P10.
             03  F          PIC  X(010).
             03  P-ONA      PIC  N(020).
             03  F          PIC  X(038).
           02  W-P11.
             03  F          PIC  X(010).
             03  P-UNA      PIC  N(020).
             03  F          PIC  X(038).
           02  W-P12.
             03  F          PIC  X(026).
             03  P-MSU      PIC Z9.
             03  F          PIC  X(050).
       01  W-DATA.
           02  W-KEY.
             03  W-DATE     PIC  9(006).
             03  W-DATED REDEFINES W-DATE.
               04  W-UN     PIC  9(002).
               04  W-UG     PIC  9(002).
               04  W-UP     PIC  9(002).
             03  W-TCD      PIC  9(004).
           02  W-RSN        PIC  9(006).
           02  W-NO.
             03  W-SNO      PIC  9(006).
             03  W-ENO      PIC  9(006) VALUE 999999.
           02  W-HNGP       PIC  9(006).
           02  W-HNGPD REDEFINES W-HNGP.
             03  W-HN       PIC  9(002).
             03  W-HG       PIC  9(002).
             03  W-HP       PIC  9(002).
           02  W-CHK        PIC  9(001).
           02  W-TNAD       PIC  N(018).
           02  W-TNAAD REDEFINES W-TNAD.
             03  W-TNA   OCCURS 18  PIC  N(001).
           02  W-NAME       PIC  N(016).
           02  CNT          PIC  9(002).
           02  W-TPC        PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  W-MSU        PIC  9(002).
           02  W-KIN        PIC \\\\,\\\,\\9.
           02  W-RSK.
             03  W-RK       PIC  N(012).
             03  F          PIC  N(001) VALUE "※".
       01  ERR-STAT         PIC  X(002).
           COPY LNAMW.
           COPY LSTAT.
      *
           COPY LITM.
      *FD  TNO-M
       01  TNO-M_TSR100.
           02  TNO-M_PNAME1 PIC  X(004) VALUE "TNOM".
           02  F            PIC  X(001).
           02  TNO-M_LNAME  PIC  X(012) VALUE "TNO-M_TSR100".
           02  F            PIC  X(001).
           02  TNO-M_KEY1   PIC  X(100) VALUE SPACE.
           02  TNO-M_SORT   PIC  X(100) VALUE SPACE.
           02  TNO-M_IDLST  PIC  X(100) VALUE SPACE.
           02  TNO-M_RES    USAGE  POINTER.
       01  TNO-R.
           02  NO-KEY       PIC  X(002).
           02  F            PIC  X(032).
           02  NO-RSN       PIC  9(006).
           02  F            PIC  X(216).
       77  F                PIC  X(001).
      *FD  RS-F
       01  RS-F_TSR100.
           02  RS-F_PNAME1  PIC  X(016) VALUE SPACE.
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR100".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R.
           02  RS-RSN       PIC  9(006).
           02  RS-HNGP      PIC  9(006).
           02  RS-KEY.
             03  RS-DATE    PIC  9(006).
             03  RS-TCD     PIC  9(004).
           02  RS-KIN       PIC S9(009).
           02  RS-INS       PIC  9(005).
           02  RS-SOC       PIC  9(001).
           02  RS-SHC       PIC  9(001).
           02  F            PIC  X(023).
           02  RS-SEQ       PIC  9(003).
       77  F                PIC  X(001).
      *FD  SP-F
       77  SP-R             PIC  X(170).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "[  TEST PRINT  ｽﾙ=9  ｼﾅｲ=1   ﾘﾀｰﾝ  ]".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  FILLER.
             03  A-SNO   PIC  9(006).
             03  A-ENO   PIC  9(006).
           02  A-CHK   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1  PIC  N(012) VALUE
                "　領　収　書　　発　行　".
           02  D-MID2.
             03  FILLER  PIC  N(012) VALUE
                  "領　収　書　　再　発　行".
             03  FILLER  PIC  N(012) VALUE
                  "（　再発行表示　なし　）".
             03  FILLER  PIC  X(040) VALUE
                  "｛　領収書№ 000000 より 999999 まで　｝".
           02  D-NKM   PIC  X(030) VALUE
                "領収書№更新  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  TNOM ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  TNOM REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  N(017) VALUE
                  "＊＊＊　　業務放棄　せよ　　＊＊＊".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
            "C-MID" " " "0" "0" "366" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "14" "36" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "12" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "16" "0" "12" "A-TPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNO" "9" "16" "25" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNO" BY REFERENCE W-SNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENO" "9" "16" "37" "6" "A-SNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENO" BY REFERENCE W-ENO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "16" "42" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "38" "1" "A-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "142" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" "N" "6" "20" "24" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID2" " " "0" "0" "88" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID2" "N" "6" "20" "24" " " "D-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID2" "N" "7" "20" "24" "01D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID2" "X" "16" "12" "40" "02D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NKM" "X" "16" "17" "30" "D-MID2" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "110" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "N" "24" "15" "34" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIN FROM ARGUMENT-VALUE.
           IF  JS-SIN > 1
               GO TO M-05
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIN = 0
               MOVE W-FNM1 TO W-FNM
               MOVE W-FNM TO RS-F_PNAME1
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU
           ELSE
               MOVE W-FNM2 TO W-FNM
               MOVE W-FNM TO RS-F_PNAME1
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU
           END-IF
           ACCEPT W-HNGP FROM DATE.
           IF  W-HN < 90
               ADD 12 TO W-HN
           ELSE
               SUBTRACT 88 FROM W-HN
           END-IF
           MOVE SPACE TO W-P1 W-P2 W-P3      W-P5
                         W-P6 W-P7 W-P8 W-P9 W-P10 W-P11 W-P12.
           MOVE W-15K TO P-15K2 P-15K5 P-15K6.
           MOVE W-20K TO P-20K1 P-20K3.
           MOVE W-40K TO P-40K.
           MOVE W-10K TO P-10K.
           MOVE ALL "Ｎ" TO P-SHM P-TNA P-JSU P-JSS P-ONA P-UNA.
           MOVE "￥９９９，９９９，９９９※" TO P-RSK.
           MOVE "平成" TO P-SW4 P-SW5.
           MOVE 99 TO P-UN P-UG P-UP P-HN P-HG P-HP.
           MOVE 999999 TO P-RSN.
           MOVE 99999 TO P-INS.
           MOVE "　〒" TO P-UNM.
           MOVE "XXXXXXXX" TO P-UNO.
           MOVE 99 TO P-MSU.
           CALL "PR_Open" RETURNING RESP.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = 1
               GO TO M-15
           END-IF
           IF  W-TPC NOT = 9
               GO TO M-10
           END-IF
           PERFORM S-05 THRU S-10.
           PERFORM S-20 THRU S-25.
           GO TO M-10.
       M-15.
           IF  JS-SIN = 0
               GO TO M-25
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SNO "A-SNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ENO "A-ENO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SNO > W-ENO
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIN = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIN NOT = 0
                   GO TO M-20
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
           IF  JS-SIN NOT = 0
               GO TO M-35
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TNO-M_PNAME1 "SHARED" BY REFERENCE TNO-M_IDLST "1"
            "NO-KEY" BY REFERENCE NO-KEY.
           MOVE "01" TO NO-KEY.
      *           READ TNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TNO-M_PNAME1 BY REFERENCE TNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TNO-M_IDLST TNO-M_PNAME1
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE NO-RSN TO W-RSN.
       M-35.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  JS-SIN = 0
               CALL "DB_F_Open" USING
                "I-O" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0"
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0"
           END-IF.
       M-40.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JS-SIN NOT = 0
               IF  RS-RSN < W-SNO OR > W-ENO
                   GO TO M-40
               END-IF
           END-IF.
       M-45.
           MOVE ZERO TO W-MSU.
           MOVE RS-KEY TO W-KEY.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　　ＴＭ　ナシ　　＊＊＊" TO T-NAME T-TNA
               MOVE SPACE TO T-JSU T-JSS T-UNO T-SJSU T-SJSS T-SUNO
           END-IF
           PERFORM S-30 THRU S-45.
       M-50.
           MOVE SPACE TO W-P1 W-P2 W-P3      W-P5.
           MOVE W-15K TO P-15K2 P-15K5.
           MOVE W-20K TO P-20K1 P-20K3.
           MOVE W-40K TO P-40K.
           MOVE W-10K TO P-10K.
           MOVE "平成" TO P-SW4.
           IF  RS-SHC = 5
               MOVE "再　発　行" TO P-SHM
           ELSE
               MOVE SPACE TO P-SHM
           END-IF
           IF  JS-SIN = 0
               ADD 1 TO W-RSN
               MOVE W-RSN TO P-RSN
           ELSE
               MOVE RS-RSN TO P-RSN
           END-IF
           MOVE W-TNAD TO P-TNA.
           MOVE RS-KIN TO W-KIN.
           MOVE W-KIN TO W-RK.
           MOVE W-RSK TO P-RSK.
           IF  RS-SOC = 0
               MOVE RS-INS TO P-INS
           END-IF
           IF  RS-SOC = 5
               MOVE "相殺" TO P-SSM
           END-IF
           MOVE W-UN TO P-UN.
           MOVE W-UG TO P-UG.
           MOVE W-UP TO P-UP.
           PERFORM S-05 THRU S-10.
           ADD 1 TO W-MSU.
           IF  JS-SIN = 0
               MOVE W-RSN TO RS-RSN
               MOVE W-HNGP TO RS-HNGP
      *               REWRITE RS-R
      *///////////////
               CALL "DB_Update" USING
                RS-F_PNAME1 RS-F_LNAME RS-R RETURNING RET
           ELSE
               MOVE RS-HNGP TO W-HNGP
           END-IF.
       M-55.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  JS-SIN NOT = 0
               IF  RS-RSN < W-SNO OR > W-ENO
                   GO TO M-55
               END-IF
           END-IF
           IF  W-KEY = RS-KEY
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "29" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO M-50
           END-IF
           PERFORM S-15 THRU S-25.
           GO TO M-45.
       M-80.
           PERFORM S-15 THRU S-25.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  JS-SIN NOT = 0
               GO TO M-95
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           CALL "SD_Output" USING "D-NKM" D-NKM "p" RETURNING RESU.
       M-85.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-85
           END-IF
           IF  W-CHK NOT = 1 AND 9
               GO TO M-85
           END-IF.
       M-90.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-85
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-90
           END-IF
           IF  W-DMM = 9
               GO TO M-85
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-90
           END-IF
           IF  W-CHK = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           MOVE W-RSN TO NO-RSN.
      *           REWRITE TNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TNO-M_PNAME1 TNO-M_LNAME TNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           IF  JS-SIN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE TNO-M_IDLST TNO-M_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_LineFeed" USING "10" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-10.
           EXIT.
       S-15.
           MOVE SPACE TO W-P6 W-P7 W-P8 W-P9 W-P10 W-P11 W-P12.
           MOVE W-15K TO P-15K6.
           MOVE "平成" TO P-SW5.
           MOVE W-HN TO P-HN.
           MOVE W-HG TO P-HG.
           MOVE W-HP TO P-HP.
           MOVE "　〒" TO P-UNM.
           IF  T-SUNO NOT = SPACE
               MOVE T-SUNO TO P-UNO
           ELSE
               MOVE T-UNO TO P-UNO
           END-IF
           IF  T-SJSU NOT = SPACE
               MOVE T-SJSU TO P-JSU
           ELSE
               MOVE T-JSU TO P-JSU
           END-IF
           IF  T-SJSU NOT = SPACE
               MOVE T-SJSS TO P-JSS
           ELSE
               MOVE T-JSS TO P-JSS
           END-IF
           MOVE WN-ONAME TO P-ONA.
           MOVE WN-UNAME TO P-UNA.
           MOVE W-MSU TO P-MSU.
       S-20.
           MOVE SPACE TO SP-R.
           MOVE W-P6 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P7 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P10 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P11 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P12 TO SP-R.
           CALL "PR_LineFeed" USING "12" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           EXIT.
       S-30.
           IF  T-TNA = SPACE
               MOVE T-NAME TO W-NAME
           ELSE
               MOVE T-TNA TO W-NAME
           END-IF
           MOVE SPACE TO W-TNAD.
           MOVE W-NAME TO W-TNAD.
           MOVE 17 TO CNT.
       S-35.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO S-40
           END-IF
           IF  W-TNA(CNT) = SPACE
               GO TO S-35
           END-IF
           ADD 2 TO CNT.
           MOVE "殿" TO W-TNA(CNT).
       S-40.
           MOVE T-NAME TO WN-NAME.
           COPY LNAMP.
       S-45.
           EXIT.
