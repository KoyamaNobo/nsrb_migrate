       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG255.
      *******************************************************************
      *    PROGRAM         :  請求明細書　入力・発行  (見出しのみ)      *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  W-FILE             PIC  X(013).
       01  HEAD1.
           02  H-15K          PIC  X(005).
           02  F              PIC  X(094).
           02  H-SNO          PIC  9(006).
           02  H-V            PIC  X(001).
           02  H-PAGE         PIC  9(002).
       01  HEAD2.
           02  F              PIC  X(088).
           02  H-NEN          PIC  9(004).
           02  F              PIC  X(003).
           02  H-GET          PIC Z9.
           02  F              PIC  X(003).
           02  H-PEY          PIC Z9.
           02  F              PIC  X(001).
       01  HEAD3.
           02  F              PIC  X(003).
           02  H-UNOM         PIC  N(002).
           02  H-UNO          PIC  X(008).
           02  F              PIC  X(089).
       01  HEAD4.
           02  F              PIC  X(005).
           02  H-JSU          PIC  N(020).
           02  F              PIC  X(017).
           02  H-SNEN         PIC  9(002).
           02  F              PIC  X(004).
           02  H-SGET         PIC Z9.
           02  F              PIC  X(043).
       01  HEAD5.
           02  F              PIC  X(005).
           02  H-JSS          PIC  N(020).
           02  F              PIC  X(068).
       01  HEAD6.
           02  F              PIC  X(006).
           02  H-TNAO         PIC  N(020).
           02  F              PIC  X(067).
       01  HEAD7.
           02  F              PIC  X(006).
           02  H-TNAU         PIC  N(020).
           02  F              PIC  X(004).
           02  H-F            PIC  X(001).
           02  H-TCD          PIC  9(004).
           02  H-R            PIC  X(001).
           02  F              PIC  X(002).
           02  H-TNC1         PIC  9(001).
           02  H-DV           PIC  X(001).
           02  H-DCC          PIC  9(001).
           02  F              PIC  X(052).
       01  HEAD8.
           02  F              PIC  X(002).
           02  H-ZSK          PIC ---------9.
           02  F              PIC  X(001).
           02  H-ZNK          PIC ---------9.
           02  F              PIC  X(001).
           02  H-CSK          PIC -------9.
           02  F              PIC  X(001).
           02  H-KKZ          PIC ---------9.
           02  F              PIC  X(001).
           02  H-URK          PIC ---------9.
           02  F              PIC  X(001).
           02  H-SHZ          PIC --------9.
           02  F              PIC  X(003).
           02  H-SKK          PIC ---------9.
           02  F              PIC  X(026).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DNO          PIC  9(006).
           02  W-KEI.
             03  W-ZSK        PIC S9(009).
             03  W-ZNK        PIC S9(009).
             03  W-CSK        PIC S9(007).
             03  W-KKZ        PIC S9(009).
             03  W-URK        PIC S9(009).
             03  W-SHZ        PIC S9(007).
             03  W-SKK        PIC S9(009).
           02  W-PAGE         PIC  9(002).
           02  W-TPC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-NAME         PIC  N(028).
           02  W-NAM   REDEFINES W-NAME.
             03  W-NAD   OCCURS 28.
               04  W-NA       PIC  N(001).
       01  ERR-STAT           PIC  X(002).
           COPY LNAMW.
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITSKF.
           COPY LISKDF.
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
       01  C-MID0.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　請求明細書　入力・発行　　＊＊＊".
           02  FILLER  PIC  X(037) VALUE
                "テスト印字  する=9 , しない=1 .....  ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　請求明細書　入力・発行　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "得意先".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "日　付".
             03  FILLER  PIC  N(003) VALUE "伝票№".
           02  FILLER.
             03  FILLER  PIC  N(004) VALUE "前回請求".
             03  FILLER  PIC  N(004) VALUE "前回入金".
             03  FILLER  PIC  N(003) VALUE "調整額".
             03  FILLER  PIC  N(004) VALUE "繰越残高".
             03  FILLER  PIC  N(004) VALUE "当月売上".
             03  FILLER  PIC  N(003) VALUE "消費税".
             03  FILLER  PIC  N(004) VALUE "今回請求".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-NGP   PIC  9(008).
             03  A-DNO   PIC  9(006).
           02  FILLER.
             03  A-ZSK   PIC S9(009).
             03  A-ZNK   PIC S9(009).
             03  A-CSK   PIC S9(007).
             03  A-URK   PIC S9(009).
             03  A-SHZ   PIC S9(007).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
           02  FILLER.
             03  D-ZSK   PIC ZZZZZZZZZ- .
             03  D-ZNK   PIC ZZZZZZZZZ- .
             03  D-CSK   PIC ZZZZZZZ- .
             03  D-KKZ   PIC ZZZZZZZZZ- .
             03  D-URK   PIC ZZZZZZZZZ- .
             03  D-SHZ   PIC ZZZZZZZ- .
             03  D-SKK   PIC ZZZ,ZZZ,ZZ9- .
       01  C-ERR.
           02  FILLER.
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
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
      *C-MID0
       CALL "SD_Init" USING 
            "C-MID0" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID0" "N" "1" "17" "42" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID0" "X" "12" "19" "37" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID0" "X" "20" "45" "22" "02C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "143" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "1" "17" "42" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" " " "5" "0" "15" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID1" "N" "5" "1" "6" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID1" "X" "5" "70" "9" "0102C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" " " "8" "0" "12" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID1" "N" "8" "1" "6" " " "03C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID1" "N" "8" "20" "6" "0103C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" " " "11" "0" "52" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID1" "N" "11" "3" "8" " " "04C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID1" "N" "11" "14" "8" "0104C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304C-MID1" "N" "11" "26" "6" "0204C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0404C-MID1" "N" "11" "35" "8" "0304C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0504C-MID1" "N" "11" "46" "8" "0404C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0604C-MID1" "N" "11" "58" "6" "0504C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0704C-MID1" "N" "11" "69" "8" "0604C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" "X" "20" "45" "22" "04C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "61" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "12" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "8" "4" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "8" "0" "14" "A-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGP" "9" "8" "8" "8" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGP" BY REFERENCE W-NGP "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "8" "27" "6" "A-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "12" "0" "41" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZSK" "S9" "12" "2" "9" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZSK" BY REFERENCE W-ZSK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZNK" "S9" "12" "13" "9" "A-ZSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZNK" BY REFERENCE W-ZNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CSK" "S9" "12" "25" "7" "A-ZNK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CSK" BY REFERENCE W-CSK "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-URK" "S9" "12" "45" "9" "A-CSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-URK" BY REFERENCE W-URK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHZ" "S9" "12" "57" "7" "A-URK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHZ" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "62" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "5" "13" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "12" "0" "68" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZSK" "ZZZZZZZZZ-" "12" "2" "10" " " "02C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZSK" BY REFERENCE W-ZSK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZNK" "ZZZZZZZZZ-" "12" "13" "10" "D-ZSK" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZNK" BY REFERENCE W-ZNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CSK" "ZZZZZZZ-" "12" "25" "8" "D-ZNK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-CSK" BY REFERENCE W-CSK "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KKZ" "ZZZZZZZZZ-" "12" "34" "10" "D-CSK" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KKZ" BY REFERENCE W-KKZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-URK" "ZZZZZZZZZ-" "12" "45" "10" "D-KKZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-URK" BY REFERENCE W-URK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHZ" "ZZZZZZZ-" "12" "57" "8" "D-URK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHZ" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKK" "ZZZ,ZZZ,ZZ9-" "12" "66" "12" "D-SHZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SKK" BY REFERENCE W-SKK "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "18" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID0" C-MID0 "p" RETURNING RESU.
           MOVE SPACE TO HEAD1 HEAD2 HEAD3 HEAD4 HEAD5
                         HEAD6 HEAD7 HEAD8.
           MOVE W-15K TO H-15K.
           MOVE "　〒" TO H-UNOM.
           MOVE 999999 TO H-SNO.
           MOVE "-" TO H-V.
           MOVE 99 TO H-PAGE H-GET H-PEY H-SNEN H-SGET.
           MOVE 9999 TO H-NEN H-TCD.
           MOVE "XXXXXXXX" TO H-UNO.
           MOVE ALL "Ｎ" TO H-JSU H-JSS H-TNAO H-TNAU.
           MOVE "(" TO H-F.
           MOVE ")" TO H-R.
           MOVE 999999999 TO H-ZSK H-ZNK H-KKZ H-URK H-SKK.
           MOVE 99999999 TO H-SHZ.
           MOVE 9999999 TO H-CSK.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-TPC = 9
               PERFORM TST-RTN THRU TST-EX
               GO TO M-060
           END-IF
           IF  W-TPC NOT = 1
               GO TO M-060
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-DMM = 9
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-080
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
       M-100.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-ZSDD
           END-IF
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-NGP
           ELSE
               IF  TSK-ZNGP(4) NOT = ZERO
                   MOVE TSK-ZNGP(4) TO W-NGP
               ELSE
                   IF  TSK-ZNGP(3) NOT = ZERO
                       MOVE TSK-ZNGP(3) TO W-NGP
                   ELSE
                       IF  TSK-ZNGP(2) NOT = ZERO
                           MOVE TSK-ZNGP(2) TO W-NGP
                       ELSE
                           IF  TSK-ZNGP(1) NOT = ZERO
                               MOVE TSK-ZNGP(1) TO W-NGP
                           ELSE
                               MOVE ZERO TO W-NGP
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-NGP NOT = ZERO
               CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-NGP "A-NGP" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF
           IF  W-NEN < 2003
               GO TO M-140
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-140
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-140
           END-IF
      *
           MOVE ZERO TO W-DNO.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF.
       M-160.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
           IF  SKD-SKD NOT = W-NGP
               GO TO M-160
           END-IF
           IF  W-TCD NOT = SKD-TCD
               MOVE ZERO TO SKD-SNO
           END-IF
           MOVE SKD-SNO TO W-DNO.
       M-180.
           IF  W-DNO NOT = ZERO
               CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-ZSK "A-ZSK" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           CALL "SD_Output" USING "D-ZSK" D-ZSK "p" RETURNING RESU.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-ZNK "A-ZNK" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           CALL "SD_Output" USING "D-ZNK" D-ZNK "p" RETURNING RESU.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-CSK "A-CSK" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           CALL "SD_Output" USING "D-CSK" D-CSK "p" RETURNING RESU.
      *
           COMPUTE W-KKZ = W-ZSK - W-ZNK + W-CSK.
           CALL "SD_Output" USING "D-KKZ" D-KKZ "p" RETURNING RESU.
       M-280.
           CALL "SD_Accept" USING BY REFERENCE A-URK "A-URK" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-280
           END-IF
           CALL "SD_Output" USING "D-URK" D-URK "p" RETURNING RESU.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-SHZ "A-SHZ" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "D-SHZ" D-SHZ "p" RETURNING RESU.
      *
           COMPUTE W-SKK = W-KKZ + W-URK + W-SHZ.
           CALL "SD_Output" USING "D-SKK" D-SKK "p" RETURNING RESU.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-DMM = 9
               GO TO M-220
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-320
           END-IF
      *
           MOVE ZERO TO W-PAGE.
           MOVE W-NEN TO H-NEN.
           MOVE W-NEN2 TO H-SNEN.
           MOVE W-GET TO H-GET H-SGET.
           MOVE W-PEY TO H-PEY.
           MOVE W-DNO TO H-SNO.
           MOVE W-TCD TO H-TCD.
           MOVE T-TNC1 TO H-TNC1.
           MOVE "-" TO H-DV.
           MOVE T-DCC TO H-DCC.
           MOVE "　〒" TO H-UNOM.
           IF  T-SUNO NOT = SPACE
               MOVE T-SUNO TO H-UNO
           ELSE
               MOVE T-UNO TO H-UNO
           END-IF
           IF  T-SJSU NOT = SPACE
               MOVE T-SJSU TO H-JSU
           ELSE
               MOVE T-JSU TO H-JSU
           END-IF
           IF  T-SJSS NOT = SPACE
               MOVE T-SJSS TO H-JSS
           ELSE
               MOVE T-JSS TO H-JSS
           END-IF
           IF  T-SNA NOT = SPACE
               MOVE T-SNA TO WN-NAME
           ELSE
               MOVE T-NAME TO WN-NAME
           END-IF.
           COPY LNAMP.
           MOVE WN-ONAME TO H-TNAO.
           MOVE WN-UNAME TO H-TNAU.
           MOVE W-ZSK TO H-ZSK.
           MOVE W-ZNK TO H-ZNK.
           MOVE W-CSK TO H-CSK.
           MOVE W-KKZ TO H-KKZ.
           MOVE W-URK TO H-URK.
           MOVE W-SHZ TO H-SHZ.
           MOVE W-SKK TO H-SKK.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           ELSE
               PERFORM MID-RTN THRU MID-EX
           END-IF
      *
           GO TO M-100.
       M-900.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *･････････････････････････････････････････････････････････････････
       TST-RTN.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER = 63
               PERFORM MID-RTN THRU MID-EX
           END-IF.
       TST-EX.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD7 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_LineFeed" USING "8" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
