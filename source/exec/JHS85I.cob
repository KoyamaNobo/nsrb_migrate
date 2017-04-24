       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS85I.
      *************************************************************
      *    PROGRAM         :  赤ちゃん本舗納品先ファイル　メンテ  *
      *    PRINTER TYPE    :  JIPS                                *
      *    SCREEN          :  ******                              *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-CRT.
             03  W-ACT       PIC  9(001).
      *
             03  W-STC       PIC  9(007).
             03  W-CTC       PIC  9(007).
             03  W-HP        PIC  X(001).
             03  W-CSC       PIC  9(007).
             03  W-NHSN      PIC  N(016).
      *
             03  W-STCD      PIC  9(007).
      *
             03  W-DMM       PIC  9(001).
      *
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-CHK          PIC  X(001).
           02  W-END          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITCM.
           COPY LIAHNH.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(012) VALUE
                "赤ちゃん本舗納品先　入力".
           02  FILLER  PIC  X(043) VALUE
                "登録=1 修正=2 削除=3 問合せ=5 終了=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "社店".
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(003) VALUE "納品先".
           02  FILLER.
             03  FILLER  PIC  X(008) VALUE "ｾﾝﾀｰｺｰﾄﾞ".
             03  FILLER  PIC  X(004) VALUE "ｾﾝﾀｰ".
             03  FILLER  PIC  N(001) VALUE "名".
             03  FILLER  PIC  X(004) VALUE "ｾﾝﾀｰ".
             03  FILLER  PIC  N(001) VALUE "は".
             03  FILLER  PIC  X(004) VALUE "ZERO".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "配送".
             03  FILLER  PIC  X(005) VALUE "ﾊﾟﾀｰﾝ".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｾﾝﾀｰ".
             03  FILLER  PIC  N(002) VALUE "のみ".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "直送".
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(003) VALUE "直送先".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｾﾝﾀｰ".
             03  FILLER  PIC  N(002) VALUE "のみ".
           02  FILLER  PIC  X(028) VALUE
                "確認（OK=1,NO=9）--->   ﾘﾀｰﾝ".
           02  FILLER.
             03  FILLER  PIC X(18)   VALUE "UND (07,07) (,22)_".
             03  FILLER  PIC X(18)   VALUE "UND (07,25) (,63)_".
           02  FILLER    PIC X(18)   VALUE "UND (07,07) (,22)_".
           02  FILLER    PIC X(18)   VALUE "UND (11,07) (,22)_".
           02  FILLER    PIC X(18)   VALUE "UND (13,07) (,22)_".
       01  C-MID2.
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(005) VALUE "納　品　先".
             03  FILLER  PIC  X(001) VALUE "P".
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(005) VALUE "直　送　先".
           02  FILLER  PIC  X(041) VALUE
                "ＮＥＸＴ:ﾘﾀｰﾝ , ｺｰﾄﾞ入力:F10 , 終了:F9   ".
           02  FILLER PIC X(37) 
                 VALUE "OVE (05,01) (,80)_ UND (05,01) (,80)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,08) (22)_".
           02  FILLER        PIC X(17)   VALUE "VER (05,42) (22)_".
           02  FILLER        PIC X(18)   VALUE "UND (05,01) (,80)_".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-STCD  PIC  9(007).
           02  FILLER.
             03  A-STC   PIC  9(007).
             03  A-NHSN  PIC  N(016).
           02  A-CTC   PIC  9(007).
           02  A-HP    PIC  X(001).
           02  A-CSC   PIC  9(007).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NHSN.
             03  FILLER  PIC  N(016).
           02  D-CSN.
             03  FILLER  PIC  N(024).
           02  D-MEI.
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  N(016).
             03  FILLER  PIC  X(001).
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  N(015).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME3   PIC  X(020) VALUE
                  "***  ｾﾝﾀｰ ﾐﾄｳﾛｸ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾁｮｸｿｳ ﾅｼ  ***".
             03  E-ME8   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME9   PIC  N(005) VALUE
                  "キャンセル".
             03  E-ME11  PIC  X(026) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  REWRITE ｴﾗｰ   ***".
             03  E-ME13  PIC  X(027) VALUE
                  "***  DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(007).
             03  E-CSC   PIC  X(007).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "67" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "28" "24" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "19" "43" "01C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "195" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" " " "7" "0" "14" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID1" "N" "7" "7" "4" " " "01C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID1" "X" "7" "11" "4" "0101C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-MID1" "N" "7" "25" "6" "0201C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" " " "9" "0" "24" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID1" "X" "9" "7" "8" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID1" "X" "9" "25" "4" "0102C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID1" "N" "9" "29" "2" "0202C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-MID1" "X" "9" "66" "4" "0302C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-MID1" "N" "9" "70" "2" "0402C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-MID1" "X" "9" "72" "4" "0502C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" " " "11" "0" "9" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID1" "N" "11" "7" "4" " " "03C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID1" "X" "11" "11" "5" "0103C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" " " "12" "0" "8" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0104C-MID1" "X" "12" "16" "4" " " "04C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID1" "N" "12" "20" "4" "0104C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" " " "13" "0" "14" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID1" "N" "13" "7" "4" " " "05C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205C-MID1" "X" "13" "11" "4" "0105C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0305C-MID1" "N" "13" "25" "6" "0205C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID1" " " "14" "0" "8" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0106C-MID1" "X" "14" "16" "4" " " "06C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0206C-MID1" "N" "14" "20" "4" "0106C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID1" "X" "23" "43" "28" "06C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID1" " " "7" "0" "0" "07C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID1" "A" "7" "7" "18" " " "08C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0208C-MID1" "A" "7" "25" "18" "0108C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID1" "A" "9" "7" "18" "08C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID1" "A" "11" "7" "18" "09C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID1" "A" "13" "7" "18" "10C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "157" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" " " "5" "0" "29" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID2" "X" "5" "3" "4" " " "01C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID2" "N" "5" "9" "10" "0101C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-MID2" "X" "5" "41" "1" "0201C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-MID2" "X" "5" "45" "4" "0301C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0501C-MID2" "N" "5" "51" "10" "0401C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" "X" "23" "25" "41" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID2" "A" "5" "1" "37" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID2" "A" "5" "8" "17" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID2" "A" "5" "42" "17" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID2" "A" "5" "1" "17" "05C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "6" "1" "7" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "7" "0" "39" "A-STCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STC" "9" "7" "16" "7" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STC" BY REFERENCE W-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NHSN" "N" "7" "32" "32" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NHSN" BY REFERENCE W-NHSN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CTC" "9" "9" "16" "7" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CTC" BY REFERENCE W-CTC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HP" "X" "11" "22" "1" "A-CTC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HP" BY REFERENCE W-HP "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CSC" "9" "13" "16" "7" "A-HP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CSC" BY REFERENCE W-CSC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "A-CSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "157" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NHSN" " " "0" "0" "32" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NHSN" "N" "9" "32" "32" " " "D-NHSN" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NHSN" BY REFERENCE AHNH-NHSN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CSN" " " "0" "0" "48" "D-NHSN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CSN" "N" "13" "32" "48" " " "D-CSN" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-CSN" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "77" "D-CSN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "9" "W-L" "1" "7" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE AHNH-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "N" "W-L" "9" "32" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE AHNH-NHSN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "X" "W-L" "41" "1" "02D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE AHNH-HP "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MEI" "9" "W-L" "43" "7" "03D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MEI" BY REFERENCE AHNH-CSC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MEI" "N" "W-L" "51" "30" "04D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-MEI" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "195" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "195" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "20" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "N" "24" "15" "10" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "27" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "7" "E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE AHNH-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CSC" "X" "24" "53" "7" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-CSC" BY REFERENCE TC-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           MOVE ZERO TO W-CRT.
      *
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "I-O" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-900
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3 AND 5
               GO TO M-040
           END-IF
           IF  W-ACT = 5
               GO TO M-200
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-STC < 3700000 OR > 3799999
               GO TO M-060
           END-IF
      *
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-080
           END-IF
           MOVE AHNH-NHSN TO W-NHSN.
           MOVE AHNH-CTC TO W-CTC.
           MOVE AHNH-HP TO W-HP.
           MOVE AHNH-CSC TO W-CSC.
           IF  W-CTC = ZERO
               MOVE SPACE TO AHNH-NHSN
           ELSE
               MOVE W-CTC TO AHNH-KEY
      *               READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE SPACE TO AHNH-NHSN
                   MOVE "センターなし" TO AHNH-NHSN
               END-IF
           END-IF
           MOVE W-CSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "直送なし" TO TC-NAME
           END-IF
           CALL "SD_Output" USING "A-NHSN" A-NHSN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CTC" A-CTC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HP" A-HP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CSC" A-CSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CSN" D-CSN "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-ACT = 3
               GO TO M-120
           END-IF
           GO TO M-090.
       M-080.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-090.
           CALL "SD_Accept" USING BY REFERENCE A-NHSN "A-NHSN" "N" "32"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-090
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-CTC "A-CTC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-090
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-CTC = ZERO
               MOVE SPACE TO AHNH-NHSN
               CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU
               GO TO M-105
           END-IF
           MOVE W-CTC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           MOVE AHNH-CSC TO W-CSC.
           MOVE W-CSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CSC" E-CSC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-100
           END-IF
           IF  W-CSC = ZERO
               MOVE SPACE TO W-HP
           END-IF
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CSC" A-CSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CSN" D-CSN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HP" A-HP "p" RETURNING RESU.
           IF  W-STC = W-CTC
               GO TO M-100
           END-IF
           GO TO M-120.
       M-105.
           CALL "SD_Accept" USING BY REFERENCE A-HP "A-HP" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-105
           END-IF.
       M-110.
           CALL "SD_Accept" USING BY REFERENCE A-CSC "A-CSC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-105
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-110
           END-IF
           IF  W-CSC < 0077000 OR > 0077999
               GO TO M-110
           END-IF
           MOVE W-CSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-110
           END-IF
           CALL "SD_Output" USING "D-CSN" D-CSN "p" RETURNING RESU.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-060
               ELSE
                   IF  W-CTC = ZERO
                       GO TO M-110
                   ELSE
                       GO TO M-100
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-120
           END-IF
      *
           IF  W-ACT NOT = 1
               GO TO M-140
           END-IF
           INITIALIZE AHNH-R.
           MOVE W-STC TO AHNH-KEY.
           MOVE W-NHSN TO AHNH-NHSN.
           MOVE W-CTC TO AHNH-CTC.
           MOVE W-CSC TO AHNH-CSC.
           MOVE W-HP TO AHNH-HP.
      *           WRITE AHNH-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            AHNHF_PNAME1 AHNHF_LNAME AHNH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-180.
       M-140.
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-ACT NOT = 2
               GO TO M-160
           END-IF
           MOVE W-NHSN TO AHNH-NHSN.
           MOVE W-CTC TO AHNH-CTC.
           MOVE W-CSC TO AHNH-CSC.
           MOVE W-HP TO AHNH-HP.
      *           REWRITE AHNH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            AHNHF_PNAME1 AHNHF_LNAME AHNH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-180.
       M-160.
      *           DELETE AHNHF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING AHNHF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF.
       M-180.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           GO TO M-060.
       M-200.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           MOVE 0 TO W-END.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
      *
      *           READ AHNHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" AHNHF_PNAME1 BY REFERENCE AHNH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-240
           END-IF.
       M-220.
           MOVE AHNH-CSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "直送なし" TO TC-NAME
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 23
               CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU
               GO TO M-280
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND ADV
               GO TO M-240
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  ESTAT = ADV
               GO TO M-250
           END-IF
           IF  W-END = 1
               GO TO M-260
           END-IF
           GO TO M-220.
       M-250.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           MOVE W-STCD TO AHNH-KEY.
      *           START AHNHF KEY NOT < AHNH-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AHNHF_PNAME1 "AHNH-KEY" " NOT < " AHNH-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-250
           END-IF
      *           READ AHNHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" AHNHF_PNAME1 BY REFERENCE AHNH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-250
           END-IF
           GO TO M-220.
       M-260.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           MOVE ZERO TO W-CRT.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           GO TO M-040.
       M-280.
      *           READ AHNHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" AHNHF_PNAME1 BY REFERENCE AHNH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-240
           END-IF
           GO TO M-220.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
