       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRM010.
      *********************************************************
      *    PROGRAM         :  財務残高表マスタ　メンテナンス  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCPR01                          *
      *        変更　　　  :  92/11/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-20K            PIC  X(005) VALUE X"1A24212474".
       01  HEAD1.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(016) VALUE SPACE.
           02  F            PIC  N(024) VALUE
                "＊＊＊　　残高表マスタ　プルーフリスト　　＊＊＊".
           02  F            PIC  X(006) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC  Z(002).
       01  HEAD2.
           02  F            PIC  N(004) VALUE "頁区分　".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(006) VALUE "見出し科目　".
           02  F            PIC  X(015) VALUE SPACE.
           02  F            PIC  N(006) VALUE "明細科目①　".
           02  F            PIC  X(018) VALUE SPACE.
           02  F            PIC  N(006) VALUE "明細科目②　".
           02  F            PIC  X(011) VALUE SPACE.
       01  W-P.
           02  P-15K        PIC  X(005).
           02  F            PIC  X(002).
           02  P-NO         PIC  9(002).
           02  F            PIC  X(002).
           02  P-KAK        PIC  9(004).
           02  F            PIC  X(001).
           02  P-KANA       PIC  N(010).
           02  F            PIC  X(002).
           02  P-KAK1       PIC  9(004).
           02  P-V1         PIC  X(001).
           02  P-HOK1       PIC  9(004).
           02  F            PIC  X(001).
           02  P-KANA1      PIC  N(010).
           02  F            PIC  X(002).
           02  P-KAK2       PIC  9(004).
           02  P-V2         PIC  X(001).
           02  P-HOK2       PIC  9(004).
           02  F            PIC  X(001).
           02  P-KANA2      PIC  N(010).
           02  P-20K        PIC  X(005).
       01  W-R.
           02  W-KEY.
             03  W-NO       PIC  9(002).
             03  W-KAK      PIC  9(004).
             03  W-DATA1.
               04  W-KAK1   PIC  9(004).
               04  W-HOK1   PIC  9(004).
             03  W-DATA2.
               04  W-KAK2   PIC  9(004).
               04  W-HOK2   PIC  9(004).
           02  F            PIC  X(010).
       01  W-D.
           02  W-ACT        PIC  9(001).
           02  W-NOD        PIC  9(002) VALUE ZERO.
           02  W-KAKD       PIC  9(004) VALUE ZERO.
           02  W-DMM        PIC  9(001).
           02  W-SENO.
             03  W-SNO      PIC  9(002).
             03  W-ENO      PIC  9(002) VALUE 99.
           02  CHK.
             03  CHK1       PIC  9(001).
             03  CHK2       PIC  9(001).
           02  W-CC         PIC  9(001).
           02  W-DC         PIC  9(001).
           02  W-PC         PIC  9(001) VALUE ZERO.
           02  W-PAGE       PIC  9(002) VALUE ZERO.
       01  ERR-STAT         PIC  X(002).
      *
           COPY LSTAT.
           COPY KANGEL.
           COPY LSPF.
      *
       01  ZAN-K_PRM010.
           02  ZAN-K_PNAME1  PIC  X(005)  VALUE "ZAN-K".
           02  F             PIC  X(001).
           02  ZAN-K_LNAME   PIC  X(012)  VALUE "ZAN-K_PRM010".
           02  F             PIC  X(001).
           02  ZAN-K_KEY1    PIC  X(100)  VALUE SPACE.
           02  ZAN-K_KEY2    PIC  X(100)  VALUE SPACE.
           02  ZAN-K_SORT    PIC  X(100)  VALUE SPACE.
           02  ZAN-K_IDLST   PIC  X(100)  VALUE SPACE.
           02  ZAN-K_RES     USAGE  POINTER.
       01  ZAN-R.
           02  ZAN-KEY.
             03  ZAN-NO     PIC  9(002).
             03  ZAN-KAK    PIC  9(004).
             03  ZAN-DATA1.
               04  ZAN-KAK1 PIC  9(004).
               04  ZAN-HOK1 PIC  9(004).
             03  ZAN-DATA2.
               04  ZAN-KAK2 PIC  9(004).
               04  ZAN-HOK2 PIC  9(004).
           02  F            PIC  X(010).
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
           02  C-CL     PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT     PIC  9(001).
           02  FILLER.
             03  A-NO      PIC  9(002).
           02  A-KAK     PIC  9(004).
           02  A-KAK1    PIC  9(004).
           02  A-HOK1    PIC  9(004).
           02  A-KAK2    PIC  9(004).
           02  A-HOK2    PIC  9(004).
           02  FILLER.
             03  A-SNO     PIC  9(002).
             03  A-ENO     PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-KANA    PIC  N(010).
           02  D-KANA1   PIC  N(010).
           02  D-HONA1   PIC  N(010).
           02  D-KANA2   PIC  N(010).
           02  D-HONA2   PIC  N(010).
           02  FILLER.
             03  S-KAK     PIC  X(004) VALUE "    ".
             03  S-KANA    PIC  X(020) VALUE
                  "                    ".
           02  FILLER.
             03  S-KAK1    PIC  X(004) VALUE "    ".
             03  S-KANA1   PIC  X(020) VALUE
                  "                    ".
           02  FILLER.
             03  S-HOK1    PIC  X(004) VALUE "    ".
             03  S-HONA1   PIC  X(020) VALUE
                  "                    ".
           02  FILLER.
             03  S-KAK2    PIC  X(004) VALUE "    ".
             03  S-KANA2   PIC  X(020) VALUE
                  "                    ".
           02  FILLER.
             03  S-HOK2    PIC  X(004) VALUE "    ".
             03  S-HONA2   PIC  X(020) VALUE
                  "                    ".
           02  FILLER.
             03  D-PM      PIC  X(030) VALUE
                  "［　頁区分 00 より 99 まで　］".
             03  D-PMC     PIC  X(030) VALUE
                  "                              ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT    PIC  X(002).
             03  E-ME1     PIC  X(017) VALUE
                  "***  ﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME2     PIC  X(017) VALUE
                  "***  ﾄｳﾛｸｽﾞﾐ  ***".
             03  E-ME3     PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME4     PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME5     PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
             03  E-ME6     PIC  X(024) VALUE
                  "***  漢字科目　なし  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-CL      PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "3" "52" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "6" "0" "2" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-NO" "9" "6" "31" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-NO" BY REFERENCE W-NOD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-KAK" "9" "7" "31" "4" "01C-ACP" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-KAK" BY REFERENCE W-KAKD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-KAK1" "9" "10" "31" "4" "A-KAK" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-KAK1" BY REFERENCE W-KAK1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-HOK1" "9" "11" "31" "4" "A-KAK1" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-HOK1" BY REFERENCE W-HOK1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-KAK2" "9" "13" "31" "4" "A-HOK1" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-KAK2" BY REFERENCE W-KAK2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-HOK2" "9" "14" "31" "4" "A-KAK2" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-HOK2" BY REFERENCE W-HOK2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "17" "0" "4" "A-HOK2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNO" "9" "17" "34" "2" " " "02C-ACP"
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNO" BY REFERENCE W-SNO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENO" "9" "17" "42" "2" "A-SNO" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENO" BY REFERENCE W-ENO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "44" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-KANA" "N" "7" "36" "10" " " "C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KANA" BY REFERENCE KNGNMN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KANA1" "N" "10" "36" "10" "D-KANA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KANA1" BY REFERENCE KNGNMN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-HONA1" "N" "11" "36" "10" "D-KANA1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-HONA1" BY REFERENCE KNGNMN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KANA2" "N" "13" "36" "10" "D-HONA1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KANA2" BY REFERENCE KNGNMN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-HONA2" "N" "14" "36" "10" "D-KANA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-HONA2" BY REFERENCE KNGNMN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "7" "0" "24" "D-HONA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KAK" "X" "7" "31" "4" " " "01C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KANA" "X" "7" "36" "20" "S-KAK" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02C-DSP" " " "10" "0" "24" "01C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KAK1" "X" "10" "31" "4" " " "02C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KANA1" "X" "10" "36" "20" "S-KAK1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03C-DSP" " " "11" "0" "24" "02C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-HOK1" "X" "11" "31" "4" " " "03C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-HONA1" "X" "11" "36" "20" "S-HOK1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04C-DSP" " " "13" "0" "24" "03C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KAK2" "X" "13" "31" "4" " " "04C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-KANA2" "X" "13" "36" "20" "S-KAK2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05C-DSP" " " "14" "0" "24" "04C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-HOK2" "X" "14" "31" "4" " " "05C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "S-HONA2" "X" "14" "36" "20" "S-HOK2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06C-DSP" " " "17" "0" "60" "05C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "D-PM" "X" "17" "23" "30" " " "06C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "D-PMC" "X" "17" "23" "30" "D-PM" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "180" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "180" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"
            RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" "E-STAT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "19" "E-ME2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "21" "E-ME3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "20" "E-ME4" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "24" "E-ME5" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME6" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " "
            RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" ZAN-K_PNAME1 "SHARED" BY REFERENCE ZAN-K_IDLST "1"
            "ZAN-KEY" BY REFERENCE ZAN-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "SD_Screen_Output" USING "SCPR01" RETURNING RESU.
       M-040.
           CALL "SD_Output" USING "S-KAK" S-KAK "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KANA" S-KANA "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KAK1" S-KAK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KANA1" S-KANA1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HOK1" S-HOK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HONA1" S-HONA1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KAK2" S-KAK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KANA2" S-KANA2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HOK2" S-HOK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HONA2" S-HONA2 "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF.
           IF  W-ACT = 9
               GO TO M-980
           END-IF.
           IF  W-ACT = 4
               CALL "SD_Output" USING "D-PM" D-PM "p"
                                         RETURNING RESU
               GO TO M-380
           END-IF.
           IF  W-ACT NOT = 1 AND 3
               GO TO M-040
           END-IF.
       M-080.
           IF  W-NOD NOT = ZERO
               CALL "SD_Output" USING "A-NO" A-NO "p"
                                         RETURNING RESU
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF.
           IF  W-NOD = ZERO
               GO TO M-080
           END-IF.
           IF  W-ACT = 1
               GO TO M-140
           END-IF.
           MOVE 0 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-080
           END-IF.
       M-140.
           IF  W-KAKD NOT = ZERO
               CALL "SD_Output" USING "A-KAK" A-KAK "p"
                                         RETURNING RESU
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-KAK "A-KAK" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-080
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF.
           IF  W-KAKD = ZERO
               GO TO M-140
           END-IF.
           MOVE ZERO TO KNG-KEY.
           MOVE W-KAKD TO K-ACCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME6" E-ME6 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-140
           END-IF.
           CALL "SD_Output" USING "D-KANA" D-KANA "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KAK1" S-KAK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KANA1" S-KANA1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HOK1" S-HOK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HONA1" S-HONA1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KAK2" S-KAK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-KANA2" S-KANA2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HOK2" S-HOK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "S-HONA2" S-HONA2 "p"
                                         RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE W-NOD TO W-NO.
           MOVE W-KAKD TO W-KAK.
           MOVE 1 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-KAK1 "A-KAK1"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF.
           IF  W-KAK1 = ZERO
               MOVE W-KAK TO W-KAK1
               CALL "SD_Output" USING "A-KAK1" A-KAK1 "p"
                                         RETURNING RESU
           END-IF.
           MOVE ZERO TO KNG-KEY.
           MOVE W-KAK1 TO K-ACCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME6" E-ME6 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-160
           END-IF.
           CALL "SD_Output" USING "A-KAK1" A-KAK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-KANA1" D-KANA1 "p"
                                         RETURNING RESU.
           MOVE 2 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-HOK1 "A-HOK1" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF.
           IF  W-HOK1 = ZERO
               CALL "SD_Output" USING "A-HOK1" A-HOK1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "S-HONA1" S-HONA1 "p"
                                         RETURNING RESU
               GO TO M-200
           END-IF.
           MOVE W-KAK1 TO K-ACCD.
           MOVE W-HOK1 TO K-HOCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME6" E-ME6 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-180
           END-IF.
           CALL "SD_Output" USING "A-HOK1" A-HOK1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-HONA1" D-HONA1 "p"
                                         RETURNING RESU.
       M-200.
           MOVE 3 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-180
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-KAK2 "A-KAK2" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF.
           IF  W-KAK2 = ZERO
               CALL "SD_Output" USING "A-KAK2" A-KAK2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "S-KANA2" S-KANA2 "p"
                                         RETURNING RESU
               GO TO M-240
           END-IF.
           MOVE ZERO TO KNG-KEY.
           MOVE W-KAK2 TO K-ACCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME6" E-ME6 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-220
           END-IF.
           CALL "SD_Output" USING "A-KAK2" A-KAK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-KANA2" D-KANA2 "p"
                                         RETURNING RESU.
       M-240.
           MOVE 4 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-220
           END-IF.
           IF  W-KAK2 NOT = ZERO
               IF  W-KAK1 NOT = W-KAK2
                   GO TO M-220
               END-IF
           END-IF.
           IF  W-KAK2 = ZERO
               MOVE ZERO TO W-HOK2
               CALL "SD_Output" USING "A-HOK2" A-HOK2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "S-HONA2" S-HONA2 "p"
                                         RETURNING RESU
               GO TO M-300
           END-IF.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-HOK2 "A-HOK2" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF.
           IF  W-HOK2 = ZERO
               CALL "SD_Output" USING "A-HOK2" A-HOK2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "S-HONA2" S-HONA2 "p"
                                         RETURNING RESU
               GO TO M-280
           END-IF.
           MOVE W-KAK2 TO K-ACCD.
           MOVE W-HOK2 TO K-HOCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME6" E-ME6 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-260
           END-IF.
           CALL "SD_Output" USING "A-HOK2" A-HOK2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-HONA2" D-HONA2 "p"
                                         RETURNING RESU.
       M-280.
           MOVE 5 TO W-CC.
           PERFORM S-60 THRU S-65.
           IF  W-DC = 9
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-260
           END-IF.
       M-300.
           MOVE W-KEY TO ZAN-KEY.
      *           READ ZAN-K INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ZAN-K_PNAME1 BY REFERENCE ZAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-320
           END-IF.
           IF  W-ACT = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-260
           END-IF.
           GO TO M-440.
       M-320.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-260
           END-IF.
           GO TO M-440.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-SNO "A-SNO" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
               CALL "SD_Output" USING "D-PMC" D-PMC "p"
                                         RETURNING RESU
               GO TO M-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF.
       M-400.
           CALL "SD_Accept" USING BY REFERENCE A-ENO "A-ENO" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-380
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-400
           END-IF.
           IF  W-SNO > W-ENO
               GO TO M-400
           END-IF.
       M-440.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 4
                   GO TO M-400
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-ACT NOT = 4
                   IF  W-HOK2 = ZERO
                       GO TO M-220
                   ELSE
                       GO TO M-260
                   END-IF
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-440
           END-IF.
           IF  W-DMM = 9
               GO TO M-140
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-440
           END-IF.
           IF  W-ACT NOT = 1
               GO TO M-520
           END-IF.
           MOVE W-R TO ZAN-R.
      *           WRITE ZAN-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            ZAN-K_PNAME1 ZAN-K_LNAME ZAN-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-980
           END-IF.
           MOVE ZERO TO ZAN-KEY.
           MOVE W-NOD TO ZAN-NO.
      *           START ZAN-K KEY NOT < ZAN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            ZAN-K_PNAME1 "ZAN-KEY" " NOT < " ZAN-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-540
           END-IF.
       M-500.
      *           READ ZAN-K NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-540
           END-IF.
           IF  W-NOD NOT = ZAN-NO
               GO TO M-540
           END-IF.
      *           REWRITE ZAN-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            ZAN-K_PNAME1 ZAN-K_LNAME ZAN-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME4" E-ME4 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-980
           END-IF.
           GO TO M-500.
       M-520.
           IF  W-ACT NOT = 3
               GO TO M-620
           END-IF.
      *           DELETE ZAN-K INVALID KEY
      *///////////////
           CALL "DB_Delete" USING ZAN-K_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME5" E-ME5 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-980
           END-IF.
       M-540.
           CALL "SD_Screen_Output" USING "SCPR01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-NO" A-NO "p"
                                         RETURNING RESU.
           GO TO M-140.
       M-620.
           MOVE ZERO TO ZAN-KEY.
           MOVE W-SNO TO ZAN-NO.
      *           START ZAN-K KEY NOT < ZAN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            ZAN-K_PNAME1 "ZAN-KEY" " NOT < " ZAN-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-380
           END-IF
      *           READ ZAN-K NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-380
           END-IF.
           IF  ZAN-NO > W-ENO
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-380
           END-IF.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               ACCEPT H-DATE FROM DATE
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF.
           PERFORM S-20 THRU S-55.
           CALL "SD_Output" USING "D-PMC" D-PMC "p"
                                         RETURNING RESU.
           GO TO M-040.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE ZAN-K_IDLST ZAN-K_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO CHK1.
           MOVE ZAN-NO TO W-NO.
       S-25.
           MOVE ZERO TO CHK2.
           MOVE ZAN-KAK TO W-KAK.
       S-30.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-KANA P-KANA1 P-KANA2.
           IF  CHK1 = 0
               MOVE 5 TO CHK
               MOVE ZAN-NO TO P-NO
           END-IF.
           IF  CHK2 = 5
               GO TO S-35
           END-IF.
           MOVE 5 TO CHK2.
           MOVE ZERO TO KNG-KEY.
           MOVE ZAN-KAK TO K-ACCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　マスター　なし　　" TO KNGNMN
           END-IF.
           MOVE ZAN-KAK TO P-KAK.
           MOVE KNGNMN TO P-KANA.
       S-35.
           IF  ZERO = ZAN-KAK1 AND ZAN-HOK1
               GO TO S-40
           END-IF.
           MOVE ZAN-KAK1 TO K-ACCD.
           MOVE ZAN-HOK1 TO K-HOCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　マスター　なし　　" TO KNGNMN
           END-IF.
           MOVE ZAN-KAK1 TO P-KAK1.
           MOVE "-" TO P-V1.
           MOVE ZAN-HOK1 TO P-HOK1.
           MOVE KNGNMN TO P-KANA1.
       S-40.
           IF  ZERO = ZAN-KAK2 AND ZAN-HOK2
               GO TO S-45
           END-IF.
           MOVE ZAN-KAK2 TO K-ACCD.
           MOVE ZAN-HOK2 TO K-HOCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　マスター　なし　　" TO KNGNMN
           END-IF.
           MOVE ZAN-KAK2 TO P-KAK2.
           MOVE "-" TO P-V2.
           MOVE ZAN-HOK2 TO P-HOK2.
           MOVE KNGNMN TO P-KANA2.
       S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO S-50
           END-IF.
           PERFORM S-05 THRU S-10.
           MOVE ZAN-NO TO P-NO.
           MOVE ZERO TO KNG-KEY.
           MOVE ZAN-KAK TO K-ACCD.
      *           READ KNG WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　マスター　なし　　" TO KNGNMN
           END-IF.
           MOVE ZAN-KAK TO P-KAK.
           MOVE KNGNMN TO P-KANA.
       S-50.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *           READ ZAN-K NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF.
           IF  ZAN-NO > W-ENO
               GO TO S-55
           END-IF.
           IF  ZAN-NO NOT = W-NO
               GO TO S-20
           END-IF.
           IF  ZAN-KAK NOT = W-KAK
               GO TO S-25
           END-IF.
           GO TO S-30.
       S-55.
           EXIT.
       S-60.
           MOVE 0 TO W-DC.
           IF  W-ACT = 1
               GO TO S-65
           END-IF.
           MOVE ZERO TO ZAN-KEY.
           IF  W-CC > 0
               MOVE W-NO TO ZAN-NO
               MOVE W-KAK TO ZAN-KAK
           ELSE
               MOVE W-NOD TO ZAN-NO
           END-IF.
           IF  W-CC > 1
               MOVE W-KAK1 TO ZAN-KAK1
           END-IF.
           IF  W-CC > 2
               MOVE W-HOK1 TO ZAN-HOK1
           END-IF.
           IF  W-CC > 3
               MOVE W-KAK2 TO ZAN-KAK2
           END-IF.
           IF  W-CC > 4
               MOVE W-HOK2 TO ZAN-HOK2
           END-IF.
      *           START ZAN-K KEY NOT < ZAN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            ZAN-K_PNAME1 "ZAN-KEY" " NOT < " ZAN-KEY RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-DC
               GO TO S-65
           END-IF
      *           READ ZAN-K NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-DC
               GO TO S-65
           END-IF.
           IF  W-CC = 0
               IF  W-NOD NOT = ZAN-NO
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
           IF  W-CC = 1
               IF (W-NO NOT = ZAN-NO) OR (W-KAK NOT = ZAN-KAK)
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
           IF  W-CC = 2
               IF (W-NO NOT = ZAN-NO) OR (W-KAK NOT = ZAN-KAK) OR
                  (W-KAK1 NOT = ZAN-KAK1)
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
           IF  W-CC = 3
               IF (W-NO NOT = ZAN-NO) OR (W-KAK NOT = ZAN-KAK) OR
                  (W-KAK1 NOT = ZAN-KAK1) OR (W-HOK1 NOT = ZAN-HOK1)
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
           IF  W-CC = 4
               IF (W-NO NOT = ZAN-NO) OR (W-KAK NOT = ZAN-KAK) OR
                  (W-KAK1 NOT = ZAN-KAK1) OR (W-HOK1 NOT = ZAN-HOK1) OR
                  (W-KAK2 NOT = ZAN-KAK2)
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
           IF  W-CC = 5
               IF (W-NO NOT = ZAN-NO) OR (W-KAK NOT = ZAN-KAK) OR
                  (W-KAK1 NOT = ZAN-KAK1) OR (W-HOK1 NOT = ZAN-HOK1) OR
                  (W-KAK2 NOT = ZAN-KAK2) OR (W-HOK2 NOT = ZAN-HOK2)
                   MOVE 9 TO W-DC
               END-IF
           END-IF.
       S-65.
           EXIT.
