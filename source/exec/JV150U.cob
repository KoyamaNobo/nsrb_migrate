       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV150U.
      *****************************************************************
      *    PROGRAM         :  荷札・送り状ファイル　発行区分０セット  *
      *    PRINTER TYPE    :  JIPS                                    *
      *    SCREEN          :  ______                                  *
      *    COMPILE TYPE    :  COBOL                                   *
      *    JS-SIGN         :  0:荷札  ,  1:送り状                     *
      *    W-SEN           :  0:本社  ,  1:藤田  ,  2:早島            *
      *****************************************************************
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
       01  W-DATA.
           02  W-INV          PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SNO          PIC  9(006) VALUE 000000.
           02  W-ENO          PIC  9(006) VALUE 999999.
           02  W-JNI          PIC  9(007).
           02  W-OKJ          PIC  9(006).
           COPY LSTAT.
      *
           COPY L-JNIF.
           COPY LOKJF.
      *FD  JNIF-TAM
       01  JNIF-TAM_JV150U.
           02  JNIF-TAM_PNAME1    PIC  X(008) VALUE "JNIF-TAM".
           02  F                  PIC  X(001).
           02  JNIF-TAM_LNAME     PIC  X(015) VALUE "JNIF-TAM_JV150U".
           02  F                  PIC  X(001).
           02  JNIF-TAM_KEY1      PIC  X(100) VALUE SPACE.
           02  JNIF-TAM_SORT      PIC  X(100) VALUE SPACE.
           02  JNIF-TAM_IDLST     PIC  X(100) VALUE SPACE.
           02  JNIF-TAM_RES       USAGE  POINTER.
      *
       01  JNIFT1-R.
           02   JNIFT1-KEY.
                03    JNIFT1-01   PIC 9(6).
                03    JNIFT1-02   PIC 9(1).
           02   JNIFT1-03         PIC 9(6).
           02   JNIFT1-04.
                03  JNIFT1-041    PIC 9(2).
                03  JNIFT1-042    PIC 9(2).
                03  JNIFT1-043    PIC 9(2).
           02   JNIFT1-05.
                03  JNIFT1-051    PIC 9(4).
                03  JNIFT1-052    PIC 9(3).
           02   JNIFT1-06         PIC 9(1).
           02   JNIFT1-07         PIC 9(1).
           02   JNIFT1-08         PIC S9(3).
           02   JNIFT1-09    OCCURS  27.
                03  JNIFT1-091    PIC S9(3).
           02   JNIFT1-10         PIC 9(1).
           02   JNIFT1-11         PIC 9(1).
           02   JNIFT1-12         PIC 9(1).
           02   JNIFT1-13         PIC S9(3).
           02   JNIFT1-13A        PIC 9(1).
           02   JNIFT1-15         PIC 9(2).
           02   JNIFT1-14         PIC 9(6).
           02   FILLER            PIC X(1).
       01  JNIFT2-R.
           02   JNIFT2-KEY.
                03  JNIFT2-01     PIC 9(6).
                03  JNIFT2-02     PIC 9(1).
           02   JNIFT2-02A        PIC N(9).
           02   JNIFT2-03         PIC N(23).
           02   FILLER            PIC X(41).
           02   JNIFT2-04         PIC 9(1).
           02   JNIFT2-05         PIC 9(1).
           02   JNIFT2-06         PIC 9(1).
           02   JNIFT2-07         PIC S9(3).
           02   JNIFT2-07A        PIC 9(1).
           02   FILLER            PIC X(2).
           02   JNIFT2-08         PIC 9(6).
           02   FILLER            PIC X(1).
       77  F                      PIC X(1).
      *FD  JNIF-RYO
       01  JNIF-RYO_JV150U.
           02  JNIF-RYO_PNAME1    PIC  X(008) VALUE "JNIF-RYO".
           02  F                  PIC  X(001).
           02  JNIF-RYO_LNAME     PIC  X(015) VALUE "JNIF-RYO_JV150U".
           02  F                  PIC  X(001).
           02  JNIF-RYO_KEY1      PIC  X(100) VALUE SPACE.
           02  JNIF-RYO_SORT      PIC  X(100) VALUE SPACE.
           02  JNIF-RYO_IDLST     PIC  X(100) VALUE SPACE.
           02  JNIF-RYO_RES       USAGE  POINTER.
      *
       01  JNIFR1-R.
           02   JNIFR1-KEY.
                03    JNIFR1-01   PIC 9(6).
                03    JNIFR1-02   PIC 9(1).
           02   JNIFR1-03         PIC 9(6).
           02   JNIFR1-04.
                03  JNIFR1-041    PIC 9(2).
                03  JNIFR1-042    PIC 9(2).
                03  JNIFR1-043    PIC 9(2).
           02   JNIFR1-05.
                03  JNIFR1-051    PIC 9(4).
                03  JNIFR1-052    PIC 9(3).
           02   JNIFR1-06         PIC 9(1).
           02   JNIFR1-07         PIC 9(1).
           02   JNIFR1-08         PIC S9(3).
           02   JNIFR1-09    OCCURS  27.
                03  JNIFR1-091    PIC S9(3).
           02   JNIFR1-10         PIC 9(1).
           02   JNIFR1-11         PIC 9(1).
           02   JNIFR1-12         PIC 9(1).
           02   JNIFR1-13         PIC S9(3).
           02   JNIFR1-13A        PIC 9(1).
           02   JNIFR1-15         PIC 9(2).
           02   JNIFR1-14         PIC 9(6).
           02   FILLER            PIC X(1).
       01  JNIFR2-R.
           02   JNIFR2-KEY.
                03  JNIFR2-01     PIC 9(6).
                03  JNIFR2-02     PIC 9(1).
           02   JNIFR2-02A        PIC N(9).
           02   JNIFR2-03         PIC N(23).
           02   FILLER            PIC X(41).
           02   JNIFR2-04         PIC 9(1).
           02   JNIFR2-05         PIC 9(1).
           02   JNIFR2-06         PIC 9(1).
           02   JNIFR2-07         PIC S9(3).
           02   JNIFR2-07A        PIC 9(1).
           02   FILLER            PIC X(2).
           02   JNIFR2-08         PIC 9(6).
           02   FILLER            PIC X(1).
       77  F                      PIC X(1).
      ***********************************************
      *****                                     *****
      **   　　送　り　状　フ　ァ　イ　ル　　　　　**
      *****         ( O K J F  )  64/4          *****
      ***********************************************
      *FD  OKJF-TAM
       01  OKJF-TAM_JV150U.
           02  OKJF-TAM_PNAME1    PIC  X(008) VALUE "OKJF-TAM".
           02  F                  PIC  X(001).
           02  OKJF-TAM_LNAME     PIC  X(015) VALUE "OKJF-TAM_JV150U".
           02  F                  PIC  X(001).
           02  OKJF-TAM_KEY1      PIC  X(100) VALUE SPACE.
           02  OKJF-TAM_SORT      PIC  X(100) VALUE SPACE.
           02  OKJF-TAM_IDLST     PIC  X(100) VALUE SPACE.
           02  OKJF-TAM_RES       USAGE  POINTER.
       01  OKJFT-R.
           02  OKJFT-KEY.
               03  OKJFT-01 PIC 9(06).
           02  OKJFT-02     PIC 9(01).
           02  OKJFT-03     PIC 9(06).
           02  OKJFT-04     PIC 9(01).
           02  OKJFT-05     PIC 9(07).
           02  OKJFT-06     PIC N(09).
           02  OKJFT-07     PIC 9(03).
           02  OKJFT-08     PIC 9(01).
           02  OKJFT-09     PIC 9(01).
           02  OKJFT-10     PIC 9(01).
           02  OKJFT-11     PIC 9(05).
           02  OKJFT-12     PIC 9(06).
           02  OKJFT-13     PIC 9(01).
           02  F            PIC X(07).
       77  F                PIC X(01).
      *    *******************************
      *    *      印字サイン　 [OKJF-08] *
      *    *    0;  送り状未発行         *
      *    *    1;  送り状発行済　　　   *
      *    *******************************
      *    *    一般／教育区分 [OKJF-09] *
      *    *    0;   一般　　　　　      *
      *    *    1;   教育　　　 　　　   *
      *    *******************************
      *    *    更新済サイン　 [OKJF-10] *
      *    *    0;  未更新　　　　       *
      *    *    1;  更新済　　　　　　   *
      *    *******************************
      ***********************************************
      *****                                     *****
      **   　　送　り　状　フ　ァ　イ　ル　　　　　**
      *****         ( O K J F  )  64/4          *****
      ***********************************************
      *FD  OKJF-RYO
       01  OKJF-RYO_JV150U.
           02  OKJF-RYO_PNAME1    PIC  X(008) VALUE "OKJF-RYO".
           02  F                  PIC  X(001).
           02  OKJF-RYO_LNAME     PIC  X(015) VALUE "OKJF-RYO_JV150U".
           02  F                  PIC  X(001).
           02  OKJF-RYO_KEY1      PIC  X(100) VALUE SPACE.
           02  OKJF-RYO_SORT      PIC  X(100) VALUE SPACE.
           02  OKJF-RYO_IDLST     PIC  X(100) VALUE SPACE.
           02  OKJF-RYO_RES       USAGE  POINTER.
       01  OKJFR-R.
           02  OKJFR-KEY.
               03  OKJFR-01 PIC 9(06).
           02  OKJFR-02     PIC 9(01).
           02  OKJFR-03     PIC 9(06).
           02  OKJFR-04     PIC 9(01).
           02  OKJFR-05     PIC 9(07).
           02  OKJFR-06     PIC N(09).
           02  OKJFR-07     PIC 9(03).
           02  OKJFR-08     PIC 9(01).
           02  OKJFR-09     PIC 9(01).
           02  OKJFR-10     PIC 9(01).
           02  OKJFR-11     PIC 9(05).
           02  OKJFR-12     PIC 9(06).
           02  OKJFR-13     PIC 9(01).
           02  F            PIC X(07).
       77  F                PIC X(01).
      *    *******************************
      *    *      印字サイン　 [OKJF-08] *
      *    *    0;  送り状未発行         *
      *    *    1;  送り状発行済　　　   *
      *    *******************************
      *    *    一般／教育区分 [OKJF-09] *
      *    *    0;   一般　　　　　      *
      *    *    1;   教育　　　 　　　   *
      *    *******************************
      *    *    更新済サイン　 [OKJF-10] *
      *    *    0;  未更新　　　　       *
      *    *    1;  更新済　　　　　　   *
      *    *******************************
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID0.
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　荷札Ｆ　発行区分０セット　　＊＊＊".
           02  FILLER  PIC  X(031) VALUE
                  "本社=0 , 藤田=1 , 早島=2   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(028) VALUE
                  "荷札番号    000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　送り状Ｆ　発行区分０セット　　＊＊＊".
           02  FILLER  PIC  X(031) VALUE
                  "本社=0 , 藤田=1 , 早島=2   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(028) VALUE
                  "送り状番号  000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SNO   PIC  9(006).
             03  A-ENO   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  OKJF ﾅｼ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  OKJF REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  JNIF ﾅｼ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  JNIF REWRITE ｴﾗｰ  ***".
             03  E-JNI   PIC  9(007).
             03  E-OKJ   PIC  9(006).
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
      *C-MID0
       CALL "SD_Init" USING 
            "C-MID0" " " "0" "0" "125" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID0" "N" "1" "15" "44" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID0" "X" "12" "21" "31" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID0" "X" "15" "23" "28" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID0" "X" "22" "30" "22" "03C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "127" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "1" "14" "46" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "12" "21" "31" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "X" "15" "23" "28" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "X" "22" "30" "22" "03C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "12" "47" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "15" "0" "12" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNO" "9" "15" "35" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNO" BY REFERENCE W-SNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENO" "9" "15" "45" "6" "A-SNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENO" BY REFERENCE W-ENO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "116" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JNI" "9" "24" "50" "7" "E-ME8" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JNI" BY REFERENCE W-JNI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-OKJ" "9" "24" "50" "6" "E-JNI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-OKJ" BY REFERENCE W-OKJ "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID0" C-MID0 "p" RETURNING RESU
           ELSE
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "C-MID1" C-MID1 "p" RETURNING RESU
               END-IF
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 2
               GO TO M-10
           END-IF.
       M-15.
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
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           IF  JS-SIGN NOT = 0
               GO TO M-30
           END-IF
           IF  W-SEN = 0
               CALL "DB_F_Open" USING
                "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
                "JNIF1-KEY" BY REFERENCE JNIF1-KEY
           ELSE
               IF  W-SEN = 1
                   CALL "DB_F_Open" USING
                    "I-O" JNIF-TAM_PNAME1 "SHARED" BY REFERENCE
                    JNIF-TAM_IDLST "1" "JNIFT1-KEY" BY REFERENCE
                    JNIFT1-KEY
               ELSE
                   IF  W-SEN = 2
                       CALL "DB_F_Open" USING
                        "I-O" JNIF-RYO_PNAME1 "SHARED" BY REFERENCE
                        JNIF-RYO_IDLST "1" "JNIFR1-KEY" BY REFERENCE
                        JNIFR1-KEY
                   END-IF
               END-IF
           END-IF
           MOVE 0 TO W-INV.
      *
           IF  W-SEN = 0
               PERFORM S-05 THRU S-10
           END-IF
           IF  W-SEN = 1
               PERFORM S-15 THRU S-20
           END-IF
           IF  W-SEN = 2
               PERFORM S-25 THRU S-30
           END-IF
           IF  W-INV = 0
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-90.
       M-30.
           IF  W-SEN = 0
               CALL "DB_F_Open" USING
                "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
                "OKJF-KEY" BY REFERENCE OKJF-KEY
           ELSE
               IF  W-SEN = 1
                   CALL "DB_F_Open" USING
                    "I-O" OKJF-TAM_PNAME1 "SHARED" BY REFERENCE
                    OKJF-TAM_IDLST "1" "OKJFT-KEY" BY REFERENCE
                    OKJFT-KEY
               ELSE
                   IF  W-SEN = 2
                       CALL "DB_F_Open" USING
                        "I-O" OKJF-RYO_PNAME1 "SHARED" BY REFERENCE
                        OKJF-RYO_IDLST "1" "OKJFR-KEY" BY REFERENCE
                        OKJFR-KEY
                   END-IF
               END-IF
           END-IF
           MOVE 0 TO W-INV.
      *
           IF  W-SEN = 0
               PERFORM S-55 THRU S-60
           END-IF
           IF  W-SEN = 1
               PERFORM S-65 THRU S-70
           END-IF
           IF  W-SEN = 2
               PERFORM S-75 THRU S-80
           END-IF
           IF  W-INV = 0
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-90.
           IF  JS-SIGN = 0
               IF  W-SEN = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE JNIF_IDLST JNIF_PNAME1
               ELSE
                   IF  W-SEN = 1
                       CALL "DB_F_Close" USING
                        BY REFERENCE JNIF-TAM_IDLST JNIF-TAM_PNAME1
                   ELSE
                       IF  W-SEN = 2
                           CALL "DB_F_Close" USING
                            BY REFERENCE JNIF-RYO_IDLST JNIF-RYO_PNAME1
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SEN = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE OKJF_IDLST OKJF_PNAME1
               ELSE
                   IF  W-SEN = 1
                       CALL "DB_F_Close" USING
                        BY REFERENCE OKJF-TAM_IDLST OKJF-TAM_PNAME1
                   ELSE
                       IF  W-SEN = 2
                           CALL "DB_F_Close" USING
                            BY REFERENCE OKJF-RYO_IDLST OKJF-RYO_PNAME1
                       END-IF
                   END-IF
               END-IF
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
      *           READ JNIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF1-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-10
           END-IF
           IF  JNIF1-10 NOT = 1
               GO TO S-05
           END-IF
           IF  JNIF1-01 < W-SNO
               GO TO S-05
           END-IF
           IF  JNIF1-01 > W-ENO
               GO TO S-10
           END-IF
      *
           MOVE 0 TO JNIF1-10.
      *           REWRITE JNIF1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE JNIF1-KEY TO W-JNI
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JNI" E-JNI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-05.
       S-10.
           EXIT.
       S-15.
      *           READ JNIF-TAM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF-TAM_PNAME1 BY REFERENCE JNIFT1-R
            " " RETURNING RET.
           IF  RET = 1
               GO TO S-20
           END-IF
           IF  JNIFT1-10 NOT = 1
               GO TO S-15
           END-IF
           IF  JNIFT1-01 < W-SNO
               GO TO S-15
           END-IF
           IF  JNIFT1-01 > W-ENO
               GO TO S-20
           END-IF
      *
           MOVE 0 TO JNIFT1-10.
      *           REWRITE JNIFT1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF-TAM_PNAME1 JNIF-TAM_LNAME JNIFT1-R RETURNING RET.
           IF  RET = 1
               MOVE JNIFT1-KEY TO W-JNI
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JNI" E-JNI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-15.
       S-20.
           EXIT.
       S-25.
      *           READ JNIF-RYO NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF-RYO_PNAME1 BY REFERENCE JNIFR1-R
            " " RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
           IF  JNIFR1-10 NOT = 1
               GO TO S-25
           END-IF
           IF  JNIFR1-01 < W-SNO
               GO TO S-25
           END-IF
           IF  JNIFR1-01 > W-ENO
               GO TO S-30
           END-IF
      *
           MOVE 0 TO JNIFR1-10.
      *           REWRITE JNIFR1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF-RYO_PNAME1 JNIF-RYO_LNAME JNIFR1-R RETURNING RET.
           IF  RET = 1
               MOVE JNIFR1-KEY TO W-JNI
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JNI" E-JNI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-25.
       S-30.
           EXIT.
       S-55.
      *           READ OKJF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-60
           END-IF
           IF  OKJF-08 NOT = 1
               GO TO S-55
           END-IF
           IF  OKJF-01 < W-SNO
               GO TO S-55
           END-IF
           IF  OKJF-01 > W-ENO
               GO TO S-60
           END-IF
      *
           MOVE 0 TO OKJF-08.
      *           REWRITE OKJF-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE OKJF-KEY TO W-OKJ
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKJ" E-OKJ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-55.
       S-60.
           EXIT.
       S-65.
      *           READ OKJF-TAM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF-TAM_PNAME1 BY REFERENCE OKJFT-R
            " " RETURNING RET.
           IF  RET = 1
               GO TO S-70
           END-IF
           IF  OKJFT-08 NOT = 1
               GO TO S-65
           END-IF
           IF  OKJFT-01 < W-SNO
               GO TO S-65
           END-IF
           IF  OKJFT-01 > W-ENO
               GO TO S-70
           END-IF
      *
           MOVE 0 TO OKJFT-08.
      *           REWRITE OKJFT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF-TAM_PNAME1 OKJF-TAM_LNAME OKJFT-R RETURNING RET.
           IF  RET = 1
               MOVE OKJFT-KEY TO W-OKJ
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKJ" E-OKJ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-65.
       S-70.
           EXIT.
       S-75.
      *           READ OKJF-RYO NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF-RYO_PNAME1 BY REFERENCE OKJFR-R
            " " RETURNING RET.
           IF  RET = 1
               GO TO S-80
           END-IF
           IF  OKJFR-08 NOT = 1
               GO TO S-75
           END-IF
           IF  OKJFR-01 < W-SNO
               GO TO S-75
           END-IF
           IF  OKJFR-01 > W-ENO
               GO TO S-80
           END-IF
      *
           MOVE 0 TO OKJFR-08.
      *           REWRITE OKJFR-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF-RYO_PNAME1 OKJF-RYO_LNAME OKJFR-R RETURNING RET.
           IF  RET = 1
               MOVE OKJFR-KEY TO W-OKJ
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKJ" E-OKJ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
           END-IF
           GO TO S-75.
       S-80.
           EXIT.
