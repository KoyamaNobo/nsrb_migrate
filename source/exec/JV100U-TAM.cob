       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV100U.
      ************************************************************
      *    PROGRAM         :  荷札変換用ファイル作成             *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  ______                             *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-14           PIC  9(6).                                ｵｸﾘｼﾞｮｳ
           02  W-BAN          PIC  9(6).
           02  W-08           PIC  9(3).                                ｺｽｳ
           02  W-08R     PIC 9(3).
           02  W-NOC     PIC 9(3).
           02  W-GYO     PIC 9(4).
           02  W-NGP          PIC 9(8).
           02  W-NGPL  REDEFINES W-NGP.
             03  W-NEN1        PIC 9(2).
             03  W-NGPS        PIC 9(6).
           02  W-TIM          PIC 9(8).
           02  W-TIMD  REDEFINES W-TIM.
             03  W-TIME        PIC 9(6).
             03  F             PIC 9(2).
           02  W-DJAN.
             03  W-D1           PIC 9.
             03  W-KJAN.
               04  W-D2         PIC 9.
               04  W-D3         PIC 9.
               04  W-D4         PIC 9.
               04  W-D5         PIC 9.
               04  W-D6         PIC 9.
               04  W-D7         PIC 9.
             03  W-D8           PIC 9.
           02  W-01           PIC  9(6).
       01  W-KDATA.
           02  W-K1             PIC 9(3).
           02  W-K2             PIC 9(3).
           02  W-K3             PIC 9.
           02  KEISAND          PIC 9(3).
           02  KEISAN   REDEFINES  KEISAND.
               03  K-D1         PIC 9(2).
               03  K-D2         PIC 9(1).
      *
           COPY L-JNIF-TAM.
      *FD  WJNIF                                                        ﾆﾌﾀﾞﾄﾗﾝ
       01  WJNIF_JV100U.
           02  WJNIF_PNAME1   PIC X(007) VALUE "NIFUDAW".
           02  F              PIC X(001).
           02  WJNIF_LNAME    PIC X(012) VALUE "WJNIF_JV100U".
           02  F              PIC X(001).
           02  WJNIF_KEY1     PIC X(100) VALUE SPACE.
           02  WJNIF_SORT     PIC X(100) VALUE SPACE.
           02  WJNIF_IDLST    PIC X(100) VALUE SPACE.
           02  WJNIF_RES      USAGE  POINTER.
       01  WJNIF-R.
           02  WJNIF-KEY.                                               KEY
               03  WJNIF-01   PIC 9(6).                                 ﾃﾞﾝﾋﾟｮｳN
               03  WJNIF-02   PIC 9(1).                                 ｷﾞｮｳ
           02  WJNIF-03       PIC 9(6).                                 ﾋﾝｺｰﾄﾞ
           02  WJNIF-04       PIC 9(6).                                 ﾋﾂﾞｹ
           02  WJNIF-05       PIC 9(7).
           02  WJNIF-06       PIC 9(1).                                 ｳﾝｿｳ
           02  WJNIF-07       PIC 9(1).                                 ｿｳｺｰﾄﾞ
           02  WJNIF-08       PIC 9(3).                                 ｺｽｳ
           02  WJNIF-09       PIC 9(3).                                 ｻｲｽﾞﾍﾞﾂ
           02  WJNIF-SIZ      PIC X(4).                                 ｻｲｽﾞ
           02  WJNIF-JAN      PIC X(13).                                JAN
           02  WJNIF-10       PIC 9(1).                                 ｲﾝｼﾞｻｲﾝ
           02  WJNIF-11       PIC 9(1).                                 ﾆｭｳﾘｮｸ
           02  WJNIF-12       PIC 9(1).                                 ｼｭｯｶｻｲﾝ
           02  WJNIF-13       PIC 9(3).                                 ﾏｲｽｳ
           02  WJNIF-13A      PIC 9(1).                                 一般教育
           02  WJNIF-15       PIC 9(2).
           02  WJNIF-14       PIC 9(6).                                 ｵｸﾘｼﾞｮｳ
           02  WJNIF-SUNO     PIC X(8).                                 〒
           02  WJNIF-SJSU     PIC N(20).                                ｼﾞｭｳｼｮｳｴ
           02  WJNIF-SJSS     PIC N(20).                                ｼﾞｭｳｼｮｼﾀ
           02  WJNIF-SNA      PIC N(26).                                ｵｸﾘｻｷﾒｲ
           02  WJNIF-STEL     PIC X(14).                                TEL
           02  WJNIF-UNA      PIC N(6).
           02  WJNIF-MUNO     PIC X(8).                                 〒
           02  WJNIF-MJSU     PIC N(20).                                ｼﾞｭｳｼｮｳｴ
           02  WJNIF-MJSS     PIC N(20).                                ｼﾞｭｳｼｮｼﾀ
           02  WJNIF-MNA      PIC N(26).                                ｵｸﾘｻｷﾒｲ
           02  WJNIF-MTEL     PIC X(14).                                TEL
           02  WJNIF-SOK      PIC N(6).
           02  WJNIF-HNA      PIC N(24).                                ﾋﾝｺｰﾄﾞ
           02  WJNIF-TEK      PIC N(32).                                ﾃｷﾖｳ
           02  WJNIF-BAN      PIC 9(6).                                 NEW NO
           02  WJNIF-GYO      PIC 9(3).                                 NEW NO
       77  F                  PIC X(1).
      *FD  NIFUDA
       01  NIFUDA_JV100U.
           02  NIFUDA_PNAME1  PIC X(006) VALUE "NIFUDA".
           02  F              PIC X(001).
           02  NIFUDA_LNAME   PIC X(013) VALUE "NIFUDA_JV100U".
           02  F              PIC X(001).
           02  NIFUDA_KEY1    PIC X(100) VALUE SPACE.
           02  NIFUDA_SORT    PIC X(100) VALUE SPACE.
           02  NIFUDA_IDLST   PIC X(100) VALUE SPACE.
           02  NIFUDA_RES     USAGE  POINTER.
       01  NIFUDA-R.
           02  F              PIC 9(6).                                 HHT
           02  NIFUDA-05      PIC 9(7).
           02  NIFUDA-SUNO    PIC X(8).                                 〒
           02  NIFUDA-SJSU    PIC N(20).                                ｼﾞｭｳｼｮｳｴ
           02  NIFUDA-SJSS    PIC N(20).                                ｼﾞｭｳｼｮｼﾀ
           02  NIFUDA-SNA     PIC N(26).                                ｵｸﾘｻｷﾒｲ
           02  NIFUDA-STEL    PIC X(14).                                TEL
           02  NIFUDA-BAN     PIC 9(8).                                 ﾃﾞﾝﾋﾟｮｳN
           02  NIFUDA-08      PIC 9(3).                                 ｺｽｳ
           02  NIFUDA-08R     PIC 9(3).
           02  NIFUDA-NOC     PIC 9(3).
           02  NIFUDA-GYO     PIC 9(4).
           02  NIFUDA-KEI     PIC 9(4).
           02  NIFUDA-04      PIC 9(6).                                 ﾋﾂﾞｹ
           02  NIFUDA-06      PIC 9(1).                                 ｿｳｺｰﾄﾞ
           02  NIFUDA-UNA     PIC N(6).
           02  NIFUDA-14      PIC 9(6).                                 ｵｸﾘｼﾞｮｳ
           02  NIFUDA-MUNO    PIC X(8).                                 〒
           02  NIFUDA-MJSU    PIC N(20).                                ｼﾞｭｳｼｮｳｴ
           02  NIFUDA-MJSS    PIC N(20).                                ｼﾞｭｳｼｮｼﾀ
           02  NIFUDA-MNA     PIC N(26).                                ｵｸﾘｻｷﾒｲ
           02  NIFUDA-MTEL    PIC X(14).                                TEL
           02  NIFUDA-07      PIC 9(1).                                 ｿｳｺｰﾄﾞ
           02  NIFUDA-SOK     PIC N(6).
           02  NIFUDA-HNA     PIC N(24).                                ﾋﾝｺｰﾄﾞ
           02  NIFUDA-03      PIC 9(6).                                 ﾋﾝｺｰﾄﾞ
           02  NIFUDA-SIZ     PIC X(4).
           02  NIFUDA-09      PIC 9(4).                                 ｽｳﾘｮｳ
           02  NIFUDA-KSU     PIC 9(4).                                 ｽｳﾘｮｳ
           02  NIFUDA-JAN     PIC X(13).                                JAN
           02  NIFUDA-TEK     PIC N(32).                                ﾃｷﾖｳ
           02  NIFUDA-DATE    PIC 9(8).
           02  NIFUDA-TIME    PIC 9(6).
           02  NIFUDA-KNGP    PIC 9(8).
           02  NIFUDA-COR     PIC 9(1).
       77  F                  PIC X(1).
      *FD  WKEIF                                                        ﾆﾌﾀﾞﾄﾗﾝ
       01  WKEIF_JV100U.
           02  WKEIF_PNAME1   PIC X(009) VALUE SPACE.
           02  F              PIC X(001).
           02  WKEIF_LNAME    PIC X(012) VALUE "WKEIF_JV100U".
           02  F              PIC X(001).
           02  WKEIF_KEY1     PIC X(100) VALUE SPACE.
           02  WKEIF_SORT     PIC X(100) VALUE SPACE.
           02  WKEIF_IDLST    PIC X(100) VALUE SPACE.
           02  WKEIF_RES      USAGE  POINTER.
       01  WKEIF-R.
           02  WKEIF-BAN      PIC 9(6).
           02  WKEIF-KEI      PIC 9(4).
           02  F              PIC X(54).
       77  F                  PIC X(1).
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
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　荷札変換用ファイル作成　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ｺｽｳ ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ｺﾞｳｹｲ ﾅｼ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  JNIF ﾅｼ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  JNIF REWRITE ｴﾗｰ  ***".
             03  E-OKJ   PIC  9(006).
             03  E-NIF   PIC  9(006).
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
            "C-MID" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "16" "42" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "125" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "125" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-OKJ" "9" "24" "45" "6" "E-ME8" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-OKJ" BY REFERENCE W-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NIF" "9" "24" "53" "6" "E-OKJ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NIF" BY REFERENCE WJNIF-01 "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO WKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WJNIF_PNAME1 " " BY REFERENCE WJNIF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" WKEIF_PNAME1 " " BY REFERENCE WKEIF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" NIFUDA_PNAME1 " " BY REFERENCE NIFUDA_IDLST "0".
      *
      *           READ WJNIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WJNIF_PNAME1 BY REFERENCE WJNIF-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           MOVE ZERO TO W-NGP W-TIM.
           ACCEPT W-NGPS FROM DATE.
           ACCEPT W-TIM FROM TIME.
           MOVE 20 TO W-NEN1.
       M-15.
           MOVE WJNIF-14 TO W-14.
           MOVE WJNIF-08 TO W-08.
           MOVE ZERO TO W-08R.
       M-20.
           ADD 1 TO W-08R.
           MOVE WJNIF-BAN TO W-BAN.
           IF  W-08 < W-08R
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKJ" E-OKJ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NIF" E-NIF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           PERFORM S-05 THRU S-15.
      *
           MOVE ZERO TO W-DJAN.
           MOVE W-BAN TO W-KJAN.
           COMPUTE W-K1 = W-D7 + W-D5 + W-D3 + W-D1.
           COMPUTE W-K1 = W-K1 * 3.
           COMPUTE W-K2 = W-D6 + W-D4 + W-D2.
           COMPUTE KEISAND = W-K1 + W-K2.
           COMPUTE W-K3 = 10 - K-D2.
           MOVE W-K3 TO W-D8.
      *
           MOVE ZERO TO W-NOC.
       M-25.
           ADD 1 TO W-NOC.
           MOVE ZERO TO W-GYO.
       M-30.
           ADD 1 TO W-GYO.
           IF  W-GYO = 7
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO NIFUDA-R.
           MOVE WJNIF-05 TO NIFUDA-05.
           MOVE WJNIF-SUNO TO NIFUDA-SUNO.
           MOVE WJNIF-SJSU TO NIFUDA-SJSU.
           MOVE WJNIF-SJSS TO NIFUDA-SJSS.
           MOVE WJNIF-SNA TO NIFUDA-SNA.
           MOVE WJNIF-STEL TO NIFUDA-STEL.
           MOVE W-DJAN TO NIFUDA-BAN.
           MOVE WJNIF-08 TO NIFUDA-08.
           MOVE W-08R TO NIFUDA-08R.
           MOVE W-NOC TO NIFUDA-NOC.
           MOVE W-GYO TO NIFUDA-GYO.
           MOVE WKEIF-KEI TO NIFUDA-KEI.
           MOVE WJNIF-04 TO NIFUDA-04.
           MOVE WJNIF-06 TO NIFUDA-06.
           MOVE WJNIF-UNA TO NIFUDA-UNA.
           MOVE WJNIF-14 TO NIFUDA-14.
           MOVE WJNIF-MUNO TO NIFUDA-MUNO.
           MOVE WJNIF-MJSU TO NIFUDA-MJSU.
           MOVE WJNIF-MJSS TO NIFUDA-MJSS.
           MOVE WJNIF-MNA TO NIFUDA-MNA.
           MOVE WJNIF-MTEL TO NIFUDA-MTEL.
           MOVE WJNIF-07 TO NIFUDA-07.
           MOVE WJNIF-SOK TO NIFUDA-SOK.
           MOVE WJNIF-HNA TO NIFUDA-HNA.
           MOVE WJNIF-03 TO NIFUDA-03.
           MOVE WJNIF-SIZ TO NIFUDA-SIZ.
           MOVE WJNIF-09 TO NIFUDA-09.
           IF  WJNIF-JAN = "0000000000000"
               MOVE WJNIF-09 TO NIFUDA-KSU
           END-IF
           MOVE WJNIF-JAN TO NIFUDA-JAN.
           MOVE WJNIF-TEK TO NIFUDA-TEK.
           MOVE W-NGP TO NIFUDA-DATE.
           MOVE W-TIME TO NIFUDA-TIME.
      *           WRITE NIFUDA-R.
      *//////////////
           CALL "DB_Insert" USING
            NIFUDA_PNAME1 NIFUDA_LNAME NIFUDA-R RETURNING RET.
      *
      *           READ WJNIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WJNIF_PNAME1 BY REFERENCE WJNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  WJNIF-14 NOT = W-14
               IF  W-08 = W-08R
                   GO TO M-15
               ELSE
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME3" E-ME3 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-OKJ" E-OKJ "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-NIF" E-NIF "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-35
               END-IF
           END-IF
           IF  WJNIF-BAN NOT = W-BAN
               GO TO M-20
           END-IF
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE WJNIF_IDLST WJNIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NIFUDA_IDLST NIFUDA_PNAME1.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" WJNIF_PNAME1 " " BY REFERENCE WJNIF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
      *
      *           READ WJNIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WJNIF_PNAME1 BY REFERENCE WJNIF-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-40.
           MOVE WJNIF-01 TO W-01.
           MOVE SPACE TO JNIF1-KEY.
           MOVE W-01 TO JNIF1-01.
      *           START JNIF KEY NOT < JNIF1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JNIF_PNAME1 "JNIF1-KEY" " NOT < " JNIF1-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NIF" E-NIF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-45.
      *           READ JNIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  W-01 NOT = JNIF1-01
               GO TO M-50
           END-IF
           IF  JNIF1-10 = 1
               GO TO M-45
           END-IF
           MOVE 1 TO JNIF1-10.
      *           REWRITE JNIF1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NIF" E-NIF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-45.
       M-50.
      *           READ WJNIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WJNIF_PNAME1 BY REFERENCE WJNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  WJNIF-01 = W-01
               GO TO M-50
           END-IF
           GO TO M-40.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WJNIF_IDLST WJNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Close" USING
            BY REFERENCE WKEIF_IDLST WKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WKEIF_PNAME1 " " BY REFERENCE WKEIF_IDLST "0".
       S-10.
      *           READ WKEIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WKEIF_PNAME1 BY REFERENCE WKEIF-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-OKJ" E-OKJ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NIF" E-NIF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-15
           END-IF
           IF  WKEIF-BAN NOT = WJNIF-BAN
               GO TO S-10
           END-IF.
       S-15.
           EXIT.
