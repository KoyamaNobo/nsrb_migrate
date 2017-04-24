       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS00U.
      **********************************************************
      *    ＪＣＡ手順ファイルクリア                            *
      *    JS-SIGN : 0=ワークマン , 1=ナフコ , 2=赤ちゃん本舗  *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN          PIC  9(001).
       77  ERR-STAT         PIC  X(002).
       01  W-DATA.
           02  W-TNA        PIC  N(008).
           02  W-DMM        PIC  9(001).
           02  W-ERR        PIC  9(001).
           02  W-CHK        PIC  9(001).
           COPY LSTAT.
      *
           COPY LITDNA.
           COPY LSWMJC.
      *FD  JCANF
       01  JCANF_JHS01U.
           02  JCANF_PNAME1 PIC  X(005) VALUE "JCANF".
           02  F            PIC  X(001).
           02  JCANF_LNAME  PIC  X(012) VALUE "JCANF_JHS01U".
           02  F            PIC  X(001).
           02  JCANF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCANF_SORT   PIC  X(100) VALUE SPACE.
           02  JCANF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCANF_RES    USAGE  POINTER.
       01  JCAN-R           PIC  X(256).
       77  F                PIC  X(001).
      *FD  JCAAF
       01  JCAAF_JHS01U.
           02  JCAAF_PNAME1 PIC  X(005) VALUE "JCAAF".
           02  F            PIC  X(001).
           02  JCAAF_LNAME  PIC  X(012) VALUE "JCAAF_JHS01U".
           02  F            PIC  X(001).
           02  JCAAF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAAF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAAF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAAF_RES    USAGE  POINTER.
       01  JCAA-R           PIC  X(128).
       77  F                PIC  X(001).
      *FD  JCAASF
       01  JCAASF_JHS01U.
           02  JCAASF_PNAME1 PIC  X(006) VALUE "JCAASF".
           02  F             PIC  X(001).
           02  JCAASF_LNAME  PIC  X(013) VALUE "JCAASF_JHS01U".
           02  F             PIC  X(001).
           02  JCAASF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAASF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAASF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAASF_RES    USAGE  POINTER.
       01  JCAAS-R           PIC  X(128).
       77  F                 PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　受注データ　受信処理　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
           02  A-CHK   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(008).
           02  D-MSM.
             03  FILLER  PIC  N(015) VALUE
                  "未処理データ有り　クリア　する".
             03  FILLER  PIC  X(020) VALUE
                  "=1 , しない=5   ﾘﾀｰﾝ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  N(008) VALUE
                  "未処理データ有り".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "302" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "22" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "16" "55" "1" "A-DMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "13" "23" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSM" " " "16" "0" "50" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MSM" "N" "16" "10" "30" " " "D-MSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MSM" "X" "16" "40" "20" "01D-MSM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "78" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "78" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "16" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE SPACE TO W-TNA.
           IF  JS-SIGN = 0
               MOVE "【ワークマン】　" TO W-TNA
           ELSE
               IF  JS-SIGN = 1
                   MOVE "【ナ　フ　コ】　" TO W-TNA
               ELSE
                   IF  JS-SIGN = 2
                       MOVE "【赤ちゃん本舗】" TO W-TNA
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE 0 TO W-ERR.
      *
           IF  JS-SIGN = 0
               PERFORM WDC-RTN THRU WDC-EX
           ELSE
               IF  JS-SIGN = 1
                   PERFORM NDC-RTN THRU NDC-EX
               ELSE
                   IF  JS-SIGN = 2
                       PERFORM ADC-RTN THRU ADC-EX
                   END-IF
               END-IF
           END-IF
           IF  W-ERR = 0
               GO TO M-50
           END-IF
           CALL "SD_Output" USING "D-MSM" D-MSM "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-CHK = 5
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-CHK NOT = 1
               GO TO M-10
           END-IF
      *
           IF  JS-SIGN = 0
               CALL "DB_F_Open" USING
                "OUTPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE WMJCF_IDLST WMJCF_PNAME1
           ELSE
               IF  JS-SIGN = 1
                   CALL "DB_F_Open" USING
                    "OUTPUT" JCANF_PNAME1 " " BY REFERENCE
                    JCANF_IDLST "0"
                   CALL "DB_F_Close" USING
                    BY REFERENCE JCANF_IDLST JCANF_PNAME1
               ELSE
                   IF  JS-SIGN = 2
                       CALL "DB_F_Open" USING
                        "OUTPUT" JCAAF_PNAME1 " " BY REFERENCE
                        JCAAF_IDLST "0"
                       CALL "DB_F_Open" USING
                        "OUTPUT" JCAASF_PNAME1 " " BY REFERENCE
                        JCAASF_IDLST "0"
                       CALL "DB_F_Close" USING
                        BY REFERENCE JCAAF_IDLST JCAAF_PNAME1
                       CALL "DB_F_Close" USING
                        BY REFERENCE JCAASF_IDLST JCAASF_PNAME1
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 2
               GO TO M-50
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 " " BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
       M-15.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNAF_IDLST TDNAF_PNAME1
               GO TO M-50
           END-IF
           IF  TDNA-RC = 0
               GO TO M-15
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           GO TO M-95.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-50
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       WDC-RTN.
           CALL "DB_F_Open" USING
            "INPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0".
      *           READ WMJCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCF_PNAME1 BY REFERENCE WMJC-R " " RETURNING RET.
           IF  RET = 1
               GO TO WDC-010
           END-IF
           MOVE 1 TO W-ERR.
       WDC-010.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCF_IDLST WMJCF_PNAME1.
       WDC-EX.
           EXIT.
       NDC-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JCANF_PNAME1 " " BY REFERENCE JCANF_IDLST "0".
      *           READ JCANF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               GO TO NDC-010
           END-IF
           MOVE 1 TO W-ERR.
       NDC-010.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANF_IDLST JCANF_PNAME1.
       NDC-EX.
           EXIT.
       ADC-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JCAAF_PNAME1 " " BY REFERENCE JCAAF_IDLST "0".
      *           READ JCAAF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               GO TO ADC-010
           END-IF
           MOVE 1 TO W-ERR.
       ADC-010.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAAF_IDLST JCAAF_PNAME1.
           IF  W-ERR = 1
               GO TO ADC-EX
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCAASF_PNAME1 " " BY REFERENCE JCAASF_IDLST "0".
      *           READ JCAASF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAASF_PNAME1 BY REFERENCE JCAAS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ADC-020
           END-IF
           MOVE 1 TO W-ERR.
       ADC-020.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAASF_IDLST JCAASF_PNAME1.
       ADC-EX.
           EXIT.
