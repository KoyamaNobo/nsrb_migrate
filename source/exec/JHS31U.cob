       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS31U.
      ************************************************************
      *    ＪＣＡ手順ファイルデータ　クリア                      *
      *    JS-SIGN : 0=ワークマン , 1=ナフコ , 2=赤ちゃん本舗    *
      ************************************************************
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
      *
           COPY LSWMJC.
      *FD  JCANF
       01  JCANF_JHS31U.
           02  JCANF_PNAME1 PIC  X(005) VALUE "JCANF".
           02  F            PIC  X(001).
           02  JCANF_LNAME  PIC  X(012) VALUE "JCANF_JHS31U".
           02  F            PIC  X(001).
           02  JCANF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCANF_SORT   PIC  X(100) VALUE SPACE.
           02  JCANF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCANF_RES    USAGE  POINTER.
       01  JCAN-R           PIC  X(256).
       77  F                PIC  X(001).
      *FD  JCAAF
       01  JCAAF_JHS31U.
           02  JCAAF_PNAME1 PIC  X(005) VALUE "JCAAF".
           02  F            PIC  X(001).
           02  JCAAF_LNAME  PIC  X(012) VALUE "JCAAF_JHS31U".
           02  F            PIC  X(001).
           02  JCAAF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAAF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAAF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAAF_RES    USAGE  POINTER.
       01  JCAA-R           PIC  X(128).
       77  F                PIC  X(001).
      *FD  JCAASF
       01  JCAASF_JHS31U.
           02  JCAASF_PNAME1 PIC  X(006) VALUE "JCAASF".
           02  F             PIC  X(001).
           02  JCAASF_LNAME  PIC  X(013) VALUE "JCAASF_JHS31U".
           02  F             PIC  X(001).
           02  JCAASF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAASF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAASF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAASF_RES    USAGE  POINTER.
       01  JCAAS-R          PIC  X(128).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　ＪＣＡ手順ファイル　クリア　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　（受注データ）　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-TNA   PIC  N(008).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
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
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "15" "23" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "16" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
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
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
