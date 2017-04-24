       IDENTIFICATION   DIVISION.
       PROGRAM-ID.            JK900M.
      ******************************************************************
      *    Ｏ／Ｌ状況ファイル　クリア                                  *
      ******************************************************************
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       SYSTEM3100.
       OBJECT-COMPUTER.       SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE  SECTION.
       01  ERR-STAT                PIC  X(02).
       01  W-DMM                   PIC  9(01).
      *
           COPY    L-JOJF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER  PIC  X(34)
                     VALUE  "＊　Ｏ／Ｌ状況ファイル　クリア　＊".
           02  FILLER  PIC  X(22)
                     VALUE  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  A-DMM   PIC 9(01).
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-AREA" "X" "1" "20" "34" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "20" "40" "22" "01DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "57" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO M-95
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" JOJF_PNAME1 " " BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
