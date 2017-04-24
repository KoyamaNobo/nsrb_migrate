       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN410.
      *********************************************************
      *    PROGRAM         :  履物品名統計ワーク作成        　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0256".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0512".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
      *FD  HHTW
       01  HHTW_HMN410.
           02  HHTW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTW_LNAME     PIC  X(011) VALUE "HHTW_HMN410".
           02  F              PIC  X(001).
           02  HHTW_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTW_SORT      PIC  X(100) VALUE SPACE.
           02  HHTW_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTW_RES       USAGE  POINTER.
       01  HHTW-R.
           02  HHTW-KEY2.
             03  HHTW-MHCD    PIC  9(006).
             03  HHTW-KEY.
               04  HHTW-HCD   PIC  9(006).
               04  HHTW-SIZ   PIC  9(001).
           02  HHTW-AZSU.                                               前月残数
             03  HHTW-ZSUD  OCCURS  10.
               04  HHTW-ZSU   PIC S9(006) COMP-3.
           02  HHTW-ANSU.                                               入庫数
             03  HHTW-NSUD  OCCURS  10.
               04  HHTW-NSU   PIC S9(006) COMP-3.
           02  HHTW-AUSU.                                               出庫数
             03  HHTW-USUD  OCCURS  10.
               04  HHTW-USU   PIC S9(006) COMP-3.
           02  HHTW-AASS.                                               預り出荷
             03  HHTW-ASSD  OCCURS  10.
               04  HHTW-ASS   PIC S9(004) COMP-3.
           02  HHTW-ATZS.                                               棚卸帳簿
             03  HHTW-TSZD  OCCURS  10.
               04  HHTW-TZS   PIC S9(006) COMP-3.
           02  HHTW-ATSU.                                               棚卸数
             03  HHTW-TSUD  OCCURS  10.
               04  HHTW-TSU   PIC S9(006) COMP-3.
           02  F              PIC  X(012).
           02  HHTW-SEN       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HHTF
       01  HHTF_HMN410.
           02  HHTF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTF_LNAME     PIC  X(011) VALUE "HHTF_HMN410".
           02  F              PIC  X(001).
           02  HHTF_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTF_SORT      PIC  X(100) VALUE SPACE.
           02  HHTF_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTF_RES       USAGE  POINTER.
       01  HHT-R.
           02  HHT-KEY2.
             03  HHT-MHCD     PIC  9(006).
             03  HHT-KEY.
               04  HHT-HCD    PIC  9(006).
               04  HHT-SIZ    PIC  9(001).
           02  HHT-AZSU.                                                前月残数
             03  HHT-ZSUD  OCCURS  10.
               04  HHT-ZSU    PIC S9(006) COMP-3.
           02  HHT-ANSU.                                                入庫数
             03  HHT-NSUD  OCCURS  10.
               04  HHT-NSU    PIC S9(006) COMP-3.
           02  HHT-AUSU.                                                出庫数
             03  HHT-USUD  OCCURS  10.
               04  HHT-USU    PIC S9(006) COMP-3.
           02  HHT-AASS.                                                預り出荷
             03  HHT-ASSD  OCCURS  10.
               04  HHT-ASS    PIC S9(004) COMP-3.
           02  HHT-ATZS.                                                棚卸帳簿
             03  HHT-TSZD  OCCURS  10.
               04  HHT-TZS    PIC S9(006) COMP-3.
           02  HHT-ATSU.                                                棚卸数
             03  HHT-TSUD  OCCURS  10.
               04  HHT-TSU    PIC S9(006) COMP-3.
           02  HHT-BCD12.
             03  HHT-BCD1     PIC  9(003).
             03  HHT-BC22     PIC  9(001).
           02  HHT-BC3        PIC  9(002).                              分類CD3
           02  HHT-BMNO       PIC  9(001).
           02  F              PIC  X(006).
           02  F              PIC  X(256).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　品名サイズ別棚卸ワーク　作成　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(024) VALUE
                  "子ｺｰﾄﾞ=0 , 親ｺｰﾄﾞ=1 ... ".
           02  FILLER  PIC X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "382" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "22" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "15" "45" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "41" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "78" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "78" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO HHTW_PNAME1.
           MOVE W-FID2 TO WK0512ID.
           MOVE WK0512ID TO HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HHTW_PNAME1 " " BY REFERENCE HHTW_IDLST "0".
       M-20.
      *           READ HHTF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  HHT-HCD > 999899
               GO TO M-20
           END-IF.
       M-25.
           PERFORM S-05 THRU S-15.
           IF  W-SEN = 0
               MOVE HHT-HCD TO HHTW-HCD
           ELSE
               MOVE HHT-MHCD TO HHTW-HCD
           END-IF
           MOVE HHT-SIZ TO HHTW-SIZ.
           MOVE W-SEN TO HHTW-SEN.
       M-30.
           PERFORM S-20 THRU S-30.
       M-35.
      *           READ HHTF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  HHT-HCD > 999899
               GO TO M-35
           END-IF
           IF  W-SEN = 1
               IF  HHT-MHCD = HHTW-HCD
                   IF  HHT-SIZ = HHTW-SIZ
                       GO TO M-30
                   END-IF
               END-IF
           END-IF
           PERFORM S-35 THRU S-45.
           IF  W-ZC = 1
      *               WRITE HHTW-R.
      *//////////////
               CALL "DB_Insert" USING
                HHTW_PNAME1 HHTW_LNAME HHTW-R RETURNING RET
           END-IF
           GO TO M-25.
       M-80.
           PERFORM S-35 THRU S-45.
           IF  W-ZC = 1
      *               WRITE HHTW-R.
      *//////////////
               CALL "DB_Insert" USING
                HHTW_PNAME1 HHTW_LNAME HHTW-R RETURNING RET
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTW_IDLST HHTW_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO HHTW-R CNT.
       S-10.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE ZERO TO HHTW-ZSU(CNT) HHTW-NSU(CNT) HHTW-USU(CNT)
                            HHTW-ASS(CNT) HHTW-TZS(CNT) HHTW-TSU(CNT)
               GO TO S-10
           END-IF.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO CNT.
       S-25.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD HHT-ZSU(CNT) TO HHTW-ZSU(CNT)
               ADD HHT-NSU(CNT) TO HHTW-NSU(CNT)
               ADD HHT-USU(CNT) TO HHTW-USU(CNT)
               ADD HHT-ASS(CNT) TO HHTW-ASS(CNT)
               ADD HHT-TZS(CNT) TO HHTW-TZS(CNT)
               ADD HHT-TSU(CNT) TO HHTW-TSU(CNT)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO W-ZC CNT.
       S-40.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-45
           END-IF
           IF  ZERO = HHTW-ZSU(CNT) AND HHTW-NSU(CNT) AND HHTW-USU(CNT)
                 AND HHTW-ASS(CNT) AND HHTW-TZS(CNT) AND HHTW-TSU(CNT)
               GO TO S-40
           END-IF
           MOVE 1 TO W-ZC.
       S-45.
           EXIT.
