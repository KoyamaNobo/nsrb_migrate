       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR030M.
      *****************************************************
      *    PROGRAM ........ 貸借マスタメンテナンス        *
      *    AUTHOR  ........ ﾜﾀﾅﾍﾞ ｹﾝｼﾞ                    *
      *    SCREEN  USED ... GR0300                        *
      *    DATA WRITTEN ... 90/11/14                      *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
      *
       77  N-10            PIC  N(10)    VALUE  SPACE.
       77  ERR-STAT        PIC  X(02).
       77  ACT             PIC  9(01).
       01  WORK-AREA.
           03  LN          PIC  9(03).
           03  KAI         PIC  9(01).
           03  KARI        PIC  9(01).
           03  INJI1       PIC  9(01).
           03  KAMOKU1     PIC  N(10).
           03  KASHI       PIC  9(01).
           03  INJI2       PIC  9(01).
           03  KAMOKU2     PIC  N(10).
           03  TRK         PIC  9(01).
           03  OKC         PIC  X(01).
      *
           COPY  LWMSG.
           COPY  BS-LIB.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           03  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
      *    <  画面クリア項目  >
           03  FILLER  PIC  X(03)  VALUE  "   ".
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  N(10).
           03  FILLER  PIC  N(10).
           03  FILLER  PIC  X(01)  VALUE  " ".
           03  FILLER  PIC  X(01)  VALUE  " ".
      *    <  画面入力項目  >
       01  ACP-AREA.
           03  ACP-ACT      PIC  9(01).
           03  ACP-LN       PIC  9(03).
           03  ACP-KAI      PIC  9(01).
           03  ACP-KARI     PIC  9(01).
           03  ACP-INJI1    PIC  9(01).
           03  ACP-KAMOKU1  PIC  N(10).
           03  ACP-KASHI    PIC  9(01).
           03  ACP-INJI2    PIC  9(01).
           03  ACP-KAMOKU2  PIC  N(10).
           03  ACP-TRK      PIC  9(01).
           03  ACP-OKC      PIC  X(01).
      *    <  画面表示項目  >
       01  DSP-AREA.
           03  FILLER  PIC  9(01).
           03  FILLER  PIC  9(01).
           03  FILLER  PIC  9(01).
           03  FILLER  PIC  N(10).
           03  FILLER  PIC  9(01).
           03  FILLER  PIC  9(01).
           03  FILLER  PIC  N(10).
           03  FILLER  PIC  9(01).
      *
           COPY  LSMSG_PR.
      *-----------------------------------------------------------------
       PROCEDURE          DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "5" "35" "3" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "37" "1" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "7" "37" "1" "02CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA" "X" "7" "75" "1" "03CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA" "X" "8" "37" "1" "04CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA" "X" "8" "75" "1" "05CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA" "N" "9" "18" "20" "06CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "07CLR-AREA" BY REFERENCE N-10 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-AREA" "N" "9" "56" "20" "07CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "08CLR-AREA" BY REFERENCE N-10 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-AREA" "X" "10" "75" "1" "08CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-AREA" "X" "24" "77" "1" "09CLR-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "51" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-LN" "9" "5" "35" "3" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-LN" BY REFERENCE LN "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAI" "9" "6" "37" "1" "ACP-LN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAI" BY REFERENCE KAI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KARI" "9" "7" "37" "1" "ACP-KAI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KARI" BY REFERENCE KARI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-INJI1" "9" "8" "37" "1" "ACP-KARI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-INJI1" BY REFERENCE INJI1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAMOKU1" "N" "9" "18" "20" "ACP-INJI1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAMOKU1" BY REFERENCE KAMOKU1 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KASHI" "9" "7" "75" "1" "ACP-KAMOKU1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KASHI" BY REFERENCE KASHI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-INJI2" "9" "8" "75" "1" "ACP-KASHI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-INJI2" BY REFERENCE INJI2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAMOKU2" "N" "9" "56" "20" "ACP-INJI2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAMOKU2" BY REFERENCE KAMOKU2 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TRK" "9" "10" "75" "1" "ACP-KAMOKU2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TRK" BY REFERENCE TRK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "X" "24" "77" "1" "ACP-TRK" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "9" "6" "37" "1" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-AREA" BY REFERENCE BS-LIN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "9" "7" "37" "1" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-AREA" BY REFERENCE BS-GKBDR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "9" "8" "37" "1" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-AREA" BY REFERENCE BS-PKBDR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "N" "9" "18" "20" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-AREA" BY REFERENCE BS-NAMDR1 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "9" "7" "75" "1" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-AREA" BY REFERENCE BS-GKBCR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "9" "8" "75" "1" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-AREA" BY REFERENCE BS-PKBCR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "N" "9" "56" "20" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-AREA" BY REFERENCE BS-NAMCR1 "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "9" "10" "75" "1" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-AREA" BY REFERENCE BS-RKB "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0300" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-AREA" CLR-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
       PRO-000.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO   TO   OWARI
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-000
           END-IF
           IF  ACT    NOT  =  "1" AND "2" AND "3"
               GO    TO    PRO-000
           END-IF.
       PRO-005.
           CALL "SD_Output" USING
            "CLR-AREA" CLR-AREA "p" RETURNING RESU.
       PRO-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-LN "ACP-LN" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-000
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-010
           END-IF
      *****貸借マスタ　　ＲＥＡＤ
           MOVE       LN          TO      BS-KEY.
      *           READ       BS          INVALID
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID" BS_PNAME1 BY REFERENCE BS-REC " " RETURNING RET.
           IF  RET = 1
               MOVE        LOW-VALUE    TO    BS-KEY
               GO          TO      PRO-015
           END-IF
           IF  ACT         =       "1"
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO          TO      PRO-010
           END-IF.
       PRO-015.
           IF  BS-KEY      =       LOW-VALUE
               IF  ACT  NOT  =  "1"
                   CALL "SD_Output" USING
                    "INV-M01" INV-M01 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO    TO    PRO-010
               END-IF
           END-IF
           IF  ACT    NOT  =  "1"
               CALL "SD_Output" USING
                "DSP-AREA" DSP-AREA "p" RETURNING RESU
           END-IF
           IF  ACT         =  "3"
               GO    TO    PRO-100
           END-IF.
       PRO-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-KAI "ACP-KAI" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-010
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-020
           END-IF.
       PRO-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-KARI "ACP-KARI"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-020
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-030
           END-IF.
       PRO-040.
           CALL "SD_Accept" USING BY REFERENCE ACP-INJI1 "ACP-INJI1"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-030
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-040
           END-IF.
       PRO-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-KAMOKU1 "ACP-KAMOKU1"
            "N" "20" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-040
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-050
           END-IF.
       PRO-060.
           CALL "SD_Accept" USING BY REFERENCE ACP-KASHI "ACP-KASHI"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-050
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-060
           END-IF.
       PRO-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-INJI2 "ACP-INJI2"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-060
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-070
           END-IF.
       PRO-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-KAMOKU2 "ACP-KAMOKU2"
            "N" "20" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-070
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-080
           END-IF.
       PRO-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-TRK "ACP-TRK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-080
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-090
           END-IF.
       PRO-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               IF  ACT  =  3
                   GO   TO   PRO-010
               ELSE
                   GO   TO   PRO-090
               END-IF
           END-IF
           IF  OKC    NOT  =  "1" AND "9"
               GO   TO   PRO-100
           END-IF
           IF  OKC         =  "9"
               CALL "SD_Output" USING
                "CLR-AREA" CLR-AREA "p" RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               INITIALIZE  WORK-AREA
               GO     TO   PRO-005
           END-IF
           PERFORM    UPD-RTN     THRU     UPD-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           GO         TO          PRO-005.
       OWARI.
           PERFORM    CLSE-ENT    THRU     CLSE-EXT.
           CALL "DB_Close".
           STOP       RUN.
      *------------------------------*
      *    更新処理  <UPD-RTN>       *
      *------------------------------*
       UPD-RTN.
           IF  ACT       =  "3"
               GO    TO    UPD-010
           END-IF
           IF  ACT       =  "1"
               MOVE      SPACE     TO    BS-REC
               INITIALIZE                BS-REC
           END-IF
           MOVE       LN        TO        BS-KEY    ERR-K.
           MOVE       KAI       TO        BS-LIN.
           MOVE       KARI      TO        BS-GKBDR.
           MOVE       INJI1     TO        BS-PKBDR.
           MOVE       KAMOKU1   TO        BS-NAMDR1.
           MOVE       KASHI     TO        BS-GKBCR.
           MOVE       INJI2     TO        BS-PKBCR.
           MOVE       KAMOKU2   TO        BS-NAMCR1.
           MOVE       TRK       TO        BS-RKB.
           MOVE       ZERO      TO        BS-KINDR  BS-KINCR.
           IF  ACT       =         "2"
               GO   TO   UPD-000
           END-IF
      *           WRITE      BS-REC    INVALID
      *///////////////
           CALL "DB_Insert" USING
            BS_PNAME1 BS_LNAME BS-REC RETURNING RET.
           IF  RET = 1
               MOVE      "BS"      TO    ERR-F
               MOVE      "W"       TO    ERR-M
               PERFORM   ERR-ENT   THRU  ERR-EXT
           END-IF
           GO         TO        UPD-EX.
       UPD-000.
      *           REWRITE    BS-REC    INVALID
      *///////////////
           CALL "DB_Update" USING
            BS_PNAME1 BS_LNAME BY REFERENCE BS-REC RETURNING RET.
           IF  RET = 1
               MOVE      "BS"      TO    ERR-F
               MOVE      "R"       TO    ERR-M
               PERFORM   ERR-ENT   THRU  ERR-EXT
           END-IF
           GO         TO        UPD-EX.
       UPD-010.
      *           DELETE     BS        INVALID
      *///////////////
           CALL "DB_Delete" USING BS_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE      "BS"      TO    ERR-F
               MOVE      "D"       TO    ERR-M
               PERFORM   ERR-ENT   THRU  ERR-EXT
           END-IF.
       UPD-EX.
           EXIT.
      *--------------------------*
      *    ファイルｃｌｏｓｅ    *
      *--------------------------*
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
       CLSE-EXT.
           EXIT.
      *
           COPY  LPMSG_PR.
      ************ E N D     O F     P R O G R A M *********************
