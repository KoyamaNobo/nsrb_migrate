       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT400U.
      **************************************************
      *****     得意先別・品名別出荷日報　選択     *****
      **************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       01  W-DATA.
           02  W-JS           PIC  9(001).
           02  W-S            PIC  9(001).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-SNGP         PIC  9(008).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP         PIC  9(008).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-ENGPL REDEFINES W-ENGP.
             03  F            PIC  9(002).
             03  W-ENGPS      PIC  9(006).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LJSTRR.
           COPY L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　出　荷　日　報　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(025) VALUE
                  "教　育=0 , 一　般=1 ...  ".
           02  FILLER  PIC  X(039) VALUE
                "得意先別出荷日報・品名別出荷日報 ---> 1".
           02  FILLER  PIC  X(039) VALUE
                "得意先別出荷日報                 ---> 2".
           02  FILLER  PIC  X(039) VALUE
                "                　品名別出荷日報 ---> 3".
           02  FILLER  PIC  X(045) VALUE
                "                                    選択  [ ]".
           02  FILLER  PIC  X(027) VALUE
                "データは   年   月   日より".
           02  FILLER  PIC  X(029) VALUE
                "           年   月   日分作表".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-JS    PIC  9(001).
           02  A-S     PIC  9(001).
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNGP.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  JCON ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  JCON REWRITE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
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
            "C-MID" " " "0" "0" "299" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "23" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "4" "0" "25" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "4" "20" "25" " " "02C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "7" "20" "39" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "9" "20" "39" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "11" "20" "39" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "13" "20" "45" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "16" "20" "27" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "18" "20" "29" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "23" "40" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JS" "9" "4" "44" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S" "9" "13" "63" "1" "A-JS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S" BY REFERENCE W-S "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "18" "0" "6" "A-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "18" "29" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "18" "34" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "18" "39" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "57" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNGP" " " "16" "0" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SNGP" "9" "16" "29" "2" " " "D-SNGP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SNGP" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SNGP" "9" "16" "34" "2" "01D-SNGP" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SNGP" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SNGP" "9" "16" "39" "2" "02D-SNGP" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SNGP" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "120" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-NGP W-SNGP W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 " " BY REFERENCE JSTRRF_IDLST "0".
      *           READ JSTRRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE JSTRR-90 TO W-SNGP.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
      *
           COPY LIBCPR.
           CALL "SD_Output" USING "D-SNGP" D-SNGP "p" RETURNING RESU.
           ACCEPT W-ENGPS FROM DATE.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-JS "A-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-10
           END-IF
           IF  W-JS NOT = 0 AND 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-S "A-S" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-10
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-15
           END-IF
           IF  W-S < 1 OR > 3
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-15
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-20
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-25
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-25
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-30
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGP < W-SNGP OR > W-ENGP
               GO TO M-20
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-15
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-50
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-50
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE SPACE TO JCON6-KEY.
           MOVE 6 TO JCON6-01.
      *           READ JCON INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-JS  TO JCON6-08.
           MOVE W-NGP TO JCON6-09.
      *           REWRITE JCON6-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON6-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
      *
           IF  W-S = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  110
           END-IF
           IF  W-S = 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  120
           END-IF
           IF  W-S = 3
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  130
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
