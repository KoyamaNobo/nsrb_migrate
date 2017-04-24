       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY265.
      *****************************************************************
      *    PROGRAM         :  履物品名別製品受払ファイル　作成 (EXCEL)*
      *    JS-SIGN         :  0=ALL色有り 1=生協分色なし              *
      *                    :              2=ALL色なし                 *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-ENG          PIC  9(006).
           02  W-MNG.
             03  W-MNEN       PIC  9(004).
             03  W-MGET       PIC  9(002).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-FC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-NAME         PIC  N(024).
           02  W-ANAD  REDEFINES W-NAME.
             03  W-ANA   OCCURS  24.
               04  W-NA       PIC  N(001).
           02  W-HNA          PIC  N(024).
           02  W-HNAD  REDEFINES W-HNA.
             03  W-AHN   OCCURS  24.
               04  W-HN       PIC  N(001).
       01  W-R.
           02  WR-HCD         PIC  9(006).
           02  WR-HCDD  REDEFINES WR-HCD.
             03  WR-HCD1      PIC  9(004).
             03  WR-HCD2      PIC  9(002).
           02  WR-NG          PIC  9(006).
           02  WR-TD.
             03  WR-D.
               04  WR-ZS      PIC S9(006).
               04  WR-ZK      PIC S9(009).
               04  WR-NS      PIC S9(007).
               04  WR-NK      PIC S9(010).
               04  WR-SS      PIC S9(008).
               04  WR-SK      PIC S9(010).
               04  WR-YS      PIC S9(006).
               04  WR-YK      PIC S9(009).
               04  WR-UG      PIC S9(010).
             03  WR-BCD12.
               04  WR-BCD1    PIC  9(003).
               04  WR-BCW1  REDEFINES WR-BCD1.
                 05  WR-BC1   PIC  9(002).
                 05  WR-BC21  PIC  9(001).
               04  WR-BC22    PIC  9(001).
             03  WR-BCW12 REDEFINES WR-BCD12.
               04  F          PIC  9(002).
               04  WR-BC2     PIC  9(002).
             03  WR-BC3       PIC  9(002).
             03  WR-BCD3  REDEFINES WR-BC3.
               04  WR-BC31    PIC  9(001).
               04  WR-BC32    PIC  9(001).
             03  WR-BMC       PIC  9(002).
             03  WR-BMNO      PIC  9(001).
           02  F              PIC  X(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHUHM.
      *FD  HIYF
       01  HIYF_HMY265.
           02  HIYF_PNAME1    PIC  X(004) VALUE "HIYF".
           02  F              PIC  X(001).
           02  HIYF_LNAME     PIC  X(011) VALUE "HIYF_HMY265".
           02  F              PIC  X(001).
           02  HIYF_KEY1      PIC  X(100) VALUE SPACE.
           02  HIYF_KEY2      PIC  X(100) VALUE SPACE.
           02  HIYF_SORT      PIC  X(100) VALUE SPACE.
           02  HIYF_IDLST     PIC  X(100) VALUE SPACE.
           02  HIYF_RES       USAGE  POINTER.
       01  HIY-R.
           02  HIY-KEY.
             03  HIY-HCD      PIC  9(006).
             03  HIY-HCDD  REDEFINES HIY-HCD.
               04  HIY-HCD1   PIC  9(004).
               04  HIY-HCD2   PIC  9(002).
           02  HIY-NGD.
             03  HIY-NEN      PIC  9(004).
             03  HIY-GET      PIC  9(002).
           02  HIY-NG    REDEFINES HIY-NGD  PIC 9(006).
           02  HIY-D.
             03  HIY-ZS       PIC S9(006).
             03  HIY-ZK       PIC S9(009).
             03  HIY-NS       PIC S9(007).
             03  HIY-NK       PIC S9(010).
             03  HIY-SS       PIC S9(008).
             03  HIY-SK       PIC S9(010).
             03  HIY-YS       PIC S9(006).
             03  HIY-YK       PIC S9(009).
             03  HIY-UG       PIC S9(010).
           02  HIY-BCD12.
             03  HIY-BCD1     PIC  9(003).
             03  HIY-BCW1  REDEFINES HIY-BCD1.
               04  HIY-BC1    PIC  9(002).
               04  HIY-BC21   PIC  9(001).
             03  HIY-BC22     PIC  9(001).
           02  HIY-BCW12 REDEFINES HIY-BCD12.
             03  F            PIC  9(002).
             03  HIY-BC2      PIC  9(002).
           02  HIY-BC3        PIC  9(002).
           02  HIY-BCD3  REDEFINES HIY-BC3.
             03  HIY-BC31     PIC  9(001).
             03  HIY-BC32     PIC  9(001).
           02  HIY-BMC        PIC  9(002).
           02  HIY-BMNO       PIC  9(001).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  HHUHF
       01  HHUHF_HMY265.
           02  HHUHF_PNAME1   PIC  X(005) VALUE "HHUHF".
           02  F              PIC  X(001).
           02  HHUHF_LNAME    PIC  X(012) VALUE "HHUHF_HMY265".
           02  F              PIC  X(001).
           02  HHUHF_KEY1     PIC  X(100) VALUE SPACE.
           02  HHUHF_KEY2     PIC  X(100) VALUE SPACE.
           02  HHUHF_SORT     PIC  X(100) VALUE SPACE.
           02  HHUHF_IDLST    PIC  X(100) VALUE SPACE.
           02  HHUHF_RES      USAGE  POINTER.
       01  HHUH-R.
           02  HHUH-NG        PIC  9(006).
           02  HHUH-HCD       PIC  9(006).
           02  HHUH-NAME      PIC  N(024).
           02  HHUH-TD        PIC  X(084).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　品名別製品受払ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "＜　    年   月 分　＞".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(004).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-INM   PIC  N(009) VALUE
                "［　色　な　し　］".
           02  D-SKM   PIC  N(010) VALUE
                "［　生協　色なし　］".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "380" " " " " RETURNING RESU.
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
            "08C-MID" "X" "15" "23" "22" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "23" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "15" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "15" "27" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "15" "34" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-INM" "N" "12" "24" "18" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKM" "N" "12" "24" "20" "D-INM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-SKM" D-SKM "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 2
               CALL "SD_Output" USING "D-INM" D-INM "p" RETURNING RESU
           END-IF
           PERFORM S-05 THRU S-25.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" HHUHF_PNAME1 " " BY REFERENCE HHUHF_IDLST "0".
      *
           MOVE 0 TO W-FC.
           IF  W-NG = W-MNG
               MOVE 1 TO W-FC
               GO TO M-35
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HIYF_PNAME1 " " BY REFERENCE HIYF_IDLST "0".
       M-10.
      *           READ HIYF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HIYF_PNAME1 BY REFERENCE HIY-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HIY-NG < W-NG
               GO TO M-10
           END-IF
           IF  HIY-NG > W-NG
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               IF  HIY-BC1 < 26 OR > 27
                   GO TO M-10
               END-IF
           END-IF.
       M-15.
           MOVE HIY-R TO W-R.
       M-20.
           IF  JS-SIGN = 1 OR 2
               MOVE ZERO TO WR-HCD2
           ELSE
               PERFORM S-30 THRU S-50
           END-IF.
       M-25.
      *           READ HIYF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HIYF_PNAME1 BY REFERENCE HIY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  HIY-NG > W-NG
               GO TO M-30
           END-IF
           IF  JS-SIGN = 1
               IF  HIY-BC1 < 26 OR > 27
                   GO TO M-25
               END-IF
           END-IF
           IF  JS-SIGN = 0
               MOVE HIY-R TO W-R
               GO TO M-20
           END-IF
           IF  HIY-HCD1 = WR-HCD1
               ADD HIY-ZS TO WR-ZS
               ADD HIY-ZK TO WR-ZK
               ADD HIY-NS TO WR-NS
               ADD HIY-NK TO WR-NK
               ADD HIY-SS TO WR-SS
               ADD HIY-SK TO WR-SK
               ADD HIY-YS TO WR-YS
               ADD HIY-YK TO WR-YK
               ADD HIY-UG TO WR-UG
               GO TO M-20
           END-IF
           PERFORM S-30 THRU S-50.
           GO TO M-15.
       M-30.
           IF  JS-SIGN = 1 OR 2
               PERFORM S-30 THRU S-50
           END-IF
           GO TO M-90.
       M-35.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-40.
      *           READ HUH-M NEXT RECORD WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               IF  HUH-BC1 < 26 OR > 27
                   GO TO M-40
               END-IF
           END-IF.
       M-45.
           MOVE HUH-R TO W-R.
       M-50.
           IF  JS-SIGN = 1 OR 2
               MOVE ZERO TO WR-HCD2
           ELSE
               PERFORM S-30 THRU S-50
           END-IF.
       M-55.
      *           READ HUH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  JS-SIGN = 1
               IF  HUH-BC1 < 26 OR > 27
                   GO TO M-55
               END-IF
           END-IF
           IF  JS-SIGN = 0
               GO TO M-45
           END-IF
           IF  HUH-HCD1 = WR-HCD1
               ADD HUH-ZS TO WR-ZS
               ADD HUH-ZK TO WR-ZK
               ADD HUH-NS TO WR-NS
               ADD HUH-NK TO WR-NK
               ADD HUH-SS TO WR-SS
               ADD HUH-SK TO WR-SK
               ADD HUH-YS TO WR-YS
               ADD HUH-YK TO WR-YK
               ADD HUH-UG TO WR-UG
               GO TO M-50
           END-IF
           PERFORM S-30 THRU S-50.
           GO TO M-45.
       M-60.
           IF  JS-SIGN = 1 OR 2
               PERFORM S-30 THRU S-50
           END-IF.
       M-90.
           IF  W-FC = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HIYF_IDLST HIYF_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HHUHF_IDLST HHUHF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-ENG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-ENG.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           MOVE W-NG TO W-MNG.
           ADD 1 TO W-MGET.
           IF  W-MGET = 13
               MOVE 1 TO W-MGET
               ADD 1 TO W-MNEN
           END-IF
           GO TO S-20.
       S-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO S-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-10
           END-IF
           IF  W-NEN < 2002
               GO TO S-10
           END-IF.
       S-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO S-15
           END-IF
           IF  W-NG > W-MNG
               GO TO S-15
           END-IF.
       S-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-20
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO S-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-20
           END-IF.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WR-ZS AND WR-ZK AND WR-NS AND WR-NK AND
                     WR-SS AND WR-SK AND WR-YS AND WR-YK AND WR-UG
               GO TO S-50
           END-IF
           IF  JS-SIGN = 1 OR 2
               GO TO S-35
           END-IF
           MOVE WR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊　マスター　なし　＊＊" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           GO TO S-45.
       S-35.
           MOVE WR-HCD TO HI-KEY.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-NAME
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO S-45
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-NAME
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO S-45
           END-IF
           IF  WR-HCD1 NOT = HI-HCD1
               MOVE SPACE TO W-NAME
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO S-45
           END-IF
           MOVE SPACE TO W-NAME W-HNA.
           MOVE HI-NAME TO W-HNA.
           MOVE ZERO TO CNT.
       S-40.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-45
           END-IF
           MOVE W-HN(CNT) TO W-NA(CNT).
           IF  W-HN(CNT) NOT = SPACE
               GO TO S-40
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-45
           END-IF
           MOVE W-HN(CNT) TO W-NA(CNT).
           IF  W-HN(CNT) NOT = SPACE
               GO TO S-40
           END-IF.
       S-45.
           MOVE ZERO TO HHUH-R.
           MOVE WR-NG TO HHUH-NG.
           MOVE WR-HCD TO HHUH-HCD.
           MOVE W-NAME TO HHUH-NAME.
           MOVE WR-TD TO HHUH-TD.
      *           WRITE HHUH-R.
      *//////////////
           CALL "DB_Insert" USING
            HHUHF_PNAME1 HHUHF_LNAME HHUH-R RETURNING RET.
       S-50.
           EXIT.
