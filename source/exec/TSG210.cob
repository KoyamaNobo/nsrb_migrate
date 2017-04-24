       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG210.
      ****************************************************
      *****                                          *****
      *****     割引手形　決済予定表　（次月分）     *****
      *****            ( FDL : FTG210 )              *****
      ****************************************************
       AUTHOR.     F-KOTAKE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(001).
           02  F              PIC  N(004) VALUE "銀行名　".
           02  H-BKN          PIC  N(008).
           02  F              PIC  X(031).
           02  H-NEN          PIC Z9.
           02  F              PIC  X(003).
           02  H-GET          PIC Z9.
           02  F              PIC  X(003).
           02  H-PEY          PIC Z9.
           02  F              PIC  X(008).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-MAN.
             03  P-MNEN       PIC Z9.
             03  P-MGET       PIC Z9.
             03  P-MPEY       PIC Z9.
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  P-X1           PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-X2           PIC  X(001).
           02  F              PIC  X(004).
           02  P-KEY          PIC  9(004).
           02  P-KBN          PIC  N(002).
           02  P-KIN          PIC  Z(011).
           02  F              PIC  X(013).
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-TKIN         PIC  9(011).
           02  W-YBK          PIC  9(004).
           02  W-MAN          PIC  9(006).
           02  W-MAND  REDEFINES W-MAN.
             03  W-MNEN       PIC  9(002).
             03  W-MGET       PIC  9(002).
             03  W-MPEY       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
       01  W-DATED.
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-OKD          PIC  9(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSUKET.
           COPY LIBANK.
           COPY LSPF.
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　割引手形　次月決済　予定表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(030) VALUE
                "<   H   年   月分 打ち出し   >".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG210" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "374" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "18" "30" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "14" "24" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "14" "29" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP W-SNG.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT DATE-YC1 FROM W-NEN.
           MOVE D-NTNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           COMPUTE W-WNEN = W-SNEN - DATE-YC1.
           MOVE W-SGET TO W-WGET.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ADD 1 TO W-WGET.
           IF  W-WGET  = 13
               ADD 1 TO W-WNEN
               MOVE 1 TO W-WGET
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-WGET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-SNG.
           MOVE W-WNG TO W-SNGS.
           ADD DATE-YC1 TO W-SNEN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
       M-30.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-30
           END-IF
           IF  UT-ONG NOT = W-SNGS
               GO TO M-30
           END-IF
           MOVE W-NEN2 TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
       M-35.
           MOVE UT-SBC TO W-YBK.
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　なし" TO B-BNA
           END-IF
           MOVE B-BNA TO H-BKN.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-TKIN.
       M-40.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN.
           IF  UT-OKD = W-OKD
               GO TO M-45
           END-IF
           MOVE UT-OKD TO W-OKD W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT DATE-YC1 FROM W-NEN.
           MOVE W-NGPS TO W-MAN.
           MOVE W-MNEN TO P-MNEN.
           MOVE W-MGET TO P-MGET.
           MOVE W-MPEY TO P-MPEY.
       M-45.
           MOVE UT-TCD TO P-TCD.
           MOVE UT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　得意先　無し　　＊＊" TO T-NAME
           END-IF
           IF  UT-FDM = SPACE
               MOVE T-NAME TO P-NAME
           ELSE
               MOVE "(" TO P-X1
               MOVE UT-FDM TO P-NAME
               MOVE ")" TO P-X2
           END-IF
           MOVE UT-KEY TO P-KEY.
           IF  UT-TSC = 11
               MOVE "約手" TO P-KBN
           END-IF
           IF  UT-TSC = 12
               MOVE "為手" TO P-KBN
           END-IF
           IF  UT-TSC = 13
               MOVE "電債" TO P-KBN
           END-IF
           MOVE UT-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-MNEN TO P-MNEN
               MOVE W-MGET TO P-MGET
               MOVE W-MPEY TO P-MPEY
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD UT-KIN TO W-TKIN.
       M-50.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-50
           END-IF
           IF  UT-ONG NOT = W-SNGS
               GO TO M-50
           END-IF
           IF  W-YBK = UT-SBC
               GO TO M-40
           END-IF.
       M-55.
           PERFORM S-20 THRU S-25.
           GO TO M-35.
       M-90.
           PERFORM S-20 THRU S-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE HEAD TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "　　　　　　　　　　　　【　合　計　】　" TO P-NAME.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
