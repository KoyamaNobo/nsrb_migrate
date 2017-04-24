       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG010.
      **************************************
      *****     銀行別　割引手形帳     *****
      *****      ( FDL : FTG010 )      *****
      **************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  H-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  H-4K           PIC  X(008).
           02  H-BKM          PIC  N(004).
           02  H-BKN          PIC  N(008).
           02  H-2K           PIC  X(008).
           02  F              PIC  X(046).
           02  H-PM           PIC  X(002).
           02  H-PAGE         PIC Z9.
           02  H-20K          PIC  X(005).
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-IDO          PIC  9(006).
           02  P-NO           PIC  9(004).
           02  P-X1           PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-X2           PIC  X(001).
           02  P-X3           PIC  X(001).
           02  P-MFM          PIC  N(002).
           02  P-X4           PIC  X(001).
           02  P-KBN          PIC  N(002).
           02  P-MAN          PIC  9(006).
           02  P-KIN          PIC  -(011).
           02  P-ZAN          PIC  -(011).
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-IDO.
             03  W-NEN        PIC  9(002).
             03  W-GP         PIC  9(004).
           02  W-PS           PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-ZAN          PIC S9(010).
           02  W-PAGE         PIC  9(002).
       01  W-CK.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  W-4K           PIC  X(008) VALUE X"1A26212068222176".
           02  W-2K           PIC  X(008) VALUE X"1A26212068212078".
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIBANK.
           COPY LIUKET.
           COPY LSPF.
      *FD  TYB-F
       01  TYB-F_TSG010.
           02  TYB-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TYB-F_LNAME    PIC  X(012) VALUE "TYB-F_TSG010".
           02  F              PIC  X(001).
           02  TYB-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT     PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TYB-F_RES      USAGE  POINTER.
       01  TYB-R.
           02  Y-YBK          PIC  9(004).
           02  Y-IDO.
             03  Y-N          PIC  X(002).
             03  Y-GP         PIC  9(004).
           02  Y-MAN          PIC  9(006).
           02  Y-TCD          PIC  9(004).
           02  Y-KBN          PIC  9(002).
           02  Y-NO           PIC  9(004).
           02  Y-KIN          PIC  9(010).
           02  Y-FUC          PIC  9(001).
           02  F              PIC  X(006).
           02  Y-NEN          PIC  9(004).
           02  F              PIC  X(017).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊　　割引手形帳　作表　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-ZERR.
             03  FILLER.
               04  FILLER  PIC  N(006) VALUE
                     "銀行マスター".
               04  FILLER  PIC --,---,---,--- .
             03  FILLER.
               04  FILLER  PIC  N(006) VALUE
                     "割引ファイル".
               04  FILLER  PIC --,---,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(012) VALUE
                   "／　ＢＡＮＫＭ　無し　／".
             03  E-ME2   PIC  N(012) VALUE
                   "／　ＵＫＥＴＭ　無し　／".
             03  E-ME3   PIC  N(010) VALUE
                   "／　残高　エラー　／".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-YBK   PIC  9(004).
             03  E-KEY   PIC  9(004).
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG010" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "252" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZERR" " " "0" "0" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZERR" " " "13" "0" "26" " " "D-ZERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZERR" "N" "13" "15" "12" " " "01D-ZERR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ZERR" "--,---,---,---" "13" "27" "14" "0101D-ZERR"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-ZERR" BY REFERENCE B-YBZ "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZERR" " " "15" "0" "26" "01D-ZERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZERR" "N" "15" "15" "12" " " "02D-ZERR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ZERR" "--,---,---,---" "15" "27" "14" "0102D-ZERR"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-ZERR" BY REFERENCE W-ZAN "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "138" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "138" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "24" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "N" "24" "15" "20" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-YBK" "9" "24" "40" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-YBK" BY REFERENCE Y-YBK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "40" "4" "E-YBK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE Y-NO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-PS.
           CALL "PR_Open" RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
       M-10.
      *           READ BANK-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  B-YBC NOT = 1
               GO TO M-10
           END-IF
           MOVE ZERO TO W-PAGE W-IDO W-PC.
           MOVE B-ZYZ TO W-ZAN.
           IF  W-ZAN = ZERO
               GO TO M-15
           END-IF
           IF  W-PS = ZERO
               MOVE 5 TO W-PS
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE 5 TO W-PC.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-MFM P-KBN.
           MOVE  "　　　　　　　　（　前　月　繰　越　）" TO P-NAME.
           MOVE B-ZYZ TO P-ZAN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-15.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TYB-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
       M-20.
      *           READ TYB-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TYB-F_PNAME1 BY REFERENCE TYB-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TYB-F_IDLST TYB-F_PNAME1
               GO TO M-10
           END-IF
           IF  B-KEY NOT = Y-YBK
               GO TO M-20
           END-IF
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               IF  W-PS = ZERO
                   MOVE 5 TO W-PS
                   PERFORM S-10 THRU S-15
               ELSE
                   PERFORM S-05 THRU S-15
               END-IF
           END-IF.
       M-25.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-MFM P-KBN.
           IF  W-GP NOT = Y-GP
               MOVE Y-IDO TO W-IDO
               MOVE Y-IDO TO P-IDO
           END-IF
           MOVE Y-NO TO P-NO.
           MOVE Y-NO TO UT-KEY.
      *           READ UKET-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE UT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "　＊＊　　得意先　無し　　＊＊" TO T-NAME
           END-IF
           IF  UT-FDM = SPACE
               MOVE T-NAME TO P-NAME
           ELSE
               MOVE UT-FDM TO P-NAME
           END-IF
           IF  Y-KBN = 11
               MOVE  "約手" TO P-KBN
           END-IF
           IF  Y-KBN = 12
               MOVE  "為手" TO P-KBN
           END-IF
           IF  Y-KBN = 13
               MOVE "電債" TO P-KBN
           END-IF
           MOVE Y-KIN TO P-KIN.
           IF  Y-FUC = 1 OR 9
               MOVE "<" TO P-X3
               MOVE ">" TO P-X4
               SUBTRACT Y-KIN FROM W-ZAN
           END-IF
           IF  Y-FUC = 1
               MOVE  "戻し" TO P-MFM
               GO TO M-35
           END-IF
           IF  Y-FUC = 9
               MOVE  "不渡" TO P-MFM
               GO TO M-35
           END-IF
           IF  Y-MAN = 999999
               SUBTRACT Y-KIN FROM W-ZAN
               GO TO M-35
           END-IF
           MOVE Y-MAN TO P-MAN.
           ADD Y-KIN TO W-ZAN.
       M-35.
      *           READ TYB-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TYB-F_PNAME1 BY REFERENCE TYB-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  B-KEY NOT = Y-YBK
               GO TO M-35
           END-IF
           IF  W-GP NOT = Y-GP
               GO TO M-40
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-25.
       M-40.
           MOVE W-ZAN TO P-ZAN.
           PERFORM S-20 THRU S-25.
           MOVE ZERO TO W-IDO.
           GO TO M-25.
       M-90.
           MOVE W-ZAN TO P-ZAN.
           PERFORM S-20 THRU S-25.
           IF  W-ZAN NOT = B-YBZ
               CALL "SD_Output" USING
                "D-ZERR" D-ZERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-YBK" E-YBK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO HEAD1.
           MOVE W-15K TO H-15K.
           MOVE W-4K TO H-4K.
           MOVE W-2K TO H-2K.
           MOVE W-20K TO H-20K.
           MOVE  "銀行名　" TO H-BKM.
           MOVE B-BNA TO H-BKN.
           MOVE "P." TO H-PM.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER = 64
               MOVE W-IDO TO P-IDO
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  UT-FDM = SPACE
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-MFM P-KBN.
           MOVE "(" TO P-X1.
           MOVE ")" TO P-X2.
           MOVE T-NAME TO P-NAME.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER = 64
               MOVE W-IDO TO P-IDO
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
