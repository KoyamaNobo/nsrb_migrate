       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG220.
      ******************************************************
      *****                                            *****
      *****     割引手形決済予定表　（期日別合計）     *****
      *****             ( FDL : FTG210 )               *****
      ******************************************************
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
           02  F              PIC  N(004) VALUE  "銀行名　".
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
           02  P-20K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-MAN.
             03  P-MNEN       PIC Z9.
             03  P-MGET       PIC Z9.
             03  P-MPEY       PIC Z9.
             03  P-MPM   REDEFINES P-MPEY  PIC  N(001).
           02  F              PIC  X(001).
           02  P-EM           PIC  N(001).
           02  F              PIC  X(026).
           02  P-NAME         PIC  N(008).
           02  F              PIC  X(012).
           02  P-KIN          PIC  Z(011).
           02  F              PIC  X(013).
       01  W-DATA.
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
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENDL REDEFINES W-NEND.
               04  W-NEND1    PIC  9(002).
               04  W-NEND2    PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGDL  REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGDS       PIC  9(004).
           02  W-MAN.
             03  W-MNEN       PIC  9(002).
             03  W-MGET       PIC  9(002).
           02  W-YBK          PIC  9(004).
           02  W-TKIN         PIC  9(011).
           02  W-KIN.
             03  W-KIN1       PIC  9(011).
             03  W-KIN2       PIC  9(011).
             03  W-KIN3       PIC  9(011).
             03  W-KIN4       PIC  9(011).
           02  CHK            PIC  9(001).
       01  W-ENO              PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
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
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊　　割引手形　期日別　決済予定表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-ENO   PIC  9(002).
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
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "12" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ENO" "9" "24" "1" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-ENO" BY REFERENCE W-ENO "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT DATE-YC1 FROM W-NEN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
       M-10.
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
               GO TO M-10
           END-IF
           MOVE W-NEN2 TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "PR_Open" RETURNING RESP.
       M-15.
           MOVE UT-SBC TO W-YBK.
           MOVE W-YBK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "ＢＡＮＫＭ　なし" TO B-BNA
           END-IF
           MOVE B-BNA TO H-BKN.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF.
       M-20.
           MOVE ZERO TO W-NGD.
           MOVE UT-ONG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE ZERO TO W-MAN.
           COMPUTE W-MNEN = W-NEND - DATE-YC1.
           MOVE W-GETD TO W-MGET.
           MOVE ZERO TO W-KIN.
       M-25.
           IF  UT-OKP < 6
               ADD UT-KIN TO W-KIN1
           END-IF
           IF  UT-OKP > 5 AND < 11
               ADD UT-KIN TO W-KIN2
           END-IF
           IF  UT-OKP > 10 AND < 26
               ADD UT-KIN TO W-KIN3
           END-IF
           IF  UT-OKP > 25
               ADD UT-KIN TO W-KIN4
           END-IF.
       M-30.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-30
           END-IF
           IF  W-YBK NOT = UT-SBC
               GO TO M-35
           END-IF
           IF  UT-ONG = W-NGDS
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-50.
           GO TO M-20.
       M-35.
           PERFORM S-20 THRU S-50.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-50.
       M-95.
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
           MOVE SPACE TO SP-R.
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
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-EM P-NAME.
           MOVE W-MNEN TO P-MNEN.
           MOVE W-MGET TO P-MGET.
           IF  W-KIN1 NOT = ZERO
               MOVE 5 TO P-MPEY
               MOVE  "迄" TO P-EM
               MOVE W-KIN1 TO P-KIN
               PERFORM S-55 THRU S-60
           END-IF
           IF  W-KIN2 NOT = ZERO
               MOVE 10 TO P-MPEY
               MOVE  "迄" TO P-EM
               MOVE W-KIN2 TO P-KIN
               PERFORM S-55 THRU S-60
           END-IF
           IF  W-KIN3 NOT = ZERO
               MOVE 25 TO P-MPEY
               MOVE  "迄" TO P-EM
               MOVE W-KIN3 TO P-KIN
               PERFORM S-55 THRU S-60
           END-IF
           IF  W-KIN4 NOT = ZERO
               MOVE  "末" TO P-MPM
               MOVE  "迄" TO P-EM
               MOVE W-KIN4 TO P-KIN
               PERFORM S-55 THRU S-60
           END-IF
           COMPUTE W-TKIN = W-KIN1 + W-KIN2 + W-KIN3 + W-KIN4.
           MOVE  "【　合　計　】　" TO P-NAME.
           MOVE W-TKIN TO P-KIN.
           PERFORM S-55 THRU S-60.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
       S-50.
           EXIT.
       S-55.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-MNEN TO P-MNEN
               MOVE W-MGET TO P-MGET
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P SP-R.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-EM P-NAME.
       S-60.
           EXIT.
