       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK250.
      *********************************************************
      *    PROGRAM         :  教育シューズ　販売足数合計表    *
      *    PRINTER TYPE    :  JIPS*                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(002) VALUE "【　".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  N(002).
           02  F              PIC  N(004) VALUE "月分　】".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　教育シューズ　販売足数　合計表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(007) VALUE "品　　　　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(003) VALUE "出荷数".
           02  F              PIC  X(007) VALUE "   :   ".
           02  F              PIC  N(007) VALUE "品　　　　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(003) VALUE "出荷数".
       01  W-P.
           02  W-PD    OCCURS  57.
             03  P-15K        PIC  X(005).
             03  P-NAME1      PIC  N(024).
             03  P-SU1        PIC ----,---,--9.
             03  F            PIC  X(003).
             03  P-X          PIC  X(001).
             03  F            PIC  X(003).
             03  P-NAME2      PIC  N(024).
             03  P-SU2        PIC ----,---,--9.
       01  W-D.
           02  W-NGD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  Z(002).
           02  W-NC           PIC  9(001).
           02  W-HCD          PIC  9(004).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NMD          PIC  N(024).
           02  W-NAMED REDEFINES W-NMD.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NAD          PIC  N(024).
           02  W-NAME  REDEFINES W-NAD.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-SU           PIC S9(007).
           02  W-SSU          PIC S9(008).
           02  W-ASU          PIC S9(008).
           02  W-PAGE         PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  W-PC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKS-F
       01  HKS-F_HMK250.
           02  HKS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMK250".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  HK-TCD         PIC  9(004).
           02  HK-HCD1        PIC  9(004).
           02  HK-SU          PIC S9(006).
           02  HK-KIN         PIC S9(008).
           02  HK-AC          PIC  9(001).
           02  HK-NC          PIC  9(001).
           02  F              PIC  X(004).
           02  HK-NG          PIC  9(004).
           02  F              PIC  X(032).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　教育シューズ　販売足数　合計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
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
           MOVE DATE-02R TO H-DATE.
           MOVE D-NING TO W-NGD.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
       M-10.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKS-F_IDLST HKS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HK-NG NOT = W-NGD
               GO TO M-10
           END-IF
           IF  HK-SU = ZERO
               GO TO M-10
           END-IF
           MOVE W-NEND TO W-NEN.
           MOVE W-GETD TO W-GET.
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           PERFORM S-85 THRU S-95.
           MOVE ZERO TO W-ASU W-PC W-PAGE W-LD W-CD.
       M-15.
           MOVE HK-NC TO W-NC.
           MOVE ZERO TO W-SSU.
       M-20.
           MOVE HK-HCD1 TO W-HCD.
           MOVE ZERO TO W-SU.
       M-25.
           ADD HK-SU TO W-SU.
       M-30.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HK-NG NOT = W-NGD
               GO TO M-30
           END-IF
           IF  HK-SU = ZERO
               GO TO M-30
           END-IF
           IF  HK-NC NOT = W-NC
               GO TO M-35
           END-IF
           IF  HK-HCD1 = W-HCD
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-35.
           GO TO M-20.
       M-35.
           PERFORM S-20 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-60 THRU S-65.
           IF  W-CD = 0
               MOVE "　【　　総　合　計　　】　　　" TO P-NAME1(W-LD)
               MOVE W-ASU TO P-SU1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　【　　総　合　計　　】　　　" TO P-NAME2(W-LD)
               MOVE W-ASU TO P-SU2(W-LD)
           END-IF
           PERFORM S-70 THRU S-80.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  W-SU = ZERO
               GO TO S-35
           END-IF
           MOVE SPACE TO W-NAD.
           MOVE "　＊＊　マスター　なし　＊＊　" TO W-NAD.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
           IF  HI-HCD1 NOT = W-HCD
               GO TO S-30
           END-IF
           MOVE SPACE TO W-NMD W-NAD.
           MOVE ZERO TO CNT.
           MOVE HI-NAME TO W-NMD.
       S-25.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF.
       S-30.
           PERFORM S-60 THRU S-65.
           IF  W-CD = 0
               MOVE W-NAD TO P-NAME1(W-LD)
               MOVE W-SU TO P-SU1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE W-NAD TO P-NAME2(W-LD)
               MOVE W-SU TO P-SU2(W-LD)
           END-IF
           ADD W-SU TO W-SSU.
           PERFORM S-60 THRU S-65.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       S-35.
           EXIT.
       S-40.
           PERFORM S-60 THRU S-65.
           IF  W-CD = 0
               MOVE "　　　　　［　　小　計　　］　" TO P-NAME1(W-LD)
               MOVE W-SSU TO P-SU1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　　［　　小　計　　］　" TO P-NAME2(W-LD)
               MOVE W-SSU TO P-SU2(W-LD)
           END-IF
           ADD W-SSU TO W-ASU.
           PERFORM S-60 THRU S-65.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       S-45.
           EXIT.
       S-60.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               GO TO S-65
           END-IF
           IF  W-CD = 0
               MOVE 5 TO W-CD
               MOVE ZERO TO W-LD
               GO TO S-60
           END-IF
           PERFORM S-70 THRU S-80.
           PERFORM S-85 THRU S-95.
           MOVE ZERO TO W-LD W-CD.
           GO TO S-60.
       S-65.
           EXIT.
       S-70.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-75.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               MOVE SPACE TO SP-R
               MOVE W-PD(W-LD) TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-75
           END-IF.
       S-80.
           EXIT.
       S-85.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-90.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               MOVE W-15K TO P-15K(W-LD)
               MOVE SPACE TO P-NAME1(W-LD) P-NAME2(W-LD)
               GO TO S-90
           END-IF
           MOVE ZERO TO W-LD.
       S-95.
           EXIT.
