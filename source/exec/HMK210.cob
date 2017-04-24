       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK210.
      *********************************************************
      *    PROGRAM         :  教育シューズ　販売足数表        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "【　".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  N(002).
           02  F              PIC  N(004) VALUE "月分　】".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　教育シューズ販売足数表　　＊＊＊".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(008) VALUE "日進ゴム株式会社".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "地区名　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(048) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "地区名　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(047) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　出荷数".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　出荷数".
           02  F              PIC  X(003) VALUE SPACE.
       01  W-P.
           02  W-PD    OCCURS  57.
             03  P-15K        PIC  X(005).
             03  P-ADR1       PIC  N(004).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  N(028).
             03  P-NMDA1 REDEFINES P-NAME1.
               04  P-TNM1     PIC  N(026).
               04  F          PIC  N(002).
             03  P-NMDB1 REDEFINES P-NAME1.
               04  F          PIC  N(004).
               04  P-HNM1     PIC  N(024).
             03  P-SU1        PIC --,---,---,--9.
             03  P-SE1        PIC  N(002).
             03  F            PIC  X(001).
             03  P-X          PIC  X(001).
             03  F            PIC  X(002).
             03  P-ADR2       PIC  N(004).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  N(028).
             03  P-NMDA2 REDEFINES P-NAME2.
               04  P-TNM2     PIC  N(026).
               04  F          PIC  N(002).
             03  P-NMDB2 REDEFINES P-NAME2.
               04  F          PIC  N(004).
               04  P-HNM2     PIC  N(024).
             03  P-SU2        PIC --,---,---,--9.
             03  P-SE2        PIC  N(002).
       01  W-D.
           02  W-NGD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  Z(002).
           02  W-ADR          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-UT           PIC S9(005).
           02  W-NAME1.
             03  W-HNA1  OCCURS  24  PIC  N(001).
           02  W-NAME2.
             03  W-HNA2  OCCURS  24  PIC  N(001).
           02  W-NAME  REDEFINES W-NAME2 PIC  N(024).
           02  W-NC           PIC  9(002).
           02  W-PC           PIC  9(001).
           02  W-GC           PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-L            PIC  9(002).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-UKI         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-UKI         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-UKI         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HKSR-F
       01  HKSR-F_HMK210.
           02  HKSR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKSR-F_LNAME   PIC  X(013) VALUE "HKSR-F_HMK210".
           02  F              PIC  X(001).
           02  HKSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HKSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  HKSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HKSR-F_RES     USAGE  POINTER.
       01  HKSR-R.
           02  HK-KEY.
             03  HK-TCD       PIC  9(004).
             03  HK-HCD       PIC  9(004).
           02  HK-SU          PIC S9(006).
           02  HK-UKI         PIC S9(008).
           02  HK-ADR         PIC  9(001).
           02  F              PIC  X(005).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　教育シューズ　販売足数表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  FILLER  PIC  X(007).
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
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "61" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "61" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" " " "24" "0" "34" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME2" "X" "24" "15" "27" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME2" "X" "24" "29" "7" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           MOVE WK0064ID TO HKSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKSR-F_PNAME1 " " BY REFERENCE HKSR-F_IDLST "0".
       M-10.
      *           READ HKSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HK-NG NOT = W-NGD
               GO TO M-10
           END-IF
           IF  ZERO = HK-SU AND HK-UKI
               GO TO M-10
           END-IF
           MOVE W-NEND TO W-NEN.
           MOVE W-GETD TO W-GET.
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           PERFORM SPA-RTN THRU SPA-EX.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE ZERO TO W-LD W-CD WA-D W-PC W-PAGE.
       M-15.
           MOVE ZERO TO WS-D CHK1.
           MOVE HK-ADR TO W-ADR.
           MOVE SPACE TO HKB-KEY.
           MOVE "08" TO HKB-NO.
           MOVE W-ADR TO HKB-KTKC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-KTNA
           END-IF.
       M-20.
           MOVE ZERO TO WT-D W-GC CHK2.
           MOVE HK-TCD TO W-TCD T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF.
       M-25.
           PERFORM MEI-RTN THRU MEI-EX.
       M-40.
      *           READ HKSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HK-NG NOT = W-NGD
               GO TO M-40
           END-IF
           IF  ZERO = HK-SU AND HK-UKI
               GO TO M-40
           END-IF
           IF  W-ADR NOT = HK-ADR
               GO TO M-45
           END-IF
           IF  W-TCD = HK-TCD
               GO TO M-25
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-20.
       M-45.
           PERFORM TOT-RTN THRU TOT-EX.
           PERFORM STO-RTN THRU STO-EX.
           GO TO M-15.
       M-90.
           PERFORM TOT-RTN THRU TOT-EX.
           PERFORM STO-RTN THRU STO-EX.
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　【　　数量総合計　　】　　　" TO P-TNM1(W-LD)
               MOVE WA-SU TO P-SU1(W-LD)
               MOVE "足　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　【　　数量総合計　　】　　　" TO P-TNM2(W-LD)
               MOVE WA-SU TO P-SU2(W-LD)
               MOVE "足　" TO P-SE2(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　【　　金額総合計　　】　　　" TO P-TNM1(W-LD)
               MOVE WA-UKI TO P-SU1(W-LD)
               MOVE "円　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　【　　金額総合計　　】　　　" TO P-TNM2(W-LD)
               MOVE WA-UKI TO P-SU2(W-LD)
               MOVE "円　" TO P-SE2(W-LD)
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       MEI-RTN.
           ADD HK-SU TO WT-SU.
           ADD HK-UKI TO WT-UKI.
           MOVE SPACE TO W-NAME.
           MOVE "　＊＊　マスター　なし　＊＊　　" TO W-NAME.
           MOVE ZERO TO HI-KEY.
           MOVE HK-HCD TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               GO TO MEI-040
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MEI-040
           END-IF
           IF  HI-HCD1 NOT = HK-HCD
               GO TO MEI-040
           END-IF
           MOVE SPACE TO W-NAME1 W-NAME.
           MOVE HI-NAME TO W-NAME1.
           MOVE ZERO TO W-NC.
       MEI-020.
           ADD 1 TO W-NC.
           IF  W-NC = 25
               GO TO MEI-040
           END-IF
           MOVE W-HNA1(W-NC) TO W-HNA2(W-NC).
           IF  W-HNA1(W-NC) NOT = SPACE
               GO TO MEI-020
           END-IF
           ADD 1 TO W-NC.
           IF  W-NC = 25
               GO TO MEI-040
           END-IF
           MOVE W-HNA1(W-NC) TO W-HNA2(W-NC).
           IF  W-HNA1(W-NC) NOT = SPACE
               GO TO MEI-020
           END-IF.
       MEI-040.
           IF  0 = CHK1 OR CHK2
               PERFORM TKG-RTN THRU TKG-EX
           END-IF
           IF  HK-SU = ZERO
               GO TO MEI-EX
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  CHK1 = 0
               GO TO MEI-040
           END-IF
           IF  W-CD = 0
               MOVE W-NAME TO P-HNM1(W-LD)
               MOVE HK-SU TO P-SU1(W-LD)
           ELSE
               MOVE W-NAME TO P-HNM2(W-LD)
               MOVE HK-SU TO P-SU2(W-LD)
           END-IF
           IF  W-GC = 5
               MOVE 9 TO W-GC
           END-IF
           IF  W-GC = 0
               MOVE 5 TO W-GC
           END-IF.
       MEI-EX.
           EXIT.
       TOT-RTN.
           ADD WT-SU TO WS-SU.
           ADD WT-UKI TO WS-UKI.
           IF  W-GC NOT = 9
               GO TO TOT-020
           END-IF
           IF  CHK1 = 0
               PERFORM TKG-RTN THRU TKG-EX
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　　　　　　　　　合　計　　　" TO P-HNM1(W-LD)
               MOVE WT-SU TO P-SU1(W-LD)
               MOVE "足　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　　　　　　合　計　　　" TO P-HNM2(W-LD)
               MOVE WT-SU TO P-SU2(W-LD)
               MOVE "足　" TO P-SE2(W-LD)
           END-IF.
       TOT-020.
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  CHK1 = 0
               PERFORM TKG-RTN THRU TKG-EX
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　　　　　　　　出荷金額　　　" TO P-HNM1(W-LD)
               MOVE WT-UKI TO P-SU1(W-LD)
               MOVE "円　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　　　　　出荷金額　　　" TO P-HNM2(W-LD)
               MOVE WT-UKI TO P-SU2(W-LD)
               MOVE "円　" TO P-SE2(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       TOT-EX.
           EXIT.
       STO-RTN.
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　　　　［　　数量小計　　］　" TO P-TNM1(W-LD)
               MOVE WS-SU TO P-SU1(W-LD)
               MOVE "足　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　［　　数量小計　　］　" TO P-TNM2(W-LD)
               MOVE WS-SU TO P-SU2(W-LD)
               MOVE "足　" TO P-SE2(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE "　　　　［　　金額小計　　］　" TO P-TNM1(W-LD)
               MOVE WS-UKI TO P-SU1(W-LD)
               MOVE "円　" TO P-SE1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　［　　金額小計　　］　" TO P-TNM2(W-LD)
               MOVE WS-UKI TO P-SU2(W-LD)
               MOVE "円　" TO P-SE2(W-LD)
           END-IF
           PERFORM HKG-RTN THRU HKG-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           ADD WS-SU TO WA-SU.
           ADD WS-UKI TO WA-UKI.
       STO-EX.
           EXIT.
       TKG-RTN.
           ADD 1 TO W-LD.
           IF  W-LD < 57
               GO TO TKG-020
           END-IF
           IF  W-LD = 57
               IF  W-CD = 0
                   MOVE ":" TO P-X(W-LD)
               END-IF
           END-IF
           IF  W-CD = 0
               MOVE 5 TO W-CD
               MOVE ZERO TO W-LD CHK
               GO TO TKG-RTN
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM SPA-RTN THRU SPA-EX.
           MOVE ZERO TO W-LD W-CD CHK.
           GO TO TKG-RTN.
       TKG-020.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  CHK1 = 0
               MOVE 5 TO CHK1 CHK2
               IF  W-CD = 0
                   MOVE HKB-KTNA TO P-ADR1(W-LD)
                   MOVE T-NAME TO P-TNM1(W-LD)
               ELSE
                   MOVE HKB-KTNA TO P-ADR2(W-LD)
                   MOVE T-NAME TO P-TNM2(W-LD)
               END-IF
           END-IF
           IF  CHK2 = 0
               MOVE 5 TO CHK2
               IF  W-CD = 0
                   MOVE T-NAME TO P-TNM1(W-LD)
               ELSE
                   MOVE T-NAME TO P-TNM2(W-LD)
               END-IF
           END-IF.
       TKG-EX.
           EXIT.
       HKG-RTN.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               GO TO HKG-EX
           END-IF
           IF  W-CD = 0
               MOVE 5 TO W-CD
               MOVE ZERO TO W-LD CHK
               GO TO HKG-RTN
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM SPA-RTN THRU SPA-EX.
           MOVE ZERO TO W-LD W-CD CHK.
           GO TO HKG-RTN.
       HKG-EX.
           EXIT.
       PRI-RTN.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           ELSE
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE ZERO TO W-LD.
       PRI-020.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO PRI-020
               END-IF
           END-IF.
       PRI-EX.
           EXIT.
       SPA-RTN.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       SPA-020.
           ADD 1 TO W-LD.
           IF  W-LD < 58
               MOVE W-15K TO P-15K(W-LD)
               MOVE SPACE TO P-ADR1(W-LD) P-NAME1(W-LD) P-SE1(W-LD)
                             P-ADR2(W-LD) P-NAME2(W-LD) P-SE2(W-LD)
               GO TO SPA-020
           END-IF
           MOVE ZERO TO W-LD.
       SPA-EX.
           EXIT.
