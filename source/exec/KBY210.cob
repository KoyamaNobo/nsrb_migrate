       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY210.
      *********************************************************
      *    PROGRAM         :  年間品目区分別集計表　　        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PCNT               PIC  9(002) VALUE ZERO.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　年　間　品　目　区　分　別　集　計　表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE1        PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(044) VALUE
                " 日付　　 原　　反　　型　　底　クラリーノ　".
           02  F              PIC  X(044) VALUE
                " 中　　底 糸･紐･ﾃｰﾌﾟ       　糊     馳･鳩目 ".
           02  F              PIC  X(044) VALUE
                "　ｼｰﾙ･ﾈｰﾑ　 紙　　函　　 ケース　　 合　　計".
       01  HEAD3.
           02  F              PIC  X(046) VALUE
                "       I------------   素　　材   -----------I".
           02  F              PIC  X(041) VALUE
                "  I-----------   工　　　品   ----------I".
           02  F              PIC  X(045) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(046) VALUE
                " 日付        ゴ　ム      薬   品      合　　計".
           02  F              PIC  X(041) VALUE
                "      防　　振     　その他      合　　計".
           02  F              PIC  X(045) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(026) VALUE
                "       I---------------   ".
           02  F              PIC  X(036) VALUE
                "　中　国　送　り   --------------I  ".
           02  F              PIC  X(040) VALUE
                "I----------   製　品　仕　入  ---------I".
           02  F              PIC  X(030) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(036) VALUE
                " 日付      　原反他    ゴム･薬品    ".
           02  F              PIC  X(040) VALUE
                " 送　　料       合　　計        　履　物".
           02  F              PIC  X(042) VALUE
                "     工　品       合　　計   　 総　合　計".
           02  F              PIC  X(014) VALUE SPACE.
       01  W-P1.
           02  P-NG1          PIC 99/99B.
           02  P-NGD1  REDEFINES P-NG1  PIC  X(006).
           02  P-GNT          PIC ----,---,--9.
           02  P-KTZ          PIC ----,---,--9.
           02  P-KRR          PIC ----,---,--9.
           02  P-NKZ          PIC ---,---,--9.
           02  P-IHT          PIC ---,---,--9.
           02  P-NRI          PIC ---,---,--9.
           02  P-HSH          PIC ---,---,--9.
           02  P-SNM          PIC ---,---,--9.
           02  P-KMB          PIC ---,---,--9.
           02  P-KAS          PIC ---,---,--9.
           02  P-HMT          PIC -----,---,--9.
       01  W-P2.
           02  P-NG2          PIC 99/99.
           02  P-NGD2  REDEFINES P-NG2  PIC  X(005).
           02  P-SZG          PIC --,---,---,--9.
           02  P-SZY          PIC -----,---,--9.
           02  P-SZT          PIC --,---,---,--9.
           02  P-KHB          PIC --,---,---,--9.
           02  P-KHS          PIC -----,---,--9.
           02  P-KHT          PIC --,---,---,--9.
       01  W-P3.
           02  P-NG3          PIC 99/99.
           02  P-NGD3  REDEFINES P-NG3  PIC  X(005).
           02  P-CGG          PIC --,---,---,--9.
           02  P-CGY          PIC -----,---,--9.
           02  P-CGS          PIC -----,---,--9.
           02  P-CGT          PIC ---,---,---,--9.
           02  P-SSH          PIC ----,---,---,--9.
           02  P-SSK          PIC ---,---,--9.
           02  P-SST          PIC ---,---,---,--9.
           02  P-AT           PIC ----,---,---,--9.
           02  F              PIC  X(014).
       01  W-D1.
           02  W-NG1          PIC  9(004).
           02  W-GNT          PIC S9(009).
           02  W-KTZ          PIC S9(009).
           02  W-KRR          PIC S9(009).
           02  W-NKZ          PIC S9(008).
           02  W-IHT          PIC S9(008).
           02  W-NRI          PIC S9(008).
           02  W-HSH          PIC S9(008).
           02  W-SNM          PIC S9(008).
           02  W-KMB          PIC S9(008).
           02  W-KAS          PIC S9(008).
           02  W-HMT          PIC S9(010).
       01  WS-D1.
           02  WS-GNT         PIC S9(009).
           02  WS-KTZ         PIC S9(009).
           02  WS-KRR         PIC S9(009).
           02  WS-NKZ         PIC S9(008).
           02  WS-IHT         PIC S9(008).
           02  WS-NRI         PIC S9(008).
           02  WS-HSH         PIC S9(008).
           02  WS-SNM         PIC S9(008).
           02  WS-KMB         PIC S9(008).
           02  WS-KAS         PIC S9(008).
           02  WS-HMT         PIC S9(010).
       01  W-D2.
           02  W-NG2          PIC  9(004).
           02  W-SZG          PIC S9(010).
           02  W-SZY          PIC S9(009).
           02  W-SZT          PIC S9(010).
           02  W-KHB          PIC S9(010).
           02  W-KHS          PIC S9(009).
           02  W-KHT          PIC S9(010).
       01  WS-D2.
           02  WS-SZG         PIC S9(010).
           02  WS-SZY         PIC S9(009).
           02  WS-SZT         PIC S9(010).
           02  WS-KHB         PIC S9(010).
           02  WS-KHS         PIC S9(009).
           02  WS-KHT         PIC S9(010).
       01  W-D3.
           02  W-NG3          PIC  9(004).
           02  W-CGG          PIC S9(010).
           02  W-CGY          PIC S9(009).
           02  W-CGS          PIC S9(009).
           02  W-CGT          PIC S9(010).
           02  W-SSH          PIC S9(009).
           02  W-SSK          PIC S9(008).
           02  W-SST          PIC S9(010).
           02  W-AT           PIC S9(010).
       01  WS-D3.
           02  WS-CGG         PIC S9(010).
           02  WS-CGY         PIC S9(009).
           02  WS-CGS         PIC S9(009).
           02  WS-CGT         PIC S9(010).
           02  WS-SSH         PIC S9(009).
           02  WS-SSK         PIC S9(008).
           02  WS-SST         PIC S9(010).
           02  WS-AT          PIC S9(010).
       01  WA-D.
           02  CNT            PIC  9(002).
           02  WA-AT  OCCURS 12  PIC S9(010).
       01  ERR-STAT           PIC  X(002).
       01  W-JCD.
           02  W-01           PIC  9(001).
           02  W-02           PIC  9(002).
      *
           COPY LIBFDD.
           COPY LSPF.
      *FD  JSY-F
       01  JSY-F_KBY210.
           02  JSY-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSY-F_LNAME    PIC  X(012) VALUE "JSY-F_KBY210".
           02  F              PIC  X(001).
           02  JSY-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSY-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSY-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSY-F_RES      USAGE  POINTER.
       01  JSY-R.
           02  F              PIC  9(002).
           02  JS-DATE        PIC  9(004).
           02  JS-JCD1        PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-JCD2        PIC  9(002).
           02  JS-KIN         PIC S9(010).
           02  F              PIC  X(044).
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
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　　年間品目区分別集計表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(025) VALUE
                  "***  JSY-F DATE 無し  ***".
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
            "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "37" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "37" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "25" "E-STAT" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JSY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSY-F_PNAME1 " " BY REFERENCE JSY-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D.
       M-10.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  JS-JCD1 NOT = 0 AND 6
               GO TO M-10
           END-IF
           IF  JS-JCD2 > 49
               GO TO M-10
           END-IF
           MOVE DATE-05R TO H-DATE1.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WS-D1.
       M-15.
           MOVE ZERO TO W-D1.
           MOVE JS-DATE TO W-NG1.
       M-20.
           PERFORM S-05 THRU S-10.
       M-25.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  JS-JCD1 NOT = 0 AND 6
               GO TO M-25
           END-IF
           IF  JS-JCD2 > 49
               GO TO M-25
           END-IF
           IF  W-NG1 = JS-DATE
               GO TO M-20
           END-IF
           PERFORM S-15 THRU S-20.
           GO TO M-15.
       M-30.
           PERFORM S-15 THRU S-20.
           MOVE SPACE TO SP-R W-P1.
           MOVE "合 計 " TO P-NGD1.
           MOVE WS-GNT TO P-GNT.
           MOVE WS-KTZ TO P-KTZ.
           MOVE WS-KRR TO P-KRR.
           MOVE WS-NKZ TO P-NKZ.
           MOVE WS-IHT TO P-IHT.
           MOVE WS-NRI TO P-NRI.
           MOVE WS-HSH TO P-HSH.
           MOVE WS-SNM TO P-SNM.
           MOVE WS-KMB TO P-KMB.
           MOVE WS-KAS TO P-KAS.
           MOVE WS-HMT TO P-HMT.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE JSY-F_IDLST JSY-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JSY-F_PNAME1 " " BY REFERENCE JSY-F_IDLST "0".
           MOVE ZERO TO CNT.
       M-40.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-65
           END-IF
           IF  JS-JCD1 NOT = 2 AND 3 AND 4
               GO TO M-40
           END-IF
           MOVE JS-JCD1 TO W-01.
           MOVE JS-JCD2 TO W-02.
           IF  W-JCD = "490"
               GO TO M-40
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WS-D2.
       M-45.
           MOVE ZERO TO W-D2.
           MOVE JS-DATE TO W-NG2.
       M-50.
           PERFORM S-25 THRU S-35.
       M-55.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  JS-JCD1 NOT = 2 AND 3 AND 4
               GO TO M-55
           END-IF
           MOVE JS-JCD1 TO W-01.
           MOVE JS-JCD2 TO W-02.
           IF  W-JCD = "490"
               GO TO M-55
           END-IF
           IF  W-NG2 = JS-DATE
               GO TO M-50
           END-IF
           PERFORM S-40 THRU S-45.
           GO TO M-45.
       M-60.
           PERFORM S-40 THRU S-45.
           MOVE SPACE TO SP-R W-P2.
           MOVE "合 計" TO P-NGD2.
           MOVE WS-SZG TO P-SZG.
           MOVE WS-SZY TO P-SZY.
           MOVE WS-SZT TO P-SZT.
           MOVE WS-KHB TO P-KHB.
           MOVE WS-KHS TO P-KHS.
           MOVE WS-KHT TO P-KHT.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-65.
           CALL "DB_F_Close" USING
            BY REFERENCE JSY-F_IDLST JSY-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JSY-F_PNAME1 " " BY REFERENCE JSY-F_IDLST "0".
           MOVE ZERO TO CNT.
       M-70.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-JCD2 NOT = 90
               IF  JS-JCD1 NOT = 1 AND 7
                   GO TO M-70
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WS-D3.
       M-75.
           MOVE ZERO TO W-D3.
           MOVE JS-DATE TO W-NG3.
       M-80.
           PERFORM S-50 THRU S-60.
       M-85.
      *           READ JSY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSY-F_PNAME1 BY REFERENCE JSY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-JCD2 NOT = 90
               IF  JS-JCD1 NOT = 1 AND 7
                   GO TO M-85
               END-IF
           END-IF
           IF  W-NG3 = JS-DATE
               GO TO M-80
           END-IF
           PERFORM S-65 THRU S-70.
           GO TO M-75.
       M-90.
           PERFORM S-65 THRU S-70.
           COMPUTE WS-AT = WS-HMT + WS-SZT + WS-KHT + WS-CGT + WS-SST.
           MOVE SPACE TO SP-R W-P3.
           MOVE "合 計" TO P-NGD3.
           MOVE WS-CGG TO P-CGG.
           MOVE WS-CGY TO P-CGY.
           MOVE WS-CGS TO P-CGS.
           MOVE WS-CGT TO P-CGT.
           MOVE WS-SSH TO P-SSH.
           MOVE WS-SSK TO P-SSK.
           MOVE WS-SST TO P-SST.
           MOVE WS-AT  TO P-AT.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSY-F_IDLST JSY-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD JS-KIN TO W-HMT.
           IF  JS-JCD2 < 05
               ADD JS-KIN TO W-GNT
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 10
               ADD JS-KIN TO W-KTZ
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 15
               ADD JS-KIN TO W-KRR
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 20
               ADD JS-KIN TO W-NKZ
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 25
               ADD JS-KIN TO W-IHT
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 30
               ADD JS-KIN TO W-NRI
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 35
               ADD JS-KIN TO W-HSH
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 40
               ADD JS-KIN TO W-SNM
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 45
               ADD JS-KIN TO W-KMB
               GO TO S-10
           END-IF
           IF  JS-JCD2 < 49
               ADD JS-KIN TO W-KAS
           END-IF.
       S-10.
           EXIT.
       S-15.
           MOVE SPACE TO SP-R W-P1.
           MOVE W-NG1 TO P-NG1.
           MOVE W-GNT TO P-GNT.
           MOVE W-KTZ TO P-KTZ.
           MOVE W-KRR TO P-KRR.
           MOVE W-NKZ TO P-NKZ.
           MOVE W-IHT TO P-IHT.
           MOVE W-NRI TO P-NRI.
           MOVE W-HSH TO P-HSH.
           MOVE W-SNM TO P-SNM.
           MOVE W-KMB TO P-KMB.
           MOVE W-KAS TO P-KAS.
           MOVE W-HMT TO P-HMT.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-GNT TO WS-GNT.
           ADD W-KTZ TO WS-KTZ.
           ADD W-KRR TO WS-KRR.
           ADD W-NKZ TO WS-NKZ.
           ADD W-IHT TO WS-IHT.
           ADD W-NRI TO WS-NRI.
           ADD W-HSH TO WS-HSH.
           ADD W-SNM TO WS-SNM.
           ADD W-KMB TO WS-KMB.
           ADD W-KAS TO WS-KAS.
           ADD W-HMT TO WS-HMT.
           ADD 1 TO CNT.
           ADD W-HMT TO WA-AT(CNT).
       S-20.
           EXIT.
       S-25.
           IF  JS-JCD1 = 2
               GO TO S-30
           END-IF
           ADD JS-KIN TO W-KHT.
           IF  JS-JCD1 = 3
               ADD JS-KIN TO W-KHB
               GO TO S-35
           END-IF
           IF  JS-JCD1 = 4
               ADD JS-KIN TO W-KHS
               GO TO S-35
           END-IF.
       S-30.
           ADD JS-KIN TO W-SZT.
           IF  JS-JCD2 < 68
               ADD JS-KIN TO W-SZG
           ELSE
               ADD JS-KIN TO W-SZY
           END-IF.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO SP-R W-P2.
           MOVE W-NG2 TO P-NG2.
           MOVE W-SZG TO P-SZG.
           MOVE W-SZY TO P-SZY.
           MOVE W-SZT TO P-SZT.
           MOVE W-KHB TO P-KHB.
           MOVE W-KHS TO P-KHS.
           MOVE W-KHT TO P-KHT.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-SZG TO WS-SZG.
           ADD W-SZY TO WS-SZY.
           ADD W-SZT TO WS-SZT.
           ADD W-KHB TO WS-KHB.
           ADD W-KHS TO WS-KHS.
           ADD W-KHT TO WS-KHT.
           ADD 1 TO CNT.
           ADD W-SZT TO WA-AT(CNT).
           ADD W-KHT TO WA-AT(CNT).
       S-45.
           EXIT.
       S-50.
           IF  JS-JCD1 NOT = 1
               GO TO S-55
           END-IF
           ADD JS-KIN TO W-CGT.
           IF  JS-JCD2 < 49
               ADD JS-KIN TO W-CGG
               GO TO S-60
           END-IF
           IF  JS-JCD2 = 50
               ADD JS-KIN TO W-CGS
           ELSE
               ADD JS-KIN TO W-CGY
           END-IF
           GO TO S-60.
       S-55.
           ADD JS-KIN TO W-SST.
           IF  JS-JCD1 = 7
               ADD JS-KIN TO W-SSH
           ELSE
               ADD JS-KIN TO W-SSK
           END-IF.
       S-60.
           EXIT.
       S-65.
           ADD 1 TO CNT.
           ADD W-CGT TO WA-AT(CNT).
           ADD W-SST TO WA-AT(CNT).
           MOVE SPACE TO SP-R W-P3.
           MOVE W-NG3 TO P-NG3.
           MOVE W-CGG TO P-CGG.
           MOVE W-CGY TO P-CGY.
           MOVE W-CGS TO P-CGS.
           MOVE W-CGT TO P-CGT.
           MOVE W-SSH TO P-SSH.
           MOVE W-SSK TO P-SSK.
           MOVE W-SST TO P-SST.
           MOVE WA-AT(CNT) TO P-AT.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-CGG TO WS-CGG.
           ADD W-CGY TO WS-CGY.
           ADD W-CGS TO WS-CGS.
           ADD W-CGT TO WS-CGT.
           ADD W-SSH TO WS-SSH.
           ADD W-SSK TO WS-SSK.
           ADD W-SST TO WS-SST.
       S-70.
           EXIT.
