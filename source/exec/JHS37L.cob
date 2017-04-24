       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS37L.
      ********************************************************
      *****    伝票計上リスト他（赤ちゃん本舗）          *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　赤ちゃん本舗　明細リスト　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "年月度　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "データ　".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　取引先".
           02  F              PIC  X(002) VALUE "CD".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票日付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "社店".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "本体金額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "消費税額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合計金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "対象仕入金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "料率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "摘要".
           02  F              PIC  X(012) VALUE SPACE.
       01  HEADV.
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(036) VALUE
                "------------------------------------".
       01  W-P.
           02  P-NG           PIC  9999/99.
           02  F              PIC  X(001).
           02  P-DNA          PIC  N(008).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-DATE         PIC 9999/99/99.
           02  F              PIC  X(001).
           02  P-BMC          PIC  9(007).
           02  F              PIC  X(001).
           02  P-TSCN         PIC  N(004).
           02  F              PIC  X(001).
           02  P-DNO          PIC  Z(011).
           02  F              PIC  X(001).
           02  P-KIN          PIC  ----,---,---.
           02  P-SHZ          PIC  --,---,---.
           02  P-KEI          PIC  ----,---,---.
           02  P-TSK          PIC  ----,---,---.
           02  F              PIC  X(001).
           02  P-RIT          PIC  ZZZ.ZZ.
           02  F              PIC  X(002).
           02  P-TEK          PIC  X(015).
       01  W-DATA.
           02  W-PAGE         PIC  9(003).
           02  W-CS           PIC  9(001).
           02  W-MC           PIC  9(002).
           02  W-TD.
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
             03  W-KEI        PIC S9(009).
             03  W-TSK        PIC S9(009).
      *
           COPY LSPF.
      *FD  AKATF
       01  AKATF_JHS37L.
           02  AKATF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  AKATF_LNAME    PIC  X(012) VALUE "AKATF_JHS37L".
           02  F              PIC  X(001).
           02  AKATF_KEY1     PIC  X(100) VALUE SPACE.
           02  AKATF_SORT     PIC  X(100) VALUE SPACE.
           02  AKATF_IDLST    PIC  X(100) VALUE SPACE.
           02  AKATF_RES      USAGE  POINTER.
       01  AKAT-R.
           02  AKAT-DC        PIC  9(002).
           02  AKAT-NG        PIC  9(006).
           02  AKAT-TCD       PIC  9(006).
           02  AKAT-CS        PIC  9(001).
           02  AKAT-MC        PIC  9(002).
           02  AKAT-DATE      PIC  9(008).
           02  AKAT-DNO       PIC  9(011).
           02  AKAT-TSC       PIC  9(002).
           02  AKAT-BMC       PIC  9(007).
           02  AKAT-KIN       PIC S9(009).
           02  AKAT-SHZ       PIC S9(007).
           02  AKAT-KEI       PIC S9(009).
           02  AKAT-TSK       PIC S9(009).
           02  AKAT-RIT       PIC  9(003)V9(02).
           02  AKAT-TEK       PIC  X(015).
           02  F              PIC  X(029).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　その他受信データリスト　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　（赤ちゃん本舗）　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME2   PIC  N(007) VALUE
                  "データ　エラー".
           COPY LSSEM.
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "26" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "12" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "14" "E-ME1" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO AKATF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" AKATF_PNAME1 " " BY REFERENCE AKATF_IDLST "0".
      *
      *           READ AKATF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" AKATF_PNAME1 BY REFERENCE AKAT-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE AKATF_IDLST AKATF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-DATA.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-10.
           MOVE AKAT-CS TO W-CS.
           MOVE AKAT-MC TO W-MC.
       M-15.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-DNA P-TSCN.
           MOVE AKAT-NG   TO P-NG.
           IF  W-CS = 1
               MOVE "伝票計上　　　　" TO P-DNA
           ELSE
               IF  W-MC = 01
                   MOVE "センターフィー　" TO P-DNA
               ELSE
                   IF  W-MC = 02
                       MOVE "ＥＤＩ手数料　　" TO P-DNA
                   END-IF
               END-IF
           END-IF
           MOVE AKAT-TCD  TO P-TCD.
           MOVE AKAT-DATE TO P-DATE.
           MOVE AKAT-BMC  TO P-BMC.
           IF  AKAT-TSC = 20
               MOVE "仕入　　" TO P-TSCN
           ELSE
               IF  AKAT-TSC = 21
                   MOVE "返品　　" TO P-TSCN
               ELSE
                   IF  AKAT-TSC = 22
                       MOVE "値引　　" TO P-TSCN
                   ELSE
                       IF  AKAT-TSC = 24
                           MOVE "仕入取消" TO P-TSCN
                       ELSE
                           IF  AKAT-TSC = 25
                               MOVE "返品取消" TO P-TSCN
                           ELSE
                               IF  AKAT-TSC = 26
                                   MOVE "値引取消" TO P-TSCN
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE AKAT-DNO  TO P-DNO.
           MOVE AKAT-KIN  TO P-KIN.
           MOVE AKAT-SHZ  TO P-SHZ.
           MOVE AKAT-KEI  TO P-KEI.
           MOVE AKAT-TSK  TO P-TSK.
           MOVE AKAT-RIT  TO P-RIT.
           MOVE AKAT-TEK  TO P-TEK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD AKAT-KIN  TO W-KIN.
           ADD AKAT-SHZ  TO W-SHZ.
           ADD AKAT-KEI  TO W-KEI.
           ADD AKAT-TSK  TO W-TSK.
      *
      *           READ AKATF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" AKATF_PNAME1 BY REFERENCE AKAT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  AKAT-CS NOT = W-CS
               GO TO M-20
           END-IF
           IF  AKAT-MC = W-MC
               GO TO M-15
           END-IF.
       M-20.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-DNA P-TSCN.
           MOVE "　合　計" TO P-TSCN.
           MOVE W-KIN  TO P-KIN.
           MOVE W-SHZ  TO P-SHZ.
           MOVE W-KEI  TO P-KEI.
           MOVE W-TSK  TO P-TSK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEADV TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-TD.
           GO TO M-10.
       M-90.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-DNA P-TSCN.
           MOVE "　合　計" TO P-TSCN.
           MOVE W-KIN  TO P-KIN.
           MOVE W-SHZ  TO P-SHZ.
           MOVE W-KEI  TO P-KEI.
           MOVE W-TSK  TO P-TSK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
      *
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE AKATF_IDLST AKATF_PNAME1.
       M-95.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
