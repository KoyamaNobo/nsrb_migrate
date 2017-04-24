       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG410.
      *********************************************************
      *    PROGRAM         :  材料区分別　仕入・棚卸明細表    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/09                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1A.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　材料区分別　仕入明細表　　＊＊＊".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  HA-DATE        PIC 99B99B99.
       01  HEAD1B.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　材料区分別　棚卸明細表　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  HB-DATE        PIC 99B99B99.
       01  HEAD1C.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　原　材　料　　受　払　表　　＊＊＊".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  HC-DATE        PIC 99B99B99.
       01  HEAD2A.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "部　門".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(003) VALUE "原材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "補助材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "荷造材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "製品仕入".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(005) VALUE "仕入品材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "輸出材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "小　　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合　　計".
       01  HEAD2B.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "部　門".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(003) VALUE "原材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "補助材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "荷造材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合　　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "中国材料".
       01  HEAD2C.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(013) VALUE "I------------".
           02  F              PIC  N(008) VALUE "　　履　　物　　".
           02  F              PIC  X(014) VALUE "------------I ".
           02  F              PIC  X(012) VALUE "I-----------".
           02  F              PIC  N(008) VALUE "　　工　　品　　".
           02  F              PIC  X(020) VALUE "-----------I I------".
           02  F              PIC  N(008) VALUE "　　素　　材　　".
           02  F              PIC  X(007) VALUE "------I".
       01  HEAD3C.
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(008) VALUE "原材料　補助材料".
           02  F              PIC  N(005) VALUE "　荷造材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(008) VALUE "原材料　補助材料".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "荷造材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "原材料".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "補助材料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(009) VALUE "合　計　総　合　計".
       01  W-PA.
           02  P-BMN          PIC  N(005).
           02  F              PIC  X(001).
           02  P-D     OCCURS   9  PIC -----,---,---.
       01  W-PC.
           02  P-15K          PIC  X(005).
           02  P-KM           PIC  N(004).
           02  P-HG           PIC ----,---,--9.
           02  P-HH           PIC --,---,--9.
           02  P-HN           PIC --,---,--9.
           02  P-HT           PIC ---,---,--9.
           02  P-KG           PIC ---,---,--9.
           02  P-KH           PIC --,---,--9.
           02  P-KN           PIC -----,--9.
           02  P-KT           PIC ---,---,--9.
           02  P-SG           PIC ---,---,--9.
           02  P-SH           PIC -----,--9.
           02  P-ST           PIC ---,---,--9.
           02  P-GT           PIC ----,---,--9.
           02  P-20K          PIC  X(005).
       01  W-TOTAL.
           02  W-HT.
             03  W-HTA   OCCURS   9  PIC S9(009).
             03  W-HTB   OCCURS   5  PIC S9(009).
           02  W-KT.
             03  W-KTA   OCCURS   9  PIC S9(009).
             03  W-KTB   OCCURS   5  PIC S9(009).
           02  W-ST.
             03  W-STA   OCCURS   9  PIC S9(009).
             03  W-STB   OCCURS   5  PIC S9(009).
           02  W-AT.
             03  W-ATA   OCCURS   9  PIC S9(009).
             03  W-ATB   OCCURS   5  PIC S9(009).
       01  W-BM.
           02  W-BMD   OCCURS   4  PIC  N(005).
       01  W-WM.
           02  F              PIC  N(020) VALUE
                "履　　物　工　　品　素　　材　合　　計　".
       01  W-ZAIK             PIC S9(009).
       01  W-UH.
           02  W-ZKM.
             03  W-ZK    OCCURS  12  PIC S9(009).
           02  W-TSM.
             03  W-TS    OCCURS  12  PIC S9(009).
           02  W-YKM.
             03  W-YK    OCCURS  12  PIC S9(009).
           02  W-THM.
             03  W-TH    OCCURS  12  PIC S9(009).
           02  W-ADM.
             03  W-AD    OCCURS  12  PIC S9(009).
       01  CNT.
           02  CNT1           PIC  9(002).
           02  CNT2           PIC  9(002).
       01  ERR-STAT           PIC  X(002).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
           COPY LSPF.
      *FD  JSSR-F
       01  JSSR-F_KBG410.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBG410".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R.
           02  JS-DC          PIC  9(002).
           02  F              PIC  X(012).
           02  JS-BC          PIC  9(001).
           02  F              PIC  X(030).
           02  JS-SHZ         PIC S9(007).
           02  F              PIC  X(019).
           02  JS-YC          PIC  9(001).
           02  F              PIC  X(030).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　材料区分別仕入・棚卸明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-KEY   PIC  9(006).
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
            "C-ERR" " " "0" "0" "31" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "31" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "40" "6" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE JT-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JT-M_PNAME1 
            "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           CALL "PR_Open" RETURNING RESP.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-HT W-KT W-ST W-AT W-UH.
           MOVE W-WM TO W-BM.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-10.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               GO TO M-15
           END-IF
           IF  JS-SHZ = ZERO
               GO TO M-10
           END-IF
           IF  JS-DC = 30
               GO TO M-10
           END-IF
           ADD JS-SHZ TO W-ATA(8) W-ATA(9).
           GO TO M-10.
       M-15.
      *           READ JT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  JT-KEY = "999000"
               GO TO M-15
           END-IF
           MOVE JT-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO J-ST
           END-IF
           MOVE ZERO TO W-ZAIK.
           IF  JT-ZC = ZERO
               COMPUTE W-ZAIK = (JT-ZKS + JT-SSU - JT-HSU) * J-ST
           END-IF
           IF  JT-YC = 4
               MOVE ZERO TO W-ZAIK
           END-IF
           ADD JT-SIK TO W-ATA(7) W-ATA(9).
           IF  JT-BC NOT = 1 AND 5
               ADD W-ZAIK TO W-ATB(4)
           END-IF
           IF  JT-BC < 2 OR = 5
               GO TO M-20
           END-IF
           IF  JT-BC = 6
               GO TO M-20
           END-IF
           IF  JT-BC = 2
               GO TO M-25
           END-IF
           IF  JT-BC < 5
               GO TO M-30
           END-IF
           IF  JT-BC < 7
               GO TO M-35
           END-IF.
       M-20.
           ADD JT-SIK TO W-HTA(7).
           IF  JT-ZC = ZERO
               IF  JT-BC NOT = 5
                   ADD W-ZAIK TO W-HTB(4)
               END-IF
           END-IF
           IF  JT-BC = 6
               ADD JT-SIK TO W-HTA(5) W-ATA(5)
               GO TO M-40
           END-IF
           IF  JT-BC = 1
               ADD JT-SIK TO W-HTA(6) W-ATA(6)
               GO TO M-40
           END-IF
           ADD JT-SIK TO W-HTA(JT-YC) W-ATA(JT-YC).
           IF  JT-YC = 4
               GO TO M-40
           END-IF
           IF  JT-ZC = ZERO
               IF  JT-BC = 5
                   ADD W-ZAIK TO W-HTB(5)
               ELSE
                   ADD W-ZAIK TO W-HTB(JT-YC) W-ATB(JT-YC)
               END-IF
           END-IF
           GO TO M-40.
       M-25.
           ADD JT-SIK TO W-STA(7) W-STA(JT-YC) W-ATA(JT-YC).
           IF  JT-ZC = ZERO
               ADD W-ZAIK TO W-STB(4) W-STB(JT-YC) W-ATB(JT-YC).
           GO TO M-40.
       M-30.
           ADD JT-SIK TO W-KTA(7) W-KTA(JT-YC) W-ATA(JT-YC).
           IF  JT-ZC = ZERO
               ADD W-ZAIK TO W-KTB(4) W-KTB(JT-YC) W-ATB(JT-YC)
           END-IF
           GO TO M-40.
       M-35.
           IF  JT-ZC = ZERO
               IF  JT-BC NOT = 5
                   ADD W-ZAIK TO W-ATB(JT-YC)
               END-IF
           END-IF.
       M-40.
           IF  JT-BC NOT = 0 AND 2 AND 3 AND 4 AND 5
               GO TO M-15
           END-IF
           IF  JT-YC NOT = 1 AND 2 AND 3
               GO TO M-15
           END-IF
           IF  JT-BC NOT = 2
               GO TO M-45
           END-IF
           IF  JT-YC = 3
               GO TO M-15
           END-IF.
       M-45.
           MOVE JT-YC TO CNT1.
           MOVE 4 TO CNT2.
           IF  JT-BC = 2
               ADD 8 TO CNT1
               MOVE 11 TO CNT2
           END-IF
           IF  JT-BC = 3 OR 4
               ADD 4 TO CNT1
               MOVE 8 TO CNT2
           END-IF
           ADD JT-SIK TO W-TS(CNT1) W-TS(CNT2) W-TS(12).
           IF  JT-ZC = ZERO
               ADD JT-ZKK TO W-ZK(CNT1) W-ZK(CNT2) W-ZK(12)
               ADD W-ZAIK TO W-YK(CNT1) W-YK(CNT2) W-YK(12)
           END-IF
           GO TO M-15.
       M-50.
           MOVE ZERO TO CNT.
       M-55.
           ADD 1 TO CNT1.
           IF  CNT1 NOT = 13
               COMPUTE W-TH(CNT1) = W-ZK(CNT1) + W-TS(CNT1) - W-YK(CNT1)
               GO TO M-55
           END-IF
      *
           MOVE SPACE TO SP-R.
           MOVE DATE-05R TO HA-DATE.
           MOVE HEAD1A TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2A TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO CNT.
       M-60.
           MOVE SPACE TO SP-R W-PA.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO M-65
           END-IF
           MOVE W-BMD(CNT1) TO P-BMN.
           PERFORM S-05 THRU S-30.
           MOVE W-PA TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-60.
       M-65.
           MOVE SPACE TO SP-R.
           MOVE DATE-05R TO HB-DATE.
           MOVE HEAD1B TO SP-R.
           CALL "PR_LineFeed" USING "7" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2B TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO CNT.
       M-70.
           MOVE SPACE TO SP-R W-PA.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO M-75
           END-IF
           MOVE W-BMD(CNT1) TO P-BMN.
           PERFORM S-35 THRU S-60.
           MOVE W-PA TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-70.
       M-75.
           MOVE SPACE TO SP-R.
           MOVE DATE-05R TO HC-DATE.
           MOVE HEAD1C TO SP-R.
           CALL "PR_LineFeed" USING "7" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2C TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3C TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO CNT.
       M-80.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO M-95
           END-IF
           PERFORM S-65 THRU S-70.
           GO TO M-80.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO CNT2.
           IF  CNT1 = 1
               GO TO S-10
           END-IF
           IF  CNT1 = 2
               GO TO S-15
           END-IF
           IF  CNT1 = 3
               GO TO S-20
           END-IF
           IF  CNT1 = 4
               GO TO S-25
           END-IF.
       S-10.
           ADD 1 TO CNT2.
           IF  CNT2 = 10
               GO TO S-30
           END-IF
           MOVE W-HTA(CNT2) TO P-D(CNT2).
           GO TO S-10.
       S-15.
           ADD 1 TO CNT2.
           IF  CNT2 = 10
               GO TO S-30
           END-IF
           MOVE W-KTA(CNT2) TO P-D(CNT2).
           GO TO S-15.
       S-20.
           ADD 1 TO CNT2.
           IF  CNT2 = 10
               GO TO S-30
           END-IF
           MOVE W-STA(CNT2) TO P-D(CNT2).
           GO TO S-20.
       S-25.
           ADD 1 TO CNT2.
           IF  CNT2 NOT = 10
               MOVE W-ATA(CNT2) TO P-D(CNT2)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO CNT2.
           IF  CNT1 = 1
               GO TO S-40
           END-IF
           IF  CNT1 = 2
               GO TO S-45
           END-IF
           IF  CNT1 = 3
               GO TO S-50
           END-IF
           IF  CNT1 = 4
               GO TO S-55
           END-IF.
       S-40.
           ADD 1 TO CNT2.
           IF  CNT2 = 6
               GO TO S-60
           END-IF
           MOVE W-HTB(CNT2) TO P-D(CNT2).
           GO TO S-40.
       S-45.
           ADD 1 TO CNT2.
           IF  CNT2 = 6
               GO TO S-60
           END-IF
           MOVE W-KTB(CNT2) TO P-D(CNT2).
           GO TO S-45.
       S-50.
           ADD 1 TO CNT2.
           IF  CNT2 = 6
               GO TO S-60
           END-IF
           MOVE W-STB(CNT2) TO P-D(CNT2).
           GO TO S-50.
       S-55.
           ADD 1 TO CNT2.
           IF  CNT2 NOT = 6
               MOVE W-ATB(CNT2) TO P-D(CNT2)
               GO TO S-55
           END-IF.
       S-60.
           EXIT.
       S-65.
           MOVE SPACE TO W-PC.
           MOVE W-15K TO P-15K.
           IF  CNT1 = 1
               MOVE "前月繰越" TO P-KM
               MOVE W-ZKM TO W-ADM
           END-IF
           IF  CNT1 = 2
               MOVE "当月仕入" TO P-KM
               MOVE W-TSM TO W-ADM
           END-IF
           IF  CNT1 = 3
               MOVE "現在残高" TO P-KM
               MOVE W-YKM TO W-ADM
           END-IF
           IF  CNT1 = 4
               MOVE "当月払出" TO P-KM
               MOVE W-THM TO W-ADM
           END-IF
           MOVE W-AD(01) TO P-HG.
           MOVE W-AD(02) TO P-HH.
           MOVE W-AD(03) TO P-HN.
           MOVE W-AD(04) TO P-HT.
           MOVE W-AD(05) TO P-KG.
           MOVE W-AD(06) TO P-KH.
           MOVE W-AD(07) TO P-KN.
           MOVE W-AD(08) TO P-KT.
           MOVE W-AD(09) TO P-SG.
           MOVE W-AD(10) TO P-SH.
           MOVE W-AD(11) TO P-ST.
           MOVE W-AD(12) TO P-GT.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO SP-R.
           MOVE W-PC TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-70.
           EXIT.
