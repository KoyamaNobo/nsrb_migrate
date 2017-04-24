       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKE010.
       AUTHOR.     S-NAKAO.
      *********************************************************
      *    PROGRAM         :  得意先担当区分　各ファイル更新  *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/21                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-DMM              PIC  9(001).
       01  W-FIL              PIC  X(005).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITHTM.
           COPY LITSKF.
           COPY LISKDF.
           COPY LSNYUR.
           COPY LJMSTD.
      *FD  TZNT-M
       01  TZNT-M_HKE010.
           02  TZNT-M_PNAME1  PIC  X(005) VALUE "TZNTM".
           02  F              PIC  X(001).
           02  TZNT-M_LNAME   PIC  X(013) VALUE "TZNT-M_HKE010".
           02  F              PIC  X(001).
           02  TZNT-M_KEY1    PIC  X(100) VALUE SPACE.
           02  TZNT-M_KEY2    PIC  X(100) VALUE SPACE.
           02  TZNT-M_SORT    PIC  X(100) VALUE SPACE.
           02  TZNT-M_IDLST   PIC  X(100) VALUE SPACE.
           02  TZNT-M_RES     USAGE  POINTER.
       01  TZNT-R.
           02  TZNT-KEY.
             03  TZNT-TCD     PIC  9(004).
             03  TZNT-IKC     PIC  9(001).
           02  F              PIC  X(478).
           02  TZNT-TC        PIC  9(002).
           02  F              PIC  X(027).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HKE010.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HKE010".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  X(008).
           02  SNTR-TCD       PIC  9(004).
           02  F              PIC  X(072).
           02  SNTR-TC        PIC  9(002).
           02  F              PIC  X(035).
       77  F                  PIC  X(001).
      *FD  SM-F
       01  SM-F_HKE010.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKE010".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  9(004).
           02  F              PIC  X(088).
           02  SM-TC          PIC  9(002).
           02  F              PIC  X(008).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　担当者を各ファイルに変換　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(034) VALUE
                "［　　変換　OK=1 NO=9   ﾘﾀｰﾝ    ］".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-FIL.
             03  FILLER  PIC  X(001) VALUE "(".
             03  02D-FIL PIC  X(005).
             03  FILLER  PIC  X(001) VALUE ")".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  TZNTM REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  TSKF REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  THTM REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  JMSTD REWRITE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-TCD   PIC  9(004).
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
           "C-MID" " " "0" "0" "342" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "14" "16" "34" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "14" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-FIL" " " "11" "0" "7" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-FIL" "X" "11" "29" "1" " " "D-FIL" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-FIL" "X" "11" "31" "5" "01D-FIL" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-FIL" BY REFERENCE W-FIL "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-FIL" "X" "11" "37" "1" "02D-FIL" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "163" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "163" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "40" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-00.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-05.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-05
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-05
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TZNT-M_PNAME1 " " BY REFERENCE TZNT-M_IDLST "1"
            "TZNT-KEY" BY REFERENCE TZNT-KEY.
           MOVE "TZNTM" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-10.
      *           READ TZNT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE TZNT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE T-TNC TO TZNT-TC.
      *           REWRITE TZNT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           MOVE "TSKF " TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-20.
      *           READ TSKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           MOVE TSK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE T-TNC TO TSK-TNC.
           MOVE T-DCC TO TSK-DCC.
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           MOVE "SKDF " TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-30.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO M-30
           END-IF
           MOVE SKD-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  T-TNC =  SKD-TNC
               IF  T-DCC = SKD-DCC
                   GO TO M-30
               END-IF
           END-IF
           MOVE T-TNC TO SKD-TNC.
           MOVE T-DCC TO SKD-DCC.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
           MOVE "NYURF" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-40.
      *           READ NYUR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           MOVE NUR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-40
           END-IF
           MOVE T-TNC TO NUR-TC.
      *           REWRITE NYUR-R.
      *///////////////
           CALL "DB_Update" USING
            NYUR-F_PNAME1 NYUR-F_LNAME NYUR-R RETURNING RET.
           GO TO M-40.
       M-45.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
           MOVE "SNTRF" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-50.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-50
           END-IF
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           MOVE T-TNC TO SNTR-TC.
      *           REWRITE SNTR-R.
      *///////////////
           CALL "DB_Update" USING
            SNTR-F_PNAME1 SNTR-F_LNAME SNTR-R RETURNING RET.
           GO TO M-50.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           MOVE "THTM" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
           MOVE ZERO TO T-KEY.
       M-60.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  THT-TCD = T-KEY
               GO TO M-65
           END-IF
           MOVE THT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-60
           END-IF.
       M-65.
           MOVE T-TNC TO THT-TNC.
      *           REWRITE THT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-60.
       M-70.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
           MOVE "SMF" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-75.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           MOVE SM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-75
           END-IF
           MOVE T-TNC TO SM-TC.
      *           REWRITE SM-R.
      *///////////////
           CALL "DB_Update" USING
            SM-F_PNAME1 SM-F_LNAME SM-R RETURNING RET.
           GO TO M-75.
       M-80.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
           BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3.
           MOVE "JMSTD" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-85.
      *           READ JMSTD NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE JMSTD-04 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
           IF  T-TNC = JMSTD-91
               GO TO M-85
           END-IF
           MOVE T-TNC TO JMSTD-91.
      *           REWRITE JMSTD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-85.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
