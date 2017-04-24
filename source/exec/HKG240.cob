       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG240.
      *******************************************************************
      *    PROGRAM         :  請求チェックデータ　抽出                  *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *    JS-SIGN         :  0=請求書 , 1=再請求書                     *
      *    COMPLETION_CODE :  000=その他 , 010=ワークマン・ナフコ       *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-JR               PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
             03  W-NGPM  REDEFINES W-NGPS.
               04  W-NGS      PIC  9(004).
               04  F          PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-NGPD         PIC  9(008).
           02  W-C            PIC  9(001).
           02  W-INV          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LISKDF.
           COPY LITSKF.
      *FD  SKDPF
       01  SKDPF_HKG240.
           02  SKDPF_PNAME1   PIC  X(005) VALUE "SKDPF".
           02  F              PIC  X(001).
           02  SKDPF_LNAME    PIC  X(012) VALUE "SKDPF_HKG240".
           02  F              PIC  X(001).
           02  SKDPF_KEY1     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY2     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY3     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY4     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY5     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY6     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY7     PIC  X(100) VALUE SPACE.
           02  SKDPF_KEY8     PIC  X(100) VALUE SPACE.
           02  SKDPF_SORT     PIC  X(100) VALUE SPACE.
           02  SKDPF_IDLST    PIC  X(100) VALUE SPACE.
           02  SKDPF_RES      USAGE  POINTER.
       01  SKDP-R.
           02  SKDP-KEY.                                                KEY
             03  SKDP-TCD     PIC  9(004).                              得意先C
             03  SKDP-DATE    PIC  9(008).                              日付
             03  SKDP-DTC     PIC  9(001).                              区分
             03  SKDP-DNO     PIC  9(006).                              伝票№
             03  SKDP-HCD     PIC  X(006).
             03  SKDP-T       PIC S9(006)V9(02).                        単価
             03  SKDP-DC      PIC  9(001).                              伝区
             03  SKDP-CSC     PIC  9(001).
           02  SKDP-SU        PIC S9(006)V9(02).                        数量
           02  SKDP-KIN       PIC S9(009).                              金額
           02  SKDP-SKD       PIC  9(008).                              請求日
           02  SKDP-TNC       PIC  9(002).                              担当Ｃ
           02  SKDP-BMC       PIC  9(001).                              部門C
           02  SKDP-DCC       PIC  9(001).
           02  F              PIC  X(002).
           02  SKDP-TCD2      PIC  9(004).
           02  SKDP-CCD       PIC  9(003).                              直送№
           02  SKDP-BI        PIC  N(024).                              備考
           02  SKDP-HNO       PIC  9(006).
           02  F              PIC  X(030).
           02  SKDP-SHZ       PIC S9(007).                              消費税
           02  SKDP-KSU       PIC  9(003).                              個数
           02  SKDP-JCD       PIC  9(006).
           02  F              PIC  X(013).
           02  SKDP-SNO       PIC  9(006).
       77  F                  PIC  X(001).
      *FD  NGPF
       01  NGPF_HKG240.
           02  NGPF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NGPF_LNAME     PIC  X(011) VALUE "NGPF_HKG240".
           02  F              PIC  X(001).
           02  NGPF_KEY1      PIC  X(100) VALUE SPACE.
           02  NGPF_SORT      PIC  X(100) VALUE SPACE.
           02  NGPF_IDLST     PIC  X(100) VALUE SPACE.
           02  NGPF_RES       USAGE  POINTER.
       01  NGP-R.
           02  NGP-DATE       PIC  9(008).
           02  F              PIC  X(056).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　請求データ　抽出　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER.
             03  FILLER      PIC  X(019) VALUE
                  "    年   月   日 分".
             03  0201C-MID1  PIC  9(004).
             03  0301C-MID1  PIC Z9 .
             03  0401C-MID1  PIC Z9 .
       01  C-MID2.
           02  FILLER  PIC  N(009) VALUE
                "ナフコ・ワークマン".
       01  C-MID3.
           02  FILLER  PIC  N(005) VALUE "（再発行）".
           02  FILLER  PIC  N(003) VALUE "得意先".
           02  FILLER  PIC  X(019) VALUE
                "    年   月   日 分".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-RNEN  PIC  9(004).
             03  A-RGET  PIC  9(002).
             03  A-RPEY  PIC  9(002).
      *
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  TSKF REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  TSKF WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME5   PIC  X(025) VALUE
                  "***  SKDPF WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  SKDPF REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(023) VALUE
                  "***  ﾐﾊｯｺｳ ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  ｾｲｷｭｳｼｮ ﾊｯｺｳ ｽﾞﾐ  ***".
             03  E-TCD   PIC  9(004).
             03  E-SKDP  PIC  X(035).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "330" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "20" "21" "22" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" " " "0" "0" "27" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" " " "13" "0" "27" " " "C-MID1"  RETURNING RESU.
       CALL "SD_Init" USING
            "0101C-MID1" "X" "13" "22" "19" " " "01C-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0201C-MID1" "9" "13" "22" "4" "0101C-MID1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0201C-MID1" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0301C-MID1" "Z9" "13" "29" "2" "0201C-MID1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301C-MID1" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401C-MID1" "Z9" "13" "34" "2" "0301C-MID1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0401C-MID1" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
            "C-MID2" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID2" "N" "5" "22" "18" " " "C-MID2"  RETURNING RESU.
      *C-MID3
       CALL "SD_Init" USING
            "C-MID3" " " "0" "0" "35" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID3" "N" "7" "26" "10" " " "C-MID3"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID3" "N" "13" "10" "6" "01C-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID3" "X" "15" "30" "19" "02C-MID3" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "13" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCD" "9" "13" "17" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "15" "0" "8" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-RNEN" "9" "15" "30" "4" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-RNEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-RGET" "9" "15" "37" "2" "A-RNEN" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-RGET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-RPEY" "9" "15" "42" "2" "A-RGET" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-RPEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "38" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "52" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-NAME" "N" "13" "22" "52" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "225" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "225" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "26" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "24" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "18" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "25" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "27" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "15" "23" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME8" "X" "24" "15" "26" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "45" "4" "E-ME8" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-TCD" BY REFERENCE TSK-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-SKDP" "X" "24" "45" "35" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-SKDP" BY REFERENCE SKDP-KEY "35" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  COMPLETION_CODE = 010
               MOVE 1 TO W-JR
               CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU
           END-IF
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
      *           READ TSKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE TSK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF (ZERO = TSK-HTN AND TSK-SZN AND TSK-HTC AND TSK-SZC AND
                     TSK-HTU AND TSK-SZU AND TSK-KNGP)
             AND (T-TNC = TSK-TNC) AND (T-BC = TSK-BMC) AND
                 (T-DCC = TSK-DCC)
               GO TO M-10
           END-IF
           MOVE ZERO TO TSK-KKD.
           MOVE T-TNC TO TSK-TNC.
           MOVE T-BC TO TSK-BMC.
           MOVE T-DCC TO TSK-DCC.
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" SKDPF_PNAME1 " " BY REFERENCE SKDPF_IDLST "1"
            "SKDP-KEY" BY REFERENCE SKDP-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDPF_IDLST SKDPF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" SKDPF_PNAME1 " " BY REFERENCE SKDPF_IDLST "1"
            "SKDP-KEY" BY REFERENCE SKDP-KEY.
           MOVE SPACE TO SKD-KEY.
           IF  W-JR = 1
               MOVE 5000 TO SKD-TCD
           END-IF
           IF  JS-SIGN = 1
               MOVE W-TCD TO SKD-TCD
           END-IF.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
       M-20.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-TCD NOT = W-TCD
                   GO TO M-90
               END-IF
           END-IF
           IF  W-JR = 1
               IF  SKD-TCD > 9850
                   GO TO M-90
               ELSE
                   IF  SKD-TCD NOT = 5000 AND 9850
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  W-JR NOT = 1
               IF  SKD-TCD = 5000 OR 9850
                   GO TO M-20
               END-IF
           END-IF
           IF  SKD-SKD NOT = W-DATE
               GO TO M-20
           END-IF
           IF  JS-SIGN = 0
               IF  SKD-SNO NOT = ZERO
                   GO TO M-20
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-SNO = ZERO
                   GO TO M-20
               END-IF
           END-IF.
       M-25.
           MOVE 0 TO W-ZC.
           PERFORM TSK-RTN THRU TSK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  W-DATE = TSK-ZNGP(4) OR TSK-ZNGP(5)
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-TCD" E-TCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-90
               END-IF
           END-IF.
       M-30.
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC = 0
                       IF  SKD-DC NOT = 8
                           ADD SKD-KIN TO TSK-HTU
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZU
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC = 0
                       IF  SKD-DC = 8
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZU
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-DC NOT = 8
                           ADD SKD-KIN TO TSK-HTC
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-DC = 8
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 4
                   IF  SKD-GNO = 1
                       ADD SKD-SHZ TO TSK-SZU
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC = 0
                       IF  SKD-BMC = 0
                           SUBTRACT SKD-KIN FROM TSK-HTU
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZU
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-BMC = 0
                           SUBTRACT SKD-KIN FROM TSK-HTC
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC = 0
                       IF  SKD-BMC NOT = 0
                           ADD SKD-KIN TO TSK-HTU
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZU
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-BMC NOT = 0
                           ADD SKD-KIN TO TSK-HTC
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO TSK-SZC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 1
               IF  SKD-CSC = 0
                   SUBTRACT SKD-KIN FROM TSK-HTU
                   IF  SKD-GNO = 1
                       SUBTRACT SKD-SHZ FROM TSK-SZU
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 1
               IF  SKD-CSC NOT = 0
                   SUBTRACT SKD-KIN FROM TSK-HTC
                   IF  SKD-GNO = 1
                       SUBTRACT SKD-SHZ FROM TSK-SZC
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 3
               ADD SKD-KIN TO TSK-HTN
               ADD SKD-SHZ TO TSK-SZN
           END-IF
           IF  W-ZC = 0
               IF  SKD-DTC NOT = 5
                   MOVE 1 TO W-ZC
               END-IF
           END-IF
           MOVE 1 TO W-DC.
      *
           INITIALIZE SKDP-R.
           MOVE SKD-TCD TO SKDP-TCD.
           MOVE SKD-DATE TO SKDP-DATE.
           MOVE SKD-DTC TO SKDP-DTC.
           MOVE SKD-DNO TO SKDP-DNO.
           MOVE SKD-HCD TO SKDP-HCD.
           MOVE SKD-T TO SKDP-T.
           MOVE SKD-DC TO SKDP-DC.
           MOVE SKD-CSC TO SKDP-CSC.
           IF  SKD-BMC = 3
               GO TO M-40
           END-IF
      *           READ SKDPF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SKDPF_PNAME1 BY REFERENCE SKDP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           ADD SKD-SU TO SKDP-SU.
           ADD SKD-KIN TO SKDP-KIN.
      *           REWRITE SKDP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDPF_PNAME1 SKDPF_LNAME SKDP-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SKDP" E-SKDP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-50.
       M-40.
           MOVE SKD-SU TO SKDP-SU.
           MOVE SKD-KIN TO SKDP-KIN.
           MOVE SKD-SKD TO SKDP-SKD.
           MOVE SKD-TNC TO SKDP-TNC.
           MOVE SKD-BMC TO SKDP-BMC.
           MOVE SKD-DCC TO SKDP-DCC.
           MOVE SKD-TCD2 TO SKDP-TCD2.
           MOVE SKD-CCD TO SKDP-CCD.
           MOVE SKD-BI TO SKDP-BI.
           MOVE SKD-HNO TO SKDP-HNO.
           MOVE SKD-SHZ TO SKDP-SHZ.
           MOVE SKD-KSU TO SKDP-KSU.
           MOVE SKD-SNO TO SKDP-SNO.
           MOVE SKD-JCD TO SKDP-JCD.
      *           WRITE SKDP-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SKDPF_PNAME1 SKDPF_LNAME SKDP-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SKDP" E-SKDP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-50.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-TCD NOT = W-TCD
                   GO TO M-60
               END-IF
           END-IF
           IF  W-JR = 1
               IF  SKD-TCD > 9850
                   GO TO M-60
               ELSE
                   IF  SKD-TCD NOT = 5000 AND 9850
                       GO TO M-50
                   END-IF
               END-IF
           END-IF
           IF  W-JR NOT = 1
               IF  SKD-TCD = 5000 OR 9850
                   GO TO M-50
               END-IF
           END-IF
           IF  SKD-SKD NOT = W-DATE
               GO TO M-50
           END-IF
           IF  JS-SIGN = 0
               IF  SKD-SNO NOT = ZERO
                   GO TO M-50
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-SNO = ZERO
                   GO TO M-50
               END-IF
           END-IF
           IF  SKD-TCD = W-TCD
               GO TO M-30
           END-IF
           IF  W-ZC = 0
               IF  JS-SIGN NOT = 1
                   IF  ZERO = TSK-HTS(3) AND TSK-SZS(3)
                       GO TO M-25
                   END-IF
               END-IF
           END-IF
           IF  W-ZC = 0
               IF  JS-SIGN = 1
                   IF  TSK-ZNGP(2) = W-DATE
                       IF  ZERO = TSK-HTS(1) AND TSK-SZS(1)
                           GO TO M-25
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-ZC = 0
               IF  JS-SIGN = 1
                   IF  TSK-ZNGP(3) = W-DATE
                       IF  ZERO = TSK-HTS(2) AND TSK-SZS(2)
                           GO TO M-25
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE 0 TO W-INV.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO T-R
           END-IF
           IF  T-SS NOT = 99
               MOVE W-DATE TO TSK-KNGP
           END-IF
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-25.
       M-60.
           IF  W-ZC = 0
               IF  JS-SIGN = 1
                   IF  ZERO = TSK-HTS(3) AND TSK-SZS(3)
                       GO TO M-90
                   END-IF
               END-IF
           END-IF
           IF  W-ZC = 0
               IF  JS-SIGN = 1
                   IF  TSK-ZNGP(2) = W-DATE
                       IF  ZERO = TSK-HTS(1) AND TSK-SZS(1)
                           GO TO M-90
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-ZC = 0
               IF  JS-SIGN = 1
                   IF  TSK-ZNGP(3) = W-DATE
                       IF  ZERO = TSK-HTS(2) AND TSK-SZS(2)
                           GO TO M-90
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO T-R
           END-IF
           IF  T-SS NOT = 99
               MOVE W-DATE TO TSK-KNGP
           END-IF
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDPF_IDLST SKDPF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TSK-RTN.
           MOVE SKD-TCD TO W-TCD.
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TSK-010
           END-IF
           GO TO TSK-EX.
       TSK-010.
           MOVE 0 TO W-INV.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO T-R
           END-IF.
       TSK-020.
           MOVE ZERO TO TSK-R.
           MOVE W-TCD TO TSK-KEY.
           MOVE T-TNC TO TSK-TNC.
           MOVE T-BC TO TSK-BMC.
           MOVE T-DCC TO TSK-DCC.
      *           WRITE TSK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
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
               GO TO TSK-030
           END-IF
           GO TO TSK-EX.
       TSK-030.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TSK-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           MOVE "TSKF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           GO TO TSK-020.
       TSK-EX.
           EXIT.
       ACP-RTN.
           IF  JS-SIGN = 1
               GO TO ACP-010
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NGPF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NGPF_PNAME1 " " BY REFERENCE NGPF_IDLST "0".
      *           READ NGPF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NGPF_PNAME1 BY REFERENCE NGP-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO ACP-EX
           END-IF
           MOVE NGP-DATE TO W-DATE.
           CALL "DB_F_Close" USING BY REFERENCE NGPF_IDLST NGPF_PNAME1.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           GO TO ACP-090.
       ACP-010.
           CALL "SD_Output" USING "C-MID3" C-MID3 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  W-JR = 1
               IF  W-TCD NOT = 5000 AND 9850
                   GO TO ACP-020
               END-IF
           END-IF
           IF  W-JR NOT = 1
               IF  W-TCD = 5000 OR 9850
                   GO TO ACP-020
               END-IF
           END-IF
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  ZERO = TSK-ZNGP(2) AND TSK-ZNGP(3)
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  TSK-ZNGP(3) NOT = ZERO
               MOVE TSK-ZNGP(3) TO W-DATE
           ELSE
               MOVE TSK-ZNGP(2) TO W-DATE
           END-IF
           CALL "SD_Output" USING "A-RNEN" A-RNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-RGET" A-RGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-RPEY" A-RPEY "p" RETURNING RESU.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-RNEN "A-RNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-030
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-RGET "A-RGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-030
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO ACP-040
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-RPEY "A-RPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-050
           END-IF
           IF  W-DATE NOT = TSK-ZNGP(2) AND TSK-ZNGP(3)
               GO TO ACP-020
           END-IF.
      *
       ACP-090.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               IF  JS-SIGN = 0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   GO TO ACP-EX
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN NOT = 0
                   GO TO ACP-050
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-090
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   GO TO ACP-EX
               ELSE
                   GO TO ACP-020
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-090
           END-IF
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
           END-IF.
       ACP-EX.
           EXIT.
