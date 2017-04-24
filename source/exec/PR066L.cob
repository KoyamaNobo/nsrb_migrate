       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR066L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  経費マスタリスト　　　　　　  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  99/07/02                      *
      *    COMPILE TYPE  :  COBOL                         *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.     SYSTEM3100.
       OBJECT-COMPUTER.     SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(019) VALUE
               "＊＊＊　　経費マスタ　リスト　　＊＊＊".
           02  F              PIC  X(034) VALUE SPACE.
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  H-PEY          PIC  Z9.
           02  F              PIC  N(003) VALUE "日作成".
           02  F              PIC  X(004) VALUE SPACE.
           02  H-PAGE         PIC ZZ9.
           02  F              PIC  N(001) VALUE "頁".
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "部門".
           02  F              PIC  X(002) VALUE "C ".
           02  F              PIC  N(004) VALUE "部門名称".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "科目".
           02  F              PIC  X(002) VALUE "C-".
           02  F              PIC  N(002) VALUE "補助".
           02  F              PIC  X(002) VALUE "C ".
           02  F              PIC  N(004) VALUE "科目名称".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "補助名称".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(002) VALUE "C ".
           02  F              PIC  N(004) VALUE "改行数　".
           02  F              PIC  X(062) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(052) VALUE SPACE.
           02  H-TMD1  OCCURS  6.
             03  F            PIC  X(008) VALUE SPACE.
             03  H-TM1        PIC  N(004).
       01  HEAD4.
           02  F              PIC  X(052) VALUE SPACE.
           02  H-TMD2  OCCURS  6.
             03  F            PIC  X(008) VALUE SPACE.
             03  H-TM2        PIC  N(004).
       01  HEAD5.
           02  F              PIC  X(052) VALUE SPACE.
           02  H-TMD3  OCCURS  3.
             03  F            PIC  X(008) VALUE SPACE.
             03  H-TM3        PIC  N(004).
           02  F              PIC  X(042) VALUE SPACE.
       01  W-P1.
           02  P-BMC          PIC  9(004).
           02  F              PIC  X(001).
           02  P-BMN          PIC  N(010).
           02  F              PIC  X(001).
           02  P-KMC          PIC  9(004).
           02  P-V            PIC  X(001).
           02  P-HJC          PIC  9(004).
           02  F              PIC  X(001).
           02  P-KMN          PIC  N(010).
           02  F              PIC  X(001).
           02  P-HJN          PIC  N(010).
           02  F              PIC  X(002).
           02  P-GKC          PIC  9(003).
           02  F              PIC  X(004).
           02  P-KGS          PIC  9(001).
           02  F              PIC  X(064).
       01  W-P2.
           02  F              PIC  X(052).
           02  P-KIND  OCCURS  6.
             03  P-KIN        PIC  ------,---,--9.
       01  W-PD.
           02  W-TM           PIC  N(004).
           02  W-TMD.
             03  F            PIC  N(001) VALUE SPACE.
             03  W-TUKI       PIC  N(002).
             03  F            PIC  N(001) VALUE "月".
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-NGPD  REDEFINES  W-DATE.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-HC           PIC  9(001).
           02  W-KGET         PIC  9(002).
           02  W-GET          PIC  9(002).
           02  W-TD           PIC Z9.
           02  W-AREA.
             03  W-SD.
               04  W-SBMC     PIC  9(004).
               04  W-SKMC     PIC  9(004).
               04  W-SHJC     PIC  9(004).
             03  W-ED.
               04  W-EBMC     PIC  9(004).
               04  W-EKMC     PIC  9(004).
               04  W-EHJC     PIC  9(004).
           02  W-KN.
             03  W-KIND  OCCURS  15.
               04  W-KIN      PIC S9(011).
      *
           COPY  KEIHI.
           COPY  BUMONF.
           COPY  KANGEL.
           COPY  FCTL.
      *
      *       FD  SP-F
       77  SP-R               PIC  X(250).
      *
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(010) VALUE
               " 経費マスタ　リスト ".
           02  FILLER  PIC N(004) VALUE  "ＦＲＯＭ".
           02  FILLER  PIC N(002) VALUE  "ＴＯ".
           02  FILLER  PIC N(005) VALUE  "部門コード".
           02  FILLER  PIC N(005) VALUE  "科目コード".
           02  FILLER  PIC N(007) VALUE
               "補助科目コード".
           02  FILLER  PIC N(001) VALUE  "～".
           02  FILLER  PIC N(001) VALUE  "～".
           02  FILLER  PIC N(001) VALUE  "～".
           02  FILLER  PIC X(018) VALUE
               "確認 OK=1,NO=9 ( )".
       01  C-ACP.
           02  FILLER.
             03  A-SBMC    PIC  9(004).
             03  A-EBMC    PIC  9(004).
           02  FILLER.
             03  A-SKMC    PIC  9(004).
             03  A-EKMC    PIC  9(004).
           02  FILLER.
             03  A-SHJC    PIC  9(004).
             03  A-EHJC    PIC  9(004).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(032) VALUE
                  "***  ｺﾝﾄﾛｰﾙﾌｧｲﾙ(FCONTRL) ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "90" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "RN" "1" "32" "20" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "31" "8" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "51" "4" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "11" "10" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "8" "11" "10" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "10" "11" "14" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "6" "43" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "N" "8" "43" "2" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "N" "10" "43" "2" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "N" "23" "61" "18" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "6" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SBMC" "9" "6" "33" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SBMC" BY REFERENCE W-SBMC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EBMC" "9" "6" "51" "4" "A-SBMC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EBMC" BY REFERENCE W-EBMC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "8" "0" "8" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKMC" "9" "8" "33" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKMC" BY REFERENCE W-SKMC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EKMC" "9" "8" "51" "4" "A-SKMC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKMC" BY REFERENCE W-EKMC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-ACP" " " "10" "0" "8" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SHJC" "9" "10" "33" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SHJC" BY REFERENCE W-SHJC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EHJC" "9" "10" "51" "4" "A-SHJC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EHJC" BY REFERENCE W-EHJC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "77" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "59" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "32" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           INITIALIZE W-DATA.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SBMC "A-SBMC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING
                 BY REFERENCE A-EBMC "A-EBMC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-15
           END-IF.
           IF  W-SBMC > W-EBMC
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SKMC "A-SKMC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING
                 BY REFERENCE A-EKMC "A-EKMC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-20
           END-IF.
       M-30.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SHJC "A-SHJC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING
                 BY REFERENCE A-EHJC "A-EHJC" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-30
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-35
           END-IF.
           IF  W-SD > W-ED
               GO TO M-20
           END-IF.
       M-40.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-35
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-40
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" HH-F_PNAME1 "SHARED" BY REFERENCE HH-F_IDLST "1"
            "HH-KEY" BY REFERENCE HH-KEY.
           MOVE W-SD TO HH-KEY.
      *           START HH-F KEY NOT < HH-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            HH-F_PNAME1 "HH-KEY" " NOT < " HH-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HH-F_IDLST HH-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
      *           READ HH-F NEXT WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HH-F_PNAME1 BY REFERENCE HH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HH-F_IDLST HH-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           IF  HH-KEY > W-ED
               CALL "DB_F_Close" USING
                BY REFERENCE HH-F_IDLST HH-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  " TO FCTL-KEY.
      *           READ FCTL-F WITH UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HH-F_IDLST HH-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE FCTL-KSMM TO W-GET W-KGET.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           MOVE 0 TO W-HC.
       M-45.
           ADD 1 TO W-HC.
           IF  W-HC = 4
               GO TO M-55
           END-IF.
           MOVE ZERO TO CNT.
       M-50.
           ADD 1 TO CNT.
           IF  W-HC = 1 OR 2
               IF  CNT = 7
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-HC = 3
               IF  CNT = 4
                   GO TO M-45
               END-IF
           END-IF.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
           END-IF.
           MOVE W-GET TO W-TD.
           MOVE W-TD TO W-TUKI.
           MOVE W-TMD TO W-TM.
           IF  W-HC = 1
               MOVE W-TM TO H-TM1(CNT)
           END-IF.
           IF  W-HC = 2
               MOVE W-TM TO H-TM2(CNT)
           END-IF.
           IF  W-HC = 3
               MOVE W-TM TO H-TM3(CNT)
           END-IF.
           GO TO M-50.
       M-55.
           ACCEPT W-DATE FROM DATE.
           MOVE W-NEND TO H-NEN.
           MOVE W-GETD TO H-GET.
           MOVE W-PEYD TO H-PEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-60.
           MOVE ZERO TO W-KN W-C.
           MOVE W-KGET TO CNT.
       M-65.
           ADD 1 TO W-C CNT.
           IF  W-C = 16
               GO TO M-70
           END-IF.
           IF  W-C = 13
               MOVE 13 TO CNT
           END-IF.
           IF (CNT = 13) AND (W-C < 13)
               MOVE 1 TO CNT
           END-IF.
           MOVE HH-GEL(CNT) TO W-KIN(W-C).
           GO TO M-65.
       M-70.
           MOVE HH-BUCD TO BNM-KEY.
      *           READ BNM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
           END-IF.
           MOVE HH-KACD TO K-ACCD.
           MOVE ZERO TO K-HOCD.
      *           READ KNG UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KNGNMN
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-BMN P-KMN P-HJN.
           MOVE HH-BUCD TO P-BMC.
           MOVE BNMNMN TO P-BMN.
           MOVE HH-KACD TO P-KMC.
           MOVE "-" TO P-V.
           MOVE HH-HOCD TO P-HJC.
           MOVE KNGNMN TO P-KMN.
           IF  HH-HOCD = ZERO
               GO TO M-75
           END-IF.
           MOVE HH-KACD TO K-ACCD.
           MOVE HH-HOCD TO K-HOCD.
      *           READ KNG UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KNGNMN
           END-IF.
           MOVE KNGNMN TO P-HJN.
       M-75.
           MOVE HH-GOCD TO P-GKC.
           MOVE HH-GYO TO P-KGS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 57
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE W-KIN(01) TO P-KIN(01).
           MOVE W-KIN(02) TO P-KIN(02).
           MOVE W-KIN(03) TO P-KIN(03).
           MOVE W-KIN(04) TO P-KIN(04).
           MOVE W-KIN(05) TO P-KIN(05).
           MOVE W-KIN(06) TO P-KIN(06).
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE W-KIN(07) TO P-KIN(01).
           MOVE W-KIN(08) TO P-KIN(02).
           MOVE W-KIN(09) TO P-KIN(03).
           MOVE W-KIN(10) TO P-KIN(04).
           MOVE W-KIN(11) TO P-KIN(05).
           MOVE W-KIN(12) TO P-KIN(06).
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE W-KIN(13) TO P-KIN(01).
           MOVE W-KIN(14) TO P-KIN(02).
           MOVE W-KIN(15) TO P-KIN(03).
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *           READ HH-F NEXT WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HH-F_PNAME1 BY REFERENCE HH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  HH-KEY > W-ED
               GO TO M-90
           END-IF.
           GO TO M-60.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
