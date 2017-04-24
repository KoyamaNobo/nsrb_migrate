       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG200.
      ******************************************************************
      *    PROGRAM         :  請求チェックデータ　抽出                 *
      *    JS-SIGN         :  0=請求リスト , 1=非請求リスト            *
      *                    :  2=生協請求リスト                         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
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
           02  W-TCD          PIC  9(004).
           02  W-SETN.
             03  W-STNC       PIC  9(002).
             03  W-ETNC       PIC  9(002).
           02  W-SKC          PIC  9(001).
           02  W-SETC.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004).
           02  W-SED.
             03  W-SNGP       PIC  9(008).
             03  W-SNGPD REDEFINES W-SNGP.
               04  W-SNEN     PIC  9(004).
               04  W-SGET     PIC  9(002).
               04  W-SPEY     PIC  9(002).
             03  W-ENGP       PIC  9(008).
             03  W-ENGPD REDEFINES W-ENGP.
               04  W-ENEN     PIC  9(004).
               04  W-EGET     PIC  9(002).
               04  W-EPEY     PIC  9(002).
           02  W-SS           PIC  9(002).
           02  W-SSD          PIC  9(002).
           02  W-INV          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LRSKDF.
      *FD  SKCF
       01  SKCF_HKG200.
           02  SKCF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKCF_LNAME     PIC  X(011) VALUE "SKCF_HKG200".
           02  F              PIC  X(001).
           02  SKCF_KEY1      PIC  X(100) VALUE SPACE.
           02  SKCF_SORT      PIC  X(100) VALUE SPACE.
           02  SKCF_IDLST     PIC  X(100) VALUE SPACE.
           02  SKCF_RES       USAGE  POINTER.
       01  SKC-R.
           02  SKC-D          PIC  X(192).
           02  F              PIC  X(048).
           02  SKC-SNGP       PIC  9(008).
           02  SKC-ENGP       PIC  9(008).
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
                "＊＊＊　　請求チェックデータ　抽出　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "担当者ｺｰﾄﾞ    00 ～ 99".
           02  FILLER  PIC  X(024) VALUE
                "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(038) VALUE
                "    年   月   日　～　    年   月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  X(019) VALUE
                "    年   月   日 分".
       01  C-MID2.
           02  FILLER  PIC  N(001) VALUE "非".
           02  FILLER  PIC  X(030) VALUE
                "  日締め（マスター）   全部=99".
           02  FILLER  PIC  X(028) VALUE
                "( 生協印字  ｼﾅｲ=0 , ｽﾙ=1   )".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(004).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  FILLER.
             03  A-STNC  PIC  9(002).
             03  A-ETNC  PIC  9(002).
             03  A-SKC   PIC  9(001).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  FILLER.
             03  A-SNEN  PIC  9(004).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(004).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-SS    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SKS   PIC  N(006) VALUE
                "【　生協　】".
           02  FILLER.
             03  D-SKM   PIC  X(029) VALUE
                  "( 生協印字  ｼﾅｲ=0 , ｽﾙ=1    )".
             03  D-SKMC  PIC  X(029) VALUE
                  "                             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-TCD   PIC  9(004).
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
            "C-MID" " " "0" "0" "414" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "14" "13" "22" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "13" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "18" "13" "38" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "22" "21" "22" "10C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "19" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "X" "12" "22" "19" " " "C-MID1" RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "60" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" "N" "6" "18" "2" " " "C-MID2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" "X" "20" "22" "30" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID2" "X" "14" "40" "28" "02C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "12" "22" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "12" "29" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "12" "34" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "5" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STNC" "9" "14" "27" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETNC" "9" "14" "33" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKC" "9" "14" "66" "1" "A-ETNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKC" BY REFERENCE W-SKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "8" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "16" "25" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "16" "33" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "18" "0" "16" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "18" "13" "4" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "18" "20" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "18" "25" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "18" "35" "4" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "18" "42" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "18" "47" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SS" "9" "20" "22" "2" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SS" BY REFERENCE W-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "38" "1" "A-SS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "70" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKS" "N" "7" "26" "12" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "14" "0" "58" "D-SKS" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKM" "X" "14" "40" "29" " " "02C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKMC" "X" "14" "40" "29" "D-SKM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "39" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "39" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "35" "4" "E-ME4" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE SKD-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-ETNC.
           MOVE 9999 TO W-ETCD.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 2
               CALL "SD_Output" USING "D-SKS" D-SKS "p" RETURNING RESU
           END-IF
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SKCF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SKCF_PNAME1 " " BY REFERENCE SKCF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
      *           IF JS-SIGN = 0 OR 2
      *               SELECT SKDF WHERE SKD-TCD >= W-STCD AND
      *                                 SKD-TCD <= W-ETCD AND
      *                                 SKD-TNC >= W-STNC AND
      *                                 SKD-TNC <= W-ETNC AND
      *                                 SKD-DATE >= W-SNGP AND
      *                                 SKD-DATE <= W-ENGP AND
      *                                 SKD-DTC NOT = 3 AND
      *                                 SKD-DTC NOT = 5 AND
      *                                 SKD-SKD = W-DATE
      *                           ORDER BY SKD-KEY.
      *           IF JS-SIGN = 1
      *               SELECT SKDF WHERE SKD-TCD >= W-STCD AND
      *                                 SKD-TCD <= W-ETCD AND
      *                                 SKD-TNC >= W-STNC AND
      *                                 SKD-TNC <= W-ETNC AND
      *                                 SKD-DATE >= W-SNGP AND
      *                                 SKD-DATE <= W-ENGP AND
      *                                 SKD-DTC NOT = 3 AND
      *                                 SKD-DTC NOT = 5
      *                           ORDER BY SKD-KEY.
           IF JS-SIGN = 0 OR 2
               CALL "DB_Select" USING
                SKDF_PNAME1 "WHERE" 
                "SKD-TCD" ">=" W-STCD "AND"
                "SKD-TCD" "<=" W-ETCD "AND"
                "SKD-TNC" ">=" W-STNC "AND"
                "SKD-TNC" "<=" W-ETNC "AND"
                "SKD-DATE" ">=" W-SNGP "AND"
                "SKD-DATE" "<=" W-ENGP "AND"
                "SKD-DTC" "NOT =" "3" "AND"
                "SKD-DTC" "NOT =" "5" "AND"
                "SKD-SKD" "=" W-DATE
                "ORDER BY" "SKD-KEY" RETURNING RET
           END-IF
           IF JS-SIGN = 1
               CALL "DB_Select" USING
                SKDF_PNAME1 "WHERE" 
                "SKD-TCD" ">="  W-STCD "AND"
                "SKD-TCD" "<=" W-ETCD "AND"
                "SKD-TNC" ">=" W-STNC "AND"
                "SKD-TNC" "<=" W-ETNC "AND"
                "SKD-DATE" ">=" W-SNGP "AND"
                "SKD-DATE" "<=" W-ENGP "AND"
                "SKD-DTC" "NOT =" "3" "AND"
                "SKD-DTC" "NOT =" "5"
                "ORDER BY" "SKD-KEY" RETURNING RET
           END-IF.
       M-10.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING SKDF_PNAME1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-SKD NOT = ZERO AND 99999999
                   GO TO M-10
               END-IF
           END-IF
           MOVE 0 TO W-INV.
           MOVE SKD-TCD TO T-KEY.
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
           IF  JS-SIGN = 0
               IF  W-SKC = 0
                   IF  T-SSC = 1
                       GO TO M-10
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  W-STCD NOT = W-ETCD
                   IF  T-SSC NOT = 1
                       GO TO M-10
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 1
                   GO TO M-15
           END-IF
           IF  T-SS = 00 OR 99
               GO TO M-10
           END-IF
           IF  W-SS = 99
               GO TO M-15
           END-IF
           MOVE T-SS TO W-SSD.
           IF  W-SSD = 30
               MOVE 31 TO W-SSD
           END-IF
           IF  W-SS NOT = W-SSD
               GO TO M-10
           END-IF.
       M-15.
           IF  SKD-TCD NOT = W-TCD
               MOVE SKD-TCD TO W-TCD
           END-IF
           MOVE 1 TO W-DC.
           INITIALIZE SKC-D.
           MOVE SKD-R TO SKC-D.
           MOVE W-SNGP TO SKC-SNGP.
           MOVE W-ENGP TO SKC-ENGP.
      *           WRITE SKC-R.
      *//////////////
           CALL "DB_Insert" USING
            SKCF_PNAME1 SKCF_LNAME SKC-R RETURNING RET.
       M-20.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING  SKDF_PNAME1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               IF  SKD-SKD NOT = ZERO AND 99999999
                   GO TO M-20
               END-IF
           END-IF
           IF  SKD-TCD = W-TCD
               GO TO M-15
           END-IF
           MOVE 0 TO W-INV.
           MOVE SKD-TCD TO T-KEY.
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
           IF  JS-SIGN = 0
               IF  W-SKC = 0
                   IF  T-SSC = 1
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  W-STCD NOT = W-ETCD
                   IF  T-SSC NOT = 1
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 1
               GO TO M-15
           END-IF
           IF  T-SS = 00 OR 99
               GO TO M-20
           END-IF
           IF  W-SS = 99
               GO TO M-15
           END-IF
           MOVE T-SS TO W-SSD.
           IF  W-SSD = 30
               MOVE 31 TO W-SSD
           END-IF
           IF  W-SS NOT = W-SSD
               GO TO M-20
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKCF_IDLST SKCF_PNAME1.
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
       ACP-RTN.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NEN TO W-SNEN W-ENEN.
           MOVE W-GET TO W-SGET W-EGET.
           MOVE 1 TO W-SPEY.
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SPEY" A-SPEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EPEY" A-EPEY "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU
               GO TO ACP-032
           ELSE
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU
               CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU
               GO TO ACP-030
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-NEN = ZERO
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO ACP-020
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-030
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-030
           END-IF
           IF  JS-SIGN = 2
               MOVE 00 TO W-STNC
               MOVE 99 TO W-ETNC
               CALL "SD_Output" USING "A-STNC" A-STNC "p" RETURNING RESU
               CALL "SD_Output" USING "A-ETNC" A-ETNC "p" RETURNING RESU
               GO TO ACP-036
           END-IF.
       ACP-032.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO ACP-030
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-032
           END-IF.
       ACP-034.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-032
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-034
           END-IF
           IF  W-STNC > W-ETNC
               GO TO ACP-034
           END-IF
           IF  13 < W-STNC OR > W-ETNC
               CALL "SD_Output" USING "D-SKMC" D-SKMC "p" RETURNING RESU
               MOVE 0 TO W-SKC
               GO TO ACP-036
           END-IF
           CALL "SD_Output" USING "D-SKM" D-SKM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SKC" A-SKC "p" RETURNING RESU.
       ACP-035.
           CALL "SD_Accept" USING BY REFERENCE A-SKC "A-SKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-034
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-035
           END-IF
           IF  W-SKC > 1
               GO TO ACP-035
           END-IF.
       ACP-036.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 2
                   GO TO ACP-030
               ELSE
                   IF  JS-SIGN = 1
                       GO TO ACP-034
                   ELSE
                       IF  13 < W-STNC OR > W-ETNC
                           GO TO ACP-034
                       ELSE
                           GO TO ACP-035
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO ACP-032
           END-IF.
       ACP-038.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-036
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-038
           END-IF
           IF  W-STCD > W-ETCD
               GO TO ACP-038
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-038
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-SNEN = ZERO
               GO TO ACP-060
           END-IF.
       ACP-070.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-070
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO ACP-070
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-070
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO ACP-080
           END-IF.
       ACP-090.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-090
           END-IF
           IF  W-ENEN < W-SNEN
               GO TO ACP-090
           END-IF.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-090
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO ACP-100
           END-IF.
       ACP-110.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-110
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO ACP-110
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO ACP-060
           END-IF
           IF  JS-SIGN = 0 OR 2
               GO TO ACP-130
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-SS "A-SS" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-110
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF
           IF  W-SS NOT = 99
               IF  W-SS < 1 OR > 31
                   GO TO ACP-120
               END-IF
           END-IF
           IF  W-SS = 30
               MOVE 31 TO W-SS
           END-IF.
       ACP-130.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 0 OR 2
                   GO TO ACP-110
               ELSE
                   GO TO ACP-120
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-130
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 0 OR 2
                   GO TO ACP-030
               ELSE
                   GO TO ACP-036
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-130
           END-IF.
       ACP-EX.
           EXIT.
