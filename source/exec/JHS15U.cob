       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS15U.
      ********************************************************
      *****    受注受信データ生成（赤ちゃん本舗）        *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       01  W-DATE             PIC  9(006).
       01  W-DATA.
           02  W-DNOS         PIC  9(007).
           02  W-DNOE         PIC  9(007).
           02  W-DD.
             03  W-DNO        PIC  9(007).
             03  W-HNO        PIC  9(009).
             03  W-DPC        PIC  9(002).
             03  W-HNGP       PIC  9(006).
             03  W-NNGP       PIC  9(006).
             03  W-THC        PIC  9(006).
             03  W-STC        PIC  9(007).
             03  W-BI         PIC  X(010).
             03  W-SNGP       PIC  9(008).
             03  W-HNA        PIC  X(006).
             03  W-ZON        PIC  9(004).
           02  W-DGN          PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-DSZ          PIC  X(004).
           02  W-DSZD  REDEFINES W-DSZ.
             03  F            PIC  X(003).
             03  W-DSZH       PIC  X(001).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-AHN          PIC  N(024).
           02  W-AHND  REDEFINES W-AHN.
             03  W-HND   OCCURS  24.
               04  W-HN       PIC  N(001).
           02  W-COR          PIC  N(004).
           02  W-ANAD  REDEFINES W-COR.
             03  W-NAD  OCCURS   4.
               04  W-NA       PIC  N(001).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           02  W-NO           PIC  9(004).
      *
           COPY LITDNA.
           COPY LICODE.
           COPY LIAHNH.
           COPY LIHIM.
      *FD  JCAAF
       01  JCAAF_JHS15U.
           02  JCAAF_PNAME1   PIC  X(005) VALUE "JCAAF".
           02  F              PIC  X(001).
           02  JCAAF_LNAME    PIC  X(012) VALUE "JCAAF_JHS15U".
           02  F              PIC  X(001).
           02  JCAAF_KEY1     PIC  X(100) VALUE SPACE.
           02  JCAAF_SORT     PIC  X(100) VALUE SPACE.
           02  JCAAF_IDLST    PIC  X(100) VALUE SPACE.
           02  JCAAF_RES      USAGE  POINTER.
       01  JCAA-R.
           02  JCAA-RC1       PIC  X(001).
           02  JCAA-HR.
             03  JCAA-DC1     PIC  9(002).
             03  JCAA-HNO     PIC  9(009).
             03  F            PIC  X(013).
             03  JCAA-DPC     PIC  9(002).
             03  JCAA-HNGP    PIC  9(006).
             03  JCAA-NNGP    PIC  9(006).
             03  JCAA-THC     PIC  9(006).
             03  F            PIC  X(012).
             03  JCAA-STC     PIC  9(007).
             03  F            PIC  X(013).
             03  JCAA-BI      PIC  X(010).
             03  F            PIC  X(012).
             03  JCAA-SNGP    PIC  9(008).
             03  JCAA-HNA     PIC  X(006).
             03  F            PIC  X(002).
             03  JCAA-ZON     PIC  9(004).
             03  JCAA-DNO     PIC  9(007).
             03  F            PIC  X(001).
           02  JCAA-MR    REDEFINES JCAA-HR.
             03  JCAA-DC2     PIC  9(002).
             03  JCAA-DGN     PIC  9(002).
             03  JCAA-JAN     PIC  X(013).
             03  JCAA-ISU     PIC  9(004).
             03  F            PIC  X(004).
             03  JCAA-TY      PIC  X(002).
             03  JCAA-SU      PIC  9(006).
             03  JCAA-GTN     PIC  9(007).
             03  F            PIC  X(002).
             03  JCAA-UTN     PIC  9(007).
             03  JCAA-GKIN    PIC  9(010).
             03  JCAA-UKIN    PIC  9(010).
             03  F            PIC  X(002).
             03  JCAA-DPM     PIC  X(002).
             03  JCAA-CLS     PIC  X(003).
             03  JCAA-SHM     PIC  X(013).
             03  JCAA-MKH     PIC  X(010).
             03  JCAA-MSB     PIC  X(010).
             03  F            PIC  X(017).
           02  JCAA-RSC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JCAARF
       01  JCAARF_JHS15U.
           02  JCAARF_PNAME1  PIC  X(006) VALUE "JCAARF".
           02  F              PIC  X(001).
           02  JCAARF_LNAME   PIC  X(013) VALUE "JCAARF_JHS15U".
           02  F              PIC  X(001).
           02  JCAARF_KEY1    PIC  X(100) VALUE SPACE.
           02  JCAARF_SORT    PIC  X(100) VALUE SPACE.
           02  JCAARF_IDLST   PIC  X(100) VALUE SPACE.
           02  JCAARF_RES     USAGE  POINTER.
       01  JCAAR-R.
           02  JCAAR-RC1      PIC  X(001).
           02  JCAAR-HR.
             03  JCAAR-DC1    PIC  9(002).
             03  JCAAR-HNO    PIC  9(009).
             03  F            PIC  X(013).
             03  JCAAR-DPC    PIC  9(002).
             03  JCAAR-HNGP   PIC  9(006).
             03  JCAAR-NNGP   PIC  9(006).
             03  JCAAR-THC    PIC  9(006).
             03  F            PIC  X(012).
             03  JCAAR-STC    PIC  9(007).
             03  F            PIC  X(013).
             03  JCAAR-BI     PIC  X(010).
             03  F            PIC  X(012).
             03  JCAAR-SNGP   PIC  9(008).
             03  JCAAR-HNA    PIC  X(006).
             03  F            PIC  X(002).
             03  JCAAR-ZON    PIC  9(004).
             03  JCAAR-DNO    PIC  9(007).
             03  F            PIC  X(002).
           02  JCAAR-MR    REDEFINES JCAAR-HR.
             03  JCAAR-DC2    PIC  9(002).
             03  JCAAR-DGN    PIC  9(002).
             03  JCAAR-JAN    PIC  X(013).
             03  JCAAR-ISU    PIC  9(004).
             03  F            PIC  X(004).
             03  JCAAR-TY     PIC  X(002).
             03  JCAAR-SU     PIC  9(006).
             03  JCAAR-GTN    PIC  9(007).
             03  F            PIC  X(002).
             03  JCAAR-UTN    PIC  9(007).
             03  JCAAR-GKIN   PIC  9(010).
             03  JCAAR-UKIN   PIC  9(010).
             03  F            PIC  X(002).
             03  JCAAR-DPM    PIC  X(002).
             03  JCAAR-CLS    PIC  X(003).
             03  JCAAR-SHM    PIC  X(013).
             03  JCAAR-MKH    PIC  X(010).
             03  JCAAR-MSB    PIC  X(010).
             03  F            PIC  X(018).
           02  JCAAR-DATE     PIC  9(006).
           02  JCAAR-NO       PIC  9(004).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
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
                "＊＊＊　　受注受信データ　累積・生成　　＊＊＊".
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
                  "データエラー１".
             03  E-ME3   PIC  N(007) VALUE
                  "データエラー２".
             03  E-ME4   PIC  N(005) VALUE
                  "行　エラー".
             03  E-ME5   PIC  N(005) VALUE
                  "累積　済み".
             03  E-ME6   PIC  X(025) VALUE
                  "***  TDNAF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(027) VALUE
                  "***  TDNAF REWRITE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(020) VALUE
                  "***  JANｺｰﾄﾞ ﾅｼ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  ﾉｳﾋﾝｻｷ ﾅｼ  ***".
             03  E-ME12  PIC  X(026) VALUE
                  "***  JCAARF WRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME16.
               04  E-NO   PIC  9(002).
               04  FILLER PIC  9(002).
             03  E-ME17.
               04  FILLER PIC  N(008) VALUE
                    "データ２重エラー".
               04  FILLER PIC  9(009).
               04  FILLER PIC  9(006).
             03  E-JAN   PIC  X(013).
             03  E-STC   PIC  9(007).
             03  E-TDNA  PIC  X(016).
           COPY LSSEM.
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
            "C-ERR" " " "0" "0" "264" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "264" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "N" "24" "15" "12" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "N" "24" "15" "14" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "N" "24" "15" "14" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "N" "24" "15" "10" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "N" "24" "15" "10" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "15" "27" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "20" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "19" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME12" "X" "24" "15" "26" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME13" "X" "24" "15" "16" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME16" " " "24" "0" "4" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-NO" "9" "24" "28" "2" " " "E-ME16" RETURNING RESU.
       CALL "SD_From" USING
            "E-NO" BY REFERENCE W-DGN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME16" "9" "24" "31" "2" "E-NO" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME16" BY REFERENCE JCAA-DGN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME17" " " "24" "0" "31" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME17" "N" "24" "15" "16" " " "E-ME17" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME17" "9" "24" "33" "9" "01E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME17" BY REFERENCE W-HNO "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03E-ME17" "9" "24" "43" "6" "02E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING
            "03E-ME17" BY REFERENCE W-HNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-JAN" "X" "24" "37" "13" "E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-JAN" BY REFERENCE CODE-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STC" "9" "24" "37" "7" "E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-STC" BY REFERENCE AHNH-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-TDNA" "X" "24" "45" "16" "E-STC" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-TDNA" BY REFERENCE TDNA-KEY "16" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM DCK-RTN THRU DCK-EX.
           IF  W-DC = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" JCAAF_PNAME1 " " BY REFERENCE JCAAF_IDLST "0".
       M-06.
      *           READ JCAAF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAAF_IDLST JCAAF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JCAA-RSC = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-06
           END-IF
           IF  JCAA-RC1 NOT = "A"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAAF_IDLST JCAAF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT W-DATE FROM DATE.
           MOVE ZERO TO W-NO.
           CALL "DB_F_Open" USING
            "EXTEND" JCAARF_PNAME1 " " BY REFERENCE JCAARF_IDLST "0".
       M-15.
           INITIALIZE JCAAR-R.
           MOVE JCAA-R TO JCAAR-R.
           MOVE W-DATE TO JCAAR-DATE.
           ADD 1 TO W-NO.
           MOVE W-NO TO JCAAR-NO.
      *           WRITE JCAAR-R.
      *//////////////
           CALL "DB_Insert" USING
            JCAARF_PNAME1 JCAARF_LNAME JCAAR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME12" E-ME12 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAAF_IDLST JCAAF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCAARF_IDLST JCAARF_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAARF_IDLST JCAARF_PNAME1.
           MOVE "JCAARF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JCAARF_PNAME1 " " BY REFERENCE JCAARF_IDLST "0".
           GO TO M-15.
       M-20.
           MOVE 1 TO JCAA-RSC.
      *           REWRITE JCAA-R.
      *///////////////
           CALL "DB_Update" USING
            JCAAF_PNAME1 JCAAF_LNAME JCAA-R RETURNING RET.
      *
      *           READ JCAAF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           GO TO M-15.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAAF_IDLST JCAAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAARF_IDLST JCAARF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCAAF_PNAME1 " " BY REFERENCE JCAAF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           MOVE W-MSIZ TO W-ASIZD.
           MOVE ZERO TO W-DNOS W-DNOE.
       M-30.
      *           READ JCAAF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  JCAA-RC1 NOT = "B"
               GO TO M-30
           END-IF.
       M-35.
           MOVE ZERO TO W-DD.
           MOVE JCAA-DNO TO W-DNO.
           MOVE JCAA-HNO TO W-HNO.
           MOVE JCAA-DPC TO W-DPC.
           MOVE JCAA-HNGP TO W-HNGP.
           MOVE JCAA-NNGP TO W-NNGP.
           MOVE JCAA-THC TO W-THC.
           MOVE JCAA-STC TO W-STC.
           MOVE JCAA-BI TO W-BI.
           MOVE JCAA-SNGP TO W-SNGP.
           MOVE JCAA-HNA TO W-HNA.
           MOVE JCAA-ZON TO W-ZON.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = 1
               GO TO M-30
           END-IF
           IF  W-DNOS = ZERO
               MOVE W-DNO TO W-DNOS
           END-IF
           IF  W-DNO < W-DNOS
               MOVE W-DNO TO W-DNOS
           END-IF
           IF  W-DNO > W-DNOE
               MOVE W-DNO TO W-DNOE
           END-IF
           MOVE ZERO TO W-DGN.
      *
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               MOVE ZERO TO AHNH-CCD
               MOVE ALL "＊" TO AHNH-NHSN
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STC" E-STC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-40.
      *           READ JCAAF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  JCAA-RC1 = "A"
               GO TO M-40
           END-IF
           IF  JCAA-RC1 = "B"
               GO TO M-35
           END-IF
           IF  JCAA-RC1 NOT = "D"
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-35
           END-IF
      *
           MOVE ALL "*" TO W-DSZ.
           MOVE ALL "＊" TO W-COR.
           MOVE ZERO TO W-HCD.
           MOVE ZERO TO CODE-KEY.
           MOVE JCAA-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-45
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF (CODE-TCD NOT = ZERO) OR (CODE-JAN NOT = JCAA-JAN)
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-45
           END-IF
           MOVE CODE-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-45
           END-IF
           MOVE HI-HCD TO W-HCD.
           PERFORM COR-RTN THRU COR-EX.
           IF (CODE-SIZ < 1 OR > 4) OR (CODE-SNO < 1 OR > 10)
               GO TO M-45
           END-IF
           MOVE W-SIZ(CODE-SIZ,CODE-SNO) TO W-DSZ.
           IF  W-DSZ = SPACE
               MOVE 1 TO W-INV
               MOVE ALL "*" TO W-DSZ
           ELSE
               IF  HI-HKB = 1
                   MOVE 5 TO W-DSZH
               END-IF
           END-IF.
       M-45.
           ADD 1 TO W-DGN.
           IF  W-DGN > 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JCAA-DGN NOT = W-DGN
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM MOV-RTN THRU MOV-EX.
       M-50.
      *           WRITE TDNA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-40.
       M-55.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-70
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           MOVE "TDNAF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           GO TO M-50.
       M-70.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAAF_IDLST JCAAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           IF  W-INV = 0
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
       M-75.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TDNA-NRC NOT = 0
               GO TO M-75
           END-IF
           IF  TDNA-PC NOT = 0
               GO TO M-75
           END-IF
           IF  W-DNOS <= W-DNOE
               IF  TDNA-DNO >= W-DNOS AND <= W-DNOE
                   GO TO M-80
               END-IF
           END-IF
           IF  W-DNOS > W-DNOE
               IF  TDNA-DNO >= W-DNOS OR <= W-DNOE
                   GO TO M-80
               END-IF
           END-IF
           GO TO M-75.
       M-80.
           MOVE 1 TO TDNA-RC.
      *           REWRITE TDNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TDNA" E-TDNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-85
           END-IF
           GO TO M-75.
       M-85.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MOV-RTN.
           INITIALIZE TDNA-R.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
           MOVE JCAA-DGN TO TDNA-DGN.
           MOVE JCAA-JAN TO TDNA-JAN.
           MOVE JCAA-SU TO TDNA-SU.
           MOVE JCAA-GTN TO TDNA-GTN.
           MOVE JCAA-UTN TO TDNA-UTN.
           MOVE JCAA-GKIN TO TDNA-GKIN.
           MOVE JCAA-UKIN TO TDNA-UKIN.
           MOVE JCAA-DPM TO TDNA-DPM.
           MOVE JCAA-CLS TO TDNA-CLS.
           MOVE JCAA-SHM TO TDNA-SHM.
           MOVE JCAA-MKH TO TDNA-MKH.
           MOVE JCAA-MSB TO TDNA-MSB.
           MOVE JCAA-TY TO TDNA-TY.
           MOVE W-HNO TO TDNA-HNO.
           MOVE W-HNGP TO TDNA-HNGP.
           MOVE W-NNGP TO TDNA-NNGP.
           MOVE W-THC TO TDNA-THC.
           MOVE W-BI TO TDNA-BI.
           MOVE W-SNGP TO TDNA-SNGP.
           MOVE W-HNA TO TDNA-HNA.
           MOVE W-ZON TO TDNA-ZON.
           MOVE W-DPC TO TDNA-DC.
           MOVE W-HCD TO TDNA-HCD.
           MOVE W-COR TO TDNA-COR.
           MOVE W-DSZ TO TDNA-SIZ.
           MOVE AHNH-NHSN TO TDNA-TNA.
           MOVE AHNH-CCD TO TDNA-CCD.
       MOV-EX.
           EXIT.
       CHK-RTN.
           MOVE 0 TO CHK.
           MOVE JCAA-STC TO TDNA-STC.
           MOVE JCAA-DNO TO TDNA-DNO.
      *           READ TDNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNAF_PNAME1 BY REFERENCE TDNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-EX
           END-IF
           MOVE 1 TO CHK.
           CALL "SD_Output" USING "E-ME17" E-ME17 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       CHK-EX.
           EXIT.
       COR-RTN.
           MOVE SPACE TO W-AHN W-COR.
           MOVE HI-NAME TO W-AHN.
           MOVE ZERO TO CNT.
       COR-010.
           ADD 1 TO CNT.
           IF  CNT < 25
               IF  W-HN(CNT) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF
           ADD 1 TO CNT.
           IF  CNT < 25
               IF  W-HN(CNT) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF.
       COR-020.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO COR-EX
           END-IF
           IF  W-HN(CNT) = SPACE
               GO TO COR-020
           END-IF
           MOVE ZERO TO CNTD.
       COR-030.
           ADD 1 TO CNTD.
           IF  CNTD > 4
               GO TO COR-EX
           END-IF
           MOVE W-HN(CNT) TO W-NA(CNTD).
           ADD 1 TO CNT.
           IF  CNT < 25
               GO TO COR-030
           END-IF.
       COR-EX.
           EXIT.
       DCK-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JCAAF_PNAME1 " " BY REFERENCE JCAAF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           MOVE 0 TO W-DC.
       DCK-010.
      *           READ JCAAF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCAAF_PNAME1 BY REFERENCE JCAA-R " " RETURNING RET.
           IF  RET = 1
               GO TO DCK-090
           END-IF
           IF  JCAA-RC1 NOT = "B"
               GO TO DCK-010
           END-IF
           MOVE JCAA-STC TO AHNH-KEY W-STC.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-DC
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STC" E-STC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO DCK-010.
       DCK-090.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAAF_IDLST JCAAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
       DCK-EX.
           EXIT.
