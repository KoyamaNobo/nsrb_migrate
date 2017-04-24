       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD330.
      *********************************************************
      *    PROGRAM         :  倉庫間移動一括入力　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  ERR-STAT           PIC  X(002).
       77  W-POC              PIC  9(001) VALUE 0.
       77  W-IPC              PIC  9(001) VALUE 0.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　倉庫間移動　一括入力リスト　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "倉　　庫".
           02  F              PIC  X(120) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(021) VALUE
                "   28.0   29.0   30.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "2   12.5   13.0   13.5   14.0   15.0   ".
           02  F              PIC  X(032) VALUE
                "16.0   17.0   18.0   19.0   20.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "3   21.0   21.5   22.0   22.5   23.0   ".
           02  F              PIC  X(032) VALUE
                "23.5   24.0   24.5   25.0       ".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "4   24.0   24.5   25.0   25.5   26.0   ".
           02  F              PIC  X(032) VALUE
                "26.5   27.0   27.5              ".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
       01  W-P1.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SKC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-SKN          PIC  N(006).
           02  F              PIC  X(115).
       01  W-P2.
           02  F              PIC  X(004).
           02  P-UNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GYO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SU           PIC ---,---       OCCURS  10.
           02  P-TSU          PIC ----,--9.
       01  W-DATA.
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006).
           02  W-IDM          PIC  9(001).
           02  W-IDS          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-UNO1         PIC  9(006).
           02  W-UNO2         PIC  9(006).
           02  W-GN           PIC  9(001).
           02  W-AMEI.
             03  W-MEID  OCCURS   6.
               04  W-HCD      PIC  9(006).
               04  W-SIZ      PIC  9(001).
               04  W-ASUD.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(005).
           02  WC-ASUD.
             03  WC-SUD   OCCURS  10.
               04  WC-SU      PIC S9(005).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SIZD         PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-ZC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-TSU          PIC S9(006).
           02  W-SUT          PIC S9(006).
           02  W-PAGE         PIC  9(002).
           COPY LSTAT.
           COPY LWMSG.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LNJZAI.
           COPY L-JCON.
           COPY LSPF.
      *FD  HSKIF
       01  HSKIF_HMD330.
           02  HSKIF_PNAME1   PIC  X(005) VALUE "HSKIF".
           02  F              PIC  X(001).
           02  HSKIF_LNAME    PIC  X(012) VALUE "HSKIF_HMD330".
           02  F              PIC  X(001).
           02  HSKIF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSKIF_SORT     PIC  X(100) VALUE SPACE.
           02  HSKIF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSKIF_RES      USAGE  POINTER.
       01  HSKI-R.
           02  HSKI-NO        PIC  9(007).                              受入№
           02  HSKI-NOD   REDEFINES HSKI-NO.
             03  HSKI-UNO     PIC  9(006).
             03  HSKI-GYO     PIC  9(001).
           02  HSKI-DATE      PIC  9(008).
           02  HSKI-NGPD  REDEFINES HSKI-DATE.
             03  HSKI-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  HSKI-NGP   REDEFINES HSKI-DATE.
             03  F            PIC  9(002).
             03  HSKI-NGPS    PIC  9(006).
           02  HSKI-HCD       PIC  9(006).
           02  HSKI-SIZ       PIC  9(001).                              ｻｲｽﾞ区分
           02  HSKI-SUD.                                                数量
             03  HSKI-SU      PIC S9(005)  OCCURS  10.
           02  HSKI-SKC       PIC  9(001).                              倉庫C
           02  HSKI-IDC       PIC  9(001).
           02  F              PIC  X(008).
           02  HSKI-IKC       PIC  9(001).
           02  HSKI-PRN       PIC  9(001).
           02  HSKI-UPD       PIC  9(001).
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
           02  FILLER  PIC  X(012)  VALUE "CLEAR SCREEN".
       01  C-MID.
           03  FILLER  PIC  N(020) VALUE
                "＊＊＊　　倉庫間移動　一括入力　　＊＊＊".
           03  FILLER  PIC  X(043) VALUE
                "品名ｺｰﾄﾞ  000000 ～ 999999          終了=F9".
           03  FILLER  PIC  X(024) VALUE
                "移動元                  ".
           03  FILLER  PIC  X(024) VALUE
                "      ↓　↓　↓　↓    ".
           03  FILLER  PIC  X(024) VALUE
                "移動先                  ".
           03  FILLER  PIC  X(025) VALUE
                "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD   PIC  9(006).
             03  A-EHCD   PIC  9(006).
           02  A-IDM   PIC  9(001).
           02  A-IDS   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-IDMN  PIC  N(006).
           02  D-IDSN  PIC  N(006).
           02  D-PRN.
             03  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　倉庫間移動　入力リスト　　＊＊＊".
             03  FILLER  PIC  X(025) VALUE
                  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME6   PIC  N(011) VALUE
                  "コントロールＦ　未登録".
             03  E-ME7   PIC  N(007) VALUE
                  "倉庫名　未登録".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  JCON ﾃﾞﾝﾋﾟｮｳNO ﾅｼ  ***".
             03  E-ME13  PIC  X(026) VALUE
                  "***  JCON REWRITE ｴﾗｰ  ***".
           COPY LIBSCR.
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "180" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "19" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "9" "27" "43" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "12" "27" "24" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "13" "27" "24" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "14" "27" "24" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "23" "41" "25" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "9" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "9" "37" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "9" "47" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDM" "9" "12" "37" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDM" BY REFERENCE W-IDM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDS" "9" "14" "37" "1" "A-IDM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDS" BY REFERENCE W-IDS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "60" "1" "A-IDS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "91" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-IDMN" "N" "12" "39" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-IDMN" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-IDSN" "N" "14" "39" "12" "D-IDMN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-IDSN" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN" " " "0" "0" "67" "D-IDSN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRN" "N" "1" "19" "42" " " "D-PRN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRN" "X" "23" "41" "25" "01D-PRN" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "125" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "125" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "N" "24" "15" "22" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "N" "24" "15" "14" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "26" "E-ME12" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           MOVE ZERO TO W-DATA.
           MOVE 999999 TO W-EHCD.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           MOVE "18" TO JCON5-KEY.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           COPY LIBCPR.
           ACCEPT W-NGPS FROM DATE.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-IDM "A-IDM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-IDM TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU
               GO TO M-160
           END-IF
           CALL "SD_Output" USING "D-IDMN" D-IDMN "p" RETURNING RESU.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-IDS "A-IDS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-IDS TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU
               GO TO M-180
           END-IF
           CALL "SD_Output" USING "D-IDSN" D-IDSN "p" RETURNING RESU.
           IF  W-IDM = W-IDS
               GO TO M-180
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-DMM = 9
               GO TO M-120
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-200
           END-IF
      *
           MOVE SPACE TO NJZAI-KEY.
           MOVE W-IDM TO NJZAI-01.
           MOVE W-SHCD TO NJZAI-02.
      *           START NJZAI KEY NOT < NJZAI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               GO TO M-980
           END-IF.
       M-220.
      *           READ NJZAI NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               GO TO M-980
           END-IF
           IF  NJZAI-01 > W-IDM
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               GO TO M-980
           END-IF
           IF  NJZAI-02 > W-EHCD
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               GO TO M-980
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-ZC = 0
               GO TO M-220
           END-IF.
       M-240.
           MOVE ZERO TO W-AMEI.
           MOVE 0 TO W-GN.
       M-260.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO M-300
           END-IF
           MOVE NJZAI-02 TO W-HCD(W-GN).
           MOVE NJZAI-03 TO W-SIZ(W-GN).
           MOVE WC-ASUD TO W-ASUD(W-GN).
       M-280.
      *           READ NJZAI NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-320
           END-IF
           IF  NJZAI-01 > W-IDM
               GO TO M-320
           END-IF
           IF  NJZAI-02 > W-EHCD
               GO TO M-320
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-ZC = 0
               GO TO M-280
           END-IF
           GO TO M-260.
       M-300.
           PERFORM WRI-RTN THRU WRI-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-600
           END-IF
           GO TO M-240.
       M-320.
           PERFORM WRI-RTN THRU WRI-EX.
       M-600.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
       M-620.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-980
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-620
           END-IF.
       M-640.
      *           READ HSKIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  HSKI-DATE NOT = W-NGP
               GO TO M-640
           END-IF
           IF  HSKI-HCD < W-SHCD OR > W-EHCD
               GO TO M-640
           END-IF
           IF  HSKI-UPD NOT = 0
               GO TO M-640
           END-IF
           IF  HSKI-PRN NOT = 0
               GO TO M-640
           END-IF
           IF  HSKI-IKC NOT = 1
               GO TO M-640
           END-IF.
       M-660.
           MOVE 0 TO CHK.
           MOVE ZERO TO W-SUT.
           MOVE HSKI-UNO TO W-UNO1.
       M-680.
           PERFORM MEI-RTN THRU MEI-EX.
       M-700.
      *           READ HSKIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-980
           END-IF
           IF  HSKI-DATE NOT = W-NGP
               GO TO M-700
           END-IF
           IF  HSKI-HCD < W-SHCD OR > W-EHCD
               GO TO M-700
           END-IF
           IF  HSKI-UPD NOT = 0
               GO TO M-700
           END-IF
           IF  HSKI-PRN NOT = 0
               GO TO M-700
           END-IF
           IF  HSKI-IKC NOT = 1
               GO TO M-700
           END-IF
           IF  HSKI-UNO = W-UNO1
               GO TO M-680
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-660.
       M-900.
           IF  W-IPC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           IF  W-POC NOT = ZERO
               PERFORM KEI-RTN THRU KEI-EX
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO WC-ASUD CNT W-ZC.
       CHK-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO CHK-EX
           END-IF
           COMPUTE WC-SU(CNT) = NJZAI-0411(CNT) - NJZAI-0511(CNT)
                              + NJZAI-0611(CNT) + NJZAI-0711(CNT)
                              - NJZAI-0811(CNT) - NJZAI-0911(CNT)
                              + NJZAI-1111(CNT).
           IF  W-ZC = 0
               IF  WC-SU(CNT) NOT = ZERO
                   MOVE 1 TO W-ZC
               END-IF
           END-IF
           GO TO CHK-020.
       CHK-EX.
           EXIT.
       WRI-RTN.
           MOVE ZERO TO W-UNO1 W-UNO2.
           MOVE "18" TO JCON5-KEY  ERR-K.
      *           READ JCON INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO W-UNO1.
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO W-UNO2.
      *           REWRITE JCON5-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON5-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           MOVE ZERO TO CHK.
       WRI-020.
           ADD 1 TO CHK.
           IF  CHK = 3
               GO TO WRI-EX
           END-IF
           MOVE ZERO TO W-GN.
       WRI-040.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO WRI-020
           END-IF
           IF  W-HCD(W-GN) = ZERO
               GO TO WRI-020
           END-IF.
       WRI-060.
           MOVE ZERO TO HSKI-R.
           MOVE W-GN TO HSKI-GYO.
           MOVE W-NGP TO HSKI-DATE.
           MOVE W-HCD(W-GN) TO HSKI-HCD.
           MOVE W-SIZ(W-GN) TO HSKI-SIZ.
           IF  CHK = 1
               MOVE W-UNO1 TO HSKI-UNO
               MOVE W-SU(W-GN,01) TO HSKI-SU(01)
               MOVE W-SU(W-GN,02) TO HSKI-SU(02)
               MOVE W-SU(W-GN,03) TO HSKI-SU(03)
               MOVE W-SU(W-GN,04) TO HSKI-SU(04)
               MOVE W-SU(W-GN,05) TO HSKI-SU(05)
               MOVE W-SU(W-GN,06) TO HSKI-SU(06)
               MOVE W-SU(W-GN,07) TO HSKI-SU(07)
               MOVE W-SU(W-GN,08) TO HSKI-SU(08)
               MOVE W-SU(W-GN,09) TO HSKI-SU(09)
               MOVE W-SU(W-GN,10) TO HSKI-SU(10)
               MOVE W-IDS TO HSKI-SKC
               MOVE 1 TO HSKI-IDC
               MOVE 1 TO HSKI-IKC
           ELSE
               MOVE W-UNO2 TO HSKI-UNO
               COMPUTE HSKI-SU(01) = -1 * W-SU(W-GN,01)
               COMPUTE HSKI-SU(02) = -1 * W-SU(W-GN,02)
               COMPUTE HSKI-SU(03) = -1 * W-SU(W-GN,03)
               COMPUTE HSKI-SU(04) = -1 * W-SU(W-GN,04)
               COMPUTE HSKI-SU(05) = -1 * W-SU(W-GN,05)
               COMPUTE HSKI-SU(06) = -1 * W-SU(W-GN,06)
               COMPUTE HSKI-SU(07) = -1 * W-SU(W-GN,07)
               COMPUTE HSKI-SU(08) = -1 * W-SU(W-GN,08)
               COMPUTE HSKI-SU(09) = -1 * W-SU(W-GN,09)
               COMPUTE HSKI-SU(10) = -1 * W-SU(W-GN,10)
               MOVE W-IDM TO HSKI-SKC
               MOVE 1 TO HSKI-IKC
           END-IF
      *           WRITE HSKI-R.
      *//////////////
           CALL "DB_Insert" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-100
           END-IF
           IF  W-IPC = 0
               MOVE 1 TO W-IPC
           END-IF
           GO TO WRI-040.
       WRI-100.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
           MOVE "HSKIF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           GO TO WRI-060.
       WRI-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  W-POC = ZERO
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               ACCEPT H-DATE FROM DATE
               PERFORM MID-010 THRU MID-EX
           END-IF
           IF  CHK NOT = 0
               GO TO MEI-020
           END-IF
           MOVE 1 TO CHK.
           MOVE 3 TO JCON3-01.
           MOVE HSKI-SKC TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-SKN.
           MOVE HSKI-NGPS TO P-DATE.
           MOVE HSKI-SKC TO P-SKC.
           MOVE JCON3-03 TO P-SKN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-020.
           MOVE HSKI-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           COMPUTE W-TSU = HSKI-SU(01) + HSKI-SU(02) + HSKI-SU(03)
                         + HSKI-SU(04) + HSKI-SU(05) + HSKI-SU(06)
                         + HSKI-SU(07) + HSKI-SU(08) + HSKI-SU(09)
                         + HSKI-SU(10).
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           IF  CHK = 1
               MOVE 2 TO CHK
               MOVE HSKI-UNO TO P-UNO
           END-IF
           MOVE "-" TO P-V.
           MOVE HSKI-GYO TO P-GYO.
           MOVE HSKI-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE HSKI-SIZ TO P-SIZ.
           MOVE HSKI-SU(01) TO P-SU(01).
           MOVE HSKI-SU(02) TO P-SU(02).
           MOVE HSKI-SU(03) TO P-SU(03).
           MOVE HSKI-SU(04) TO P-SU(04).
           MOVE HSKI-SU(05) TO P-SU(05).
           MOVE HSKI-SU(06) TO P-SU(06).
           MOVE HSKI-SU(07) TO P-SU(07).
           MOVE HSKI-SU(08) TO P-SU(08).
           MOVE HSKI-SU(09) TO P-SU(09).
           MOVE HSKI-SU(10) TO P-SU(10).
           MOVE W-TSU TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE HSKI-UNO TO P-UNO
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-TSU TO W-SUT.
      *
           MOVE 1 TO HSKI-PRN.
      *           REWRITE HSKI-R.
      *///////////////
           CALL "DB_Update" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
       MEI-EX.
           EXIT.
       KEI-RTN.
           IF  CHK NOT = 2
               GO TO KEI-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　　　　　［　合　計　］" TO P-NAME.
           MOVE W-SUT TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
