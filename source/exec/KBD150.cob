       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD150.
      ****************************************************************
      *    PROGRAM         :  製品仕入 販売変換可能・営業更新,リスト *
      *    PRINTER TYPE    :  JIPS                                   *
      *    COMPILE TYPE    :  COBOL                                  *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　製品仕入　営業変換・販売変換可能リスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(101) VALUE SPACE.
           02  F              PIC  X(035) VALUE
                "合計'*'印は単価未登録で販売変換不可".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "SS".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "M".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(001) VALUE "L".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(002) VALUE "LL".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
       01  HEAD4.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(018) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(024) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(010) VALUE SPACE.
       01  W-P.
           02  P-NGP          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SKC          PIC  9(001).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-ASU.
             03  P-SUD   OCCURS  10.
               04  P-SU       PIC --,---.
           02  P-TSU          PIC ----,--9.
           02  P-ZERO         PIC  X(001).
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-SNO          PIC  9(001).
       01  W-DATA.
           02  W-DNO          PIC  9(006).
           02  W-DNOD         PIC  9(006).
           02  W-NGPD         PIC  9(006).
           02  W-SKC          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-TSU          PIC S9(006).
           02  WS-TSU         PIC S9(006).
           02  WA-TSU         PIC S9(006).
           02  W-PAGE         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
             03  CHK3         PIC  9(001).
           02  W-GC           PIC  9(001).
           02  W-GCD          PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-SKCD         PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-SEQ          PIC  9(002).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  W-END              PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHSSF.
           COPY LIHSHF.
           COPY LIHSHN.
           COPY L-JNSR.
           COPY L-JCON.
           COPY LNJZAI.
           COPY LJNYZ.
           COPY LSPF.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　製品仕入　営業変換　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　販売変換可能リスト　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-HSS   PIC  9(007).
             03  E-JNSR.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(008).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(001).
             03  E-NJZAI PIC  X(008).
             03  E-HSH   PIC  9(008).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "358" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "76" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HSS" "9" "24" "45" "7" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HSS" BY REFERENCE HSS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JNSR" " " "24" "0" "23" "E-HSS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-JNSR" "9" "24" "45" "6" " " "E-JNSR" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-JNSR" BY REFERENCE HSS-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-JNSR" "9" "24" "51" "8" "01E-JNSR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-JNSR" BY REFERENCE HSS-DATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-JNSR" "9" "24" "59" "2" "02E-JNSR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-JNSR" BY REFERENCE JNSR-03 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-JNSR" "9" "24" "61" "6" "03E-JNSR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-JNSR" BY REFERENCE HSS-UNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05E-JNSR" "9" "24" "67" "1" "04E-JNSR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05E-JNSR" BY REFERENCE W-GCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NJZAI" "X" "24" "45" "8" "E-JNSR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NJZAI" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HSH" "9" "24" "45" "8" "E-NJZAI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HSH" BY REFERENCE HSH-KEY "8" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE SPACE TO JCON6-KEY.
           MOVE 6 TO JCON6-01.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               MOVE "***  JCON ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE JCON6-0312 TO W-NEN2.
           MOVE JCON6-032 TO W-GET.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           COPY LIBCPR.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HSS-F_PNAME1 " " BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-15.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               GO TO M-95
           END-IF
           IF  HSS-PRC = 0
               GO TO M-15
           END-IF
           IF  HSS-HKC = 1
               GO TO M-15
           END-IF
           IF  HSS-NG > W-NG
               GO TO M-15
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
            "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
            HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3.
           CALL "DB_F_Open" USING
            "I-O" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
            "JNYZ-KEY" BY REFERENCE JNYZ-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-020 THRU MID-EX.
       M-20.
           MOVE HSS-NGPS TO W-NGPD.
           MOVE ZERO TO WA-TSU CHK.
       M-25.
           MOVE HSS-SKC TO W-SKC.
           MOVE ZERO TO WS-TSU CHK2 CHK3.
       M-30.
           MOVE HSS-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE HI-KEY TO HI-MHCD
               MOVE ZERO TO HI-FT
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF
           MOVE ZERO TO CHK3.
       M-35.
           PERFORM MEI-RTN THRU MEI-EX.
      *
           PERFORM UPD-RTN THRU UPD-EX.
           IF  W-END = 1
               GO TO M-90
           END-IF.
       M-40.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  HSS-PRC = 0
               GO TO M-40
           END-IF
           IF  HSS-HKC = 1
               GO TO M-40
           END-IF
           IF  HSS-NG > W-NG
               GO TO M-40
           END-IF
           IF  HSS-NGPS NOT = W-NGPD
               GO TO M-50
           END-IF
           IF  HSS-SKC NOT = W-SKC
               GO TO M-45
           END-IF
           IF  HSS-HCD = W-HCD
               GO TO M-35
           END-IF
           GO TO M-30.
       M-45.
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-25.
       M-50.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
           GO TO M-20.
       M-55.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNYZ_IDLST JNYZ_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-NGPD TO P-NGP
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-SKC TO P-SKC
           END-IF
           IF  CHK3 = 0
               MOVE 1 TO CHK3
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
           END-IF
           MOVE 1 TO W-GC.
       MEI-020.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               MOVE 1 TO W-GC
           END-IF
           IF  ZERO = HSS-SU(W-GC,01) AND HSS-SU(W-GC,02) AND
                     HSS-SU(W-GC,03) AND HSS-SU(W-GC,04) AND
                     HSS-SU(W-GC,05) AND HSS-SU(W-GC,06) AND
                     HSS-SU(W-GC,07) AND HSS-SU(W-GC,08) AND
                     HSS-SU(W-GC,09) AND HSS-SU(W-GC,10)
               IF  W-GC = 1
                   GO TO MEI-EX
               ELSE
                   GO TO MEI-020
               END-IF
           END-IF
           MOVE W-GC TO P-SIZ.
           MOVE ZERO TO W-SC W-TSU.
       MEI-040.
           ADD 1 TO W-SC.
           IF  W-SC NOT = 11
               MOVE HSS-SU(W-GC,W-SC) TO P-SU(W-SC)
               ADD HSS-SU(W-GC,W-SC) TO W-TSU
               GO TO MEI-040
           END-IF
           MOVE W-TSU TO P-TSU.
           IF  HI-FT = ZERO
               MOVE "*" TO P-ZERO
           END-IF
           MOVE HSS-DNO TO P-DNO.
           MOVE "-" TO P-V.
           MOVE HSS-SNO TO P-SNO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-NGPD TO P-NGP
               MOVE W-SKC TO P-SKC
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-TSU TO WS-TSU.
           IF  W-GC NOT = 1
               MOVE SPACE TO W-P
               MOVE SPACE TO P-HNA
               GO TO MEI-020
           END-IF.
       MEI-EX.
           EXIT.
       SUB-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　　　　　（　小　計　）" TO P-HNA.
           MOVE WS-TSU TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-NGPD TO P-NGP
               MOVE W-SKC TO P-SKC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-TSU TO WA-TSU.
       SUB-EX.
           EXIT.
       ALL-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　［　合　計　］" TO P-HNA.
           MOVE WA-TSU TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-NGPD TO P-NGP
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ALL-EX.
           EXIT.
       UPD-RTN.
           IF  HSS-RNO = ZERO
               GO TO UPD-180
           END-IF
           MOVE ZERO TO W-SEQ.
           MOVE SPACE TO HSHN-KEY.
           MOVE HSS-RNO TO HSHN-RNO.
           MOVE HSS-DATE TO HSHN-DATE.
      *           START HSHNF KEY NOT < HSHN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHNF_PNAME1 "HSHN-KEY" " NOT < " HSHN-KEY RETURNING RET.
           IF  RET = 1
               GO TO UPD-040
           END-IF.
       UPD-020.
      *           READ HSHNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-040
           END-IF
           IF  HSS-RNO = HSHN-RNO
               IF  HSS-DATE = HSHN-DATE
                   MOVE HSHN-SNO TO W-SEQ
                   GO TO UPD-020
               END-IF
           END-IF.
       UPD-040.
           ADD 1 TO W-SEQ.
           INITIALIZE HSHN-R.
           MOVE HSS-RNO TO HSHN-RNO.
           MOVE HSS-DATE TO HSHN-DATE.
           MOVE W-SEQ TO HSHN-SNO.
           MOVE HSS-SCD TO HSHN-SCD.
           MOVE HSS-HCD TO HSHN-HCD.
           MOVE HSS-ASUD TO HSHN-ASUD.
           MOVE HSS-DNO TO HSHN-KBNO.
           MOVE HSS-KRC TO HSHN-KRC.
           MOVE HSS-HPC TO HSHN-HPC.
           MOVE HSS-UNO TO HSHN-UNO.
       UPD-060.
      *           WRITE HSHN-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSHNF_PNAME1 HSHNF_LNAME HSHN-R RETURNING RET.
           IF  RET = 1
               MOVE "***  HSHNF WRITE ｴﾗｰ  ***     " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSS" E-HSS "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-080
           END-IF
           GO TO UPD-100.
       UPD-080.
           IF  ERR-STAT NOT = "24"
               MOVE 1 TO W-END
               GO TO UPD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
           MOVE "HSHNF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
           GO TO UPD-060.
       UPD-100.
           MOVE HSS-RNO TO HSH-KEY.
      *           READ HSHF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  HSHF ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSH" E-HSH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           MOVE 0 TO W-GC HSH-ICHK.
       UPD-120.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               GO TO UPD-160
           END-IF
           MOVE ZERO TO W-SC.
       UPD-140.
           ADD 1 TO W-SC.
           IF  W-SC = 11
               GO TO UPD-120
           END-IF
           IF  HSS-SU(W-GC,W-SC) NOT = ZERO
               SUBTRACT HSS-SU(W-GC,W-SC) FROM HSH-ISU(W-GC,W-SC)
               ADD HSS-SU(W-GC,W-SC) TO HSH-NSU(W-GC,W-SC)
           END-IF
           IF  HSH-ICHK = 0
               IF  HSH-ISU(W-GC,W-SC) NOT = ZERO
                   MOVE 1 TO HSH-ICHK
               END-IF
           END-IF
           GO TO UPD-140.
       UPD-160.
      *           REWRITE HSH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSHF_PNAME1 HSHF_LNAME HSH-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  HSHF REWRITE ｴﾗｰ  ***    " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSH" E-HSH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF.
       UPD-180.
           IF  HI-MHCD > 999899
               GO TO UPD-400
           END-IF
           MOVE ZERO TO W-GC W-GCD.
       UPD-200.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               GO TO UPD-400
           END-IF
           IF  ZERO = HSS-SU(W-GC,01) AND HSS-SU(W-GC,02) AND
                     HSS-SU(W-GC,03) AND HSS-SU(W-GC,04) AND
                     HSS-SU(W-GC,05) AND HSS-SU(W-GC,06) AND
                     HSS-SU(W-GC,07) AND HSS-SU(W-GC,08) AND
                     HSS-SU(W-GC,09) AND HSS-SU(W-GC,10)
               GO TO UPD-200
           END-IF
           ADD 1 TO W-GCD.
      *
           MOVE SPACE TO JNSR-R.
           INITIALIZE JNSR-R.
           MOVE HI-MHCD TO JNSR-01.
           MOVE HSS-DATE TO JNSR-02 JNSR-16 JNSR-20.
           MOVE 10 TO JNSR-03.
           MOVE HSS-UNO TO JNSR-04 JNSR-181 JNSR-221.
           MOVE W-GCD TO JNSR-05 JNSR-182 JNSR-222.
           MOVE HSS-SKC TO JNSR-06.
           MOVE W-GC TO JNSR-07.
           MOVE HSS-SU(W-GC,01) TO JNSR-081(01).
           MOVE HSS-SU(W-GC,02) TO JNSR-081(02).
           MOVE HSS-SU(W-GC,03) TO JNSR-081(03).
           MOVE HSS-SU(W-GC,04) TO JNSR-081(04).
           MOVE HSS-SU(W-GC,05) TO JNSR-081(05).
           MOVE HSS-SU(W-GC,06) TO JNSR-081(06).
           MOVE HSS-SU(W-GC,07) TO JNSR-081(07).
           MOVE HSS-SU(W-GC,08) TO JNSR-081(08).
           MOVE HSS-SU(W-GC,09) TO JNSR-081(09).
           MOVE HSS-SU(W-GC,10) TO JNSR-081(10).
           MOVE 0 TO JNSR-09.
           MOVE 5 TO JNSR-17 JNSR-21.
           MOVE HSS-RNO TO JNSR-81.
           MOVE HI-BCD1 TO JNSR-82.
           IF  W-NG > HSS-NG
               MOVE 1 TO JNSR-91
           ELSE
               MOVE 0 TO JNSR-91
           END-IF.
       UPD-220.
      *           WRITE JNSR-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            JNSR_PNAME1 JNSR_LNAME JNSR-R RETURNING RET.
           IF  RET = 1
               MOVE "***  JNSR WRITE ｴﾗｰ  ***      " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JNSR" E-JNSR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-240
           END-IF
           GO TO UPD-260.
       UPD-240.
           IF  ERR-STAT NOT = "24"
               MOVE 1 TO W-END
               GO TO UPD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           MOVE "JNSR         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           GO TO UPD-220.
       UPD-260.
           MOVE 0 TO W-SKCD.
       UPD-280.
           MOVE 2 TO W-ACT.
           IF  W-SKCD = 0
               MOVE HSS-SKC TO NJZAI-01
           ELSE
               MOVE 9 TO NJZAI-01
           END-IF
           MOVE HI-MHCD TO NJZAI-02.
           MOVE W-GC TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-ACT
           END-IF
           IF  W-ACT = 1
               INITIALIZE NJZAI-R
               MOVE HI-MHCD TO NJZAI-02
               MOVE W-GC TO NJZAI-03
               IF  W-SKCD = 0
                   MOVE HSS-SKC TO NJZAI-01
               ELSE
                   MOVE 9 TO NJZAI-01
               END-IF
           END-IF
           IF  W-NG > HSS-NG
               ADD HSS-SU(W-GC,01) TO NJZAI-0411(01)
               ADD HSS-SU(W-GC,02) TO NJZAI-0411(02)
               ADD HSS-SU(W-GC,03) TO NJZAI-0411(03)
               ADD HSS-SU(W-GC,04) TO NJZAI-0411(04)
               ADD HSS-SU(W-GC,05) TO NJZAI-0411(05)
               ADD HSS-SU(W-GC,06) TO NJZAI-0411(06)
               ADD HSS-SU(W-GC,07) TO NJZAI-0411(07)
               ADD HSS-SU(W-GC,08) TO NJZAI-0411(08)
               ADD HSS-SU(W-GC,09) TO NJZAI-0411(09)
               ADD HSS-SU(W-GC,10) TO NJZAI-0411(10)
           ELSE
               ADD HSS-SU(W-GC,01) TO NJZAI-0711(01)
               ADD HSS-SU(W-GC,02) TO NJZAI-0711(02)
               ADD HSS-SU(W-GC,03) TO NJZAI-0711(03)
               ADD HSS-SU(W-GC,04) TO NJZAI-0711(04)
               ADD HSS-SU(W-GC,05) TO NJZAI-0711(05)
               ADD HSS-SU(W-GC,06) TO NJZAI-0711(06)
               ADD HSS-SU(W-GC,07) TO NJZAI-0711(07)
               ADD HSS-SU(W-GC,08) TO NJZAI-0711(08)
               ADD HSS-SU(W-GC,09) TO NJZAI-0711(09)
               ADD HSS-SU(W-GC,10) TO NJZAI-0711(10)
           END-IF
           IF  W-ACT = 2
               GO TO UPD-340
           END-IF.
       UPD-300.
      *           WRITE NJZAI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE "***  NJZAI WRITE ｴﾗｰ  ***     " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-320
           END-IF
           IF  W-SKCD = 0
               MOVE 1 TO W-SKCD
               GO TO UPD-280
           END-IF
           GO TO UPD-360.
       UPD-320.
           IF  ERR-STAT NOT = "24"
               MOVE 1 TO W-END
               GO TO UPD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           MOVE "NJZAI        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           GO TO UPD-300.
       UPD-340.
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  NJZAI REWRITE ｴﾗｰ  ***   " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           IF  W-SKCD = 0
               MOVE 1 TO W-SKCD
               GO TO UPD-280
           END-IF.
       UPD-360.
           IF  W-NG NOT > HSS-NG
               GO TO UPD-200
           END-IF
           MOVE HI-MHCD TO JNYZ-01.
           MOVE W-GC TO JNYZ-02.
      *           READ JNYZ INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNYZ_PNAME1 BY REFERENCE JNYZ-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-200
           END-IF
           SUBTRACT HSS-SU(W-GC,01) FROM JNYZ-0311(01).
           SUBTRACT HSS-SU(W-GC,02) FROM JNYZ-0311(02).
           SUBTRACT HSS-SU(W-GC,03) FROM JNYZ-0311(03).
           SUBTRACT HSS-SU(W-GC,04) FROM JNYZ-0311(04).
           SUBTRACT HSS-SU(W-GC,05) FROM JNYZ-0311(05).
           SUBTRACT HSS-SU(W-GC,06) FROM JNYZ-0311(06).
           SUBTRACT HSS-SU(W-GC,07) FROM JNYZ-0311(07).
           SUBTRACT HSS-SU(W-GC,08) FROM JNYZ-0311(08).
           SUBTRACT HSS-SU(W-GC,09) FROM JNYZ-0311(09).
           SUBTRACT HSS-SU(W-GC,10) FROM JNYZ-0311(10).
           IF  ZERO = JNYZ-0311(01) AND JNYZ-0311(02) AND
                     JNYZ-0311(03) AND JNYZ-0311(04) AND
                     JNYZ-0311(05) AND JNYZ-0311(06) AND
                     JNYZ-0311(07) AND JNYZ-0311(08) AND
                     JNYZ-0311(09) AND JNYZ-0311(10)
               GO TO UPD-380
           END-IF
      *           REWRITE JNYZ-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNYZ_PNAME1 JNYZ_LNAME JNYZ-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  JNYZ REWRITE ｴﾗｰ  ***    " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSS" E-HSS "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           GO TO UPD-200.
       UPD-380.
      *           DELETE JNYZ INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JNYZ_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  JNYZ DELETE ｴﾗｰ  ***     " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSS" E-HSS "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           GO TO UPD-200.
       UPD-400.
           MOVE 1 TO HSS-HKC.
      *           REWRITE HSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSS-F_PNAME1 HSS-F_LNAME HSS-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               MOVE "***  HSSF REWRITE ｴﾗｰ  ***    " TO W-EM
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HSS" E-HSS "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       UPD-EX.
           EXIT.
