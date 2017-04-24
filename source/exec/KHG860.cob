       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KHG860.
       AUTHOR.             H.KAMASAKA    1996-04-17.
      **********************************************
      ******    工品　製造原価　明細表　Ｉ    ******
      **********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  JS-SIGN                 PIC  9.
       77  WK0256ID                PIC  X(009)    VALUE  SPACE.
       01  STN-NO.
           02  STN-NO1             PIC  X(003).
           02  STN-NO2             PIC  X(003).
       01  W-FID.
           02  W-FID1              PIC  X(006)    VALUE  "WK0256".
           02  W-FID2              PIC  X(003).
       01  HEAD1.
           02  F                   PIC  X(005)    VALUE  X"1A24212474".
           02  H-MID               PIC  N(007).
           02  F                   PIC  X(028)    VALUE  SPACE.
           02  F                   PIC  N(023)    VALUE
                 "＊＊＊　　工品　製造原価　明細表　Ｉ　　＊＊＊".
           02  F                   PIC  X(026)    VALUE  SPACE.
           02  F                   PIC  X(005)    VALUE  "DATE ".
           02  H-DATE              PIC  99/99/99.
           02  F                   PIC  X(007)    VALUE  "     P.".
           02  H-PAGE              PIC  Z9.
       01  HEAD2.
           02  F                   PIC  X(005)    VALUE  X"1A24212078".
           02  F                   PIC  X(041)    VALUE  SPACE.
           02  F                   PIC  X(016)    VALUE
                   "I-------------- ".
           02  F                   PIC  N(006)    VALUE
                 "　材　料　費".
           02  F                   PIC  X(018)    VALUE
                   "  --------------I ".
           02  F                   PIC  X(017)    VALUE
                   "I--------------  ".
           02  F                   PIC  N(004)    VALUE  "Ｍ　　Ｈ".
           02  F                   PIC  X(018)    VALUE
                   "  ---------------I".
           02  F                   PIC  X(011)    VALUE  SPACE.
       01  HEAD3.
           02  F                   PIC  N(002)    VALUE  "機種".
           02  F                   PIC  X(005)    VALUE  " ｺｰﾄﾞ".
           02  F                   PIC  X(004)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　単　価".
           02  F                   PIC  X(004)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　数　量".
           02  F                   PIC  X(006)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　金　額".
           02  F                   PIC  X(005)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　ゴ　ム".
           02  F                   PIC  X(005)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　金　具".
           02  F                   PIC  X(004)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　接着剤".
           02  F                   PIC  X(005)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　合　計".
           02  F                   PIC  N(004)    VALUE  "　ゴ　ム".
           02  F                   PIC  N(004)    VALUE  "　研　磨".
           02  F                   PIC  N(004)    VALUE  "　被　膜".
           02  F                   PIC  X(002)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　塗　装".
           02  F                   PIC  X(002)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　加　硫".
           02  F                   PIC  X(002)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　合　計".
           02  F                   PIC  X(005)    VALUE  SPACE.
           02  F                   PIC  N(004)    VALUE  "　加工費".
           02  F                   PIC  X(005)    VALUE  X"1A24212474".
       01  W-P.
           02  P-KOU               PIC  X(008).
           02  P-KOUD  REDEFINES P-KOU.
             03  P-KISR            PIC  9(002).
             03  P-KISM  REDEFINES P-KISR.
               04  F               PIC  X.
               04  P-KIS           PIC  9.
             03  F                 PIC  X.
             03  P-HCD             PIC  X(005).
           02  P-TNK               PIC  ZZZ,ZZ9.99.
           02  P-SRY               PIC  --,---,---.
           02  P-KIN               PIC  ----,---,---.
           02  P-TGM               PIC  ---,---,---.
           02  P-TKN               PIC  ---,---,---.
           02  P-TSZ               PIC  --,---,---.
           02  P-KEI1              PIC  ---,---,---.
           02  P-MGS               PIC  ---9.9.
           02  P-MKM               PIC  ---9.9.
           02  P-MKH               PIC  ---9.9.
           02  P-MTS               PIC  --,--9.9.
           02  P-MKR               PIC  --,--9.9.
           02  P-KEI2              PIC  --,--9.9.
           02  P-KKH               PIC  ---,---,---.
       01  W-RS.
           02  W-SSRY              PIC S9(007).
           02  W-SKIN              PIC S9(009).
           02  W-STGM              PIC S9(008).
           02  W-STKN              PIC S9(008).
           02  W-STSZ              PIC S9(007).
           02  W-SKEI1             PIC S9(008).
           02  W-SMGS              PIC S9(004)V9.
           02  W-SMKM              PIC S9(004)V9.
           02  W-SMTS              PIC S9(004)V9.
           02  W-SMKR              PIC S9(004)V9.
           02  W-SMKH              PIC S9(004)V9.
           02  W-SKEI2             PIC S9(004)V9.
           02  W-SKKH              PIC S9(008).
       01  W-RG.
           02  W-GSRY              PIC S9(007).
           02  W-GKIN              PIC S9(009).
           02  W-GTGM              PIC S9(008).
           02  W-GTKN              PIC S9(008).
           02  W-GTSZ              PIC S9(007).
           02  W-GKEI1             PIC S9(008).
           02  W-GMGS              PIC S9(004)V9.
           02  W-GMKM              PIC S9(004)V9.
           02  W-GMTS              PIC S9(004)V9.
           02  W-GMKR              PIC S9(004)V9.
           02  W-GMKH              PIC S9(004)V9.
           02  W-GKEI2             PIC S9(004)V9.
           02  W-GKKH              PIC S9(008).
       01  W-DATA.
           02  W-DMM               PIC  9(001).
           02  W-KISR              PIC  9(002).
           02  W-KISM  REDEFINES W-KISR.
             03  W-KIS             PIC  9(001).
             03  F                 PIC  X(001).
           02  W-PAGE              PIC  9(002).
           02  W-KEI1              PIC S9(008).
           02  W-KEI2              PIC S9(004)V9.
       01  ERR-STAT                PIC  X(002).
           COPY LSTAT.
      *
           COPY  LIBFDD.
      *FD  WK0256-F
       01  WK0256-F_KHG860.
           02  WK0256-F_PNAME1     PIC  X(009) VALUE SPACE.
           02  F                   PIC  X(001).
           02  WK0256-F_LNAME      PIC  X(015) VALUE "WK0256-F_KHG860".
           02  F                   PIC  X(001).
           02  WK0256-F_KEY1       PIC  X(100) VALUE SPACE.
           02  WK0256-F_KEY2       PIC  X(100) VALUE SPACE.
           02  WK0256-F_SORT       PIC  X(100) VALUE SPACE.
           02  WK0256-F_IDLST      PIC  X(100) VALUE SPACE.
           02  WK0256-F_RES        USAGE  POINTER.
       01  WK0256-R.
           02  WK-KISR             PIC  9(002).
           02  WK-KISM  REDEFINES WK-KISR.
             03  WK-KIS            PIC  9.
             03  F                 PIC  X(001).
           02  WK-HCD              PIC  X(005).
           02  WK-TNK              PIC  9(006)V9(002).
           02  WK-SRY              PIC S9(006).
           02  WK-KIN              PIC S9(008).
           02  WK-TGM              PIC S9(007).
           02  WK-TKN              PIC S9(008).
           02  WK-TSZ              PIC S9(006).
           02  WK-MGS              PIC S9(003)V9.
           02  WK-MKM              PIC S9(003)V9.
           02  WK-MKH              PIC S9(003)V9.
           02  WK-MTS              PIC S9(003)V9.
           02  WK-MKR              PIC S9(003)V9.
           02  WK-KKH              PIC S9(007).
           02  WK-SBB              PIC S9(007).
           02  WK-STS              PIC S9(006).
           02  WK-SNE              PIC S9(005).
           02  WK-SKP              PIC S9(006).
           02  WK-SKY              PIC S9(005).
           02  WK-SMK              PIC S9(006).
           02  WK-SPK              PIC S9(005).
           02  WK-SKG              PIC S9(005).
           02  WK-SAN              PIC S9(005).
           02  WK-SST              PIC S9(007).
           02  WK-DRH              PIC S9(006).
           02  WK-KPS              PIC S9(007).
           02  WK-SKH              PIC S9(007).
           02  FILLER              PIC  X(102).
       77  F                       PIC  X(001).
           COPY LSPHF.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(023)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(023)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(023)    VALUE
                 "＊＊＊　　工品　製造原価　明細表　Ｉ　　＊＊＊".
           02  FILLER  PIC N(023)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(023)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(022)    VALUE
                   "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9 .
       01  D-DSP.
           02  D-DMM   PIC  9 .
           02  D-DC    PIC  N(007).
       01  C-ERR.
           02  FILLER.
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
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
            "C-MID" " " "0" "0" "252" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "20" "22" "22" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *D-DSP
       CALL "SD_Init" USING 
            "D-DSP" " " "0" "0" "15" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMM" "9" "20" "39" "1" " " "D-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DC" "N" "11" "26" "14" "D-DMM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-DC" BY REFERENCE H-MID "14" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 1 AND 2 AND 3 AND 4
               GO  TO  M-05
           END-IF
           IF  JS-SIGN = 1
               MOVE  "【　加　硫　】"  TO  H-MID
           END-IF
           IF  JS-SIGN = 2
               MOVE  "【　出　荷　】"  TO  H-MID
           END-IF
           IF  JS-SIGN = 3
               MOVE  "【　廃　却　】"  TO  H-MID
           END-IF
           IF  JS-SIGN = 4
               MOVE  "【加硫（実）】"  TO  H-MID
           END-IF
           COPY  LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DC" D-DC "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DMM" D-DMM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DC" D-DC "p" RETURNING RESU.
           CALL  "CBLSTNNO"  USING  STN-NO USER_ID.
           MOVE  STN-NO2  TO  W-FID2.
           MOVE  W-FID    TO  WK0256ID.
           MOVE  WK0256ID   TO  WK0256-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WK0256-F_PNAME1 " " BY REFERENCE WK0256-F_IDLST "0".
       M-15.
      *           READ  WK0256-F  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WK0256-F_PNAME1 BY REFERENCE WK0256-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE WK0256-F_IDLST WK0256-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE  ZERO  TO  W-RS  W-RG.
           MOVE  DATE-03R  TO  H-DATE.
           PERFORM  S-10  THRU  S-15.
       M-20.
           IF  JS-SIGN = 4
               MOVE WK-KISR TO W-KISR
           ELSE
               MOVE  WK-KIS TO  W-KIS
           END-IF
           MOVE  ZERO   TO  W-RS.
       M-25.
           PERFORM  S-20  THRU  S-25.
       M-30.
      *           READ  WK0256-F  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WK0256-F_PNAME1 BY REFERENCE WK0256-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN NOT = 4
               IF  WK-KIS     = W-KIS
                   GO  TO  M-25
               END-IF
           END-IF
           IF  JS-SIGN = 4
               IF  WK-KISR    = W-KISR
                   GO  TO  M-25
               END-IF
           END-IF
           PERFORM  S-30  THRU  S-35.
           GO TO M-20.
       M-90.
           PERFORM  S-30  THRU  S-35.
           PERFORM  S-40  THRU  S-45.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE WK0256-F_IDLST WK0256-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE   TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD    1       TO     W-PAGE.
           MOVE   W-PAGE  TO     H-PAGE.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD1   TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD2   TO     SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD3   TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE  SPACE   TO  W-P.
           IF  JS-SIGN = 4
               MOVE W-KISR TO P-KISR
           ELSE
               MOVE  W-KIS  TO  P-KIS
           END-IF
           MOVE  WK-HCD  TO  P-HCD.
           MOVE  WK-TNK  TO  P-TNK.
           MOVE  WK-SRY  TO  P-SRY.
           MOVE  WK-KIN  TO  P-KIN.
           MOVE  WK-TGM  TO  P-TGM.
           MOVE  WK-TKN  TO  P-TKN.
           MOVE  WK-TSZ  TO  P-TSZ.
           COMPUTE  W-KEI1 = WK-TGM + WK-TKN + WK-TSZ.
           MOVE  W-KEI1  TO  P-KEI1.
           IF  WK-MGS NOT = ZERO
               MOVE  WK-MGS  TO  P-MGS
           END-IF
           IF  WK-MKM NOT = ZERO
               MOVE  WK-MKM  TO  P-MKM
           END-IF
           IF  WK-MTS NOT = ZERO
               MOVE  WK-MTS  TO  P-MTS
           END-IF
           IF  WK-MKR NOT = ZERO
               MOVE  WK-MKR  TO  P-MKR
           END-IF
           IF  WK-MKH NOT = ZERO
               MOVE  WK-MKH  TO  P-MKH
           END-IF
           COMPUTE  W-KEI2  ROUNDED =
                       WK-MGS + WK-MKM + WK-MTS + WK-MKR + WK-MKH.
           IF  W-KEI2 NOT = ZERO
               MOVE  W-KEI2  TO  P-KEI2
           END-IF
           MOVE  WK-KKH  TO  P-KKH.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 27
               PERFORM  S-05  THRU  S-15
           END-IF
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD   WK-SRY  TO  W-SSRY  W-GSRY.
           ADD   WK-KIN  TO  W-SKIN  W-GKIN.
           ADD   WK-TGM  TO  W-STGM  W-GTGM.
           ADD   WK-TKN  TO  W-STKN  W-GTKN.
           ADD   WK-TSZ  TO  W-STSZ  W-GTSZ.
           ADD   W-KEI1  TO  W-SKEI1 W-GKEI1.
           ADD   WK-MGS  TO  W-SMGS  W-GMGS.
           ADD   WK-MKM  TO  W-SMKM  W-GMKM.
           ADD   WK-MTS  TO  W-SMTS  W-GMTS.
           ADD   WK-MKR  TO  W-SMKR  W-GMKR.
           ADD   WK-MKH  TO  W-SMKH  W-GMKH.
           ADD   W-KEI2  TO  W-SKEI2 W-GKEI2.
           ADD   WK-KKH  TO  W-SKKH  W-GKKH.
       S-25.
           EXIT.
       S-30.
           MOVE  SPACE       TO  W-P.
           MOVE  "  小  計"  TO  P-KOU.
           MOVE  W-SSRY      TO  P-SRY.
           MOVE  W-SKIN      TO  P-KIN.
           MOVE  W-STGM      TO  P-TGM.
           MOVE  W-STKN      TO  P-TKN.
           MOVE  W-STSZ      TO  P-TSZ.
           MOVE  W-SKEI1     TO  P-KEI1.
           IF  W-SMGS NOT = ZERO
               MOVE  W-SMGS      TO  P-MGS
           END-IF
           IF  W-SMKM NOT = ZERO
               MOVE  W-SMKM      TO  P-MKM
           END-IF
           IF  W-SMTS NOT = ZERO
               MOVE  W-SMTS      TO  P-MTS
           END-IF
           IF  W-SMKR NOT = ZERO
               MOVE  W-SMKR      TO  P-MKR
           END-IF
           IF  W-SMKH NOT = ZERO
               MOVE  W-SMKH      TO  P-MKH
           END-IF
           IF  W-SKEI2 NOT = ZERO
               MOVE  W-SKEI2     TO  P-KEI2
           END-IF
           MOVE  W-SKKH      TO  P-KKH.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 27
               PERFORM  S-05  THRU  S-15
           END-IF
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
           EXIT.
       S-40.
           MOVE  SPACE       TO  W-P.
           MOVE  "合  計  "  TO  P-KOU.
           MOVE  W-GSRY      TO  P-SRY.
           MOVE  W-GKIN      TO  P-KIN.
           MOVE  W-GTGM      TO  P-TGM.
           MOVE  W-GTKN      TO  P-TKN.
           MOVE  W-GTSZ      TO  P-TSZ.
           MOVE  W-GKEI1     TO  P-KEI1.
           IF  W-GMGS NOT = ZERO
               MOVE  W-GMGS      TO  P-MGS
           END-IF
           IF  W-GMKM NOT = ZERO
               MOVE  W-GMKM      TO  P-MKM
           END-IF
           IF  W-GMTS NOT = ZERO
               MOVE  W-GMTS      TO  P-MTS
           END-IF
           IF  W-GMKR NOT = ZERO
               MOVE  W-GMKR      TO  P-MKR
           END-IF
           IF  W-GMKH NOT = ZERO
               MOVE  W-GMKH      TO  P-MKH
           END-IF
           IF  W-GKEI2 NOT = ZERO
               MOVE  W-GKEI2     TO  P-KEI2
           END-IF
           MOVE  W-GKKH      TO  P-KKH.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 27
               PERFORM  S-05  THRU  S-15
           END-IF
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           EXIT.
