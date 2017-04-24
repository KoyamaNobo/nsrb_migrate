       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD710.
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 1974-05-15.
      *********************************************************
      *    PROGRAM         :  履物日計表　　　　　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                "＊＊＊　　履物　日計表　　＊＊＊".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(014) VALUE "I-----------  ".
           02  F              PIC  N(004) VALUE "売　　上".
           02  F              PIC  X(013) VALUE "  ----------I".
           02  F              PIC  X(015) VALUE " I-----------  ".
           02  F              PIC  N(004) VALUE "入　　金".
           02  F              PIC  X(025) VALUE
                "  ----------I I--------  ".
           02  F              PIC  N(007) VALUE "売　掛　残　高".
           02  F              PIC  X(010) VALUE "  -------I".
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "本　体".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "本　体".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(006) VALUE "消費税　　　".
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "本　体".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(006) VALUE "消費税　　　".
           02  F              PIC  N(003) VALUE "合　計".
       01  HEAD4.
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(018) VALUE "I---------------  ".
           02  F              PIC  N(006) VALUE "分類別　売上".
           02  F              PIC  X(018) VALUE "  ---------------I".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(018) VALUE "I---------------  ".
           02  F              PIC  N(006) VALUE "分類別　入庫".
           02  F              PIC  X(018) VALUE "  ---------------I".
           02  F              PIC  X(005) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(005) VALUE "カジュアル".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "ワーク".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "教　育".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(005) VALUE "カジュアル".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "ワーク".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "教　育".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(005) VALUE SPACE.
       01  W-P1.
           02  P-DATE1        PIC 99/99/99.
           02  P-M1    REDEFINES P-DATE1 PIC  X(008).
           02  P-UKH          PIC -----,---,--9.
           02  P-UKZ          PIC ---,---,--9.
           02  P-UKT          PIC ----,---,--9.
           02  P-NKH          PIC -----,---,--9.
           02  P-NKZ          PIC ---,---,--9.
           02  P-NKT          PIC ----,---,--9.
           02  P-UZH          PIC -----,---,--9.
           02  P-UZZ          PIC ---,---,--9.
           02  P-UZT          PIC ----,---,--9.
       01  W-P2.
           02  P-DATE2        PIC 99/99/99.
           02  P-M2    REDEFINES P-DATE2 PIC  X(008).
           02  P-U10          PIC -----,---,--9.
           02  P-U20          PIC ----,---,--9.
           02  P-U30          PIC ----,---,--9.
           02  P-UT           PIC ----,---,--9.
           02  F              PIC  X(006).
           02  P-S10          PIC ----,---,--9.
           02  P-S20          PIC ----,---,--9.
           02  P-S30          PIC ----,---,--9.
           02  P-ST           PIC ----,---,--9.
           02  F              PIC  X(005).
       01  W-DATA.
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-DATE  REDEFINES W-NGP   PIC  9(006).
           02  W-NGL.
             03  F            PIC  9(002).
             03  W-NGD        PIC  9(004).
           02  W-UZ.
             03  W-UZH        PIC S9(009).
             03  W-UZZ        PIC S9(008).
             03  W-UZT        PIC S9(009).
           02  WC-UZ.
             03  WC-UZH       PIC S9(009).
             03  WC-UZZ       PIC S9(008).
             03  WC-UZT       PIC S9(009).
           02  W-KIN          PIC S9(008).
           02  W-DMM          PIC  9(001).
       01  W-D.
           02  W-UKH   OCCURS  31  PIC S9(009).
           02  W-UKZ   OCCURS  31  PIC S9(008).
           02  W-UKT   OCCURS  31  PIC S9(009).
           02  W-NKH   OCCURS  31  PIC S9(009).
           02  W-NKZ   OCCURS  31  PIC S9(008).
           02  W-NKT   OCCURS  31  PIC S9(009).
           02  W-U10   OCCURS  31  PIC S9(009).
           02  W-U20   OCCURS  31  PIC S9(009).
           02  W-U30   OCCURS  31  PIC S9(009).
           02  W-UT    OCCURS  31  PIC S9(009).
           02  W-S10   OCCURS  31  PIC S9(009).
           02  W-S20   OCCURS  31  PIC S9(009).
           02  W-S30   OCCURS  31  PIC S9(009).
           02  W-ST    OCCURS  31  PIC S9(009).
       01  WT-D.
           02  WT-UKH         PIC S9(009).
           02  WT-UKZ         PIC S9(008).
           02  WT-UKT         PIC S9(009).
           02  WT-NKH         PIC S9(009).
           02  WT-NKZ         PIC S9(008).
           02  WT-NKT         PIC S9(009).
           02  WT-U10         PIC S9(009).
           02  WT-U20         PIC S9(009).
           02  WT-U30         PIC S9(009).
           02  WT-UT          PIC S9(009).
           02  WT-S10         PIC S9(009).
           02  WT-S20         PIC S9(009).
           02  WT-S30         PIC S9(009).
           02  WT-ST          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITTM.
           COPY LSNYUR.
           COPY LUTRAN.
           COPY LSPF.
      *FD  STRAN
       01  STRAN_HMD710.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMD710".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R            PIC  X(128).
       77  F                  PIC  X(001).
      *FD  NYU-F
       01  NYU-F_HMD710.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HMD710".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  F              PIC  X(037).
           02  NYU-BC         PIC  9(001).
           02  F              PIC  X(005).
           02  NYU-KEY        PIC  X(007).
           02  F              PIC  X(031).
           02  NYU-RSC        PIC  9(001).
           02  NYU-DPC        PIC  9(001).
           02  F              PIC  X(019).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMD710.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMD710".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  9(006).
           02  SNTR-PEY       PIC  9(002).
           02  SNTR-D1.
             03  F            PIC  X(004).
             03  SNTR-HCD     PIC  9(006).
             03  F            PIC  X(041).
             03  SNTR-KIN     PIC S9(008).
             03  F            PIC  X(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(012).
             03  SNTR-BC3     PIC  9(002).
             03  F            PIC  X(037).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  F            PIC  X(088).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  UTR-F
       01  UTR-F_HMD710.
           02  UTR-F_PNAME1   PIC  X(004) VALUE "UTRF".
           02  F              PIC  X(001).
           02  UTR-F_LNAME    PIC  X(012) VALUE "UTR-F_HMD710".
           02  F              PIC  X(001).
           02  UTR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  UTR-F_SORT     PIC  X(100) VALUE SPACE.
           02  UTR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  UTR-F_RES      USAGE  POINTER.
       01  UTR-R.
           02  UTR-NO         PIC  9(007).
           02  UTR-DATE.
             03  F            PIC  9(002).
             03  UTR-NG       PIC  9(004).
             03  UTR-PEY      PIC  9(002).
           02  UTR-HCD        PIC  9(006).
           02  UTR-SIZ        PIC  9(001).
           02  UTR-SUD.
             03  UTR-SU       PIC S9(004)  OCCURS  10.
           02  UTR-SUT        PIC S9(005).
           02  UTR-BKIN       PIC S9(008).
           02  UTR-FKIN       PIC S9(008).
           02  UTR-NRC        PIC  9(001).
           02  UTR-SSC        PIC  9(001).
           02  UTR-HPC        PIC  9(001).
           02  UTR-SKC        PIC  9(001).
           02  UTR-BC.
             03  UTR-BC1      PIC  9(002).
             03  UTR-BC2.
               04  UTR-BC21   PIC  9(001).
               04  UTR-BC22   PIC  9(001).
             03  UTR-BC3      PIC  9(002).
           02  F              PIC  X(035).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　履物　日計表　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-NN.
             03  FILLER  PIC  N(013) VALUE
                  "日計更新をやってない".
             03  FILLER  PIC  N(021) VALUE
                  "実行＝ＲＥＳＥＴ　終了＝プログラム放棄".
           02  D-UZ.
             03  FILLER  PIC  N(013) VALUE
                  "［　マスターの売掛残高　］".
             03  FILLER  PIC  N(021) VALUE
                  "　　　　本　体　　　　消費税　　　　合　計".
             03  FILLER.
               04  FILLER  PIC --,---,---,--9.
               04  FILLER  PIC --,---,---,--9.
               04  FILLER  PIC --,---,---,--9.
       01  A-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  ｳﾘｶｹｻﾞﾝﾀﾞｶ ｴﾗｰ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  ﾆｭｳｷﾝﾋｮｳ ﾐﾊｯｺｳ ｱﾘ  ***".
             03  E-ME3   PIC  X(019) VALUE
                  "***  ﾌﾞﾝﾙｲ ｴﾗｰ  ***".
             03  E-SNTR  PIC  9(006).
             03  E-UTR   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "252" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "36" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "36" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "36" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "36" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "36" "06C-MID" " "  RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "178" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NN" " " "0" "0" "68" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NN" "N" "11" "10" "26" " " "D-NN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NN" "N" "13" "10" "42" "01D-NN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UZ" " " "0" "0" "110" "D-NN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-UZ" "RN" "15" "15" "26" " " "D-UZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-UZ" "N" "17" "7" "42" "01D-UZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-UZ" " " "18" "0" "42" "02D-UZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-UZ" "--,---,---,--9" "18" "7" "14" " " "03D-UZ"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-UZ" BY REFERENCE WC-UZH "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-UZ" "--,---,---,--9" "18" "21" "14" "0103D-UZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-UZ" BY REFERENCE WC-UZZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-UZ" "--,---,---,--9" "18" "35" "14" "0203D-UZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-UZ" BY REFERENCE WC-UZT "9" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "A-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "50" "1" " " "A-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "142" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "142" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-SNTR" "9" "24" "36" "6" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-SNTR" BY REFERENCE SNTR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-UTR" "9" "24" "36" "6" "E-SNTR" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-UTR" BY REFERENCE UTR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-UTR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           MOVE ZERO TO W-DATE.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NG.
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-35 THRU S-75.
           CALL "DB_F_Open" USING
            "INPUT SEQUENTIAL" TT-M_PNAME1 "SHARED" 
            BY REFERENCE TT-M_IDLST "1" "TT-KEY" BY REFERENCE TT-KEY.
           MOVE ZERO TO W-UZ WC-UZ.
       M-10.
      *           READ TT-M WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  TT-BC NOT = 0
               GO TO M-10
           END-IF
           ADD TT-TZZ TO W-UZH W-UZT.
           ADD TT-TZZZ TO W-UZZ W-UZT.
           ADD TT-TUZ TO WC-UZH WC-UZT.
           ADD TT-TUZZ TO WC-UZZ WC-UZT.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           MOVE ZERO TO W-D WT-D.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
       M-20.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-25
           END-IF
           IF (SNTR-KIN = ZERO) OR (SNTR-DC = 4 OR 8)
               GO TO M-20
           END-IF
           IF  SNTR-BC3 NOT = 10 AND 20 AND 30
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SNTR" E-SNTR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = -1 * SNTR-KIN
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  SNTR-BC3 = 10
               ADD W-KIN TO W-U10(SNTR-PEY)
           END-IF
           IF  SNTR-BC3 = 20
               ADD W-KIN TO W-U20(SNTR-PEY)
           END-IF
           IF  SNTR-BC3 = 30
               ADD W-KIN TO W-U30(SNTR-PEY)
           END-IF
           ADD W-KIN TO W-UKH(SNTR-PEY) W-UKT(SNTR-PEY) W-UT(SNTR-PEY).
           GO TO M-20.
       M-25.
           IF  SNTR-SHZ = ZERO
               GO TO M-20
           END-IF
           IF  SNTR-SNC = 0
               ADD SNTR-SHZ TO W-UKZ(SNTR-PEY) W-UKT(SNTR-PEY)
           ELSE
               SUBTRACT SNTR-SHZ FROM W-UKZ(SNTR-PEY) W-UKT(SNTR-PEY)
           END-IF
           GO TO M-20.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 "SHARED" BY REFERENCE
            NYUR-F_IDLST "0".
       M-35.
      *           READ NYUR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           MOVE NUR-NG TO W-NGL.
           IF  W-NG NOT = W-NGD
               GO TO M-35
           END-IF
           IF  NUR-KIN = ZERO
               GO TO M-35
           END-IF
           IF  NUR-BC NOT = ZERO
               GO TO M-35
           END-IF
           IF  NUR-NC2 > 7
               ADD NUR-KIN TO W-NKZ(NUR-PEY)
           ELSE
               ADD NUR-KIN TO W-NKH(NUR-PEY)
           END-IF
           ADD NUR-KIN TO W-NKT(NUR-PEY).
           GO TO M-35.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UTR-F_PNAME1 "SHARED" BY REFERENCE UTR-F_IDLST "0".
       M-45.
      *           READ UTR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTR-F_PNAME1 BY REFERENCE UTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  UTR-NRC = 2
               GO TO M-45
           END-IF
           IF  UTR-FKIN = ZERO
               GO TO M-45
           END-IF
           IF  UTR-BC3 NOT = 10 AND 20 AND 30
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-UTR" E-UTR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  UTR-BC3 = 10
               IF  UTR-NRC = 5
                   SUBTRACT UTR-FKIN FROM W-S10(UTR-PEY)
               ELSE
                   ADD UTR-FKIN TO W-S10(UTR-PEY)
               END-IF
           END-IF
           IF  UTR-BC3 = 20
               IF  UTR-NRC = 5
                   SUBTRACT UTR-FKIN FROM W-S20(UTR-PEY)
               ELSE
                   ADD UTR-FKIN TO W-S20(UTR-PEY)
               END-IF
           END-IF
           IF  UTR-BC3 = 30
               IF  UTR-NRC = 5
                   SUBTRACT UTR-FKIN FROM W-S30(UTR-PEY)
               ELSE
                   ADD UTR-FKIN TO W-S30(UTR-PEY)
               END-IF
           END-IF
           IF  UTR-NRC = 5
               SUBTRACT UTR-FKIN FROM W-ST(UTR-PEY)
           ELSE
              ADD UTR-FKIN TO W-ST(UTR-PEY)
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE UTR-F_IDLST UTR-F_PNAME1.
      *
           CALL "PR_Open" RETURNING RESP.
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
           MOVE SPACE TO W-P1.
           MOVE " *繰越* " TO P-M1.
           MOVE W-UZH TO P-UZH.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZT TO P-UZT.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO WT-D W-PEY.
           PERFORM S-05 THRU S-15.
       M-60.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WT-D W-PEY.
           PERFORM S-20 THRU S-30.
           CALL "PR_Close" RETURNING RESP.
       M-65.
           IF  W-UZH NOT = WC-UZH
               GO TO M-70
           END-IF
           IF  W-UZZ NOT = WC-UZZ
               GO TO M-70
           END-IF
           IF  W-UZT NOT = WC-UZT
               GO TO M-70
           END-IF
           GO TO M-95.
       M-70.
           CALL "SD_Output" USING "D-UZ" D-UZ "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD 1 TO W-PEY.
           IF  W-PEY = 32
               GO TO S-10
           END-IF
           IF  ZERO = W-UKH(W-PEY) AND W-UKZ(W-PEY) AND W-UKT(W-PEY)
                 AND W-NKH(W-PEY) AND W-NKZ(W-PEY) AND W-NKT(W-PEY)
               GO TO S-05
           END-IF
           COMPUTE W-UZH = W-UZH + W-UKH(W-PEY) - W-NKH(W-PEY).
           COMPUTE W-UZZ = W-UZZ + W-UKZ(W-PEY) - W-NKZ(W-PEY).
           COMPUTE W-UZT = W-UZT + W-UKT(W-PEY) - W-NKT(W-PEY).
           MOVE SPACE TO W-P1.
           MOVE W-DATE TO P-DATE1.
           MOVE W-UKH(W-PEY) TO P-UKH.
           MOVE W-UKZ(W-PEY) TO P-UKZ.
           MOVE W-UKT(W-PEY) TO P-UKT.
           MOVE W-NKH(W-PEY) TO P-NKH.
           MOVE W-NKZ(W-PEY) TO P-NKZ.
           MOVE W-NKT(W-PEY) TO P-NKT.
           MOVE W-UZH TO P-UZH.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZT TO P-UZT.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-UKH(W-PEY) TO WT-UKH.
           ADD W-UKZ(W-PEY) TO WT-UKZ.
           ADD W-UKT(W-PEY) TO WT-UKT.
           ADD W-NKH(W-PEY) TO WT-NKH.
           ADD W-NKZ(W-PEY) TO WT-NKZ.
           ADD W-NKT(W-PEY) TO WT-NKT.
           GO TO S-05.
       S-10.
           MOVE SPACE TO W-P1.
           MOVE "< 合計 >" TO P-M1.
           MOVE WT-UKH TO P-UKH.
           MOVE WT-UKZ TO P-UKZ.
           MOVE WT-UKT TO P-UKT.
           MOVE WT-NKH TO P-NKH.
           MOVE WT-NKZ TO P-NKZ.
           MOVE WT-NKT TO P-NKT.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           ADD 1 TO W-PEY.
           IF  W-PEY = 32
               GO TO S-25
           END-IF
           IF  ZERO = W-U10(W-PEY) AND W-U20(W-PEY) AND W-U30(W-PEY)
                                                   AND W-UT(W-PEY)
                 AND W-S10(W-PEY) AND W-S20(W-PEY) AND W-S30(W-PEY)
                                                   AND W-ST(W-PEY)
               GO TO S-20
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-DATE TO P-DATE2.
           MOVE W-U10(W-PEY) TO P-U10.
           MOVE W-U20(W-PEY) TO P-U20.
           MOVE W-U30(W-PEY) TO P-U30.
           MOVE W-UT(W-PEY) TO P-UT.
           MOVE W-S10(W-PEY) TO P-S10.
           MOVE W-S20(W-PEY) TO P-S20.
           MOVE W-S30(W-PEY) TO P-S30.
           MOVE W-ST(W-PEY) TO P-ST.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-U10(W-PEY) TO WT-U10.
           ADD W-U20(W-PEY) TO WT-U20.
           ADD W-U30(W-PEY) TO WT-U30.
           ADD W-UT(W-PEY) TO WT-UT.
           ADD W-S10(W-PEY) TO WT-S10.
           ADD W-S20(W-PEY) TO WT-S20.
           ADD W-S30(W-PEY) TO WT-S30.
           ADD W-ST(W-PEY) TO WT-ST.
           GO TO S-20.
       S-25.
           MOVE SPACE TO W-P2.
           MOVE "< 合計 >" TO P-M2.
           MOVE WT-U10 TO P-U10.
           MOVE WT-U20 TO P-U20.
           MOVE WT-U30 TO P-U30.
           MOVE WT-UT TO P-UT.
           MOVE WT-S10 TO P-S10.
           MOVE WT-S20 TO P-S20.
           MOVE WT-S30 TO P-S30.
           MOVE WT-ST TO P-ST.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-30.
           EXIT.
       S-35.
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
      *           READ STRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           GO TO S-70.
       S-40.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT SEQUENTIAL" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "NYU-KEY" BY REFERENCE NYU-KEY.
       S-50.
      *           READ NYU-F NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF
           IF  NYU-BC NOT = 0
               GO TO S-50
           END-IF
           IF  NYU-DPC = 0
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO S-50
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           GO TO S-70.
       S-55.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
      *           READ UTRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-60
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           GO TO S-70.
       S-60.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           GO TO S-75.
       S-70.
           CALL "SD_Output" USING "D-NN" D-NN "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       S-75.
           EXIT.
