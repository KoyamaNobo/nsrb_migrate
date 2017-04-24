       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD610.
       AUTHOR. S-NAKAO.
      *********************************************************
      *    PROGRAM         :  工品他日計表　　　　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *    DATA WRITTN     :  1974-05-15.                     *
      *        変更　　　  :  87/04/02                        *
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
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　工品・材料　日計表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(024) VALUE
                 "I-----------------------".
           02  F              PIC  N(006) VALUE "　売　　上　".
           02  F              PIC  X(031) VALUE
                "----------------------I I------".
           02  F              PIC  N(005) VALUE "　入　金　".
           02  F              PIC  X(012) VALUE "-----I I----".
           02  F              PIC  N(007) VALUE "　売　掛　残　".
           02  F              PIC  X(004) VALUE "---I".
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "　防　振　　　その他　　工品合計　　　材　料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  P-TM    REDEFINES P-DATE  PIC  N(004).
           02  P-KGU          PIC ----,---,--9.
           02  P-KSU          PIC ----,---,--9.
           02  P-KTO          PIC ----,---,--9.
           02  P-BND   REDEFINES P-KTO.
             03  P-BN         PIC  N(006).
           02  P-ZRU          PIC ----,---,--9.
           02  P-USZ          PIC ---,---,--9.
           02  P-NKU          PIC -----,---,--9.
           02  P-NKZ          PIC ---,---,--9.
           02  P-UZU          PIC -----,---,--9.
           02  P-UZZ          PIC ---,---,--9.
       01  W-D.
           02  W-KGU   OCCURS 31  PIC S9(009).
           02  W-KSU   OCCURS 31  PIC S9(009).
           02  W-KTO   OCCURS 31  PIC S9(009).
           02  W-ZRU   OCCURS 31  PIC S9(009).
           02  W-USZ   OCCURS 31  PIC S9(008).
           02  W-NKU   OCCURS 31  PIC S9(009).
           02  W-NKZ   OCCURS 31  PIC S9(008).
       01  WT-D.
           02  WT-KGU         PIC S9(009).
           02  WT-KSU         PIC S9(009).
           02  WT-KTO         PIC S9(009).
           02  WT-ZRU         PIC S9(009).
           02  WT-USZ         PIC S9(008).
           02  WT-NKU         PIC S9(009).
           02  WT-NKZ         PIC S9(008).
      *
           02  WT-21          PIC S9(009).
           02  WT-22          PIC S9(009).
           02  WT-23          PIC S9(009).
           02  WT-24          PIC S9(009).
           02  WT-25          PIC S9(009).
           02  WT-26          PIC S9(009).
           02  WT-27          PIC S9(009).
           02  WT-28          PIC S9(009).
           02  WT-29          PIC S9(009).
           02  WT-31          PIC S9(009).
           02  WT-32          PIC S9(009).
           02  WT-33          PIC S9(009).
           02  WT-41          PIC S9(009).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-YMD          PIC  9(008).
           02  W-YMDD  REDEFINES W-YMD.
             03  W-YM.
               04  W-YD       PIC  9(004).
               04  W-MD       PIC  9(002).
             03  W-DD         PIC  9(002).
           02  W-UZ.
             03  W-UZU        PIC S9(009).
             03  W-UZZ        PIC S9(008).
           02  WC-UZ.
             03  WC-UZU       PIC S9(009).
             03  WC-UZZ       PIC S9(008).
           02  W-DMM          PIC  9(001).
           02  W-KIN          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITTM.
           COPY LICAL.
           COPY BUMONF.
           COPY LSNYUR.
           COPY LSPF.
      *FD  URI-F
       01  URI-F_KHD610.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHD610".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R              PIC  X(128).
       77  F                  PIC  X(001).
      *FD  NYU-F
       01  NYU-F_KHD610.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_KHD610".
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
      *FD  URIR-F
       01  URIR-F_KHD610.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHD610".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  UR-DC          PIC  9(001).
           02  F              PIC  X(006).
           02  UR-PEY         PIC  9(002).
           02  F              PIC  X(004).
           02  UR-HCD         PIC  X(005).
           02  F              PIC  X(016).
           02  UR-KIN         PIC S9(008).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(012).
           02  UR-BKC         PIC  9(002).
           02  F              PIC  X(070).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　工品・材料　日計表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-NN.
             03  FILLER  PIC  N(013) VALUE
                  "日計更新をやってない".
             03  FILLER  PIC  N(021) VALUE
                  "実行＝ＲＥＳＥＴ　終了＝プログラム放棄".
           02  D-UZ.
             03  FILLER  PIC  N(013) VALUE
                  "［　マスターの売掛残高　］".
             03  FILLER  PIC  N(014) VALUE
                  "　　　　売　上　　　　消費税".
             03  FILLER.
               04  0103D-UZ  PIC --,---,---,--9 .
               04  0203D-UZ  PIC --,---,---,--9 .
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  E-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  ｳﾘｶｹｻﾞﾝﾀﾞｶ ｴﾗｰ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  ﾆｭｳｷﾝ ﾐﾊｯｺｳ ｱﾘ  ***".
             03  E-KEY   PIC  X(008).
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
            "C-MID" " " "0" "0" "266" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " "  RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "150" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NN" " " "0" "0" "68" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NN" "N" "11" "10" "26" " " "D-NN"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NN" "N" "13" "10" "42" "01D-NN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UZ" " " "0" "0" "82" "D-NN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-UZ" "RN" "15" "16" "26" " " "D-UZ"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-UZ" "N" "17" "15" "28" "01D-UZ" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-UZ" " " "18" "0" "28" "02D-UZ" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-UZ" "--,---,---,--9" "18" "15" "14" " " "03D-UZ"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-UZ" BY REFERENCE WC-UZU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-UZ" "--,---,---,--9" "18" "29" "14" "0103D-UZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-UZ" BY REFERENCE WC-UZZ "8" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "43" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "E-ERR" " " "0" "0" "134" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ERR" " " "24" "0" "134" " " "E-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "24" " " "01E-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "8" "E-ME3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE CL-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           MOVE D-NKNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-25 THRU S-50.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TT-M_PNAME1 
            "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
       M-10.
      *           READ TT-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  TT-BC = 0
               GO TO M-10
           END-IF
           ADD TT-TZZ TO W-UZU.
           ADD TT-TZZZ TO W-UZZ.
           ADD TT-TUZ TO WC-UZU.
           ADD TT-TUZZ TO WC-UZZ.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           MOVE ZERO TO W-D WT-D.
           MOVE W-DATE TO W-YMD.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       M-20.
      *           READ URIR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  UR-KIN = ZERO
               GO TO M-20
           END-IF
           MOVE UR-PEY TO W-DD.
           PERFORM S-15 THRU S-20.
           IF  UR-DC > 7
               GO TO M-30
           END-IF
           IF  UR-YC NOT = 00
               GO TO M-25
           END-IF
           IF  UR-DC = 5 OR 9
               GO TO M-25
           END-IF
           ADD UR-KIN TO W-ZRU(W-DD).
           IF  UR-BKC = 21
               ADD UR-KIN TO WT-21
           END-IF
           IF  UR-BKC = 22
               ADD UR-KIN TO WT-22
           END-IF
           IF  UR-BKC = 23
               ADD UR-KIN TO WT-23
           END-IF
           IF  UR-BKC = 24
               ADD UR-KIN TO WT-24
           END-IF
           IF  UR-BKC = 25
               ADD UR-KIN TO WT-25
           END-IF
           IF  UR-BKC = 26
               ADD UR-KIN TO WT-26
           END-IF
           IF  UR-BKC = 27
               ADD UR-KIN TO WT-27
           END-IF
           IF  UR-BKC = 28
               ADD UR-KIN TO WT-28
           END-IF
           IF  UR-BKC = 29
               ADD UR-KIN TO WT-29
           END-IF
           IF  UR-BKC = 31
               ADD UR-KIN TO WT-31
           END-IF
           IF  UR-BKC = 32
               ADD UR-KIN TO WT-32
           END-IF
           IF  UR-BKC = 33
               ADD UR-KIN TO WT-33
           END-IF
           IF  UR-BKC = 41
               ADD UR-KIN TO WT-41
           END-IF
           GO TO M-20.
       M-25.
           IF  UR-DC = 5
               ADD UR-KIN TO W-USZ(W-DD)
               GO TO M-20
           END-IF
           IF  UR-YC NOT = 10 AND 11
               ADD UR-KIN TO W-KSU(W-DD) W-KTO(W-DD)
           ELSE
               ADD UR-KIN TO W-KGU(W-DD) W-KTO(W-DD)
           END-IF
           GO TO M-20.
       M-30.
           IF  UR-YC NOT = 00
               GO TO M-35
           END-IF
           IF  UR-DC = 5 OR 9
               GO TO M-35
           END-IF
           SUBTRACT UR-KIN FROM W-ZRU(W-DD).
           IF  UR-BKC = 21
               SUBTRACT UR-KIN FROM WT-21
           END-IF
           IF  UR-BKC = 22
               SUBTRACT UR-KIN FROM WT-22
           END-IF
           IF  UR-BKC = 23
               SUBTRACT UR-KIN FROM WT-23
           END-IF
           IF  UR-BKC = 24
               SUBTRACT UR-KIN FROM WT-24
           END-IF
           IF  UR-BKC = 25
               SUBTRACT UR-KIN FROM WT-25
           END-IF
           IF  UR-BKC = 26
               SUBTRACT UR-KIN FROM WT-26
           END-IF
           IF  UR-BKC = 27
               SUBTRACT UR-KIN FROM WT-27
           END-IF
           IF  UR-BKC = 28
               SUBTRACT UR-KIN FROM WT-28
           END-IF
           IF  UR-BKC = 29
               SUBTRACT UR-KIN FROM WT-29
           END-IF
           IF  UR-BKC = 31
               SUBTRACT UR-KIN FROM WT-31
           END-IF
           IF  UR-BKC = 32
               SUBTRACT UR-KIN FROM WT-32
           END-IF
           IF  UR-BKC = 33
               SUBTRACT UR-KIN FROM WT-33
           END-IF
           IF  UR-BKC = 41
               SUBTRACT UR-KIN FROM WT-41
           END-IF
           GO TO M-20.
       M-35.
           IF  UR-DC = 9
               SUBTRACT UR-KIN FROM W-USZ(W-DD)
               GO TO M-20
           END-IF
           IF  UR-YC NOT = 10 AND 11
               SUBTRACT UR-KIN FROM W-KSU(W-DD) W-KTO(W-DD)
           ELSE
               SUBTRACT UR-KIN FROM W-KGU(W-DD) W-KTO(W-DD)
           END-IF
           GO TO M-20.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
       M-45.
      *           READ NYUR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  W-NG NOT = NUR-NG
               GO TO M-45
           END-IF
           IF  NUR-KIN = ZERO
               GO TO M-45
           END-IF
           IF  NUR-BC = ZERO
               GO TO M-45
           END-IF
           IF  NUR-NC2 > 7
               ADD NUR-KIN TO W-NKZ(NUR-PEY)
           ELSE
               ADD NUR-KIN TO W-NKU(NUR-PEY)
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
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
           MOVE SPACE TO SP-R W-P.
           MOVE W-UZU TO P-UZU.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-55.
           ADD 1 TO W-PEY.
           IF  W-PEY = 32
               GO TO M-60
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-55.
       M-60.
           MOVE SPACE TO SP-R W-P.
           MOVE "合　　計" TO P-TM.
           MOVE WT-KGU TO P-KGU.
           MOVE WT-KSU TO P-KSU.
           MOVE WT-KTO TO P-KTO.
           MOVE WT-ZRU TO P-ZRU.
           MOVE WT-USZ TO P-USZ.
           MOVE WT-NKU TO P-NKU.
           MOVE WT-NKZ TO P-NKZ.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  ZERO = WT-21 AND WT-22 AND WT-23 AND WT-24 AND WT-25 AND
                     WT-26 AND WT-27 AND WT-28 AND WT-29 AND
                     WT-31 AND WT-32 AND WT-33 AND WT-41
               GO TO M-65
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           IF  WT-21 NOT = ZERO
               MOVE WT-21 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 21 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-22 NOT = ZERO
               MOVE WT-22 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 22 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-23 NOT = ZERO
               MOVE WT-23 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 23 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-24 NOT = ZERO
               MOVE WT-24 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 24 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-25 NOT = ZERO
               MOVE WT-25 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 25 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-26 NOT = ZERO
               MOVE WT-26 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 26 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-27 NOT = ZERO
               MOVE WT-27 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 27 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-28 NOT = ZERO
               MOVE WT-28 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 28 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-29 NOT = ZERO
               MOVE WT-29 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 29 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-31 NOT = ZERO
               MOVE WT-31 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 31 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-32 NOT = ZERO
               MOVE WT-32 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 32 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-33 NOT = ZERO
               MOVE WT-33 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 33 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           IF  WT-41 NOT = ZERO
               MOVE WT-41 TO W-KIN
               MOVE ZERO TO BNM-KEY
               MOVE 41 TO BNM-BU
               PERFORM S-55 THRU S-60
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
       M-65.
           CALL "PR_Close" RETURNING RESP.
           IF  W-UZU NOT = WC-UZU
               GO TO M-90
           END-IF
           IF  W-UZZ NOT = WC-UZZ
               GO TO M-90
           END-IF
           GO TO M-95.
       M-90.
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
           IF  ZERO = W-KGU(W-PEY) AND W-KSU(W-PEY) AND W-ZRU(W-PEY)
                 AND W-USZ(W-PEY) AND W-NKU(W-PEY) AND W-NKZ(W-PEY)
               GO TO S-10
           END-IF
           COMPUTE W-UZU = W-UZU + W-KTO(W-PEY) + W-ZRU(W-PEY)
                                                - W-NKU(W-PEY).
           COMPUTE W-UZZ = W-UZZ + W-USZ(W-PEY) - W-NKZ(W-PEY).
      *
           MOVE SPACE TO SP-R W-P.
           MOVE W-NGPS TO P-DATE.
           MOVE W-KGU(W-PEY) TO P-KGU.
           MOVE W-KSU(W-PEY) TO P-KSU.
           MOVE W-KTO(W-PEY) TO P-KTO.
           MOVE W-ZRU(W-PEY) TO P-ZRU.
           MOVE W-USZ(W-PEY) TO P-USZ.
           MOVE W-NKU(W-PEY) TO P-NKU.
           MOVE W-NKZ(W-PEY) TO P-NKZ.
           MOVE W-UZU TO P-UZU.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-KGU(W-PEY) TO WT-KGU.
           ADD W-KSU(W-PEY) TO WT-KSU.
           ADD W-KTO(W-PEY) TO WT-KTO.
           ADD W-ZRU(W-PEY) TO WT-ZRU.
           ADD W-USZ(W-PEY) TO WT-USZ.
           ADD W-NKU(W-PEY) TO WT-NKU.
           ADD W-NKZ(W-PEY) TO WT-NKZ.
       S-10.
           EXIT.
       S-15.
           MOVE W-YMD TO CL-KEY.
      *           READ CALNM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO CL-YB
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  CL-YB = 1
               SUBTRACT 1 FROM W-DD
           END-IF
           IF  W-DD = ZERO
               MOVE 2 TO W-DD
           END-IF.
       S-20.
           EXIT.
       S-25.
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           GO TO S-45.
       S-30.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "NYU-KEY" BY REFERENCE NYU-KEY.
       S-35.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           IF  NYU-BC = 0
               GO TO S-35
           END-IF
           IF  NYU-DPC = 0
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO S-35
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           GO TO S-45.
       S-40.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           GO TO S-50.
       S-45.
           CALL "SD_Output" USING "D-NN" D-NN "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       S-50.
           EXIT.
       S-55.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
           END-IF
           MOVE SPACE TO W-P.
           MOVE BNMNMN TO P-BN.
           MOVE W-KIN TO P-ZRU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-60.
           EXIT.
