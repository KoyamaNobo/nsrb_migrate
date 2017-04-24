       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD540.
      *********************************************************
      *    PROGRAM         :  日計更新　　　クリア　　　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *        変更　　　  :  62/04/02                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-P.
           02  F            PIC  X(010) VALUE "<<   DATE ".
           02  P-NEN        PIC  9(002).
           02  F            PIC  X(001) VALUE "/".
           02  P-GET        PIC  9(002).
           02  F            PIC  X(001) VALUE "/".
           02  P-PEY        PIC  9(002).
           02  F            PIC  X(008) VALUE "   TIME ".
           02  P-GKN        PIC  9(002).
           02  F            PIC  X(001) VALUE ":".
           02  P-FUN        PIC  9(002).
           02  F            PIC  X(001) VALUE ":".
           02  P-BYO        PIC  9(002).
           02  F            PIC  X(025) VALUE
                "   工品日計更新終り　  >>".
       01  W-DATA.
           02  W-K          PIC S9(009).
           02  W-UZ         PIC S9(009).
           02  W-UZZ        PIC S9(007).
           02  W-DATE.
             03  W-NEN      PIC  9(002).
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
           02  W-TIME.
             03  W-GKN      PIC  9(002).
             03  W-FUN      PIC  9(002).
             03  W-BYO      PIC  9(002).
             02  F          PIC  X(002).
       01  ERR-STAT         PIC  X(002).
      *
           COPY LIKKBM.
           COPY LSPF.
      *FD  KNH-F
       01  KNH-F_KHD540.
           02  KNH-F_PNAME1 PIC  X(004) VALUE "KNHF".
           02  F            PIC  X(001).
           02  KNH-F_LNAME  PIC  X(012) VALUE "KNH-F_KHD540".
           02  F            PIC  X(001).
           02  KNH-F_KEY1   PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT   PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST  PIC  X(100) VALUE SPACE.
           02  KNH-F_RES    USAGE  POINTER.
       01  KNH-R            PIC  X(064).
       77  F                PIC  X(001).
      *FD  NYU-F
       01  NYU-F_KHD540.
           02  NYU-F_PNAME1 PIC  X(004) VALUE "NYUF".
           02  F            PIC  X(001).
           02  NYU-F_LNAME  PIC  X(012) VALUE "NYU-F_KHD540".
           02  F            PIC  X(001).
           02  NYU-F_KEY1   PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT   PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST  PIC  X(100) VALUE SPACE.
           02  NYU-F_RES    USAGE  POINTER.
       01  NYU-R.
           02  F            PIC  X(037).
           02  NYU-BC       PIC  9(001).
           02  F            PIC  X(005).
           02  NYU-KEY      PIC  X(007).
           02  F            PIC  X(031).
           02  NYU-RSC      PIC  9(001).
           02  F            PIC  X(020).
       77  F                PIC  X(001).
      *FD  URI-F
       01  URI-F_KHD540.
           02  URI-F_PNAME1 PIC  X(004) VALUE "URIF".
           02  F            PIC  X(001).
           02  URI-F_LNAME  PIC  X(012) VALUE "URI-F_KHD540".
           02  F            PIC  X(001).
           02  URI-F_KEY1   PIC  X(100) VALUE SPACE.
           02  URI-F_SORT   PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST  PIC  X(100) VALUE SPACE.
           02  URI-F_RES    USAGE  POINTER.
       01  URI-R            PIC  X(128).
       77  F                PIC  X(001).
      *
       77  ESTAT            PIC  X(002).
       77  RESU             PIC  9(001).
       77  RESP             PIC  9(001).
       77  RET              PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER    PIC  9(003).
       77  USER_ID          PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE  PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品　売掛更新・ファイルクリア　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME2   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "350" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "103" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "103" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "N" "24" "15" "44" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME90" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING "I-O SEQUENTIAL" NYU-F_PNAME1 
            " " BY REFERENCE NYU-F_IDLST "1"
            "NYU-KEY" BY REFERENCE NYU-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
       M-40.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  NYU-BC = 0
               GO TO M-40
           END-IF
           IF  NYU-RSC = 0
               GO TO M-40
           END-IF
      *           DELETE NYU-F.
      *///////////////
           CALL "DB_Delete" USING NYU-F_PNAME1 RETURNING RET.
           GO TO M-40.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           ACCEPT W-DATE FROM DATE.
           ACCEPT W-TIME FROM TIME.
           MOVE W-NEN TO P-NEN.
           MOVE W-GET TO P-GET.
           MOVE W-PEY TO P-PEY.
           MOVE W-GKN TO P-GKN.
           MOVE W-FUN TO P-FUN.
           MOVE W-BYO TO P-BYO.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       M-85.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE 0 TO KKB-SC13.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
