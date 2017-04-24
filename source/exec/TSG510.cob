       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG510.
      **************************************
      *****     手　形　日　計　表     *****
      *****      ( FDL : FTG510 )      *****
      **************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(022) VALUE SPACE.
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(001) VALUE  "年".
           02  H-GET          PIC  N(002).
           02  F              PIC  N(002) VALUE  "月分".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(006) VALUE  "手形　日計表".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "日付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "受手受取".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "割　引".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "割手決済".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "取立入金".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "取消他".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "合　計".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "支手支払".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "支手決済".
       01  HEAD5.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(012) VALUE
                 "（　割引手形買戻し分　）".
           02  F              PIC  X(065) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "日付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "　№".
           02  F              PIC  X(008) VALUE "   ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE  "取　引　先　名　".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "金　　額".
           02  F              PIC  X(014) VALUE SPACE.
       01  HEAD7.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(015) VALUE
                 "［　支払手形　期日別合計表　］".
           02  F              PIC  X(070) VALUE SPACE.
       01  HEAD8.
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "満期日".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "金　額".
           02  F              PIC  X(065) VALUE SPACE.
       01  W-P1.
           02  F              PIC  X(001).
           02  P-TM           PIC  N(002).
           02  P-DATE  REDEFINES P-TM.
             03  P-X1         PIC  X(001).
             03  P-PEY        PIC Z9.
             03  P-X2         PIC  X(001).
           02  P-UU           PIC  Z(010).
           02  P-YB           PIC  Z(010).
           02  P-YK           PIC  Z(010).
           02  P-TN           PIC  Z(010).
           02  P-FT           PIC  Z(010).
           02  P-ST           PIC  Z(010).
           02  P-SS           PIC  Z(010).
           02  P-SK           PIC  Z(010).
       01  W-P2.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(003).
           02  P-PEYD         PIC Z9.
           02  F              PIC  X(002).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-KIN          PIC ZZ,ZZZ,ZZZ,ZZZ.
       01  W-P3.
           02  P-NEN          PIC Z9.
           02  P-NM           PIC  N(001).
           02  P-GET          PIC Z9.
           02  P-GM           PIC  N(004).
           02  F              PIC  X(003).
           02  P-MKD          PIC 99/99/99.
           02  P-TMDD  REDEFINES  P-MKD.
             03  P-TMD        PIC  N(003).
             03  F            PIC  X(002).
           02  P-MKIN         PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  W-D.
           02  W-UU    OCCURS  31  PIC  9(010).
           02  W-YB    OCCURS  31  PIC  9(010).
           02  W-YK    OCCURS  31  PIC  9(010).
           02  W-TN    OCCURS  31  PIC  9(010).
           02  W-FT    OCCURS  31  PIC  9(010).
           02  W-ST    OCCURS  31  PIC  9(010).
           02  W-SS    OCCURS  31  PIC  9(010).
           02  W-SK    OCCURS  31  PIC  9(010).
       01  W-TD.
           02  WT-UU          PIC  9(010).
           02  WT-YB          PIC  9(010).
           02  WT-YK          PIC  9(010).
           02  WT-TN          PIC  9(010).
           02  WT-FT          PIC  9(010).
           02  WT-ST          PIC  9(010).
           02  WT-SS          PIC  9(010).
           02  WT-SK          PIC  9(010).
       01  W-YKMD.
           02  W-YKM   OCCURS  20.
             03  W-PEYD       PIC  9(002).
             03  W-KEY        PIC  9(004).
             03  W-TCD        PIC  9(004).
             03  W-KIN        PIC  9(010).
       01  W-DATA.
           02  W-NGP.
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
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  W-UNG.
             03  W-UNEN       PIC  9(004).
             03  W-UGET       PIC  9(002).
           02  W-ING.
             03  W-INEN       PIC  9(004).
             03  W-IGET       PIC  9(002).
           02  W-FNG.
             03  W-FNEN       PIC  9(004).
             03  W-FGET       PIC  9(002).
           02  W-MNG.
             03  W-MNEN       PIC  9(004).
             03  W-MGET       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  Z(002).
             03  W-GETD       PIC  Z(002).
           02  W-DC           PIC  9(002).
           02  W-TKIN         PIC  9(010).
           02  W-SC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-SMD.
             03  W-SM    OCCURS  20.
               04  W-MKD      PIC  9(006).
               04  W-MKIN     PIC  9(011).
               04  W-MC       PIC  9(001).
           02  W-MKDD         PIC  9(006).
           02  W-CD           PIC  9(002).
           02  W-AKIN         PIC  9(011).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LITM.
           COPY LIUKET.
           COPY LISHIT.
           COPY LSPF.
      *FD  TYB-F
       01  TYB-F_TSG510.
           02  TYB-F_PNAME1   PIC  X(004) VALUE "TYBF".
           02  F              PIC  X(001).
           02  TYB-F_LNAME    PIC  X(012) VALUE "TYB-F_TSG510".
           02  F              PIC  X(001).
           02  TYB-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT     PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TYB-F_RES      USAGE  POINTER.
       01  YRIT-F.
           02  Y-BCD          PIC  9(004).
           02  Y-YKD          PIC  9(006).
           02  Y-YKDD  REDEFINES Y-YKD.
             03  Y-YNG        PIC  9(004).
             03  Y-YP         PIC  9(002).
           02  Y-MKD          PIC  9(006).
           02  Y-MKDD  REDEFINES Y-MKD.
             03  Y-MNG        PIC  9(004).
             03  Y-MP         PIC  9(002).
           02  Y-TCD          PIC  9(004).
           02  Y-TSC          PIC  9(002).
           02  Y-KEY          PIC  9(004).
           02  Y-KIN          PIC  9(010).
           02  Y-FC           PIC  9(001).
           02  F              PIC  X(006).
           02  Y-SNI          PIC  9(004).
           02  Y-SNM          PIC  9(004).
       77  F                  PIC  X(001).
      *FD  TDT-M
       01  TDT-M_TSG510.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSG510".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-TKC       PIC  9(002).
             03  TD-TNO       PIC  9(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DATE        PIC  9(006).
           02  TD-NGP   REDEFINES TD-DATE.
             03  TD-NG        PIC  9(004).
             03  F            PIC  9(002).
           02  TD-MND         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BKC         PIC  9(004).
           02  TD-FRN         PIC  N(024).
           02  TD-SAD.
             03  TD-S     OCCURS   7  PIC S9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PC          PIC  9(001).
           02  TD-RSC         PIC  9(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　手形　日計表　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(021) VALUE "[   H   年  月 分   ]".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  Z(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME2   PIC  X(035) VALUE
                  "***  ﾜﾘﾃ ｶｲﾓﾄﾞｼ ﾃﾞｰﾀ ｶﾞ ｵｵｽｷﾞﾙ  ***".
             03  E-ME3   PIC  X(028) VALUE
                  "***  ｼﾃ ﾏﾝｷﾋﾞ ｶﾞ ｵｵｽｷﾞﾙ  ***".
             03  E-ME4   PIC  X(028) VALUE
                  "***  未変換データ　有り  ***".
             03  E-KEY   PIC  X(008).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG510" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "263" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" " " "12" "0" "25" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID" "X" "12" "17" "21" " " "08C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0208C-MID" "Z" "12" "23" "2" "0108C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0208C-MID" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0308C-MID" "Z" "12" "27" "2" "0208C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0308C-MID" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "127" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "127" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "35" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "28" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "28" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "40" "8" "E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE CL-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-04R TO H-DATE.
           MOVE ZERO TO W-NGP.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-WGET.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-45 THRU S-60.
           MOVE W-WNEN TO W-NEND.
           MOVE W-WGET TO W-GETD.
           MOVE W-NEND TO H-NEN.
           MOVE W-GETD TO H-GET.
           DIVIDE 4 INTO W-NEN GIVING CNT REMAINDER CHK.
           IF  W-GET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 32 TO CNT
           END-IF
           IF  W-GET = 4 OR 6 OR 9 OR 11
               MOVE 31 TO CNT
           END-IF
           IF  W-GET = 2
               IF  CHK = ZERO
                   MOVE 30 TO CNT
               ELSE
                   MOVE 29 TO CNT
               END-IF
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           MOVE ZERO TO W-D W-TD W-SMD W-EC W-AKIN.
       M-15.
      *           READ UKET-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" UKET-M_PNAME1 BY REFERENCE UKET-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           MOVE UT-SNU TO W-UNEN.
           MOVE UT-UTG TO W-UGET.
           MOVE UT-SNI TO W-INEN.
           MOVE UT-IDG TO W-IGET.
           IF  W-UNG = W-NG
               ADD UT-KIN TO W-UU(UT-UTP)
           END-IF
           IF  W-ING NOT = W-NG
               GO TO M-15
           END-IF
           IF  UT-SKC = 19
               ADD UT-KIN TO W-TN(UT-IDP) W-ST(UT-IDP)
           END-IF
           IF  UT-SKC NOT = 60
               GO TO M-15
           END-IF
           IF  UT-SBC = ZERO
               ADD UT-KIN TO W-FT(UT-IDP) W-ST(UT-IDP)
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
       M-25.
      *           READ SHIT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SHIT-M_PNAME1 BY REFERENCE SHIT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  ST-SKC = 90
               GO TO M-25
           END-IF
           MOVE ST-SNF TO W-FNEN.
           MOVE ST-FDG TO W-FGET.
           MOVE ST-SNM TO W-MNEN.
           MOVE ST-MKG TO W-MGET.
           IF  W-FNG = W-NG
               ADD ST-KIN TO W-SS(ST-FDP) W-AKIN
               PERFORM S-05 THRU S-15
           END-IF
           IF  W-MNG = W-NG
               ADD ST-KIN TO W-SK(ST-MKP)
           END-IF
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           MOVE ZERO TO W-YKMD W-DC W-TKIN.
       M-35.
      *           READ TYB-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TYB-F_PNAME1 BY REFERENCE YRIT-F " " RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  Y-MKD NOT = 999999
               ADD Y-KIN TO W-YB(Y-YP)
               GO TO M-35
           END-IF
           IF  Y-FC = 9
               GO TO M-35
           END-IF
           IF  Y-FC NOT = 1
               ADD Y-KIN TO W-YK(Y-YP) W-ST(Y-YP)
               GO TO M-35
           END-IF
           ADD 1 TO W-DC.
           IF  W-DC > 20
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               MOVE Y-YP TO W-PEYD(W-DC)
               MOVE Y-KEY TO W-KEY(W-DC)
               MOVE Y-TCD TO W-TCD(W-DC)
               MOVE Y-KIN TO W-KIN(W-DC)
               ADD Y-KIN TO W-TKIN
           END-IF
           GO TO M-35.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-PEY.
       M-45.
           ADD 1 TO W-PEY.
           IF  W-PEY = CNT
               GO TO M-50
           END-IF
           MOVE W-NGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 0 TO CL-SJ
           END-IF
           MOVE SPACE TO W-P1.
           MOVE W-PEY TO P-PEY.
           IF  CL-SJ = 1
               MOVE "(" TO P-X1
               MOVE ")" TO P-X2
           END-IF
           MOVE W-UU(W-PEY) TO P-UU.
           MOVE W-YB(W-PEY) TO P-YB.
           MOVE W-YK(W-PEY) TO P-YK.
           MOVE W-TN(W-PEY) TO P-TN.
           MOVE W-FT(W-PEY) TO P-FT.
           MOVE W-ST(W-PEY) TO P-ST.
           MOVE W-SS(W-PEY) TO P-SS.
           MOVE W-SK(W-PEY) TO P-SK.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-UU(W-PEY) TO WT-UU.
           ADD W-YB(W-PEY) TO WT-YB.
           ADD W-YK(W-PEY) TO WT-YK.
           ADD W-TN(W-PEY) TO WT-TN.
           ADD W-FT(W-PEY) TO WT-FT.
           ADD W-ST(W-PEY) TO WT-ST.
           ADD W-SS(W-PEY) TO WT-SS.
           ADD W-SK(W-PEY) TO WT-SK.
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           COMPUTE CHK = 34 - CNT.
           MOVE SPACE TO W-P1.
           MOVE  "合計" TO P-TM.
           MOVE WT-UU TO P-UU.
           MOVE WT-YB TO P-YB.
           MOVE WT-YK TO P-YK.
           MOVE WT-TN TO P-TN.
           MOVE WT-FT TO P-FT.
           MOVE WT-ST TO P-ST.
           MOVE WT-SS TO P-SS.
           MOVE WT-SK TO P-SK.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING CHK RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-DC = ZERO
               GO TO M-65
           END-IF
      *
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-DC.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-55.
           ADD 1 TO W-DC.
           IF  W-DC > 20
               GO TO M-60
           END-IF
           IF  W-PEYD(W-DC) = ZERO
               GO TO M-60
           END-IF
           MOVE W-TCD(W-DC) TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE  "　＊＊＊　ＴＭ　なし　　＊＊＊　" TO T-NAME
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE W-PEYD(W-DC) TO P-PEYD.
           MOVE W-KEY(W-DC) TO P-KEY.
           MOVE W-TCD(W-DC) TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE W-KIN(W-DC) TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-55.
       M-60.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  W-DC = 2
               GO TO M-65
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE  "　　　　　　　　　　　　　　　（　計　）" TO P-NAME.
           MOVE W-TKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-65.
           IF  W-EC = 5
               GO TO M-95
           END-IF
           IF  W-MKD(1) = ZERO
               GO TO M-95
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD7 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 0 TO W-SC.
           PERFORM S-20 THRU S-40.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-EC = 5
               GO TO S-15
           END-IF
           MOVE ZERO TO W-C.
       S-10.
           ADD 1 TO W-C.
           IF  W-C = 21
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO S-15
           END-IF
           IF  W-MKD(W-C) = ZERO
               MOVE 1 TO W-MC(W-C)
               MOVE ST-MKD TO W-MKD(W-C)
               ADD ST-KIN TO W-MKIN(W-C)
               GO TO S-15
           END-IF
           IF  W-MKD(W-C) = ST-MKD
               ADD ST-KIN TO W-MKIN(W-C)
               GO TO S-15
           END-IF
           GO TO S-10.
       S-15.
           EXIT.
       S-20.
           MOVE 999999 TO W-MKDD.
           MOVE ZERO TO W-C.
       S-25.
           ADD 1 TO W-C.
           IF  W-C = 21
               GO TO S-30
           END-IF
           IF  W-MC(W-C) = 0
               GO TO S-25
           END-IF
           IF  W-MKD(W-C) < W-MKDD
               MOVE W-MKD(W-C) TO W-MKDD
               MOVE W-C TO W-CD
           END-IF
           GO TO S-25.
       S-30.
           IF  W-MKDD = 999999
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P3.
           IF  W-SC = 0
               MOVE 5 TO W-SC
               MOVE W-NEND TO P-NEN
               MOVE  "年" TO P-NM
               MOVE W-GETD TO P-GET
               MOVE  "月振出し" TO P-GM
           END-IF
           MOVE W-MKD(W-CD) TO P-MKD.
           MOVE W-MKIN(W-CD) TO P-MKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD7 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD8 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-NEND TO P-NEN
               MOVE  "年" TO P-NM
               MOVE W-GETD TO P-GET
               MOVE  "月振出し" TO P-GM
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 0 TO W-MC(W-CD).
           GO TO S-20.
       S-35.
           MOVE SPACE TO W-P3.
           MOVE  "（計）" TO P-TMD.
           MOVE W-AKIN TO P-MKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD7 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD8 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-NEND TO P-NEN
               MOVE  "年" TO P-NM
               MOVE W-GETD TO P-GET
               MOVE  "月振出し" TO P-GM
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.
       S-45.
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 " " BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       S-50.
      *           READ TDT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF
           IF  TD-HCT NOT = 0
               GO TO S-50
           END-IF
           IF  TD-NG NOT = W-WNG
               GO TO S-50
           END-IF
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       S-55.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
       S-60.
           EXIT.
