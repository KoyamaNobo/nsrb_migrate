       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         TSM010.
       AUTHOR.             KAMASAKA    1995-10-12.
      **********************************************
      ******    銀行マスター　メンテナンス    ******
      ******       ( SCREEN : SCTM01 )        ******
      **********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  ERR-STAT                PIC X(02).
       01  HEAD1.
           02  F               PIC X(05)    VALUE  X"1A24212474".
           02  F               PIC X(34)    VALUE  SPACE.
           02  F               PIC N(24)    VALUE
                  "＊＊＊　　銀行マスター　プルーフリスト　　＊＊＊".
           02  F               PIC X(23)    VALUE  SPACE.
           02  F               PIC X(05)    VALUE  "DATE ".
           02  H-DATE          PIC 99B99B99.
           02  F               PIC X(07)    VALUE  "     P.".
           02  H-PAGE          PIC Z9.
       01 HEAD2.
           02  F               PIC X(05)    VALUE  X"1A24212078".
           02  F               PIC X(05)    VALUE  "ｺｰﾄﾞ ".
           02  F               PIC N(06)    VALUE   "銀　行　名　".
           02  F               PIC X(04)    VALUE  SPACE.
           02  F               PIC N(08)    VALUE   "本　支　店　名　".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "割引".
           02  F               PIC X(07)    VALUE  SPACE.
           02  F               PIC N(04)    VALUE   "割引残高".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "作表".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "使用".
           02  F               PIC N(04)    VALUE   "　年　月".
           02  F               PIC X(02)    VALUE  SPACE.
           02  F               PIC X(01)    VALUE  ":".
           02  F               PIC X(02)    VALUE  SPACE.
           02  F               PIC X(05)    VALUE  "ｺｰﾄﾞ ".
           02  F               PIC N(06)    VALUE   "銀　行　名　".
           02  F               PIC X(04)    VALUE  SPACE.
           02  F               PIC N(08)    VALUE   "本　支　店　名　".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "割引".
           02  F               PIC X(07)    VALUE  SPACE.
           02  F               PIC N(04)    VALUE   "割引残高".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "作表".
           02  F               PIC X(01)    VALUE  SPACE.
           02  F               PIC N(02)    VALUE   "使用".
           02  F               PIC N(04)    VALUE   "　年　月".
       01  W-P.
           02  W-PD        OCCURS  58.
               03  P-KEY1         PIC 9(04).
               03  F              PIC X(01).
               03  P-BNA1         PIC N(08).
               03  F              PIC X(01).
               03  P-SNA1         PIC N(08).
               03  F              PIC X(02).
               03  P-YBC1         PIC 9(01).
               03  P-YBZ1         PIC --,---,---,---.
               03  F              PIC X(02).
               03  P-PRC1         PIC 9(02).
               03  F              PIC X(02).
               03  P-SC1          PIC Z.
               03  F              PIC X(02).
               03  P-NG1          PIC 99/99.
               03  F              PIC X(02).
               03  P-X            PIC X(01).
               03  F              PIC X(02).
               03  P-KEY2         PIC 9(04).
               03  F              PIC X(01).
               03  P-BNA2         PIC N(08).
               03  F              PIC X(01).
               03  P-SNA2         PIC N(08).
               03  F              PIC X(02).
               03  P-YBC2         PIC 9(01).
               03  P-YBZ2         PIC --,---,---,---.
               03  F              PIC X(02).
               03  P-PRC2         PIC 9(02).
               03  F              PIC X(02).
               03  P-SC2          PIC Z.
               03  F              PIC X(02).
               03  P-NG2          PIC 99/99.
       01  W-R.
           02  W-KEY               PIC 9(04).
           02  W-BNA               PIC N(08).
           02  W-BNAD          REDEFINES    W-BNA    PIC X(16).
           02  W-SNA               PIC N(08).
           02  W-SNAD          REDEFINES    W-SNA    PIC X(16).
           02  F                   PIC X(20).
           02  W-YBZ               PIC 9(10).
           02  W-YBC               PIC 9(01).
           02  F                   PIC X(11).
           02  W-SC                PIC 9(01).
           02  W-NG                PIC 9(04).
           02  W-PRC               PIC 9(02).
       01  W-DATA.
           02  W-PAGE              PIC 9(02).
           02  W-PC                PIC 9(01).
           02  W-DMMD              PIC 9(01).
           02  W-CHK               PIC 9(01).
           02  W-ACT               PIC 9(01).
           02  W-L                 PIC 9(02).
           02  W-DMM               PIC 9(01).
           02  W-SKEY              PIC 9(04).
           02  W-EKEY              PIC 9(04) VALUE 9999.
           02  W-NA                PIC X(16).
           02  W-NAAD          REDEFINES     W-NA.
               03  W-NAD       OCCURS  16    PIC X(01).
           02  CNT                 PIC 9(02).
           02  W-EC                PIC 9(01).
           02  W-ATBLD             PIC X(50)    VALUE
                   "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔ ﾕ ﾖﾗﾘﾙﾚﾛﾜ   ﾝ".
           02  W-ATBL          REDEFINES     W-ATBLD.
               03  W-TBLD      OCCURS  50    PIC X(01).
           02  W-APRCD.
               03  F               PIC X(50)    VALUE
                   "01020304051112131415212223242531323334354142434445".
               03  F               PIC X(50)    VALUE
                   "5152535455616263646571  73  75818283848591      99".
           02  W-APRC          REDEFINES     W-APRCD.
               03  W-PRCD      OCCURS  50    PIC 9(02).
           02  W-LD                PIC 9(02).
           02  W-CD                PIC 9(01).
           02  W-FILE              PIC X(13).
           COPY LSTAT.
      *
           COPY LIBANK.
           COPY LIUKET.
           COPY LISHIT.
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
               03  A-ACT     PIC 9(01).
               03  A-KEY     PIC 9(04).
               03  A-BNA     PIC N(08).
               03  A-SNA     PIC N(08).
               03  A-YBC     PIC 9(01).
               03  A-YBZ     PIC 9(10).
               03  A-PRC     PIC 9(02).
               03  A-DMM     PIC 9(01).
           02  FILLER.
               03  A-CHK     PIC 9(01).
               03  A-SKEY    PIC 9(04).
               03  A-EKEY    PIC 9(04).
               03  A-DMMD    PIC 9(01).
       01  C-DSP.
           02  FILLER.
               03  D-BNA   PIC N(08).
               03  D-SNA   PIC N(08).
               03  D-YBC   PIC 9(01).
               03  D-YBZ   PIC Z(10).
               03  D-PRC   PIC 9(02).
               03  D-PRM   PIC X(01).
               03  D-PRMC  PIC X(01)   VALUE  " ".
           02  FILLER.
               03  D-PM     PIC X(54)    VALUE
               "使用ﾁｪｯｸ  OK=1 NO=9  [ ]  銀行ｺｰﾄﾞ 0000 ～ 9999   ﾘﾀｰﾝ".
               03  D-PMC    PIC X(54)    VALUE
               "                                                      ".
       01  C-ERR.
           02  FILLER.
               03  E-STAT    PIC X(02).
               03  E-ME1     PIC X(18)    VALUE
                     "***  BANKM ﾅｼ  ***".
               03  E-ME2     PIC X(23)    VALUE
                     "***  BANKM ﾄｳﾛｸｽﾞﾐ  ***".
               03  E-ME3     PIC X(26)    VALUE
                     "***  BANKM WRITE ｴﾗｰ  ***".
               03  E-ME4     PIC X(28)    VALUE
                     "***  BANKM REWRITE ｴﾗｰ  ***".
               03  E-ME5     PIC X(27)    VALUE
                     "***  BANKM DELETE ｴﾗｰ  ***".
               03  E-ME6     PIC X(30)    VALUE
                     "***  ｺﾓｼﾞﾉ ｽﾍﾟｰｽｶﾞﾊｲｯﾃｲﾙ  ***".
               03  E-ME71.
                   04  FILLER    PIC X(13).
                   04  FILLER    PIC N(21)    VALUE
                          "オーバーフロー、領域を拡張後、ＦＮＣ＋再開".
               03  E-ME78  PIC N(02)    VALUE   "連絡".
               03  E-ME98  PIC X(05)    VALUE  X"1B4A05".
               03  E-ME99  PIC X(05)    VALUE  X"1B4205".
               03  E-CL.
                   04  FILLER    PIC X(40)    VALUE
                         "                                       ".
                   04  FILLER    PIC X(40)    VALUE
                         "                                       ".
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
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "61" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "W-L" "0" "51" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "W-L" "3" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-KEY" "9" "W-L" "6" "4" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-BNA" "N" "W-L" "11" "16" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-BNA" BY REFERENCE W-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNA" "N" "W-L" "28" "16" "A-BNA" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNA" BY REFERENCE W-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-YBC" "9" "W-L" "49" "1" "A-SNA" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-YBC" BY REFERENCE W-YBC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-YBZ" "9" "W-L" "54" "10" "A-YBC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-YBZ" BY REFERENCE W-YBZ "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PRC" "9" "W-L" "67" "2" "A-YBZ" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-PRC" BY REFERENCE W-PRC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "W-L" "76" "1" "A-PRC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "22" "0" "10" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-CHK" "9" "22" "23" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKEY" "9" "22" "36" "4" "A-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EKEY" "9" "22" "44" "4" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMMD" "9" "22" "50" "1" "A-EKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMMD" BY REFERENCE W-DMMD "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "155" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "W-L" "0" "47" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-BNA" "N" "W-L" "11" "16" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-BNA" BY REFERENCE W-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SNA" "N" "W-L" "28" "16" "D-BNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-SNA" BY REFERENCE W-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-YBC" "9" "W-L" "49" "1" "D-SNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-YBC" BY REFERENCE W-YBC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-YBZ" "Z" "W-L" "54" "10" "D-YBC" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-YBZ" BY REFERENCE W-YBZ "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-PRC" "9" "W-L" "67" "2" "D-YBZ" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-PRC" BY REFERENCE W-PRC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-PRM" "X" "W-L" "71" "1" "D-PRC" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-PRM" BY REFERENCE W-TBLD(1) "1" "1" BY REFERENCE CNT 1
            RETURNING RESU.
       CALL "SD_Init" USING
            "D-PRMC" "X" "W-L" "71" "1" "D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "02C-DSP" " " "22" "0" "108" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-PM" "X" "22" "1" "54" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-PMC" "X" "22" "1" "54" "D-PM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "303" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "303" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "28" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "27" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "30" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME71" " " "24" "0" "55" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "SD_Screen_Output" USING "SCTM01" RETURNING RESU.
           MOVE  5  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           ACCEPT  H-DATE  FROM  DATE.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO  TO  M-980
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU
               GO  TO  M-500
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO  TO  M-040
           END-IF.
       M-100.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE  ZERO  TO  W-R.
           MOVE  ALL  "　"  TO  W-BNA  W-SNA.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-100
           END-IF
           MOVE  W-KEY  TO  B-KEY.
      *           READ  BANK-M  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-120
           END-IF
           MOVE  BANK-R  TO  W-R.
           PERFORM  S-80  THRU  S-90.
           CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YBC" D-YBC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YBZ" D-YBZ "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRC" D-PRC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRM" D-PRM "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-100
           END-IF
           IF  W-ACT = 2
               GO  TO  M-140
           END-IF
           IF  W-ACT = 3
               GO  TO  M-280
           END-IF
           IF  W-YBC = ZERO
               MOVE  ZERO  TO  B-YBZ
               GO  TO  M-220
           END-IF
           IF  W-PRC = ZERO
               MOVE  5  TO  W-EC
           ELSE
               PERFORM  S-80  THRU  S-90
           END-IF
           IF  W-EC = ZERO
               CALL "SD_Output" USING "D-PRM" D-PRM "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-PRMC" D-PRMC "p" RETURNING RESU
           END-IF
           GO  TO  M-140.
       M-120.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-100
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-BNA "A-BNA" "N" "16"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-140
           END-IF
           MOVE  W-BNAD  TO  W-NA.
           IF  SPACE = W-NAD(1) OR W-NAD(3) OR W-NAD(5) OR W-NAD(7) OR
                       W-NAD(9) OR W-NAD(11) OR W-NAD(13) OR W-NAD(15)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-140
           END-IF
           CALL "SD_Output" USING "D-BNA" D-BNA "p" RETURNING RESU.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-SNA "A-SNA" "N" "16"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-160
           END-IF
           MOVE  W-SNAD  TO  W-NA.
           IF  SPACE = W-NAD(1) OR W-NAD(3) OR W-NAD(5) OR W-NAD(7) OR
                       W-NAD(9) OR W-NAD(11) OR W-NAD(13) OR W-NAD(15)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-160
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-YBC "A-YBC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-180
           END-IF
           IF  W-YBC = ZERO
               MOVE  ZERO  TO  W-YBZ
               GO  TO  M-220
           END-IF
           IF  W-YBC NOT = 1
               GO  TO  M-180
           END-IF
           CALL "SD_Output" USING "D-YBC" D-YBC "p" RETURNING RESU.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-YBZ "A-YBZ" "9" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-200
           END-IF.
       M-220.
           CALL "SD_Output" USING "D-YBZ" D-YBZ "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-PRC "A-PRC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YBC = ZERO
                   GO  TO  M-180
               ELSE
                   GO  TO  M-200
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-220
           END-IF
           IF  W-PRC = ZERO
               CALL "SD_Output" USING "D-PRMC" D-PRMC "p" RETURNING RESU
               GO  TO  M-280
           END-IF
           PERFORM  S-80  THRU  S-90.
           IF  W-EC NOT = ZERO
               GO  TO  M-220
           END-IF
           CALL "SD_Output" USING "D-PRC" D-PRC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRM" D-PRM "p" RETURNING RESU.
       M-280.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO  TO  M-100
               ELSE
                   GO  TO  M-220
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-280
           END-IF
           IF  W-DMM = 9
               GO  TO  M-100
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-280
           END-IF
           IF  W-ACT = 2
               GO  TO  M-360
           END-IF
           IF  W-ACT = 3
               GO  TO  M-380
           END-IF.
       M-320.
           MOVE  W-R  TO  BANK-R.
      *           WRITE  BANK-R  INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               GO  TO  M-340
           END-IF
           GO  TO  M-400.
       M-340.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-980
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           MOVE  "BANK         "  TO  W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           GO  TO  M-320.
       M-360.
           MOVE  ZERO  TO  BANK-R.
           MOVE  W-R   TO  BANK-R.
      *           REWRITE  BANK-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-100
           END-IF
           GO  TO  M-400.
       M-380.
      *           DELETE  BANK-M  INVALID  KEY
      *///////////////
           CALL "DB_Delete" USING BANK-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-100
           END-IF.
       M-400.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               CALL "SD_Screen_Output" USING "SCTM01" RETURNING RESU
               MOVE  5  TO  W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           GO  TO  M-100.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "SD_Output" USING "D-PMC" D-PMC "p" RETURNING RESU
               GO  TO  M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           IF  W-CHK NOT = 1 AND 9
               GO TO M-500
           END-IF.
       M-520.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-500
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-520
           END-IF.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-520
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-540
           END-IF
           IF  W-EKEY < W-SKEY
               GO  TO  M-520
           END-IF.
       M-560.
           CALL "SD_Accept" USING BY REFERENCE A-DMMD "A-DMMD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-540
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-560
           END-IF
           IF  W-DMMD = 9
               CALL "SD_Output" USING "D-PMC" D-PMC "p" RETURNING RESU
               GO  TO  M-040
           END-IF
           IF  W-DMMD NOT = 1
               GO  TO  M-560
           END-IF
      *
           IF  W-CHK = 9
               GO TO M-800
           END-IF
           MOVE  ZERO TO  B-KEY.
      *           START  BANK-M KEY NOT < B-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            BANK-M_PNAME1 "B-KEY" " NOT < " B-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  M-800
           END-IF.
       M-580.
      *           READ  BANK-M  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-600
           END-IF
           MOVE 0 TO B-SC.
      *           REWRITE  BANK-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-800
           END-IF
           GO TO M-580.
       M-600.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
       M-620.
      *           READ UKET-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" UKET-M_PNAME1 BY REFERENCE UKET-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-660
           END-IF
           MOVE UT-BCD TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-640
           END-IF
           IF  B-SC = 1
               GO TO M-640
           END-IF
           MOVE 1 TO B-SC.
      *           REWRITE  BANK-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               GO  TO  M-800
           END-IF.
       M-640.
           IF  UT-SBC = ZERO
               GO TO M-620
           END-IF
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-620
           END-IF
           IF  B-SC = 1
               GO TO M-620
           END-IF
           MOVE 1 TO B-SC.
      *           REWRITE  BANK-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               GO  TO  M-800
           END-IF
           GO TO M-620.
       M-660.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-M_PNAME1 "SHARED" BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
       M-680.
      *           READ SHIT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SHIT-M_PNAME1 BY REFERENCE SHIT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-700
           END-IF
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-680
           END-IF
           IF  B-SC = 1
               GO TO M-680
           END-IF
           MOVE 1 TO B-SC.
      *           REWRITE  BANK-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1
               GO  TO  M-800
           END-IF
           GO TO M-680.
       M-700.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
       M-800.
           MOVE  W-SKEY  TO  B-KEY.
      *           START  BANK-M KEY NOT < B-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            BANK-M_PNAME1 "B-KEY" " NOT < " B-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-520
           END-IF
      *           READ  BANK-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-520
           END-IF
           IF  B-KEY > W-EKEY
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-520
           END-IF
           PERFORM  S-20  THRU  S-35.
           GO  TO  M-040.
       M-980.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE  TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD    1       TO  W-PAGE.
           MOVE   W-PAGE  TO  H-PAGE.
           MOVE   SPACE   TO  SP-R.
           MOVE   HEAD1   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO  SP-R.
           MOVE   HEAD2   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           PERFORM  S-40  THRU  S-50.
           MOVE  ZERO  TO  W-PC  W-LD  W-CD.
       S-25.
           PERFORM  S-55  THRU  S-60.
           IF  W-CD = ZERO
               MOVE  B-KEY  TO  P-KEY1(W-LD)
               MOVE  B-BNA  TO  P-BNA1(W-LD)
               MOVE  B-SNA  TO  P-SNA1(W-LD)
               MOVE  B-YBC  TO  P-YBC1(W-LD)
               MOVE  B-YBZ  TO  P-YBZ1(W-LD)
               MOVE  B-PRC  TO  P-PRC1(W-LD)
               MOVE  B-SC   TO  P-SC1(W-LD)
               MOVE  ":"    TO  P-X(W-LD)
               IF  B-NG NOT = ZERO
                   MOVE  B-NG   TO  P-NG1(W-LD)
               END-IF
           END-IF
           IF  W-CD NOT = ZERO
               MOVE  B-KEY  TO  P-KEY2(W-LD)
               MOVE  B-BNA  TO  P-BNA2(W-LD)
               MOVE  B-SNA  TO  P-SNA2(W-LD)
               MOVE  B-YBC  TO  P-YBC2(W-LD)
               MOVE  B-YBZ  TO  P-YBZ2(W-LD)
               MOVE  B-PRC  TO  P-PRC2(W-LD)
               MOVE  B-SC   TO  P-SC2(W-LD)
               IF  B-NG NOT = ZERO
                   MOVE  B-NG   TO  P-NG2(W-LD)
               END-IF
           END-IF
      *
      *           READ  BANK-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  S-30
           END-IF
           IF  B-KEY NOT > W-EKEY
               GO  TO  S-25
           END-IF.
       S-30.
           PERFORM  S-65  THRU  S-75.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "D-PMC" D-PMC "p" RETURNING RESU.
       S-35.
           EXIT.
       S-40.
           MOVE  SPACE  TO  W-P.
           MOVE  ZERO   TO  W-LD.
       S-45.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               MOVE  SPACE  TO  P-BNA1(W-LD)  P-SNA1(W-LD)
                                P-BNA2(W-LD)  P-SNA2(W-LD)
               GO  TO  S-45
           END-IF.
       S-50.
           EXIT.
       S-55.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               GO  TO  S-60
           END-IF
           IF  W-CD = ZERO
               MOVE  5     TO  W-CD
               MOVE  ZERO  TO  W-LD
               GO  TO  S-55
           END-IF
           PERFORM  S-65  THRU  S-75.
           PERFORM  S-40  THRU  S-50.
           MOVE  ZERO  TO  W-LD  W-CD.
           GO  TO  S-55.
       S-60.
           EXIT.
       S-65.
           IF  W-PC = ZERO
               MOVE  5  TO  W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM  S-10  THRU  S-15
           ELSE
               PERFORM  S-05  THRU  S-15
           END-IF
           MOVE  ZERO  TO  W-LD.
       S-70.
           ADD  1  TO  W-LD.
           IF  W-LD = 59
               GO  TO  S-75
           END-IF
           IF  SPACE = P-BNA1(W-LD) AND P-SNA1(W-LD) AND
                       P-BNA2(W-LD) AND P-SNA2(W-LD)
               GO  TO  S-75
           END-IF
           MOVE   SPACE       TO  SP-R.
           MOVE   W-PD(W-LD)  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE       TO  SP-R.
           GO  TO  S-70.
       S-75.
           EXIT.
       S-80.
           MOVE  ZERO  TO  CNT  W-EC.
       S-85.
           ADD  1  TO  CNT.
           IF  CNT > 50
               MOVE  9  TO  W-EC
               GO  TO  S-90
           END-IF
           IF  W-PRC NOT = W-PRCD(CNT)
               GO  TO  S-85
           END-IF.
       S-90.
           EXIT.
