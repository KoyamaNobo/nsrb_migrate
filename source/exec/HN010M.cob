       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HN010M.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  得意先品名別マスターメンテナンス*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SH010M                          *
      *    DATA WRITTN     :  91/08/23                        *
      *    COMPILE TYPE    :  CBL85 (MODE74)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  20K                    PIC X(05)  VALUE  X"1A24212474".
       77  15K                    PIC X(05)  VALUE  X"1A24212078".
       01  PRN-AREA.
           02  W-POC              PIC  9(01) VALUE  ZERO.
           02  W-SEN              PIC  9(01).
           02  W-OMSG             PIC  N(03).
           02  W-UMSG             PIC  N(03).
           02  W-FROM1            PIC  9(04).
           02  W-TO1              PIC  9(04).
           02  W-FROM2            PIC  9(06).
           02  W-TO2              PIC  9(06).
           02  LCNT               PIC  9(02) VALUE  90.
           02  PCNT               PIC  9(03) VALUE  ZERO.
           02  OLD-KEY.
               03  W-TCD          PIC  9(04).
               03  W-HCD          PIC  9(06).
               03  W-SIZ          PIC  9(01).
       01  GAM-AREA.
           02  ACT                PIC  9(01).
           02  OKC                PIC  9(01).
           02  W-L1               PIC  9(02).
           02  W-L2               PIC  9(02).
           02  W-LCNT             PIC  9(02).
           02  I                  PIC  9(01).
           02  TCNT               PIC  9(02).
           02  PRI-SW             PIC  9(01).
           02  W-MTCD             PIC  9(04).
           02  W-STCD             PIC  9(04).
           02  W-KEY.
               03  W-01           PIC  9(04).
               03  W-02           PIC  9(06).
               03  W-02R          REDEFINES  W-02.
                 04  W-021        PIC  9(04).
                 04  W-022        PIC  9(02).
               03  W-03           PIC  9(01).
           02  W-04T.
               03  W-04  OCCURS  5     PIC 9(05).
               03  W-041 OCCURS  5     PIC 9(05).
           02  W-BITT.
               03  W-BIT OCCURS  5     PIC 9(01).
           02  W-05T.
               03  W-05  OCCURS  10    PIC 9(04).
           02  W-PC.
               03  W-PC1          PIC 9(01).
               03  W-PC2          PIC 9(01).
           02  W-CD               PIC 9(01).
           02  W-LD               PIC 9(02).
           02  W-C                PIC 9(02).
           02  W-HNAD.
               03  W-HNA          PIC N(01)  OCCURS  24.
           02  W-NAME             PIC N(24).
           02  W-NAD              REDEFINES  W-NAME.
               03  W-NA           PIC N(01)  OCCURS  24.
       01  W-R.
           02  WR-KEY.
             03  WR-01      PIC 9(4).
             03  WR-02      PIC 9(6).
             03  WR-03      PIC 9(1).
           02  WR-04        PIC 9(5).
           02  WR-041       PIC 9(5).
       01  WORK-AREA.
           02  HIZUKE.
               03  HI-YY          PIC 9(02).
               03  HI-MM          PIC 9(02).
               03  HI-DD          PIC 9(02).
       01  HEAD1.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(38) VALUE SPACE.
           02  F              PIC N(26) VALUE
               "＊＊＊　　得意先品名別単価マスター　リスト　　＊＊＊".
           02  F              PIC X(22) VALUE SPACE.
           02  F              PIC X(5) VALUE "DATE.".
           02  M-YY           PIC 9(2).
           02  F              PIC X    VALUE "/".
           02  M-MM           PIC Z9.
           02  F              PIC X    VALUE "/".
           02  M-DD           PIC Z9.
           02  F              PIC X(5) VALUE SPACE.
           02  F              PIC X(2) VALUE "P.".
           02  WPCNT          PIC ZZ9.
       01  HEAD2.
           02  HEAD21.
             03  H-TCD21      PIC X(05).
             03  H-TNA21      PIC N(07).
           02  HEAD22  REDEFINES HEAD21.
             03  H-HCD21      PIC X(07).
             03  H-HNA21      PIC N(06).
           02  F              PIC X(46) VALUE SPACE.
           02  F              PIC X(05) VALUE "  :  ".
           02  HEAD23.
             03  H-TCD22      PIC X(05).
             03  H-TNA22      PIC N(07).
           02  HEAD24  REDEFINES HEAD23.
             03  H-HCD22      PIC X(07).
             03  H-HNA22      PIC N(06).
           02  F              PIC X(46) VALUE SPACE.
       01  HEAD3.
           02  F              PIC X(05)  VALUE  X"1A24212078".
           02  F              PIC X(04) VALUE SPACE.
           02  HEAD31F.
             03  F            PIC X(02).
             03  H-CD31F      PIC X(04).
             03  F            PIC X(02).
             03  H-NA31F      PIC N(10).
             03  F            PIC X(21).
           02  HEAD32F  REDEFINES HEAD31F.
             03  H-CD32F      PIC X(04).
             03  F            PIC X(01).
             03  H-NA32F      PIC N(10).
             03  F            PIC X(24).
           02  F              PIC X(07) VALUE "ｻｲｽﾞ   ".
           02  F              PIC N(02) VALUE "単価".
           02  F              PIC X(01) VALUE SPACE.
           02  F              PIC N(04) VALUE "　店単価".
           02  F              PIC X(05) VALUE "  :  ".
           02  F              PIC X(04) VALUE SPACE.
           02  HEAD31R.
             03  F            PIC X(02).
             03  H-CD31R      PIC X(04).
             03  F            PIC X(02).
             03  H-NA31R      PIC N(10).
             03  F            PIC X(21).
           02  HEAD32R  REDEFINES HEAD31R.
             03  H-CD32R      PIC X(04).
             03  F            PIC X(01).
             03  H-NA32R      PIC N(10).
             03  F            PIC X(24).
           02  F              PIC X(07) VALUE "ｻｲｽﾞ   ".
           02  F              PIC N(02) VALUE "単価".
           02  F              PIC X(01) VALUE SPACE.
           02  F              PIC N(04) VALUE "　店単価".
           02  F              PIC X(05) VALUE X"1A24212474".
       01  W-PR.
           02  W-P     OCCURS  58.
             03  P-CD1            PIC X(05).
             03  P-DF1.
               04  P-TCD1         PIC 9(04).
               04  F              PIC X(01).
               04  P-TNA1         PIC N(26).
               04  F              PIC X(21).
             03  P-DF2   REDEFINES P-DF1.
               04  P-S11          PIC N(02).
               04  F              PIC X(02).
               04  P-HCD1         PIC 9(06).
               04  F              PIC X(01).
               04  P-HNA1         PIC N(24).
               04  F              PIC X(01).
               04  P-SIZ1         PIC 9(01).
               04  P-TAN1         PIC ZZZZ,ZZ9.
               04  P-MTN1         PIC ZZZ,ZZZ.
             03  P-DF3   REDEFINES P-DF1.
               04  P-HCD11        PIC 9(06).
               04  F              PIC X(01).
               04  P-HNA11        PIC N(24).
               04  P-S12          PIC N(02).
               04  F              PIC X(19).
             03  P-DF4   REDEFINES P-DF1.
               04  F              PIC X(04).
               04  P-TCD11        PIC 9(04).
               04  F              PIC X(01).
               04  P-TNA11        PIC N(26).
               04  F              PIC X(01).
               04  P-SIZ11        PIC 9(01).
               04  P-TAN11        PIC ZZZZ,ZZ9.
               04  P-MTN11        PIC ZZZ,ZZZ.
             03  F                PIC X(02).
             03  P-C              PIC X(01).
             03  F                PIC X(02).
             03  P-DR1.
               04  P-TCD2         PIC 9(04).
               04  F              PIC X(01).
               04  P-TNA2         PIC N(26).
               04  F              PIC X(21).
             03  P-DR2   REDEFINES P-DR1.
               04  P-S21          PIC N(02).
               04  F              PIC X(02).
               04  P-HCD2         PIC 9(06).
               04  F              PIC X(01).
               04  P-HNA2         PIC N(24).
               04  F              PIC X(01).
               04  P-SIZ2         PIC 9(01).
               04  P-TAN2         PIC ZZZZ,ZZ9.
               04  P-MTN2         PIC ZZZ,ZZZ.
             03  P-DR3   REDEFINES P-DR1.
               04  P-HCD21        PIC 9(06).
               04  F              PIC X(01).
               04  P-HNA21        PIC N(24).
               04  P-S22          PIC N(02).
               04  F              PIC X(19).
             03  P-DR4   REDEFINES P-DR1.
               04  F              PIC X(04).
               04  P-TCD21        PIC 9(04).
               04  F              PIC X(01).
               04  P-TNA21        PIC N(26).
               04  F              PIC X(01).
               04  P-SIZ21        PIC 9(01).
               04  P-TAN21        PIC ZZZZ,ZZ9.
               04  P-MTN21        PIC ZZZ,ZZZ.
             03  P-CD2            PIC X(05).
           COPY LSTAT.
           COPY LWMSG.
      *
           COPY LITHTM.
           COPY LIHIM.
           COPY LITM.
      *FD  P-F
       77  P-R                    PIC X(250).
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
           02  C-CL1   PIC  X(30) VALUE
                   "                              ".
       01  PRN-CLR.
           02  CLR-01  PIC  X(01)  VALUE  " ".
           02  CLR-02  PIC  X(06)  VALUE  " ".
           02  CLR-03  PIC  X(06)  VALUE  " ".
           02  CLR-04  PIC  X(06)  VALUE  " ".
           02  CLR-05  PIC  X(06)  VALUE  " ".
       01  CPY-CLR.
           02  CLC-01  PIC  X(53)  VALUE  " ".
           02  CLC-02  PIC  X(53)  VALUE  " ".
       01  GAM-CLR.
           02  CLE-01.
               03  FILLER  PIC  X(04)  VALUE " ".
               03  FILLER  PIC  X(52)  VALUE " ".
           02  CLE-02.
               03  FILLER  PIC  X(06)  VALUE " ".
               03  FILLER  PIC  X(48)  VALUE " ".
           02  CLE-03A.
               03  CLE-031 PIC X(12)  VALUE " ".
               03  CLE-032 PIC X(12)  VALUE " ".
               03  CLE-033 PIC X(12)  VALUE " ".
               03  CLE-034 PIC X(12)  VALUE " ".
               03  CLE-035 PIC X(12)  VALUE " ".
           02  CLE-05A.
               03  CLE-051 PIC X(57)  VALUE " ".
               03  CLE-052 PIC X(57)  VALUE " ".
               03  CLE-053 PIC X(57)  VALUE " ".
               03  CLE-054 PIC X(57)  VALUE " ".
               03  CLE-055 PIC X(57)  VALUE " ".
               03  CLE-056 PIC X(57)  VALUE " ".
               03  CLE-057 PIC X(57)  VALUE " ".
               03  CLE-058 PIC X(57)  VALUE " ".
               03  CLE-059 PIC X(57)  VALUE " ".
               03  CLE-050 PIC X(57)  VALUE " ".
      *
           02  CLE-OKC     PIC X(01)  VALUE " ".
       01  GAM-CLR2.
           02  FILLER  PIC  X(11)  VALUE "CLEAR  LINE".
      ***
       01  ACP-AREA.
           02  ACP-ACT     PIC 9 .
           02  ACP-OKC     PIC 9 .
       01  GAM-ACP.
           02  ACP-01      PIC 9(04).
           02  ACP-02      PIC 9(06).
           02  ACP-021     PIC 9(04).
           02  ACP-04      PIC 9(05).
           02  ACP-041     PIC 9(05).
           02  ACP-05      PIC 9(04).
       01  PRN-ACP.
           02  ACP-SEN     PIC 9(01).
           02  ACP-OTF     PIC 9(04).
           02  ACP-OHF     PIC 9(06).
           02  ACP-OTT     PIC 9(04).
           02  ACP-OHT     PIC 9(06).
           02  ACP-UHF     PIC 9(06).
           02  ACP-UTF     PIC 9(04).
           02  ACP-UHT     PIC 9(06).
           02  ACP-UTT     PIC 9(04).
       01  CPY-ACP.
           02  ACP-MTCD    PIC 9(04).
           02  ACP-STCD    PIC 9(04).
      ***
       01  DSP-AERA.
           02  DSP-01      PIC N(26).
           02  DSP-02      PIC N(24).
           02  DSP-021     PIC N(24).
           02  DSP-04      PIC ZZZZZ .
           02  DSP-041     PIC ZZZZZ .
           02  DSP-05      PIC N(26).
           02  DSP-MTNA    PIC N(26).
           02  DSP-STNA    PIC N(26).
       01  DSP-DSP.
           02  FILLER  PIC N(18) VALUE
              "＊＊＊　　得意先品名別単価マスター　".
           02  FILLER  PIC N(11) VALUE
              "メンテナンス　　＊＊＊".
           02  FILLER  PIC X(36) VALUE
                "登録=1 修正=2 削除=3 作表=4 コピー=5".
           02  FILLER  PIC X(46) VALUE
                "一括登録=6 一括修正=7 一括削除=8 終了=9   ﾘﾀｰﾝ".
           02  FILLER  PIC X(21) VALUE
                "確認 OK=1 NO=9   ﾘﾀｰﾝ".
       01  DSP-PRN.
           02  FILLER  PIC X(11) VALUE "CLEAR  LINE".
           02  FILLER  PIC X(38) VALUE
                "得意先品名別 = 1 , 品名得意先別 = 2   ".
           02  FILLER  PIC X(21) VALUE  "ｺｰﾄﾞ        ～       ".
           02  FILLER  PIC X(21) VALUE  "ｺｰﾄﾞ        ～       ".
       01  DSP-PRN1.
           02  01DSP-PRN1  PIC N(03).
           02  02DSP-PRN1  PIC N(03).
       01  DSP-CPY.
           02  FILLER  PIC X(11) VALUE "CLEAR  LINE".
           02  FILLER  PIC N(07) VALUE
               "コピー元得意先".
           02  FILLER  PIC N(01) VALUE
               "↓".
           02  FILLER  PIC N(01) VALUE
               "↓".
           02  FILLER  PIC N(07) VALUE
               "コピー先得意先".
       01  DSP-ERR.
           02  FILLER.
               03  ERR-1      PIC N(16)   VALUE
                   "得意先マスタ　未登録".
               03  ERR-2      PIC N(16)   VALUE
                   "品名ＩＮＤＥＸＥＤマスタ　未登録".
               03  ERR-3      PIC N(08)   VALUE
                   "サイズ区分エラー".
               03  ERR-4.
                 04  FILLER   PIC N(06)   VALUE
                      "親コードあり".
                 04  01ERR-4  PIC 9(06).
               03  ERR-99     PIC X(05)   VALUE
                    X"1B4201".
       COPY  LSMSG.
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL1" "X" "24" "1" "30" "C-CL" " " RETURNING RESU.
      *PRN-CLR
       CALL "SD_Init" USING
           "PRN-CLR" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-01" "X" "6" "55" "1" " " "PRN-CLR" RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-02" "X" "9" "35" "6" "CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-03" "X" "9" "45" "6" "CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-04" "X" "11" "35" "6" "CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-05" "X" "11" "45" "6" "CLR-04" " " RETURNING RESU.
      *CPY-CLR
       CALL "SD_Init" USING
           "CPY-CLR" " " "0" "0" "106" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLC-01" "X" "6" "24" "53" " " "CPY-CLR" RETURNING RESU.
       CALL "SD_Init" USING
           "CLC-02" "X" "11" "24" "53" "CLC-01" " " RETURNING RESU.
      *GAM-CLR
       CALL "SD_Init" USING
           "GAM-CLR" " " "0" "0" "741" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-01" " " "6" "0" "56" " " "GAM-CLR" RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "6" "7" "4" " " "CLE-01" RETURNING RESU.
       CALL "SD_Init" USING
           "02CLE-01" "X" "6" "19" "52" "01CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-02" " " "8" "0" "54" "CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-02" "X" "8" "7" "6" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING
           "02CLE-02" "X" "8" "19" "48" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-03A" " " "0" "0" "60" "CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-031" "X" "11" "8" "12" " " "CLE-03A" RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-032" "X" "12" "8" "12" "CLE-031" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-033" "X" "13" "8" "12" "CLE-032" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-034" "X" "14" "8" "12" "CLE-033" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-035" "X" "15" "8" "12" "CLE-034" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-05A" " " "0" "0" "570" "CLE-03A" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-051" "X" "11" "23" "57" " " "CLE-05A" RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-052" "X" "12" "23" "57" "CLE-051" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-053" "X" "13" "23" "57" "CLE-052" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-054" "X" "14" "23" "57" "CLE-053" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-055" "X" "15" "23" "57" "CLE-054" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-056" "X" "16" "23" "57" "CLE-055" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-057" "X" "17" "23" "57" "CLE-056" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-058" "X" "18" "23" "57" "CLE-057" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-059" "X" "19" "23" "57" "CLE-058" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-050" "X" "20" "23" "57" "CLE-059" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-OKC" "X" "23" "38" "1" "CLE-05A" " " RETURNING RESU.
      *GAM-CLR2
       CALL "SD_Init" USING
           "GAM-CLR2" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01GAM-CLR2" "X" "5" "15" "11" " " "GAM-CLR2" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
           "ACP-AREA" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-ACT" "9" "4" "58" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-OKC" "9" "23" "38" "1" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *GAM-ACP
       CALL "SD_Init" USING
           "GAM-ACP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-01" "9" "6" "7" "4" " " "GAM-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-01" BY REFERENCE W-01 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-02" "9" "8" "7" "6" "ACP-01" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-02" BY REFERENCE W-02 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-021" "9" "8" "7" "4" "ACP-02" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-021" BY REFERENCE W-021 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-04" "9" "W-L1" "8" "5" "ACP-021" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-04" BY REFERENCE W-04(1) "5" "1" BY REFERENCE I 5
            RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-041" "9" "W-L1" "15" "5" "ACP-04" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-041" BY REFERENCE W-041(1) "5" "1" BY REFERENCE I 5
            RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-05" "9" "W-L2" "23" "4" "ACP-041" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-05" BY REFERENCE W-05(1) "7" "1" BY REFERENCE TCNT 4
            RETURNING RESU.
      *PRN-ACP
       CALL "SD_Init" USING
           "PRN-ACP" " " "0" "0" "41" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-SEN" "9" "6" "55" "1" " " "PRN-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-OTF" "9" "9" "37" "4" "ACP-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-OTF" BY REFERENCE W-FROM1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-OHF" "9" "9" "35" "6" "ACP-OTF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-OHF" BY REFERENCE W-FROM2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-OTT" "9" "9" "45" "4" "ACP-OHF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-OTT" BY REFERENCE W-TO1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-OHT" "9" "9" "45" "6" "ACP-OTT" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-OHT" BY REFERENCE W-TO2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-UHF" "9" "11" "35" "6" "ACP-OHT" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-UHF" BY REFERENCE W-FROM2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-UTF" "9" "11" "37" "4" "ACP-UHF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-UTF" BY REFERENCE W-FROM1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-UHT" "9" "11" "45" "6" "ACP-UTF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-UHT" BY REFERENCE W-TO2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-UTT" "9" "11" "45" "4" "ACP-UHT" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-UTT" BY REFERENCE W-TO1 "4" "0" RETURNING RESU.
      *CPY-ACP
       CALL "SD_Init" USING
           "CPY-ACP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-MTCD" "9" "6" "24" "4" " " "CPY-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-MTCD" BY REFERENCE W-MTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ACP-STCD" "9" "11" "24" "4" "ACP-MTCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "ACP-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
      *DSP-AERA
       CALL "SD_Init" USING
           "DSP-AERA" " " "0" "0" "314" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-01" "N" "6" "19" "52" " " "DSP-AERA" RETURNING RESU.
       CALL "SD_From" USING
           "DSP-01" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-02" "N" "8" "19" "48" "DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-02" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-021" "N" "8" "19" "48" "DSP-02" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-021" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-04" "ZZZZZ" "W-L1" "8" "5" "DSP-021" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-04" BY REFERENCE W-04(1) "5" "1" BY REFERENCE I 5
            RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-041" "ZZZZZ" "W-L1" "15" "5" "DSP-04" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "DSP-041" BY REFERENCE W-041(1) "5" "1" BY REFERENCE I 5
            RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-05" "N" "W-L2" "28" "52" "DSP-041" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-05" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-MTNA" "N" "6" "29" "52" "DSP-05" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-MTNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-STNA" "N" "11" "29" "52" "DSP-MTNA" " " RETURNING RESU.
       CALL "SD_From" USING
           "DSP-STNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING
           "DSP-DSP" " " "0" "0" "161" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-DSP" "N" "1" "11" "36" " " "DSP-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "02DSP-DSP" "N" "1" "47" "22" "01DSP-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03DSP-DSP" "X" "3" "17" "36" "02DSP-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04DSP-DSP" "X" "4" "17" "46" "03DSP-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05DSP-DSP" "X" "23" "22" "21" "04DSP-DSP" " "
            RETURNING RESU.
      *DSP-PRN
       CALL "SD_Init" USING
           "DSP-PRN" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-PRN" "X" "5" "22" "11" " " "DSP-PRN" RETURNING RESU.
       CALL "SD_Init" USING
           "02DSP-PRN" "X" "6" "18" "38" "01DSP-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03DSP-PRN" "X" "9" "29" "21" "02DSP-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04DSP-PRN" "X" "11" "29" "21" "03DSP-PRN" " "
            RETURNING RESU.
      *DSP-PRN1
       CALL "SD_Init" USING
           "DSP-PRN1" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-PRN1" "N" "9" "23" "6" " " "DSP-PRN1" RETURNING RESU.
       CALL "SD_From" USING
           "01DSP-PRN1" BY REFERENCE W-OMSG "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02DSP-PRN1" "N" "11" "23" "6" "01DSP-PRN1" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02DSP-PRN1" BY REFERENCE W-UMSG "6" "0" RETURNING RESU.
      *DSP-CPY
       CALL "SD_Init" USING
           "DSP-CPY" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CPY" "X" "5" "22" "11" " " "DSP-CPY" RETURNING RESU.
       CALL "SD_Init" USING
           "02DSP-CPY" "N" "6" "8" "14" "01DSP-CPY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03DSP-CPY" "N" "8" "35" "2" "02DSP-CPY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04DSP-CPY" "N" "9" "35" "2" "03DSP-CPY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05DSP-CPY" "N" "11" "8" "14" "04DSP-CPY" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "DSP-ERR" " " "0" "0" "103" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-ERR" " " "24" "0" "103" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-1" "N" "24" "1" "32" " " "01DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-2" "N" "24" "1" "32" "ERR-1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-3" "N" "24" "1" "16" "ERR-2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-4" " " "24" "0" "18" "ERR-3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-4" "N" "24" "1" "12" " " "ERR-4" RETURNING RESU.
       CALL "SD_Init" USING
           "02ERR-4" "9" "24" "17" "6" "01ERR-4" " " RETURNING RESU.
       CALL "SD_From" USING
           "02ERR-4" BY REFERENCE HI-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-99" "X" "24" "75" "5" "ERR-4" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INIT-RTN  THRU   INIT-EX.
       MR-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-DSP" DSP-DSP "p" RETURNING RESU.
       MR-20.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  MR-20
           END-IF
           IF  ACT  =  1 OR 2 OR 3 OR 6 OR 7 OR 8
               PERFORM    ENT-RTN THRU ENT-EX
           END-IF
           IF  ACT  =  4
               PERFORM    LIS-RTN THRU LIS-EX
           END-IF
           IF  ACT  =  5
               PERFORM    CPY-RTN THRU CPY-EX
           END-IF
           IF  ACT  =  9
               PERFORM    END-RTN THRU END-EX
               CALL "DB_Close"
               STOP RUN
           END-IF
           GO   TO   MR-20.
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INIT-RTN.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           ACCEPT   HIZUKE  FROM  DATE.
           MOVE  HI-YY     TO  M-YY.
           MOVE  HI-MM     TO  M-MM.
           MOVE  HI-DD     TO  M-DD.
       INIT-EX.
            EXIT.
      *********************************
      *     ﾄｳﾛｸ ｼｭｳｾｲ  R T N       ***
      *********************************
      **
       ENT-RTN.
           CALL "SD_Output" USING
            "GAM-CLR2" GAM-CLR2 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SH010M" RETURNING RESU.
       ENT-20.
           CALL "SD_Accept" USING BY REFERENCE ACP-01 "ACP-01" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-EX
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-20
           END-IF
           MOVE  W-01     TO  T-KEY.
      ***  得意先マスタ　ＲＥＡＤ
      *           READ  T-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO  TO  ENT-20
           END-IF
           CALL "SD_Output" USING "DSP-01" DSP-01 "p" RETURNING RESU.
           IF  ACT  =  1 OR 2 OR 3
               GO  TO  ENT-30
           END-IF.
       ENT-25.
           CALL "SD_Accept" USING BY REFERENCE ACP-021 "ACP-021" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-20
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-25
           END-IF
           MOVE  ZERO     TO  W-022.
           MOVE  SPACE    TO  HI-KEY.
           MOVE  W-021    TO  HI-HCD1.
      *           START HI-M  KEY  NOT <  HI-KEY  INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
      *           READ  HI-M  NEXT RECORD UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
           IF  W-021   NOT =  HI-HCD1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
           MOVE  HI-HCD   TO  W-02.
           PERFORM  NAM-RTN  THRU  NAM-EX.
           CALL "SD_Output" USING "DSP-021" DSP-021 "p" RETURNING RESU.
           IF  ACT NOT = 6
               GO  TO  ENT-35
           END-IF.
       ENT-026.
           IF  HI-MHCD NOT = HI-HCD
               CALL "SD_Output" USING "ERR-4" ERR-4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-99" ERR-99 "p" RETURNING RESU
           END-IF
      *           READ  HI-M  NEXT RECORD UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-27
           END-IF
           IF  W-021   NOT =  HI-HCD1
               GO  TO  ENT-27
           END-IF
           GO TO ENT-026.
       ENT-27.
           MOVE  SPACE    TO  HI-KEY.
           MOVE  W-021    TO  HI-HCD1.
      *           START HI-M  KEY  NOT <  HI-KEY  INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
      *           READ  HI-M  NEXT RECORD UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
           IF  W-021   NOT =  HI-HCD1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-25
           END-IF
           GO  TO  ENT-35.
       ENT-30.
           CALL "SD_Accept" USING BY REFERENCE ACP-02 "ACP-02" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-20
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-30
           END-IF
           MOVE  W-02     TO  HI-KEY.
      ***  品名ＩＮＤＥＸＥＤマスタ　ＲＥＡＤ
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU
               GO  TO  ENT-30
           END-IF
           CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU.
           IF  ACT = 1
               IF  HI-MHCD NOT = HI-HCD
                   CALL "SD_Output" USING
                    "ERR-4" ERR-4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-99" ERR-99 "p" RETURNING RESU
               END-IF
           END-IF.
       ENT-35.
      *****   使用サイズ　チェック  *****
           MOVE   ZERO   TO    W-BITT.
           IF  HI-SS(1)  =  ZERO
               MOVE  1   TO  W-BIT(2)
           END-IF
           IF  HI-SS(2)  =  ZERO
               MOVE  1   TO  W-BIT(3)
           END-IF
           IF  HI-SS(3)  =  ZERO
               MOVE  1   TO  W-BIT(4)
           END-IF
           IF  HI-SS(4)  =  "0000000001"    OR   HI-SS(4)  =  ZERO
               MOVE  1   TO  W-BIT(5)
           END-IF
           IF  W-BITT    =  "01111"
               CALL "SD_Output" USING
                "ERR-3" ERR-3 "p" RETURNING RESU
               IF  ACT   =   6 OR 7 OR 8
                   GO  TO  ENT-25
               ELSE
                   GO  TO  ENT-30
               END-IF
           END-IF.
       ENT-40.
           MOVE   ZERO    TO   W-03   W-04T.
           MOVE   W-KEY   TO   THT-KEY.
      *           START  THTM    KEY NOT <   THT-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-60
           END-IF.
       ENT-50.
      *           READ   THTM    NEXT UNLOCK   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                  GO  TO  ENT-60
           END-IF
           IF  THT-SIZ NOT = 1 AND 2 AND 3 AND 4 AND 9
               GO  TO  ENT-50
           END-IF
           IF  ACT    <  5
               IF  (  THT-TCD NOT  = W-01  )
                OR (  THT-HCD NOT  = W-02  )
                      GO  TO  ENT-60
               END-IF
           END-IF
           IF  ACT    >  4
               IF  (  THT-TCD NOT  = W-01  )
                OR (  THT-HCDF NOT  = W-021  )
                      GO  TO  ENT-60
               END-IF
           END-IF
           IF  ACT  =  1
               CALL "SD_Output" USING
                "CLE-03A" CLE-03A "p" RETURNING RESU
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO   TO   ENT-30
           END-IF
           IF  ACT  =  6
               CALL "SD_Output" USING
                "CLE-03A" CLE-03A "p" RETURNING RESU
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO   TO   ENT-25
           END-IF
           GO  TO ENT-70.
       ENT-60.
           IF  ACT NOT =  1 AND 6
               CALL "SD_Output" USING
                "CLE-03A" CLE-03A "p" RETURNING RESU
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO   TO   ENT-30
           END-IF
           MOVE   ZERO TO   W-04T.
           CALL "SD_Output" USING "CLE-03A" CLE-03A "p" RETURNING RESU.
           GO  TO   ENT-71.
       ENT-70.
           PERFORM  DIS-RTN  THRU  DIS-EX.
           IF  ACT   =  3  OR 8
               GO  TO  ENT-330
           END-IF.
       ENT-71.
           MOVE     1        TO    I.
           MOVE     11       TO    W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       ENT-75.
           IF  W-BIT(I)  =    1
               MOVE  ZERO     TO   W-04(I)  W-041(I)
               CALL "SD_Output" USING
                "DSP-04" DSP-04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-041" DSP-041 "p" RETURNING RESU
               GO  TO  ENT-88
           END-IF.
       ENT-76.
           CALL "SD_Accept" USING BY REFERENCE ACP-04 "ACP-04" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT  =  BTB
               GO  TO ENT-78
           END-IF
           IF  I      =    1
               IF   ACT    =  1 OR 2 OR 3
                   GO  TO ENT-30
               ELSE
                   GO  TO ENT-25
               END-IF
           END-IF.
       ENT-77.
           COMPUTE     I    =  I  -   1.
           COMPUTE     W-L1 =  W-L1 -  1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  I      =    1
               IF  ACT    =  1 OR 2 OR 3
                   GO  TO ENT-30
               ELSE
                   GO  TO ENT-25
               END-IF
           END-IF
           IF  W-BIT(I)    =  0
               GO  TO ENT-76
           END-IF
           GO  TO  ENT-77.
       ENT-78.
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-75
           END-IF
           CALL "SD_Output" USING "DSP-04" DSP-04 "p" RETURNING RESU.
       ENT-79.
           CALL "SD_Accept" USING BY REFERENCE ACP-041 "ACP-041" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-76
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-79
           END-IF
           CALL "SD_Output" USING "DSP-041" DSP-041 "p" RETURNING RESU.
       ENT-88.
           ADD  1      TO   I  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  I NOT  >    5
               GO  TO ENT-75
           END-IF
           IF  ZERO   =    W-04(1)  AND  W-04(2)  AND  W-04(3)
                                    AND  W-04(4)  AND  W-04(5)
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO  TO      ENT-71
           END-IF
           IF  ACT    NOT  =  1  AND  6
               MOVE  ZERO  TO  W-05T
               CALL "SD_Output" USING
                "CLE-05A" CLE-05A "p" RETURNING RESU
               GO  TO  ENT-330
           END-IF.
       ENT-90.
           MOVE  ZERO   TO  TCNT.
           MOVE  10     TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       ENT-91.
           ADD   1      TO  TCNT  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  TCNT        =  11
               GO  TO  ENT-330
           END-IF.
       ENT-95.
           CALL "SD_Accept" USING BY REFERENCE ACP-05 "ACP-05" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               MOVE 6  TO     I
               MOVE 16 TO     W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               GO  TO  ENT-77
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  ENT-95
           END-IF
           IF  W-05(TCNT)  =  ZERO
               GO  TO  ENT-97
           END-IF
      *
           MOVE  W-05(TCNT)   TO  T-KEY.
      *           READ  T-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO  TO  ENT-95
           END-IF
           CALL "SD_Output" USING "DSP-05" DSP-05 "p" RETURNING RESU.
      *
           MOVE  SPACE       TO  THT-KEY.
           MOVE  W-05(TCNT)  TO  THT-TCD.
           IF  ACT         =  1
               MOVE  W-02        TO  THT-HCD
           ELSE
               MOVE  W-021       TO  THT-HCDF
           END-IF
      *           START  THTM    KEY NOT <   THT-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-91
           END-IF
      *           READ   THTM    NEXT  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-91
           END-IF
           IF  W-05(TCNT) NOT  =  THT-TCD
               GO  TO  ENT-91
           END-IF
           IF  ACT         =  1
               IF  W-02     NOT =  THT-HCD
                   GO  TO  ENT-91
               END-IF
           END-IF
           IF  ACT         =  6
               IF  W-021    NOT =  THT-HCDF
                   GO  TO  ENT-91
               END-IF
           END-IF
           CALL "SD_Output" USING "NOR-M01" NOR-M01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU.
           GO   TO   ENT-95.
       ENT-97.
           IF  TCNT        <=  10
               MOVE  ZERO     TO  W-05(10)
               CALL "SD_Output" USING
                "CLE-050" CLE-050 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  9
               MOVE  ZERO     TO  W-05(09)
               CALL "SD_Output" USING
                "CLE-059" CLE-059 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  8
               MOVE  ZERO     TO  W-05(08)
               CALL "SD_Output" USING
                "CLE-058" CLE-058 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  7
               MOVE  ZERO     TO  W-05(07)
               CALL "SD_Output" USING
                "CLE-057" CLE-057 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  6
               MOVE  ZERO     TO  W-05(06)
               CALL "SD_Output" USING
                "CLE-056" CLE-056 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  5
               MOVE  ZERO     TO  W-05(05)
               CALL "SD_Output" USING
                "CLE-055" CLE-055 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  4
               MOVE  ZERO     TO  W-05(04)
               CALL "SD_Output" USING
                "CLE-054" CLE-054 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  3
               MOVE  ZERO     TO  W-05(03)
               CALL "SD_Output" USING
                "CLE-053" CLE-053 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  2
               MOVE  ZERO     TO  W-05(02)
               CALL "SD_Output" USING
                "CLE-052" CLE-052 "p" RETURNING RESU
           END-IF
           IF  TCNT        <=  1
               MOVE  ZERO     TO  W-05(01)
               CALL "SD_Output" USING
                "CLE-051" CLE-051 "p" RETURNING RESU
           END-IF.
       ENT-330.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  ENT-331
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  ENT-330
           END-IF
           IF  OKC              =  9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "GAM-CLR" GAM-CLR "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-OKC" CLE-OKC "p" RETURNING RESU
               GO  TO  ENT-20
           END-IF
           IF  OKC       NOT  =  1
               GO  TO  ENT-330
           END-IF
           GO  TO  ENT-340.
       ENT-331.
           IF  ACT  =  3
               GO  TO  ENT-30
           ELSE
               IF  ACT     = 8
                    GO  TO  ENT-25
               END-IF
           END-IF
           IF  ACT  NOT  =  1  AND  6
               MOVE 6  TO     I
               MOVE 16 TO     W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               GO  TO  ENT-77
           END-IF
           MOVE  11   TO  TCNT.
           MOVE  21   TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       ENT-332.
           SUBTRACT  1    FROM  TCNT  W-L2.
           IF  TCNT    =   ZERO
               MOVE 6  TO     I
               MOVE 16 TO     W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               GO  TO  ENT-77
           END-IF
           IF  W-05(TCNT)    =  ZERO
               GO  TO  ENT-332
           END-IF
           GO  TO  ENT-95.
       ENT-340.
           PERFORM    UPD-RTN   THRU    UPD-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-03A" CLE-03A "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-05A" CLE-05A "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-OKC" CLE-OKC "p" RETURNING RESU.
           IF  ACT    =   6 OR 7 OR 8
               GO  TO  ENT-25
           END-IF
           GO   TO   ENT-30.
       ENT-EX.
           EXIT.
      ***************************
      *    データ　表示         *
      ***************************
       DIS-RTN.
           IF  THT-SIZ  =  1
               MOVE  2   TO   I
           END-IF
           IF  THT-SIZ  =  2
               MOVE  3   TO   I
           END-IF
           IF  THT-SIZ  =  3
               MOVE  4   TO   I
           END-IF
           IF  THT-SIZ  =  4
               MOVE  5   TO   I
           END-IF
           IF  THT-SIZ  =  9
               MOVE  1   TO   I
           END-IF
           MOVE  THT-T      TO  W-04(I).
           MOVE  THT-TT     TO  W-041(I).
       DIS-010.
      *           READ THTM     NEXT UNLOCK   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   DIS-050
           END-IF
           IF  THT-TCD  NOT = W-01
               GO  TO  DIS-050
           END-IF
           IF  THT-HCD  NOT = W-02
               GO  TO  DIS-050
           END-IF
           IF  THT-SIZ  NOT = 1 AND 2 AND 3 AND 4 AND 9
               GO  TO  DIS-050
           END-IF
           GO  TO  DIS-RTN.
       DIS-050.
           MOVE    1   TO   I.
           MOVE    11  TO   W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       DIS-060.
           CALL "SD_Output" USING "DSP-04" DSP-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-041" DSP-041 "p" RETURNING RESU.
           ADD     1   TO   I  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  I NOT >  5
               GO  TO  DIS-060
           END-IF.
       DIS-EX.
           EXIT.
      ***********************
      *    更新　処理       *
      ***********************
       UPD-RTN.
           IF  ACT   =    1 OR 6
               GO  TO  UPD-020
           END-IF
           MOVE      ZERO        TO    THT-KEY.
           MOVE      W-01        TO    THT-TCD.
           MOVE      W-02        TO    THT-HCD.
      *           START     THTM    KEY NOT < THT-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-020
           END-IF.
       UPD-010.
      *           READ      THTM    NEXT  AT END   GO  TO  UPD-020.
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-020
           END-IF
           IF  THT-TCD NOT = W-01
               GO  TO  UPD-020
           END-IF
           IF  ACT   <    5
               IF  THT-HCD NOT = W-02
                   GO  TO  UPD-020
               END-IF
           END-IF
           IF  ACT   >    4
               IF  THT-HCDF NOT = W-021
                   GO  TO  UPD-020
               END-IF
           END-IF
           PERFORM   DEL-RTN THRU  DEL-EX.
           GO  TO    UPD-010.
       UPD-020.
           IF  ACT   =    3 OR 8
               GO  TO  UPD-EX
           END-IF
           MOVE      1     TO   I.
       UPD-030.
           IF  0        = W-04(I)  AND  W-041(I)
               GO  TO  UPD-050
           END-IF
           INITIALIZE  THT-R.
           MOVE      W-01   TO  THT-TCD THT-TCD2.
           IF  ACT   =    6 OR 7
               MOVE      HI-KEY TO  THT-HCD
           ELSE
               MOVE      W-02   TO  THT-HCD
           END-IF
           IF  I  =   1
               MOVE  9  TO  THT-SIZ
           END-IF
           IF  I  =   2
               MOVE  1  TO  THT-SIZ
           END-IF
           IF  I  =   3
               MOVE  2  TO  THT-SIZ
           END-IF
           IF  I  =   4
               MOVE  3  TO  THT-SIZ
           END-IF
           IF  I  =   5
               MOVE  4  TO  THT-SIZ
           END-IF
           MOVE      W-04(I)    TO    THT-T.
           MOVE      W-041(I)   TO    THT-TT.
           PERFORM   WRI-RTN  THRU  WRI-EX.
       UPD-050.
           ADD       1   TO     I.
           IF  I   NOT >  5
               GO  TO  UPD-030
           END-IF
           IF  ACT     <  5
               GO  TO  UPD-100
           END-IF
      *
      *           READ  HI-M  NEXT RECORD UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-100
           END-IF
           IF  W-021   NOT =  HI-HCD1
               GO  TO  UPD-100
           END-IF
           GO  TO  UPD-020.
       UPD-100.
           IF  ACT    NOT  =    1  AND  6
               GO  TO  UPD-EX
           END-IF
           MOVE  ZERO    TO  TCNT.
       UPD-120.
           ADD   1       TO  TCNT.
           IF  TCNT         =  11
               GO  TO  UPD-EX
           END-IF
           IF  W-05(TCNT)   =  ZERO
               GO  TO  UPD-EX
           END-IF
           MOVE      1     TO   I.
       UPD-130.
           IF  0        = W-04(I)  AND  W-041(I)
               GO  TO  UPD-150
           END-IF
           INITIALIZE  THT-R.
           MOVE  W-05(TCNT) TO  THT-TCD THT-TCD2.
           IF  ACT   =    6
               MOVE      HI-KEY TO  THT-HCD
           ELSE
               MOVE      W-02   TO  THT-HCD
           END-IF
           IF  I  =   1
               MOVE  9  TO  THT-SIZ
           END-IF
           IF  I  =   2
               MOVE  1  TO  THT-SIZ
           END-IF
           IF  I  =   3
               MOVE  2  TO  THT-SIZ
           END-IF
           IF  I  =   4
               MOVE  3  TO  THT-SIZ
           END-IF
           IF  I  =   5
               MOVE  4  TO  THT-SIZ
           END-IF
           MOVE      W-04(I)    TO    THT-T.
           MOVE      W-041(I)   TO    THT-TT.
           PERFORM   WRI-RTN  THRU  WRI-EX.
       UPD-150.
           ADD       1   TO     I.
           IF  I   NOT >  5
               GO  TO  UPD-130
           END-IF
           IF  ACT     <  5
               GO  TO  UPD-120
           END-IF
      *
      *           READ  HI-M  NEXT RECORD UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-120
           END-IF
           IF  W-021   NOT =  HI-HCD1
               GO  TO  UPD-120
           END-IF
           GO  TO  UPD-130.
       UPD-EX.
           EXIT.
      ******************************
      ***   ﾘ ｽ ﾄ   R T N        ***
      ******************************
      **
       LIS-RTN.
           CALL "SD_Output" USING
            "GAM-CLR2" GAM-CLR2 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-PRN" DSP-PRN "p" RETURNING RESU.
       LIS-00.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-90
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-00
           END-IF
           IF  W-SEN  NOT  =  1    AND  2
               GO  TO  LIS-00
           END-IF
      *
           MOVE  SPACE         TO  HEAD21  HEAD23.
           IF  W-SEN       =  1
               MOVE  "得意先"    TO  W-OMSG
               MOVE  "品　名"    TO  W-UMSG
               MOVE    "ｺｰﾄﾞ "     TO  H-TCD21  H-TCD22
               MOVE  "得　意　先　名"  TO  H-TNA21  H-TNA22
               MOVE  SPACE TO HEAD31F HEAD31R
               MOVE    "ｺｰﾄﾞ"      TO  H-CD31F  H-CD31R
               MOVE  "品　　　　　名　　　"  TO  H-NA31F H-NA31R
           ELSE
               MOVE  "品　名"    TO  W-OMSG
               MOVE  "得意先"    TO  W-UMSG
               MOVE    " ｺｰﾄﾞ  "   TO  H-HCD21  H-HCD22
               MOVE  "品　　　　名"  TO  H-HNA21  H-HNA22
               MOVE  SPACE TO HEAD32F HEAD32R
               MOVE    "ｺｰﾄﾞ"      TO  H-CD32F  H-CD32R
               MOVE  "得　意　先　名　　　"  TO  H-NA32F H-NA32R
           END-IF
           CALL "SD_Output" USING
            "DSP-PRN1" DSP-PRN1 "p" RETURNING RESU.
       LIS-01.
           IF  W-SEN       =  1
               CALL "SD_Accept" USING BY REFERENCE ACP-OTF "ACP-OTF"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE ACP-OHF "ACP-OHF"
                "9" "6" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-00
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-01
           END-IF.
       LIS-01A.
           IF  W-SEN       =  1
               CALL "SD_Accept" USING BY REFERENCE ACP-OTT "ACP-OTT"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE ACP-OHT "ACP-OHT"
                "9" "6" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT  =  BTB
               GO  TO LIS-01
           END-IF
           IF  ESTAT  NOT  =   HTB  AND  SKP
               GO  TO  LIS-01A
           END-IF
           IF  W-SEN       =  1
               IF  W-FROM1  >  W-TO1
                    GO  TO  LIS-01
               END-IF
           END-IF
           IF  W-SEN       =  2
               IF  W-FROM2  >  W-TO2
                    GO  TO  LIS-01
               END-IF
           END-IF.
       LIS-01B.
           IF  W-SEN       =  1
               CALL "SD_Accept" USING BY REFERENCE ACP-UHF "ACP-UHF"
                "9" "6" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE ACP-UTF "ACP-UTF"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT       =  BTB
               GO  TO  LIS-01
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-01B
           END-IF.
       LIS-01C.
           IF  W-SEN       =  1
               CALL "SD_Accept" USING BY REFERENCE ACP-UHT "ACP-UHT"
                "9" "6" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE ACP-UTT "ACP-UTT"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT  =  BTB
               GO  TO LIS-01B
           END-IF
           IF  ESTAT  NOT  =   HTB  AND  SKP
               GO  TO  LIS-01C
           END-IF
           IF  W-SEN       =  1
               IF  W-FROM2  >  W-TO2
                    GO  TO  LIS-01B
               END-IF
           END-IF
           IF  W-SEN       =  2
               IF  W-FROM1  >  W-TO1
                    GO  TO  LIS-01B
               END-IF
           END-IF.
       LIS-02.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-01B
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-02
           END-IF
           IF  OKC         =    9
               CALL "SD_Output" USING
                "PRN-CLR" PRN-CLR "p" RETURNING RESU
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-OKC" CLE-OKC "p" RETURNING RESU
               GO  TO  LIS-00
           END-IF
           IF  OKC    NOT  =    1
               GO  TO  LIS-02
           END-IF
           IF  W-SEN  NOT  =    1
               GO  TO  LIS-05
           END-IF.
       LIS-03.
           MOVE ZERO TO THT-KEY.
           MOVE  W-FROM1 TO    THT-TCD.
           MOVE  W-FROM2 TO    THT-HCD.
      *           START   THTM      KEY  IS  NOT  <   THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-90
           END-IF.
       LIS-04.
      *           READ  THTM     NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-90
           END-IF
           IF  THT-TCD  <  W-FROM1
               GO  TO  LIS-04
           END-IF
           IF  THT-TCD  >  W-TO1
               GO  TO  LIS-90
           END-IF
           IF  THT-HCD  <  W-FROM2  OR  >  W-TO2
               GO  TO  LIS-04
           END-IF
           GO  TO  LIS-09.
       LIS-05.
           MOVE  SPACE   TO    THT-KEY2.
           MOVE  W-FROM2 TO    THT-HCD2.
      *           START   THTM      KEY  IS  NOT  <   THT-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-90
           END-IF.
       LIS-06.
      *           READ  THTM     NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-90
           END-IF
           IF  THT-HCD2 <  W-FROM2
               GO  TO  LIS-06
           END-IF
           IF  THT-HCD2 >  W-TO2
               GO  TO  LIS-90
           END-IF
           IF  THT-TCD2 <  W-FROM1  OR  >  W-TO1
               GO  TO  LIS-06
           END-IF.
       LIS-09.
           PERFORM SPC-RTN THRU SPC-EX.
       LIS-10.
           MOVE ZERO TO W-PC.
           IF  W-SEN    =  1
               MOVE  THT-KEY      TO  OLD-KEY
               PERFORM READ-RTN1 THRU READ-EX1
           ELSE
               MOVE  THT-HCD2     TO  W-HCD
               MOVE  THT-SIZ2     TO  W-SIZ
               MOVE  THT-TCD2     TO  W-TCD
               PERFORM READ-RTN2 THRU READ-EX2
           END-IF.
       LIS-15.
           MOVE 0 TO W-PC2.
           IF  W-SEN    =  1
               MOVE  THT-HCD      TO  W-HCD
               PERFORM READ-RTN2 THRU READ-EX2
           ELSE
               MOVE  THT-TCD2     TO  W-TCD
               PERFORM READ-RTN1 THRU READ-EX1
           END-IF.
       LIS-20.
           PERFORM  MEI-RTN     THRU  MEI-EX.
       LIS-25.
      *           READ  THTM     NEXT  UNLOCK  AT  END    GO  TO  LIS-50.
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-50
           END-IF
           IF  W-SEN    =  1
               IF  THT-TCD  >  W-TO1
                   GO  TO  LIS-50
               END-IF
           END-IF
           IF  W-SEN    =  2
               IF  THT-HCD2 >  W-TO2
                   GO  TO  LIS-50
               END-IF
           END-IF
           IF  W-SEN    =  1
               IF  THT-HCD  <  W-FROM2  OR  >  W-TO2
                   GO  TO  LIS-25
               END-IF
           END-IF
           IF  W-SEN    =  2
               IF  THT-TCD2 <  W-FROM1  OR  >  W-TO1
                   GO  TO  LIS-25
               END-IF
           END-IF
           IF  W-SEN    =  1
               IF  THT-TCD NOT = W-TCD
                   GO TO LIS-10
               END-IF
           END-IF
           IF  W-SEN    =  2
               IF  THT-HCD2 NOT = W-HCD
                   GO TO LIS-10
               END-IF
           END-IF
           IF  W-SEN    =  1
               IF  THT-HCD NOT = W-HCD
                   GO TO LIS-15
               END-IF
           END-IF
           IF  W-SEN    =  2
               IF  THT-TCD2 NOT = W-TCD
                   GO TO LIS-15
               END-IF
           END-IF
           GO TO LIS-20.
       LIS-50.
           PERFORM PRI-RTN THRU PRI-EX.
       LIS-90.
           IF  W-POC  =  5
               MOVE  0       TO  W-POC
               CALL "PR_Close" RETURNING RESP
           END-IF.
       LIS-EX.
           EXIT.
      *****************************
      ***    M E I   R T N      ***
      *****************************
       MEI-RTN.
           PERFORM GYO-RTN THRU GYO-EX.
           IF  W-PC1 NOT = 0
               GO TO MEI-10
           END-IF
           IF  W-LD = 58
               PERFORM GYO-RTN THRU GYO-EX
           END-IF
           MOVE 1 TO W-PC1.
           IF  W-SEN    NOT  =  1
               GO  TO  MEI-05
           END-IF
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF1(W-LD)
               MOVE W-TCD TO P-TCD1(W-LD)
               MOVE T-NAME TO P-TNA1(W-LD)
           ELSE
               MOVE SPACE TO P-DR1(W-LD)
               MOVE W-TCD TO P-TCD2(W-LD)
               MOVE T-NAME TO P-TNA2(W-LD)
           END-IF
           PERFORM GYO-RTN THRU GYO-EX.
           GO TO MEI-10.
       MEI-05.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF3(W-LD)
               MOVE SPACE TO P-S12(W-LD)
               MOVE W-HCD TO P-HCD11(W-LD)
               MOVE HI-NAME TO P-HNA11(W-LD)
           ELSE
               MOVE SPACE TO P-DR3(W-LD)
               MOVE SPACE TO P-S22(W-LD)
               MOVE W-HCD TO P-HCD21(W-LD)
               MOVE HI-NAME TO P-HNA21(W-LD)
           END-IF
           PERFORM GYO-RTN THRU GYO-EX.
       MEI-10.
           IF  W-PC2 NOT = 0
               GO TO MEI-30
           END-IF
           MOVE 1 TO W-PC2.
           IF  W-SEN    NOT  =  1
               GO  TO  MEI-15
           END-IF
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF2(W-LD)
               MOVE SPACE TO P-S11(W-LD)
               MOVE W-HCD TO P-HCD1(W-LD)
               MOVE HI-NAME TO P-HNA1(W-LD)
               MOVE THT-SIZ TO P-SIZ1(W-LD)
               MOVE THT-T TO P-TAN1(W-LD)
               MOVE THT-TT TO P-MTN1(W-LD)
           END-IF
           IF  W-CD NOT = 0
               MOVE SPACE TO P-DR2(W-LD)
               MOVE SPACE TO P-S21(W-LD)
               MOVE W-HCD TO P-HCD2(W-LD)
               MOVE HI-NAME TO P-HNA2(W-LD)
               MOVE THT-SIZ TO P-SIZ2(W-LD)
               MOVE THT-T TO P-TAN2(W-LD)
               MOVE THT-TT TO P-MTN2(W-LD)
           END-IF
           GO TO MEI-EX.
       MEI-15.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF4(W-LD)
               MOVE W-TCD TO P-TCD11(W-LD)
               MOVE T-NAME TO P-TNA11(W-LD)
               MOVE THT-SIZ TO P-SIZ11(W-LD)
               MOVE THT-T TO P-TAN11(W-LD)
               MOVE THT-TT TO P-MTN11(W-LD)
           END-IF
           IF  W-CD NOT = 0
               MOVE SPACE TO P-DR4(W-LD)
               MOVE W-TCD TO P-TCD21(W-LD)
               MOVE T-NAME TO P-TNA21(W-LD)
               MOVE THT-SIZ TO P-SIZ21(W-LD)
               MOVE THT-T TO P-TAN21(W-LD)
               MOVE THT-TT TO P-MTN21(W-LD)
           END-IF
           GO TO MEI-EX.
       MEI-30.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF2(W-LD)
               MOVE SPACE TO P-HNA1(W-LD)
               MOVE SPACE TO P-S11(W-LD)
               MOVE THT-SIZ TO P-SIZ1(W-LD)
               MOVE THT-T TO P-TAN1(W-LD)
               MOVE THT-TT TO P-MTN1(W-LD)
           END-IF
           IF  W-CD NOT = 0
               MOVE SPACE TO P-DR2(W-LD)
               MOVE SPACE TO P-HNA2(W-LD)
               MOVE SPACE TO P-S21(W-LD)
               MOVE THT-SIZ TO P-SIZ2(W-LD)
               MOVE THT-T TO P-TAN2(W-LD)
               MOVE THT-TT TO P-MTN2(W-LD)
           END-IF.
       MEI-EX.
           EXIT.
       GYO-RTN.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO GYO-EX
           END-IF
           IF  W-CD = 0
               MOVE 1 TO W-CD W-LD
               MOVE ZERO TO W-PC
               GO TO GYO-EX
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM SPC-RTN THRU SPC-EX.
           GO TO GYO-RTN.
       GYO-EX.
           EXIT.
       PRI-RTN.
           IF  W-POC = 0
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM  MID-010     THRU  MID-EX
           ELSE
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF
           MOVE ZERO TO W-LD.
       PRI-050.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               IF  P-C(W-LD) NOT = SPACE
                   MOVE SPACE TO P-R
                   MOVE W-P(W-LD) TO P-R
                   CALL "PR_Write" USING P-R RETURNING RESP
                   MOVE SPACE TO P-R
                   GO TO PRI-050
               END-IF
           END-IF.
       PRI-EX.
           EXIT.
       SPC-RTN.
           MOVE SPACE TO W-PR.
           MOVE ZERO TO W-LD.
       SPC-050.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE  15K         TO  P-CD1(W-LD)
               MOVE  20K         TO  P-CD2(W-LD)
               MOVE SPACE TO P-HNA1(W-LD) P-HNA2(W-LD)
                             P-S11(W-LD) P-S21(W-LD)
               GO TO SPC-050
           END-IF
           MOVE ZERO TO W-LD W-CD W-PC.
       SPC-EX.
           EXIT.
      ******************************
      ***   ｺ ﾋﾟ-   R T N        ***
      ******************************
      **
       CPY-RTN.
           CALL "SD_Output" USING
            "GAM-CLR2" GAM-CLR2 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-CPY" DSP-CPY "p" RETURNING RESU.
       CPY-01.
           CALL "SD_Accept" USING BY REFERENCE ACP-MTCD "ACP-MTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  CPY-EX
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  CPY-01
           END-IF
           MOVE  W-MTCD   TO  T-KEY.
      *           READ  T-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO  TO  CPY-01
           END-IF
           CALL "SD_Output" USING
            "DSP-MTNA" DSP-MTNA "p" RETURNING RESU.
       CPY-01A.
           CALL "SD_Accept" USING BY REFERENCE ACP-STCD "ACP-STCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  BTB
               GO  TO CPY-01
           END-IF
           IF  ESTAT  NOT  =   HTB  AND  SKP
               GO  TO  CPY-01A
           END-IF
           MOVE  W-STCD   TO  T-KEY.
      *           READ  T-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO  TO  CPY-01A
           END-IF
           CALL "SD_Output" USING
            "DSP-STNA" DSP-STNA "p" RETURNING RESU.
           IF  W-MTCD   =  W-STCD
               GO  TO  CPY-01A
           END-IF.
       CPY-02.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  CPY-01A
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  CPY-02
           END-IF
           IF  OKC         =    9
               CALL "SD_Output" USING
                "CPY-CLR" CPY-CLR "p" RETURNING RESU
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-OKC" CLE-OKC "p" RETURNING RESU
               GO  TO  CPY-01
           END-IF
           IF  OKC    NOT  =    1
               GO  TO  CPY-02
           END-IF
      *
           MOVE ZERO TO WR-KEY.
           MOVE W-MTCD TO WR-01.
       CPY-03.
           MOVE ZERO TO THT-KEY.
           MOVE  W-STCD  TO    THT-TCD.
      *           START   THTM      KEY  IS  NOT  <   THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  CPY-10
           END-IF.
       CPY-04.
      *           READ  THTM     NEXT          AT  END    GO  TO  CPY-10.
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  CPY-10
           END-IF
           IF  THT-TCD  NOT = W-STCD
               GO  TO  CPY-10
           END-IF
           PERFORM   DEL-RTN THRU  DEL-EX.
           GO  TO  CPY-04.
       CPY-10.
           MOVE ZERO TO THT-KEY.
           MOVE  WR-KEY  TO    THT-KEY.
      *           START   THTM      KEY  IS  NOT  <   THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  CPY-EX
           END-IF.
       CPY-15.
      *           READ  THTM     NEXT  UNLOCK  AT  END    GO  TO  CPY-EX.
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  CPY-EX
           END-IF
           IF  THT-TCD  NOT = W-MTCD
               GO  TO  CPY-EX
           END-IF
           IF  THT-KEY      = WR-KEY
               GO  TO  CPY-15
           END-IF
           MOVE THT-KEY  TO WR-KEY.
           MOVE W-STCD TO THT-TCD THT-TCD2.
           PERFORM WRI-RTN THRU WRI-EX.
           GO TO CPY-10.
       CPY-EX.
           EXIT.
      *****************************
      ***   R E A D   R T N 1   ***
      *****************************
       READ-RTN1.
           MOVE  W-TCD       TO  T-KEY.
      ***  得意先マスタ　ＲＥＡＤ
      *           READ  T-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  T-NAME
           END-IF.
       READ-EX1.
           EXIT.
      *****************************
      ***   R E A D   R T N 2   ***
      *****************************
       READ-RTN2.
           MOVE  W-HCD       TO  HI-KEY.
      ***  品名ＩＮＤＥＸＥＤマスタ　ＲＥＡＤ
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  HI-NAME
           END-IF.
       READ-EX2.
           EXIT.
      *****************************
      ***    G E T   R T N      ***
      *****************************
      *
      *    ﾋﾝﾒｲ ｲﾝﾃﾞｯｸｽ ﾄｳﾛｸ   *
      *
       WRI-RTN.
      *           WRITE     THT-R     INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               MOVE  "THTM1"     TO  ERR-F
               MOVE  "W"         TO  ERR-M
               MOVE  THT-KEY     TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *
      *    ﾋﾝﾒｲ ｲﾝﾃﾞｯｸｽ ｻｸｼﾞｮ   *
      *
       DEL-RTN.
      *           DELETE    THTM       INVALID KEY
      *///////////////
           CALL "DB_Delete" USING THTM_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "THTM1  "    TO  ERR-F
               MOVE  "D"          TO  ERR-M
               MOVE  THT-KEY      TO  ERR-K
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  050
           END-IF.
      *
       DEL-EX.
           EXIT.
      *****************************
      ***    ﾐ ﾀﾞ ｼ  R T N      ***
      *****************************
      **
       MID-RTN.
           MOVE   SPACE   TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
           ADD   1   TO    PCNT.
           MOVE  PCNT  TO  WPCNT.
           MOVE   HEAD1    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   HEAD2    TO    P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   HEAD3    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
       MID-EX.
           EXIT.
      *
      *****************************
      ***  ﾋﾝﾒｲ(ｲﾛﾅｼ) ｾｯﾄ       ***
      *****************************
      **
       NAM-RTN.
           MOVE  ZERO  TO  W-C.
           MOVE ALL "　"  TO  W-NAME W-HNAD.
           MOVE  HI-NAME TO  W-HNAD.
       NAM-10.
           ADD   1     TO  W-C.
           IF  W-C     >  24
               GO  TO  NAM-EX
           END-IF
           MOVE   W-HNA(W-C) TO W-NA(W-C).
           IF  W-HNA(W-C) NOT = SPACE
               GO TO NAM-10
           END-IF
           ADD   1     TO  W-C.
           IF  W-C     >  24
               GO  TO  NAM-EX
           END-IF
           MOVE   W-HNA(W-C) TO W-NA(W-C).
           IF  W-HNA(W-C) NOT = SPACE
               GO TO NAM-10
           END-IF.
       NAM-EX.
           EXIT.
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       COPY  LPMSG.
