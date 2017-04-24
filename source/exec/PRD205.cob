       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PRD205.
      ******************************************************************
      ***  仕訳伝票チェックリスト(手形･領収書要･売掛金･買掛金支払)   ***
      ******************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC X(02).
       77  PCNT                PIC 9(05).
       77  PRINT-CNT           PIC 9(05).
       77  INV-SW              PIC 9(01).
       77  WKSPACE             PIC X(20) VALUE SPACE.
       77  WKZERO              PIC 9(10) VALUE ZERO.
       77  K20                 PIC X(05) VALUE X"1A24212474".
       77  K15                 PIC X(05) VALUE X"1A24212078".
       77  PCHK                PIC 9(01) VALUE 0.
       01  CRT-WK.
           02  W-01            PIC 9(01).
           02  W-02.
             03  W-02F         PIC 9(06).
             03  W-02T         PIC 9(06).
           02  W-OKC           PIC X(01).
       01  TOT-WK.
           02  TOT-KR          PIC S9(09).
           02  TOT-KS          PIC S9(09).
       01  PRI-WK.
           02  WSDWTEKI.
             03  WSDWTEKI1     PIC N(10).
             03  WSDWTEKI2     PIC N(10).
           02  WSDWTEKIR REDEFINES WSDWTEKI     PIC N(20).
           02  O-SDWYMD.
             03  O-SDWYY       PIC 9(04).
             03  O-SDWMM       PIC 9(02).
             03  O-SDWDD       PIC 9(02).
           02  O-SDWJNO        PIC 9(06).
           02  SYS-DATE.
             03  SYS-YY        PIC 9(02).
             03  SYS-MM        PIC 9(02).
             03  SYS-DD        PIC 9(02).
           02  W-YMD.
             03  W-YY          PIC 9(04).
             03  W-YYL   REDEFINES W-YY.
               04  W-YY1       PIC 9(02).
               04  W-YY2       PIC 9(02).
             03  W-MM          PIC 9(02).
             03  W-DD          PIC 9(02).
           02  WK-ZZ           PIC Z9.
           02  WK-ZZZZZ        PIC ZZZZ9.
           02  W-SKD           PIC 9(08).
      *****
       01  MID03.
           02  F               PIC X(5)    VALUE   X"1A24212474".
           02  F               PIC X(2)    VALUE  SPACE.
           02  M-YY            PIC N(2).
           02  F               PIC N(1)    VALUE  "年".
           02  M-MM            PIC N(2).
           02  F               PIC N(1)    VALUE  "月".
           02  M-DD            PIC N(2).
           02  F               PIC N(3)    VALUE  "日作成".
           02  F               PIC X(14)   VALUE   SPACE.
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(25)   VALUE
               "仕訳伝票入力リスト（手形，領収書要，売掛金，買掛金".
           02  F               PIC N(03)   VALUE   "関係）".
           02  F               PIC X(28)   VALUE   SPACE.
           02  M-PCNT          PIC N(5).
           02  F               PIC N(1)    VALUE   "頁".
       01  MID07.
           02  F               PIC X(18)   VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(44)   VALUE
               "　　　　　　   借　　　　 方 　　　　　　　 ".
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(44)   VALUE
               "　　　　　　   貸　　　　 方　　　　　　　　".
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC X(11)   VALUE   "取 引 先 名".
           02  F               PIC X(3)    VALUE   SPACE.
           02  F               PIC N(3)    VALUE   "取引先".
           02  F               PIC X(5)    VALUE   SPACE.
       01  MID09.
           02  F               PIC N(4)    VALUE   "日　　付".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(04)   VALUE   "伝票番号".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(5)    VALUE   "科　目　名".
           02  F               PIC X(4)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "科目".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "部門".
           02  F               PIC X(11)   VALUE   SPACE.
           02  F               PIC X(7)    VALUE   "金 　額".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(5)    VALUE   "科　目　名".
           02  F               PIC X(4)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "科目".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "部門".
           02  F               PIC X(11)   VALUE   SPACE.
           02  F               PIC X(7)    VALUE   "金 　額".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(1)    VALUE   "摘".
           02  F               PIC X(7)    VALUE   SPACE.
           02  F               PIC N(1)    VALUE   "要".
           02  F               PIC X(4)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "変換".
           02  F               PIC X(6)    VALUE   SPACE.
       01  MID10.
           02  F               PIC X(18)   VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(5)    VALUE   "補　助　名".
           02  F               PIC X(4)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "補助".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "入金".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(3)    VALUE   "請求日".
           02  F               PIC X(12)   VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC N(5)    VALUE   "補　助　名".
           02  F               PIC X(4)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "補助".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "入金".
           02  F               PIC X(1)    VALUE   SPACE.
           02  F               PIC N(3)    VALUE   "請求日".
           02  F               PIC X(12)   VALUE   SPACE.
           02  F               PIC X(01)   VALUE   ":".
           02  F               PIC X(2)    VALUE   SPACE.
           02  F               PIC X(01)   VALUE   "(".
           02  F               PIC N(5)    VALUE   "請求書日付".
           02  F               PIC X(01)   VALUE   ")".
           02  F               PIC X(3)    VALUE   SPACE.
           02  F               PIC N(2)    VALUE   "販財".
           02  F               PIC N(3)    VALUE   "手他№".
       01  MID12.
           02  F               PIC X(47)   VALUE
                "------------------:----------------------------".
           02  F               PIC X(50)   VALUE
                "----------------:---------------------------------".
           02  F               PIC X(39)   VALUE
                "-----------:---------------------------".
      *
           COPY LWMSG_PR.
           COPY SIWAIW.
           COPY KANGEL.
           COPY L-BANK.
      **
       01  P-F.
           02  P-R                 PIC X(200).
           02  P1-R                REDEFINES    P-R.
               03  P1-K15          PIC X(05).
               03  P1-01.
                 04  P1-01Y        PIC 99.
                 04  P1-01YH       PIC X(01).
                 04  P1-01M        PIC Z9.
                 04  P1-01MH       PIC X(01).
                 04  P1-01D        PIC Z9.
               03  FILLER          PIC X(01).
               03  P1-02           PIC 9(06).
               03  P1-02H          PIC X(01).
               03  P1-03           PIC 9(02).
               03  P1-V1           PIC X(01).
               03  P1-04           PIC N(10).
               03  FILLER          PIC X(01).
               03  P1-05           PIC 9(04).
               03  FILLER          PIC X(01).
               03  P1-06           PIC 9(04).
               03  P1-06R REDEFINES P1-06.
                 04  P1-06R1       PIC 9(02).
                 04  FILLER        PIC X(01).
                 04  P1-06R2       PIC Z(01).
               03  FILLER          PIC X(01).
               03  P1-09.
                 04  P1-09Y        PIC 99.
                 04  P1-09YH       PIC X(01).
                 04  P1-09M        PIC Z9.
               03  P1-07           PIC ----,---,--9.
               03  P1-08           PIC X(01).
               03  P1-V2           PIC X(01).
               03  P1-14           PIC N(10).
               03  FILLER          PIC X(01).
               03  P1-15           PIC 9(04).
               03  FILLER          PIC X(01).
               03  P1-16           PIC 9(04).
               03  P1-16R REDEFINES P1-16.
                 04  P1-16R1       PIC 9(02).
                 04  FILLER        PIC X(01).
                 04  P1-16R2       PIC Z(01).
               03  FILLER          PIC X(01).
               03  P1-19.
                 04  P1-19Y        PIC 99.
                 04  P1-19YH       PIC X(01).
                 04  P1-19M        PIC Z9.
               03  P1-17           PIC ----,---,--9.
               03  P1-18           PIC X(01).
               03  P1-V3           PIC X(01).
               03  P1-20           PIC N(10).
               03  FILLER          PIC X(01).
               03  P1-K20          PIC X(05).
               03  P1-21           PIC 9(05).
               03  P1-21R REDEFINES P1-21.
                 04  FILLER        PIC X(01).
                 04  P1-21R1       PIC N(01).
                 04  P1-21R2       PIC N(01).
               03  P1-22           PIC 9(06).
           02  P4-R                REDEFINES    P-R.
               03  P4-K20          PIC X(05).
               03  FILLER          PIC X(09).
               03  P4-01           PIC X(08).
               03  FILLER          PIC X(01).
               03  P4-V1           PIC X(01).
               03  FILLER          PIC X(31).
               03  P4-02           PIC ----,---,--9B.
               03  P4-V2           PIC X(01).
               03  FILLER          PIC X(31).
               03  P4-12           PIC ----,---,--9B.
               03  P4-V3           PIC X(01).
               03  FILLER          PIC X(03).
               03  P4-FC           PIC X(01).
               03  P4-31           PIC 9999/99/99.
               03  P4-RC           PIC X(01).
               03  FILLER          PIC X(12).
       77  F                       PIC X(001).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER  PIC  N(26) VALUE
               "仕訳伝票入力リスト（手形，領収書要，売掛金，買掛金）".
           02  FILLER  PIC  N(04) VALUE  "出力選択".
           02  FILLER.
               03  FILLER  PIC  X(03) VALUE  "(1)".
               03  FILLER  PIC  N(04) VALUE  "全　　件".
           02  FILLER.
               03  FILLER  PIC  X(03) VALUE  "(2)".
               03  FILLER  PIC  N(04) VALUE  "修正".
               03  FILLER  PIC  X(01) VALUE  "･".
               03  FILLER  PIC  N(03) VALUE  "追加分".
           02  FILLER.
               03  FILLER  PIC  X(03) VALUE  "(3)".
               03  FILLER  PIC  N(09) VALUE  "伝票番号の範囲指定".
               03  FILLER  PIC  X(07) VALUE  "--->( )".
           02  FILLER.
               03  FILLER  PIC  N(06) VALUE  "伝票番号指定".
               03  FILLER  PIC  X(01) VALUE  "[".
               03  FILLER  PIC  N(01) VALUE  "～".
               03  FILLER  PIC  X(01) VALUE  "]".
           02  FILLER.
               03  FILLER  PIC  N(02) VALUE  "確認".
               03  FILLER  PIC  X(13) VALUE  "OK=1,NO=9 ( )".
      ***
       01  ACP-AREA.
           02  ACP-W01         PIC 9(01).
           02  ACP-W02F        PIC 9(06).
           02  ACP-W02T        PIC 9(06).
           02  ACP-WOKC        PIC X(01).
      ***
       01  DSP-SP-AREA.
           02  SP-W02F         PIC Z(06).
           02  SP-W02T         PIC Z(06).
           02  SP-WOKC         PIC X(01).
      *
       COPY LSMSG_PR.
      *
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-AREA" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-AREA" "RN" "1" "14" "52" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-AREA" "N" "4" "21" "8" "01DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DISP-AREA" " " "6" "0" "11" "02DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DISP-AREA" "X" "6" "21" "3" " " "03DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DISP-AREA" "N" "6" "24" "8" "04DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DISP-AREA" " " "7" "0" "18" "03DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DISP-AREA" "X" "7" "21" "3" " " "06DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DISP-AREA" "N" "7" "24" "8" "07DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DISP-AREA" "X" "7" "28" "1" "08DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DISP-AREA" "N" "7" "29" "6" "09DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11DISP-AREA" " " "8" "0" "28" "06DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12DISP-AREA" "X" "8" "21" "3" " " "11DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "13DISP-AREA" "N" "8" "24" "18" "12DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14DISP-AREA" "X" "8" "42" "7" "13DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15DISP-AREA" " " "12" "0" "16" "11DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16DISP-AREA" "N" "12" "21" "12" " " "15DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "17DISP-AREA" "X" "12" "33" "1" "16DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18DISP-AREA" "N" "12" "40" "2" "17DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "19DISP-AREA" "X" "12" "48" "1" "18DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "20DISP-AREA" " " "24" "0" "17" "15DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "21DISP-AREA" "N" "24" "61" "4" " " "20DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "22DISP-AREA" "X" "24" "66" "13" "21DISP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01" "9" "8" "47" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01" BY REFERENCE W-01 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W02F" "9" "12" "34" "6" "ACP-W01" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W02F" BY REFERENCE W-02F "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W02T" "9" "12" "42" "6" "ACP-W02F" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W02T" BY REFERENCE W-02T "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-WOKC" "X" "24" "77" "1" "ACP-W02T" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-SP-AREA
       CALL "SD_Init" USING
            "DSP-SP-AREA" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W02F" "Z" "12" "34" "6" " " "DSP-SP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "SP-W02F" BY REFERENCE WKZERO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W02T" "Z" "12" "42" "6" "SP-W02F" " " RETURNING RESU.
       CALL "SD_From" USING
            "SP-W02T" BY REFERENCE WKZERO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-WOKC" "X" "24" "77" "1" "SP-W02T" " " RETURNING RESU.
       CALL "SD_From" USING
            "SP-WOKC" BY REFERENCE WKSPACE "20" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
       MR010.
           PERFORM W01-RTN THRU W01-EX.
           IF  ESTAT = "P9"
               GO TO MR999
           END-IF.
       MR020.
           PERFORM W02-RTN THRU W02-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR030.
           PERFORM WOKC-RTN THRU WOKC-EX.
           IF  ESTAT = "09"
               CALL "SD_Output" USING "SP-WOKC" SP-WOKC "p"
                                              RETURNING RESU
               GO TO MR020
           END-IF.
           IF  W-OKC = "9"
               CALL "SD_Output" USING "SP-WOKC" SP-WOKC "p"
                                              RETURNING RESU
               GO TO MR010
           END-IF.
      *
           PERFORM OPEN-RTN THRU OPEN-EX.
      *
       MR100.
      *           READ SDW NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               PERFORM TOT1-RTN THRU TOT1-EX
               GO TO MR900
           END-IF.
           IF  W-01 = 2
               IF  SDWSIN NOT = SPACE
                   GO TO MR100
               END-IF
           END-IF.
           IF  W-01 = 3
               IF (SDWJNO < W-02F) OR
                  (SDWJNO > W-02T)
                   GO TO MR100
               END-IF
           END-IF.
           IF  PCNT = ZERO
               GO TO MR110
           END-IF.
           IF  SDWYMD NOT = O-SDWYMD
               PERFORM TOT1-RTN THRU TOT1-EX
               GO TO MR110
           END-IF.
           IF  SDWJNO NOT = O-SDWJNO
               PERFORM TOT1-RTN THRU TOT1-EX
           END-IF.
       MR110.
           IF  SDWLNO = 1
               PERFORM TKO-RTN THRU TKO-EX
           END-IF.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM ADD-RTN THRU ADD-EX.
           PERFORM UPD-RTN THRU UPD-EX.
           MOVE SDWYMD     TO O-SDWYMD.
           MOVE SDWJNO     TO O-SDWJNO.
           GO TO MR100.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
      *****
      *****
       INI-RTN.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-AREA" DISP-AREA "p"
                                         RETURNING RESU.
           ACCEPT SYS-DATE     FROM DATE.
           MOVE SYS-YY     TO M-YY.
           MOVE SYS-MM     TO WK-ZZ.
           MOVE WK-ZZ      TO M-MM.
           MOVE SYS-DD     TO WK-ZZ.
           MOVE WK-ZZ      TO M-DD.
       INI-EX.
           EXIT.
      ***
       W01-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W01 "ACP-W01" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W01-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W01-RTN
           END-IF.
           IF  W-01 NOT = 1 AND 2 AND 3
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W01-RTN
           END-IF.
       W01-EX.
           EXIT.
      ***
       W02-RTN.
           IF  W-01 NOT = 3
               MOVE ZERO     TO W-02
               CALL "SD_Output" USING "SP-W02F" SP-W02F "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "SP-W02T" SP-W02T "p"
                                              RETURNING RESU
               GO TO W02-EX
           END-IF.
       W02-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W02F "ACP-W02F" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W02-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02-RTN
           END-IF.
       W02-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W02T "ACP-W02T" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W02-000
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02-100
           END-IF.
           IF  W-02F > W-02T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO W02-100
           END-IF.
       W02-EX.
           EXIT.
      ***
       WOKC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-WOKC "ACP-WOKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO WOKC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO WOKC-RTN
           END-IF.
           IF  W-OKC NOT = "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO WOKC-RTN
           END-IF.
       WOKC-EX.
           EXIT.
      ***
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BM_PNAME1 "SHARED" BY REFERENCE BM_IDLST "1"
            "BM-KEY" BY REFERENCE BM-KEY.
           CALL "DB_F_Open" USING
            "I-O SEQUENTIAL" SDW_PNAME1 "SHARED" BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "PR_Open" RETURNING RESP.
       OPEN-EX.
           EXIT.
      ***
       TKO-RTN.
           MOVE SDWSKD TO W-SKD.
           IF  SDWCUST = ZERO
               GO TO TKO-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF (LINAGECOUNTER > 62) OR
              (PCNT = ZERO)
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE      TO P-R.
           MOVE SDWNAMEN     TO P1-20.
           MOVE SDWCUST      TO P1-21.
           MOVE SPACE        TO P1-04 P1-14.
           MOVE ":"          TO P1-V1 P1-V2 P1-V3.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       TKO-EX.
           EXIT.
      ***
       PRI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF (LINAGECOUNTER > 62) OR
              (PCNT = ZERO)
               PERFORM MID-RTN THRU MID-EX
           END-IF.
      *
           MOVE SPACE      TO P-R.
           MOVE SDWYMD     TO W-YMD.
           IF  SDWYMD = O-SDWYMD
               GO TO PRI-030
           END-IF.
           MOVE W-YY2    TO P1-01Y.
           MOVE "."      TO P1-01YH.
           MOVE W-MM     TO P1-01M.
           MOVE "."      TO P1-01MH.
           MOVE W-DD     TO P1-01D.
           GO TO PRI-040.
       PRI-030.
           IF  SDWJNO = O-SDWJNO
               GO TO PRI-050
           END-IF.
       PRI-040.
           MOVE SDWJNO     TO P1-02.
           MOVE "-"        TO P1-02H.
       PRI-050.
           MOVE SDWLNO     TO P1-03.
           MOVE SDWTEKI    TO WSDWTEKIR.
      *
           IF  KRCDMW = ZERO
               MOVE SPACE     TO P1-04
           ELSE
               MOVE KRCDMW    TO K-ACCD
               MOVE ZERO      TO K-HOCD
               PERFORM KNGG-RTN THRU KNGG-EX
               MOVE KNGNMN    TO P1-04
           END-IF.
           IF  KRCDMW NOT = ZERO
               MOVE KRCDMW    TO P1-05
           END-IF.
           IF  KRSECTW NOT = ZERO
               MOVE KRSECTW   TO P1-06
           END-IF.
           IF  KRKINW NOT = ZERO
               MOVE KRKINW    TO P1-07
           END-IF.
           MOVE SPACE     TO P1-08.
           IF  KRTAXW    = "1" OR "5"
               MOVE "*"       TO P1-08
           END-IF.
           IF  KRTAXW    = "3" OR "7"
               MOVE "#"       TO P1-08
           END-IF.
      **
           IF  KSCDMW = ZERO
               MOVE SPACE     TO P1-14
           ELSE
               MOVE KSCDMW    TO K-ACCD
               MOVE ZERO      TO K-HOCD
               PERFORM KNGG-RTN THRU KNGG-EX
               MOVE KNGNMN    TO P1-14
           END-IF.
           IF  KSCDMW NOT = ZERO
               MOVE KSCDMW    TO P1-15
           END-IF.
           IF  KSSECTW NOT = ZERO
               MOVE KSSECTW   TO P1-16
           END-IF.
           IF  KSKINW NOT = ZERO
               MOVE KSKINW    TO P1-17
           END-IF.
           MOVE SPACE     TO P1-18.
           IF  KSTAXW    = "1" OR "5"
               MOVE "*"       TO P1-18
           END-IF.
           IF  KSTAXW    = "3" OR "7"
               MOVE "#"       TO P1-18
           END-IF.
           MOVE WSDWTEKI1    TO P1-20.
           IF (KRCDMW    = 0140) AND (KSCDMW < 5000 OR > 5999)
               MOVE SDWNKCD   TO P1-06R1
               MOVE SDWNSC    TO P1-06R2
               IF  SDWSKNG   NOT = ZERO
                   MOVE SDWSKNR   TO P1-09Y
                   MOVE "/"       TO P1-09YH
                   MOVE SDWSKGR   TO P1-09M
               END-IF
           END-IF.
           IF (KSCDMW    = 0140) AND (KRCDMW < 5000 OR > 5999)
               MOVE SDWNKCD   TO P1-16R1
               MOVE SDWNSC    TO P1-16R2
               IF  SDWSKNG   NOT = ZERO
                   MOVE SDWSKNR   TO P1-19Y
                   MOVE "/"       TO P1-19YH
                   MOVE SDWSKGR   TO P1-19M
               END-IF
           END-IF.
           IF  SDWHHC    =  0
               MOVE "未"    TO P1-21R1
           END-IF.
           IF  SDWZHC    =  0
               MOVE "未"    TO P1-21R2
           END-IF.
           IF  SDWTENO   NOT =  ZERO
               MOVE SDWTENO   TO P1-22
           END-IF.
           IF  PCHK    =  5
               MOVE 0        TO PCHK
               MOVE "."      TO P1-01YH  P1-01MH
               MOVE W-YY2    TO P1-01Y
               MOVE W-MM     TO P1-01M
               MOVE W-DD     TO P1-01D
           END-IF.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           MOVE ":"          TO P1-V1 P1-V2 P1-V3.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
      *
           IF (KRCDSW = ZERO) AND
              (KSCDSW = ZERO) AND
              (WSDWTEKI2 = SPACE)
               GO TO PRI-100
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           IF  KRCDSW = ZERO
               MOVE SPACE     TO P1-04
           ELSE
               MOVE KRCDW     TO KNG-KEY
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  INV-SW = 0
                   MOVE KNGNMN    TO P1-04
               ELSE
                   MOVE KRCDSW    TO BM-KEY
                   PERFORM BMG-RTN THRU BMG-EX
                   MOVE BANKNMN   TO P1-04
               END-IF
           END-IF.
           IF  KRCDSW NOT = ZERO
               MOVE KRCDSW    TO P1-05
           END-IF.
      **
           IF  KSCDSW = ZERO
               MOVE SPACE     TO P1-14
           ELSE
               MOVE KSCDW     TO KNG-KEY
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  INV-SW = 0
                   MOVE KNGNMN    TO P1-14
               ELSE
                   MOVE KSCDSW    TO BM-KEY
                   PERFORM BMG-RTN THRU BMG-EX
                   MOVE BANKNMN   TO P1-14
               END-IF
           END-IF.
           IF  KSCDSW NOT = ZERO
               MOVE KSCDSW    TO P1-15
           END-IF.
           MOVE WSDWTEKI2    TO P1-20.
           MOVE ":"          TO P1-V1 P1-V2 P1-V3.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       PRI-100.
      *
       PRI-EX.
           EXIT.
      ***
       ADD-RTN.
           ADD KRKINW    TO TOT-KR.
           ADD KSKINW    TO TOT-KS.
           ADD 1         TO PRINT-CNT.
       ADD-EX.
           EXIT.
      ***
       MID-RTN.
           IF  PCNT NOT = ZERO
               MOVE SPACE       TO P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD 1     TO PCNT.
           MOVE PCNT     TO WK-ZZZZZ.
           MOVE WK-ZZZZZ     TO M-PCNT.
           MOVE MID03 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID07 TO P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID09 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID10 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID12 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
           MOVE 5         TO PCHK.
       MID-EX.
           EXIT.
      ***
       TOT1-RTN.
           IF  PCNT = ZERO
               GO TO TOT1-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE "伝 票 計"     TO P4-01.
           PERFORM OUT-RTN THRU OUT-EX.
           MOVE ZERO     TO TOT-KR TOT-KS PRINT-CNT.
       TOT1-EX.
           EXIT.
      ***
       OUT-RTN.
           MOVE TOT-KR        TO P4-02.
           MOVE TOT-KS        TO P4-12.
           MOVE ":"          TO P4-V1 P4-V2 P4-V3.
           IF  W-SKD NOT = ZERO
               MOVE "(" TO P4-FC
               MOVE ")" TO P4-RC
               MOVE W-SKD TO P4-31
           END-IF.
           MOVE K20           TO P4-K20.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
       OUT-000.
           MOVE SPACE     TO P-R.
           MOVE ":"          TO P4-V1 P4-V2 P4-V3.
           MOVE K20        TO P4-K20.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       OUT-EX.
           EXIT.
      ***
       UPD-RTN.
           MOVE "1"   TO SDWSIN.
           MOVE SDW-KEY     TO ERR-K.
      *           REWRITE SDW-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SDW_PNAME1 SDW_LNAME SDW-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDW"     TO ERR-F
               MOVE "R"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       UPD-EX.
           EXIT.
      ***
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE BM_IDLST BM_PNAME1.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
       CLSE-EXT.
           EXIT.
      ***
       KNGG-RTN.
           MOVE 0     TO INV-SW.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1         TO INV-SW
               MOVE SPACE     TO KNGNMN
           END-IF.
       KNGG-EX.
           EXIT.
      ***
       BMG-RTN.
      *           READ BM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BM_PNAME1 BY REFERENCE BM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE     TO BANKNMN
           END-IF.
       BMG-EX.
           EXIT.
      *****
       COPY LPMSG_PR.
      *****
