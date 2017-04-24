       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      JT295L.
      *********************************************
      *    PROGRAM         :    品名別受注他残帳  *
      *    DATA WRITTEN    :    98/10/26          *
      *    PRINTER TYPE    :    JIPS              *
      *    COMPILE TYPE    :    COBOL74           *
      *********************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       SOURCE-COMPUTER.                 SYSTEM150.
       OBJECT-COMPUTER.                 SYSTEM150.
       DATA                             DIVISION.
       WORKING-STORAGE                  SECTION.
       01  HEAD1.
           02  W-20K               PIC  X(05)  VALUE  X"1A24212474".
           02  F                   PIC  X(26)  VALUE  SPACE.
           02  H-MID               PIC  X(50)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  H-SEN               PIC  N(05).
           02  F                   PIC  X(17)  VALUE  SPACE.
           02  H-NEN               PIC  9(02).
           02  F                   PIC  N(01)  VALUE  "年".
           02  H-GET               PIC  Z9.
           02  F                   PIC  N(01)  VALUE  "月".
           02  H-PEY               PIC  Z9.
           02  F                   PIC  N(01)  VALUE  "日".
           02  F                   PIC  X(12)  VALUE  "          P.".
           02  H-PAGE              PIC  ZZ9.
       01  HEAD2.
           02  W-15K               PIC  X(05)  VALUE  X"1A24212078".
           02  F                   PIC  X(07)  VALUE  " ｺｰﾄﾞ  ".
           02  F                   PIC  N(08)  VALUE
               "品　　　　　名　".
           02  F                   PIC  X(26)  VALUE  SPACE.
           02  F                   PIC  X(01)  VALUE  "1".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "３号".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "２号".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "１号".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "０号".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "　中".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "　大".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(02)  VALUE  "特大".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "28.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "29.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "30.0".
           02  F                   PIC  X(18)  VALUE  SPACE.
       01  HEAD3.
           02  F                   PIC  X(45)  VALUE  SPACE.
           02  F                   PIC  X(01)  VALUE  "2".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "12.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "13.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "13.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "14.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "15.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "16.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "17.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "18.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "19.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "20.0".
           02  F                   PIC  X(18)  VALUE  SPACE.
       01  HEAD4.
           02  F                   PIC  X(45)  VALUE  SPACE.
           02  F                   PIC  X(01)  VALUE  "3".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "21.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "21.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "22.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "22.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "23.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "23.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "24.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "24.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "25.0".
           02  F                   PIC  X(25)  VALUE  SPACE.
       01  HEAD5.
           02  F                   PIC  X(45)  VALUE  SPACE.
           02  F                   PIC  X(01)  VALUE  "4".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "24.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "24.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "25.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "25.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "26.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "26.5".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "27.0".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  X(04)  VALUE  "27.5".
           02  F                   PIC  X(16)  VALUE  SPACE.
           02  F                   PIC  N(04)  VALUE  "　小　計".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  F                   PIC  N(04)  VALUE  "　合　計".
       01  W-MID.
           02  W-MID1.
               03  F               PIC  X(07)  VALUE  SPACE.
               03  F               PIC  N(18)  VALUE
                   "＊＊＊　　品名別　受注残帳　　＊＊＊".
               03  F               PIC  X(07)  VALUE  SPACE.
           02  W-MID2.
               03  F               PIC  X(07)  VALUE  SPACE.
               03  F               PIC  N(18)  VALUE
                   "＊＊＊　　品名別　預り残帳　　＊＊＊".
               03  F               PIC  X(07)  VALUE  SPACE.
           02  W-MID3.
               03  F               PIC  X(06)  VALUE  SPACE.
               03  F               PIC  N(19)  VALUE
                   "＊＊＊　　品名別　取よけ残帳　　＊＊＊".
               03  F               PIC  X(06)  VALUE  SPACE.
           02  W-MID4.
               03  F               PIC  N(20)  VALUE
                   "＊＊＊　　品名別　受注・預り・取よけ残帳".
               03  F               PIC  N(05)  VALUE  "　　＊＊＊".
       01  W-P.
           02  P-15K               PIC  X(05).
           02  P-HCD               PIC  9(06).
           02  F                   PIC  X(01).
           02  P-NAME              PIC  N(24).
           02  P-20K               PIC  X(05).
           02  F                   PIC  X(02).
           02  P-SIZ               PIC  9(01).
           02  P-MEI.
               03  P-ASUD.
                   04  P-SUD      OCCURS  10.
                       05  P-SU    PIC  ---,---.
               03  P-ST            PIC  ----,--9.
           02  P-TOT   REDEFINES  P-MEI.
               03  F               PIC  X(68).
               03  P-TOTM          PIC  N(05).
           02  P-AT                PIC  --,---,--9.
       01  W-DATA.
           02  W-DATE              PIC  9(06).
           02  W-NGP   REDEFINES  W-DATE.
               03  W-NEN           PIC  9(02).
               03  W-GET           PIC  9(02).
               03  W-PEY           PIC  9(02).
           02  W-INP.
               03  W-KBN           PIC  9(01).
               03  W-HCDS1         PIC  9(06).
               03  W-HCDE1         PIC  9(06).
               03  W-HCDS2         PIC  9(06).
               03  W-HCDE2         PIC  9(06).
               03  W-HCDS3         PIC  9(06).
               03  W-HCDE3         PIC  9(06).
               03  W-SEN           PIC  9(01).
               03  W-SC            PIC  9(01).
               03  W-DMM           PIC  9(01).
           02  W-HCDE              PIC  9(06).
           02  W-PAGE              PIC  9(03).
           02  W-BCD.
               03  W-BC            PIC  9(01).
               03  F               PIC  9(01).
           02  W-HCD               PIC  9(06).
           02  W-NAME              PIC  N(24).
           02  W-ST                PIC S9(07).
           02  W-AT                PIC S9(07).
           02  CNT                 PIC  9(02).
           02  W-SIZ               PIC  9(01).
           02  W-AZCD.
               03  W-ZCD   OCCURS   4.
                   04  W-ZC        PIC  9(01).
           02  W-ASUD.
               03  W-ASU   OCCURS   4.
                   04  W-SUD   OCCURS  10.
                       05  W-SU    PIC S9(5).
           02  W-TOT               PIC S9(07).
           02  W-POC               PIC  9(01).
           02  W-END               PIC  9(01).
       01  ERR-STAT                PIC  X(002).
      *
           COPY    LJMST3.
           COPY    LIHIM2.
      *FD  SP-F
       77  SP-R                    PIC  X(206).
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
           02  C-CLR    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER   PIC  X(32) VALUE
               "                                ".
           02  FILLER   PIC  X(30) VALUE
               "品　名　別　受　注　他　残　帳".
           02  FILLER.
               03  FILLER   PIC  X(10) VALUE  "受　注 = 0".
               03  FILLER   PIC  X(09) VALUE  "０ 教　育".
           02  FILLER.
               03  FILLER   PIC  X(10) VALUE  "預　り = 5".
               03  FILLER   PIC  X(09) VALUE  "１ ワーク".
           02  FILLER.
               03  FILLER   PIC  X(10) VALUE  "取よけ = 6".
               03  FILLER   PIC  X(09) VALUE  "２ 一　般".
           02  FILLER.
               03  FILLER   PIC  X(16) VALUE  "合　計 = 9   [ ]".
               03  FILLER   PIC  X(15) VALUE  "９ 全　件   [ ]".
           02  FILLER   PIC  X(37) VALUE
               "指図分  含まない = 0 , 含む = 1   [ ]".
           02  FILLER   PIC  X(25) VALUE
               "品名ｺｰﾄﾞ1       2       3".
           02  FILLER   PIC  X(08) VALUE  "ＦＲＯＭ".
           02  FILLER   PIC  X(04) VALUE  "ＴＯ".
           02  FILLER   PIC  X(20) VALUE  "確認（OK=1,NO=9）-->".
           02  FILLER   PIC  X(04) VALUE  "ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-KBN   PIC  9(01).
           02  FILLER.
               03  A-HCDS1 PIC  9(06).
               03  A-HCDS2 PIC  9(06).
               03  A-HCDS3 PIC  9(06).
           02  FILLER.
               03  A-HCDE1 PIC  9(06).
               03  A-HCDE2 PIC  9(06).
               03  A-HCDE3 PIC  9(06).
           02  A-SEN   PIC  9(01).
           02  A-SC    PIC  9(01).
           02  A-DMM   PIC  9(01).
       01  C-SPC.
           02  S-KBN   PIC  X(01) VALUE  " ".
           02  FILLER.
               03  S-HCDS1 PIC  X(06) VALUE  "      ".
               03  S-HCDS2 PIC  X(06) VALUE  "      ".
               03  S-HCDS3 PIC  X(06) VALUE  "      ".
           02  FILLER.
               03  S-HCDE1 PIC  X(06) VALUE  "      ".
               03  S-HCDE2 PIC  X(06) VALUE  "      ".
               03  S-HCDE3 PIC  X(06) VALUE  "      ".
           02  S-SEN   PIC  X(01) VALUE  " ".
           02  S-SC    PIC  X(01) VALUE  " ".
           02  S-DMM   PIC  X(01) VALUE  " ".
       01  C-ERR.
           02  FILLER.
             03  E-ME01  PIC  N(07) VALUE
                 "ＤＡＴＡ　なし".
             03  E-ME98  PIC  X(05) VALUE X"1B4A05".
             03  E-ME99  PIC  X(05) VALUE X"1B4205".
             03  E-DATA.
                 04  FILLER   PIC  X(09) VALUE "JMST3-09=".
                 04  FILLER   PIC  9(01).
                 04  FILLER   PIC  X(06) VALUE "W-SIZ=".
                 04  FILLER   PIC  9(01).
                 04  FILLER   PIC  X(04) VALUE "CNT=".
                 04  FILLER   PIC  9(02).
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CLR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "248" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RX" "1" "21" "32" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "1" "22" "30" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" " " "4" "0" "19" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID" "X" "4" "19" "10" " " "03C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID" "X" "4" "44" "9" "0103C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" " " "6" "0" "19" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID" "X" "6" "19" "10" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID" "X" "6" "44" "9" "0104C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" " " "8" "0" "19" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID" "X" "8" "19" "10" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205C-MID" "X" "8" "44" "9" "0105C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "10" "0" "31" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "X" "10" "19" "16" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
          "0206C-MID" "X" "10" "44" "15" "0106C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "13" "17" "37" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "29" "25" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "17" "22" "8" "08C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "19" "22" "4" "09C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "23" "41" "20" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-MID" "X" "23" "62" "4" "11C-MID" " "  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KBN" "9" "10" "33" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "17" "0" "18" "A-KBN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDS1" "9" "17" "32" "6" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDS1" BY REFERENCE W-HCDS1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDS2" "9" "17" "40" "6" "A-HCDS1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDS2" BY REFERENCE W-HCDS2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDS3" "9" "17" "48" "6" "A-HCDS2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDS3" BY REFERENCE W-HCDS3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "19" "0" "18" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDE1" "9" "19" "32" "6" " " "03C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDE1" BY REFERENCE W-HCDE1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDE2" "9" "19" "40" "6" "A-HCDE1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDE2" BY REFERENCE W-HCDE2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDE3" "9" "19" "48" "6" "A-HCDE2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDE3" BY REFERENCE W-HCDE3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "57" "1" "03C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "13" "52" "1" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "61" "1" "A-SC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-KBN" "X" "10" "35" "1" " " "C-SPC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-SPC" " " "17" "0" "18" "S-KBN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDS1" "X" "17" "32" "6" " " "02C-SPC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDS2" "X" "17" "40" "6" "S-HCDS1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDS3" "X" "17" "48" "6" "S-HCDS2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-SPC" " " "19" "0" "18" "02C-SPC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDE1" "X" "19" "32" "6" " " "03C-SPC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDE2" "X" "19" "40" "6" "S-HCDE1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCDE3" "X" "19" "48" "6" "S-HCDE2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SEN" "X" "10" "57" "1" "03C-SPC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SC" "X" "13" "52" "1" "S-SEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-DMM" "X" "23" "61" "1" "S-SC" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "47" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "47" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME01" "N" "24" "10" "14" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DATA" " " "24" "0" "23" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-DATA" "X" "24" "20" "9" " " "E-DATA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-DATA" "9" "24" "29" "1" "01E-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-DATA" BY REFERENCE JMST3-09 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-DATA" "X" "24" "32" "6" "02E-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-DATA" "9" "24" "38" "1" "03E-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-DATA" BY REFERENCE W-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05E-DATA" "X" "24" "42" "4" "04E-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06E-DATA" "9" "24" "46" "2" "05E-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "06E-DATA" BY REFERENCE CNT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           INITIALIZE     W-DATA.
           ACCEPT   W-DATE FROM  DATE.
       M-10.
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           MOVE     ZERO    TO    W-INP.
           PERFORM  INP-RTN  THRU  INP-EX.
           IF  W-END      =  9
               GO  TO  M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE     SPACE    TO    JMST3-KEY.
           MOVE     W-HCDS1  TO    JMST3-03.
      *           START    JMST3 KEY  NOT <  JMST3-KEY INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-90
           END-IF.
       M-15.
      *           READ     JMST3 NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-90
           END-IF
           IF  JMST3-03   >  W-HCDE
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-90
           END-IF
           IF  W-KBN  NOT =  9
               IF  W-KBN  NOT =  JMST3-01
                   GO  TO  M-15
               END-IF
           END-IF
           IF  JMST3-03  >=  W-HCDS1 AND <= W-HCDE1
               GO  TO  M-20
           END-IF
           IF  JMST3-03  >=  W-HCDS2 AND <= W-HCDE2
               GO  TO  M-20
           END-IF
           IF  JMST3-03  >=  W-HCDS3 AND <= W-HCDE3
               GO  TO  M-20
           END-IF
           GO  TO  M-15.
       M-20.
           MOVE     JMST3-03   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-15
           END-IF
           MOVE     HI-BC3     TO    W-BCD.
           IF  W-SEN  =  0
               IF  W-BC   NOT =  3
                   GO  TO  M-15
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  W-BC   NOT =  2
                   GO  TO  M-15
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  W-BC   NOT =  1
                   GO  TO  M-15
               END-IF
           END-IF
      *
           IF  W-KBN      =  0
               MOVE  W-MID1     TO  H-MID
           END-IF
           IF  W-KBN      =  5
               MOVE  W-MID2     TO  H-MID
           END-IF
           IF  W-KBN      =  6
               MOVE  W-MID3     TO  H-MID
           END-IF
           IF  W-KBN      =  9
               MOVE  W-MID4     TO  H-MID
           END-IF
           IF  W-SEN      =  0
               MOVE  "【教　育】"   TO  H-SEN
           END-IF
           IF  W-SEN      =  1
               MOVE  "【ワーク】"   TO  H-SEN
           END-IF
           IF  W-SEN      =  2
               MOVE  "【一　般】"   TO  H-SEN
           END-IF
           IF  W-SEN      =  9
               MOVE  "【全　件】"   TO  H-SEN
           END-IF
           MOVE     W-NEN      TO     H-NEN.
           MOVE     W-GET      TO     H-GET.
           MOVE     W-PEY      TO     H-PEY.
      *
           CALL "PR_Open" RETURNING RESP.
           MOVE     1          TO     W-POC.
           PERFORM  MID-010  THRU  MID-EX.
       M-25.
           MOVE     JMST3-03   TO     W-HCD.
           MOVE     HI-NAME    TO     W-NAME.
           MOVE     ZERO       TO     W-ASUD.
       M-30.
           MOVE     ZERO       TO     CNT.
       M-35.
           ADD      1          TO     CNT.
           IF  W-SC        =  0
               IF  CNT         <  11
                   COMPUTE  W-SU(JMST3-09,CNT)  =  W-SU(JMST3-09,CNT)
                                 +  JMST3-1111(CNT)  -  JMST3-1211(CNT)
                                                     -  JMST3-141(CNT)
                   GO  TO  M-35
               END-IF
           END-IF
           IF  W-SC        =  1
               IF  CNT         <  11
                   COMPUTE  W-SU(JMST3-09,CNT)  =  W-SU(JMST3-09,CNT)
                                 +  JMST3-1111(CNT)  -  JMST3-1211(CNT)
                                 -  JMST3-141(CNT)   -  JMST3-151(CNT)
                   GO  TO  M-35
               END-IF
           END-IF.
       M-40.
      *           READ     JMST3 NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-80
           END-IF
           IF  JMST3-03   >  W-HCDE
               GO  TO  M-80
           END-IF
           IF  W-KBN  NOT =  9
               IF  W-KBN  NOT =  JMST3-01
                   GO  TO  M-40
               END-IF
           END-IF
           IF  JMST3-03  >=  W-HCDS1 AND <= W-HCDE1
               GO  TO  M-45
           END-IF
           IF  JMST3-03  >=  W-HCDS2 AND <= W-HCDE2
               GO  TO  M-45
           END-IF
           IF  JMST3-03  >=  W-HCDS3 AND <= W-HCDE3
               GO  TO  M-45
           END-IF
           GO  TO  M-40.
       M-45.
           IF  JMST3-03   =  W-HCD
               GO  TO  M-30
           END-IF
           MOVE     JMST3-03   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-40
           END-IF
           MOVE     HI-BC3     TO    W-BCD.
           IF  W-SEN  =  0
               IF  W-BC   NOT =  3
                   GO  TO  M-40
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  W-BC   NOT =  2
                   GO  TO  M-40
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  W-BC   NOT =  1
                   GO  TO  M-40
               END-IF
           END-IF
      *
           PERFORM  CHK-RTN  THRU  CHK-EX.
           IF  W-AZCD   NOT =  ZERO
               PERFORM  PRI-RTN  THRU  PRI-EX
           END-IF
           GO  TO  M-25.
       M-80.
           PERFORM  CHK-RTN  THRU  CHK-EX.
           IF  W-AZCD   NOT =  ZERO
               PERFORM  PRI-RTN  THRU  PRI-EX
           END-IF
      *
           MOVE  SPACE       TO    W-P.
           MOVE  W-15K       TO    P-15K.
           MOVE  W-20K       TO    P-20K.
           MOVE ALL "　" TO    P-NAME.
           MOVE  "［総合計］" TO    P-TOTM.
           MOVE  W-TOT       TO  P-AT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER    >  60
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE  SPACE       TO    SP-R.
           MOVE  W-P         TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           IF  W-POC      =  1
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       INP-RTN.
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           MOVE     ZERO    TO    W-INP.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE A-KBN "A-KBN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "P9"
               MOVE  9        TO  W-END
               GO  TO  INP-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-010
           END-IF
           IF  W-KBN  NOT =  0  AND  5  AND  6  AND  9
               GO  TO  INP-010
           END-IF.
       INP-012.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-012
           END-IF
           IF  W-SEN  NOT =  0 AND 1 AND 2 AND 9
               GO  TO  INP-012
           END-IF.
       INP-015.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-012
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-015
           END-IF
           IF  W-SC   NOT =  0 AND 1
               GO  TO  INP-015
           END-IF.
       INP-020.
           CALL "SD_Accept" USING BY REFERENCE A-HCDS1 "A-HCDS1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-015
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-020
           END-IF.
       INP-030.
           CALL "SD_Accept" USING BY REFERENCE A-HCDE1 "A-HCDE1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-030
           END-IF
           IF  W-HCDE1    =  ZERO
               GO  TO  INP-030
           END-IF
           IF  W-HCDS1    >  W-HCDE1
               GO  TO  INP-030
           END-IF
           IF  W-HCDE1    =  999999
               MOVE  W-HCDE1     TO  W-HCDE
               MOVE  ZERO        TO  W-HCDS2 W-HCDE2 W-HCDS3 W-HCDE3
               CALL "SD_Output" USING
                "S-HCDS2" S-HCDS2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE2" S-HCDE2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDS3" S-HCDS3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE3" S-HCDE3 "p" RETURNING RESU
               GO  TO  INP-090
           END-IF.
       INP-040.
           CALL "SD_Accept" USING BY REFERENCE A-HCDS2 "A-HCDS2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-030
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-040
           END-IF
           IF  W-HCDS2    =  ZERO
               MOVE  W-HCDE1     TO  W-HCDE
               MOVE  ZERO        TO  W-HCDE2 W-HCDS3 W-HCDE3
               CALL "SD_Output" USING
                "S-HCDS2" S-HCDS2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE2" S-HCDE2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDS3" S-HCDS3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE3" S-HCDE3 "p" RETURNING RESU
               GO  TO  INP-090
           END-IF
           IF  W-HCDE1    >  W-HCDS2
               GO  TO  INP-040
           END-IF.
       INP-050.
           CALL "SD_Accept" USING BY REFERENCE A-HCDE2 "A-HCDE2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-040
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-050
           END-IF
           IF  W-HCDS2    >  W-HCDE2
               GO  TO  INP-050
           END-IF
           IF  W-HCDE2    =  999999
               MOVE  W-HCDE2     TO  W-HCDE
               MOVE  ZERO        TO  W-HCDS3 W-HCDE3
               CALL "SD_Output" USING
                "S-HCDS3" S-HCDS3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE3" S-HCDE3 "p" RETURNING RESU
               GO  TO  INP-090
           END-IF.
       INP-060.
           CALL "SD_Accept" USING BY REFERENCE A-HCDS3 "A-HCDS3" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-050
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-060
           END-IF
           IF  W-HCDS3    =  ZERO
               MOVE  W-HCDE2     TO  W-HCDE
               MOVE  ZERO        TO  W-HCDE3
               CALL "SD_Output" USING
                "S-HCDS3" S-HCDS3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "S-HCDE3" S-HCDE3 "p" RETURNING RESU
               GO  TO  INP-090
           END-IF
           IF  W-HCDE2 >  W-HCDS3
               GO  TO  INP-060
           END-IF.
       INP-070.
           CALL "SD_Accept" USING BY REFERENCE A-HCDE3 "A-HCDE3" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-060
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-070
           END-IF
           IF  W-HCDS3    >  W-HCDE3
               GO  TO  INP-070
           END-IF
           MOVE  W-HCDE3     TO  W-HCDE.
       INP-090.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-HCDE3 NOT =  ZERO
                   GO  TO  INP-070
               ELSE
                   IF  W-HCDE2 NOT =  ZERO
                       GO  TO  INP-050
                   ELSE
                       GO  TO  INP-030
                   END-IF
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01"
               GO  TO  INP-090
           END-IF
           IF  W-DMM      =   9
               GO  TO  INP-RTN
           END-IF
           IF  W-DMM  NOT =   1
               GO  TO  INP-090
           END-IF.
       INP-EX.
           EXIT.
       CHK-RTN.
           MOVE  ZERO        TO    W-AZCD  W-SIZ.
       CHK-010.
           ADD   1           TO    W-SIZ.
           IF  W-SIZ         =  5
               GO  TO  CHK-EX
           END-IF
           MOVE  ZERO        TO    CNT.
       CHK-020.
           ADD   1           TO    CNT.
           IF  CNT           =  11
               GO  TO  CHK-010
           END-IF
           IF  W-SU(W-SIZ,CNT) NOT =  ZERO
               IF  W-ZC(W-SIZ)       =  0
                   MOVE  1          TO  W-ZC(W-SIZ)
                   GO  TO  CHK-010
               END-IF
           END-IF
           GO  TO  CHK-020.
       CHK-EX.
           EXIT.
       PRI-RTN.
           MOVE  ZERO        TO    W-SIZ  W-AT.
       PRI-010.
           ADD   1           TO    W-SIZ.
           IF  W-SIZ         =  5
               GO  TO  PRI-030
           END-IF
           IF  W-ZC(W-SIZ)   =  0
               GO  TO  PRI-010
           END-IF
      *
           MOVE  SPACE       TO    W-P.
           MOVE  W-15K       TO    P-15K.
           MOVE  W-20K       TO    P-20K.
           MOVE ALL "　" TO    P-NAME.
           IF  W-SIZ         =  1
               MOVE  W-HCD       TO  P-HCD
               MOVE  W-NAME      TO  P-NAME
           END-IF
           IF  W-SIZ         =  2
               IF  0           =  W-ZC(1)
                   MOVE  W-HCD       TO  P-HCD
                   MOVE  W-NAME      TO  P-NAME
               END-IF
           END-IF
           IF  W-SIZ         =  3
               IF  0           =  W-ZC(1) AND W-ZC(2)
                   MOVE  W-HCD       TO  P-HCD
                   MOVE  W-NAME      TO  P-NAME
               END-IF
           END-IF
           IF  W-SIZ         =  4
               IF  0           =  W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE  W-HCD       TO  P-HCD
                   MOVE  W-NAME      TO  P-NAME
               END-IF
           END-IF
      *
           MOVE  W-SIZ       TO  P-SIZ.
           MOVE  ZERO        TO    CNT  W-ST.
       PRI-020.
           ADD   1           TO    CNT.
           IF  CNT           <  11
               MOVE  W-SU(W-SIZ,CNT)  TO  P-SU(CNT)
               ADD   W-SU(W-SIZ,CNT)  TO  W-ST  W-AT  W-TOT
               GO  TO  PRI-020
           END-IF
           MOVE  W-ST        TO    P-ST.
           IF  W-SIZ         =  4
               MOVE  W-AT        TO  P-AT
           END-IF
           IF  W-SIZ         =  3
               IF  0           =  W-ZC(4)
                   MOVE  W-AT        TO  P-AT
               END-IF
           END-IF
           IF  W-SIZ         =  2
               IF  0           =  W-ZC(4) AND W-ZC(3)
                   MOVE  W-AT        TO  P-AT
               END-IF
           END-IF
           IF  W-SIZ         =  1
               IF  0           =  W-ZC(4) AND W-ZC(3) AND W-ZC(2)
                   MOVE  W-AT        TO  P-AT
               END-IF
           END-IF
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER    >  60
               MOVE  W-HCD       TO  P-HCD
               MOVE  W-NAME      TO  P-NAME
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE  SPACE       TO    SP-R.
           MOVE  W-P         TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           GO  TO  PRI-010.
       PRI-030.
           MOVE  SPACE       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       PRI-EX.
           EXIT.
       MID-RTN.
           MOVE  SPACE       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
           ADD   1           TO    W-PAGE.
           MOVE  W-PAGE      TO    H-PAGE.
           MOVE  SPACE       TO    SP-R.
           MOVE  HEAD1       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           MOVE  HEAD2       TO    SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           MOVE  HEAD3       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           MOVE  HEAD4       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           MOVE  HEAD5       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE       TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.
