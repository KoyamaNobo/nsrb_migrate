       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT051R.
      ********************************************************
      ******    出荷指図№　問合せ                      ******
      ******    JS-SIGN  :  本社=0 , 玉島=2 , 早島=3    ******
      ********************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)    VALUE  SPACE.
       77  JS-SIGN                 PIC 9(01).
       01  WORK-AREA.
           02  W-FSK               PIC 9(01).
           02  W-TSK               PIC 9(01).
           02  W-FDT               PIC 9(06).
           02  W-FDTD  REDEFINES  W-FDT.
               03  W-FNEN          PIC 9(02).
               03  W-FGET          PIC 9(02).
               03  W-FPEY          PIC 9(02).
           02  W-TDT               PIC 9(06).
           02  W-TDTD  REDEFINES  W-TDT.
               03  W-TNEN          PIC 9(02).
               03  W-TGET          PIC 9(02).
               03  W-TPEY          PIC 9(02).
           02  W-SEN               PIC 9(01).
           02  W-FNGP              PIC 9(08).
           02  W-TNGP              PIC 9(08).
           02  W-NGP.
               03  W-NEN           PIC 9(04).
               03  W-NENL  REDEFINES  W-NEN.
                   04  W-NEN1      PIC 9(02).
                   04  W-NEN2      PIC 9(02).
               03  W-GET           PIC 9(02).
               03  W-PEY           PIC 9(02).
           02  W-NGPL  REDEFINES  W-NGP.
               03  F               PIC 9(02).
               03  W-NGPS          PIC 9(06).
           02  W-SENM              PIC N(05).
           02  W-DC                PIC X(01).
           02  W-SNA               PIC N(06).
           02  W-UNA               PIC N(06).
           02  W-HK                PIC N(01).
           02  W-SS                PIC N(01).
           02  W-L.
               03  W-L1            PIC 9(02).
               03  W-L2            PIC 9(02).
           02  W-DMM               PIC 9(01).
           02  W-END               PIC 9(01).
       COPY  LWMSG.
      *
           COPY  LIBFDD.
           COPY  L-JSTR-TAM.
           COPY  LITCM.
           COPY  L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  MID-AREA.
           02  FILLER  PIC N(09)  VALUE
                "出荷指図№　問合せ".
           02  FILLER.
               03  FILLER  PIC X(08)  VALUE "倉庫ｺｰﾄﾞ".
               03  FILLER  PIC N(03)  VALUE "指図日".
               03  FILLER  PIC X(08)  VALUE "0 教　育".
           02  FILLER.
               03  FILLER  PIC N(04)  VALUE "ＦＲＯＭ".
               03  FILLER  PIC X(08)  VALUE "  /  /  ".
               03  FILLER  PIC X(08)  VALUE "1 一　般".
           02  FILLER.
               03  FILLER  PIC N(02)  VALUE "ＴＯ".
               03  FILLER  PIC X(08)  VALUE "  /  /  ".
               03  FILLER  PIC X(18)  VALUE "9 全　件   選択[ ]".
           02  FILLER  PIC X(22)  VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  FILLER.
               03  A-FSK   PIC 9(01).
               03  A-FNEN  PIC 9(02).
               03  A-FGET  PIC 9(02).
               03  A-FPEY  PIC 9(02).
           02  FILLER.
               03  A-TSK   PIC 9(01).
               03  A-TNEN  PIC 9(02).
               03  A-TGET  PIC 9(02).
               03  A-TPEY  PIC 9(02).
               03  A-SEN   PIC 9(01).
           02  A-DMM  PIC 9(01).
       01  DSP-AREA.
           02  DSP-SENM.
               03  FILLER  PIC X(01)  VALUE  "[".
               03  FILLER  PIC N(05).
               03  FILLER  PIC X(01)  VALUE  "]".
           02  DSP-DATA.
               03  FILLER.
                   04  FILLER  PIC 9(06).
                   04  FILLER  PIC X(01).
                   04  FILLER  PIC 9(04).
                   04  FILLER  PIC N(24).
                   04  FILLER  PIC N(06).
                   04  FILLER  PIC N(01).
               03  FILLER.
                   04  FILLER  PIC X(01)  VALUE  "(".
                   04  FILLER  PIC Z9 .
                   04  FILLER  PIC X(01)  VALUE  "/".
                   04  FILLER  PIC Z9 .
                   04  FILLER  PIC X(01)  VALUE  ")".
                   04  FILLER  PIC N(04).
                   04  FILLER  PIC N(06).
                   04  FILLER  PIC N(01).
           02  DSP-CNA.
               03  FILLER  PIC 9(03).
               03  FILLER  PIC N(20).
       01  ERR-MSG-AREA.
           02  FILLER.
               03  E-STAT   PIC X(02).
               03  E-ME1    PIC N(07)   VALUE
                     "該当データなし".
               03  E-ME2    PIC N(05)   VALUE
                     "データ終了".
               03  E-ME98   PIC X(05)   VALUE X"1B4A05".
               03  E-ME99   PIC X(05)   VALUE X"1B4205".
               03  E-CL     PIC X(40)   VALUE
                     "                                        ".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *MID-AREA
       CALL "SD_Init" USING 
            "MID-AREA" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MID-AREA" "N" "1" "32" "18" " " "MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02MID-AREA" " " "12" "0" "22" "01MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102MID-AREA" "X" "12" "27" "8" " " "02MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202MID-AREA" "N" "12" "37" "6" "0102MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302MID-AREA" "X" "12" "48" "8" "0202MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03MID-AREA" " " "14" "0" "24" "02MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103MID-AREA" "N" "14" "18" "8" " " "03MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203MID-AREA" "X" "14" "36" "8" "0103MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303MID-AREA" "X" "14" "48" "8" "0203MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04MID-AREA" " " "16" "0" "30" "03MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104MID-AREA" "N" "16" "18" "4" " " "04MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204MID-AREA" "X" "16" "36" "8" "0104MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304MID-AREA" "X" "16" "48" "18" "0204MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05MID-AREA" "X" "23" "48" "22" "04MID-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "14" "0" "7" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FSK" "9" "14" "30" "1" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FSK" BY REFERENCE W-FSK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FNEN" "9" "14" "36" "2" "A-FSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FNEN" BY REFERENCE W-FNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FGET" "9" "14" "39" "2" "A-FNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FPEY" "9" "14" "42" "2" "A-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "16" "0" "8" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TSK" "9" "16" "30" "1" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TSK" BY REFERENCE W-TSK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNEN" "9" "16" "36" "2" "A-TSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNEN" BY REFERENCE W-TNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TGET" "9" "16" "39" "2" "A-TNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPEY" "9" "16" "42" "2" "A-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "16" "64" "1" "A-TPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "157" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SENM" " " "1" "0" "12" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SENM" "X" "1" "61" "1" " " "DSP-SENM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-SENM" "N" "1" "62" "10" "01DSP-SENM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-SENM" BY REFERENCE W-SENM "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-SENM" "X" "1" "72" "1" "02DSP-SENM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATA" " " "0" "0" "102" "DSP-SENM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DATA" " " "W-L1" "0" "73" " " "DSP-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-DATA" "9" "W-L1" "1" "6" " " "01DSP-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101DSP-DATA" BY REFERENCE JSTR-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-DATA" "X" "W-L1" "7" "1" "0101DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201DSP-DATA" BY REFERENCE W-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-DATA" "9" "W-L1" "9" "4" "0201DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-DATA" BY REFERENCE JSTR-061 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-DATA" "N" "W-L1" "14" "48" "0301DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-DATA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-DATA" "N" "W-L1" "64" "12" "0401DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501DSP-DATA" BY REFERENCE W-SNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0601DSP-DATA" "N" "W-L1" "78" "2" "0501DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0601DSP-DATA" BY REFERENCE W-HK "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATA" " " "W-L2" "0" "29" "01DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-DATA" "X" "W-L2" "1" "1" " " "02DSP-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-DATA" "Z9" "W-L2" "2" "2" "0102DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202DSP-DATA" BY REFERENCE JSTR-042 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-DATA" "X" "W-L2" "4" "1" "0202DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-DATA" "Z9" "W-L2" "5" "2" "0302DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402DSP-DATA" BY REFERENCE JSTR-043 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502DSP-DATA" "X" "W-L2" "7" "1" "0402DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602DSP-DATA" "N" "W-L2" "55" "8" "0502DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602DSP-DATA" BY REFERENCE JSTR-15 "46" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0702DSP-DATA" "N" "W-L2" "64" "12" "0602DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702DSP-DATA" BY REFERENCE W-UNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0802DSP-DATA" "N" "W-L2" "78" "2" "0702DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802DSP-DATA" BY REFERENCE W-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CNA" " " "W-L2" "0" "43" "DSP-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CNA" "9" "W-L2" "10" "3" " " "DSP-CNA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-CNA" BY REFERENCE JSTR-062 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CNA" "N" "W-L2" "14" "40" "01DSP-CNA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-CNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
      *ERR-MSG-AREA
       CALL "SD_Init" USING 
            "ERR-MSG-AREA" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-MSG-AREA" " " "24" "0" "76" " " "ERR-MSG-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "1" "2" " " "01ERR-MSG-AREA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "5" "14" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "5" "10" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "1" "40" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-00.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 2  AND 3
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "MID-AREA" MID-AREA "p" RETURNING RESU.
           COPY  LIBCPR.
           MOVE  ZERO      TO  WORK-AREA.
           IF  JS-SIGN     =  2
               MOVE   6        TO  W-FSK  W-TSK
               CALL "SD_Output" USING "A-FSK" A-FSK "p" RETURNING RESU
               CALL "SD_Output" USING "A-TSK" A-TSK "p" RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  JS-SIGN     =  3
               MOVE   4        TO  W-FSK  W-TSK
               CALL "SD_Output" USING "A-FSK" A-FSK "p" RETURNING RESU
               CALL "SD_Output" USING "A-TSK" A-TSK "p" RETURNING RESU
               GO  TO  M-20
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-FSK "A-FSK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO  TO  M-95
           END-IF
           IF  ESTAT   NOT =  "01" AND  "06"
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TSK "A-TSK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-10
           END-IF
           IF  ESTAT   NOT =  "01" AND  "06"
               GO  TO  M-15
           END-IF
           IF  W-FSK       >  W-TSK
               GO  TO  M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-FNEN "A-FNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO  TO  M-95
           END-IF
           IF  ESTAT       =  "09"
               IF  JS-SIGN     =  0
                   GO  TO  M-15
               END-IF
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-FGET "A-FGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-20
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-25
           END-IF
           IF  W-FGET      =  ZERO
               IF  W-FNEN      =  ZERO
                   GO  TO  M-30
               END-IF
           END-IF
           IF  W-FGET      <  1  OR  >  12
               GO  TO  M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-FPEY "A-FPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-25
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-30
           END-IF
           IF  W-FPEY      =  ZERO
               IF  W-FGET      =  ZERO
                   MOVE  ZERO      TO  W-FNGP
                   GO  TO  M-35
               END-IF
           END-IF
           IF  W-FPEY      <  1  OR  >  31
               GO  TO  M-30
           END-IF
           MOVE  ZERO      TO  W-NGP.
           MOVE  W-FDT     TO  W-NGPS.
           IF  W-NEN2      >=  DATE-NF1  AND  <=  DATE-NT1
               ADD  DATE-NC1     TO  W-NEN
           END-IF
           IF  W-NEN2      >=  DATE-NF2  AND  <=  DATE-NT2
               ADD  DATE-NC2     TO  W-NEN
           END-IF
           MOVE  W-NGP     TO  W-FNGP.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-TNEN "A-TNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-30
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-TGET "A-TGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-35
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-40
           END-IF
           IF  W-TGET      =  99
               IF  W-TNEN      =  99
                   GO  TO  M-45
               END-IF
           END-IF
           IF  W-TGET      <  1  OR  >  12
               GO  TO  M-40
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-TPEY "A-TPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-40
           END-IF
           IF  ESTAT   NOT =  "00"  AND  "01"  AND  "06"
               GO  TO  M-45
           END-IF
           IF  W-TPEY      =  99
               IF  W-TGET      =  99
                   MOVE  99999999  TO  W-TNGP
                   GO  TO  M-47
               END-IF
           END-IF
           IF  W-TPEY      <  1  OR  >  31
               GO  TO  M-45
           END-IF
           MOVE  ZERO      TO  W-NGP.
           MOVE  W-TDT     TO  W-NGPS.
           IF  W-NEN2      >=  DATE-NF1  AND  <=  DATE-NT1
               ADD  DATE-NC1     TO  W-NEN
           END-IF
           IF  W-NEN2      >=  DATE-NF2  AND  <=  DATE-NT2
               ADD  DATE-NC2     TO  W-NEN
           END-IF
           MOVE  W-NGP     TO  W-TNGP.
           IF  W-FNGP      >  W-TNGP
               GO  TO  M-35
           END-IF.
       M-47.
           IF  JS-SIGN     =  2     OR   3
               MOVE  9         TO  W-SEN
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
               GO  TO  M-55
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-45
           END-IF
           IF  ESTAT   NOT =  "01"  AND  "06"
               GO  TO  M-50
           END-IF
           IF  W-SEN   NOT =  0  AND  1  AND  9
               GO  TO  M-50
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               IF  JS-SIGN     =  2     OR   3
                   GO  TO  M-45
               ELSE
                   GO  TO  M-50
               END-IF
           END-IF
           IF  ESTAT   NOT =  "01"  AND  "06"
               GO  TO  M-55
           END-IF
           IF  W-DMM       =  9
               GO  TO  M-05
           END-IF
           IF  W-DMM   NOT =  1
               GO  TO  M-55
           END-IF
      *
           MOVE  SPACE     TO  W-SENM.
           IF  W-SEN       =  0
               MOVE  "　教　育　"  TO  W-SENM
           END-IF
           IF  W-SEN       =  1
               MOVE  "　一　般　"  TO  W-SENM
           END-IF
           IF  W-SEN       =  9
               MOVE  "　全　件　"  TO  W-SENM
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ051R" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-SENM" DSP-SENM "p" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE  SPACE     TO  JSTR-KEY.
           IF  W-SEN         =  1
               MOVE  100000    TO  JSTR-01
           END-IF.
      *           START  JSTR     KEY  NOT <  JSTR-KEY   INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  M-75
           END-IF.
       M-60.
      *           READ  JSTR    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  M-75
           END-IF
           IF  JSTR-02   NOT =  1
               GO  TO  M-60
           END-IF
           IF  JSTR-07       <  W-FSK  OR  >  W-TSK
               GO  TO  M-60
           END-IF
           IF  JSTR-04       <  W-FNGP OR  >  W-TNGP
               GO  TO  M-60
           END-IF
           IF  W-SEN         =  0
               IF  JSTR-01       >  099999
                   GO  TO  M-75
               END-IF
           END-IF
      *
           IF  W-END         =  0
               MOVE  1         TO  W-END
           END-IF
           MOVE  SPACE     TO  W-DC  W-SNA  W-UNA  W-HK   W-SS.
           IF  JSTR-03       =  5  OR  6
               MOVE    "*"     TO  W-DC
           END-IF
      *
           MOVE  JSTR-061  TO  TC-TCD.
           MOVE  1         TO  TC-CCD.
      *           READ  TC-M     WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF
      *
           MOVE  3         TO  JCON1-01.
           MOVE  JSTR-07   TO  JCON1-02.
      *           READ  JCON     WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  JCON3-03
           END-IF
           MOVE  JCON3-03  TO  W-SNA.
      *
           MOVE  2         TO  JCON1-01.
           MOVE  JSTR-14   TO  JCON1-02.
      *           READ  JCON     WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  JCON2-03
           END-IF
           MOVE  JCON2-03  TO  W-UNA.
      *
           IF  JSTR-158      =  0
               MOVE  "未"    TO  W-HK
           ELSE
               MOVE  "済"    TO  W-HK
           END-IF
           IF  JSTR-17       =  0
               MOVE  "未"    TO  W-SS
           ELSE
               MOVE  "済"    TO  W-SS
           END-IF.
       M-65.
           ADD   2         TO  W-L1  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1          =  23
               GO  TO  M-70
           END-IF
           CALL "SD_Output" USING
            "DSP-DATA" DSP-DATA "p" RETURNING RESU.
           IF  JSTR-062      = 001
               GO  TO  M-60
           END-IF
           MOVE  JSTR-061  TO  TC-TCD.
           MOVE  JSTR-062  TO  TC-CCD.
      *           READ  TC-M     WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF
           CALL "SD_Output" USING "DSP-CNA" DSP-CNA "p" RETURNING RESU.
           GO  TO  M-60.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   NOT =  "01"  AND  "06"
               GO  TO  M-70
           END-IF
           IF  W-DMM       =  9
               CALL "DB_F_Close" USING
                BY REFERENCE JSTR_IDLST JSTR_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               GO  TO  M-95
           END-IF
           IF  W-DMM   NOT =  1
               GO  TO  M-70
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ051R" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-SENM" DSP-SENM "p" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           GO  TO  M-65.
       M-75.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           IF  W-END       =  0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
