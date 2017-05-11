       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JTN08R.
      **************************************************
      ******    ÉgÉâÉXÉRëºìùàÍì`ï[áÇÅ@ñ‚çáÇπ      ******
      ******    JS-SIGN  :  ñ{é–=0 , ì°ìc=2 , ëÅìá=3   ******
      **************************************************
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
           02  W-FTCD              PIC 9(04).
           02  W-TTCD              PIC 9(04).
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
           02  W-DNO               PIC 9(06).
           02  W-CCD               PIC X(04).
           02  W-CCDD  REDEFINES  W-CCD.
             03  W-CCD1            PIC X(01).
             03  W-CCD2            PIC X(03).
           02  W-CNA               PIC N(19).
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
           COPY  L-TDIF-TAM.
           COPY  LITCM.
           COPY  L-JCON.
      *FD  WTNAF
       01  WTNAF_JTN08R.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JTN08R".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TNC     PIC  9(004).
           02  WTNA-NAME      PIC  N(026).
           02  WTNA-OKS       PIC  9(001).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC N(14)  VALUE
                "ÉgÉâÉXÉRëºìùàÍì`ï[áÇÅ@ñ‚çáÇπ".
           02  FILLER.
               03  FILLER  PIC X(08)  VALUE "ëqå…∫∞ƒﬁ".
               03  FILLER  PIC N(03)  VALUE "ìæà”êÊ".
               03  FILLER  PIC N(03)  VALUE "ì˙Å@ït".
           02  FILLER.
               03  FILLER  PIC N(04)  VALUE "ÇeÇqÇnÇl".
               03  FILLER  PIC X(08)  VALUE "  /  /  ".
           02  FILLER.
               03  FILLER  PIC N(02)  VALUE "ÇsÇn".
               03  FILLER  PIC X(08)  VALUE "  /  /  ".
           02  FILLER  PIC X(22)  VALUE
                "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  ACP-AREA.
           02  FILLER.
               03  A-FSK   PIC 9(01).
               03  A-FTCD  PIC 9(04).
               03  A-FNEN  PIC 9(02).
               03  A-FGET  PIC 9(02).
               03  A-FPEY  PIC 9(02).
           02  FILLER.
               03  A-TSK   PIC 9(01).
               03  A-TTCD  PIC 9(04).
               03  A-TNEN  PIC 9(02).
               03  A-TGET  PIC 9(02).
               03  A-TPEY  PIC 9(02).
           02  A-DMM  PIC 9(01).
       01  DSP-AREA.
           02  DSP-DATA.
               03  FILLER.
                   04  FILLER  PIC 9(06).
                   04  FILLER  PIC 9(04).
                   04  FILLER  PIC N(25).
                   04  FILLER  PIC N(06).
                   04  FILLER  PIC N(01).
               03  FILLER.
                   04  FILLER  PIC Z9 .
                   04  FILLER  PIC X(01)  VALUE  "/".
                   04  FILLER  PIC Z9 .
                   04  FILLER  PIC X(10).
                   04  FILLER  PIC N(06).
                   04  FILLER  PIC N(01).
           02  DSP-CNA.
               03  FILLER  PIC X(04).
               03  FILLER  PIC N(19).
       01  ERR-MSG-AREA.
           02  FILLER.
               03  E-STAT   PIC X(02).
               03  E-ME1    PIC N(07)   VALUE
                     "äYìñÉfÅ[É^Ç»Çµ".
               03  E-ME2    PIC N(05)   VALUE
                     "ÉfÅ[É^èIóπ".
               03  E-ME98   PIC  X(005) VALUE X"1B4A05".
               03  E-ME99   PIC  X(005) VALUE X"1B4205".
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
            "MID-AREA" " " "0" "0" "98" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MID-AREA" "N" "1" "27" "28" " " "MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02MID-AREA" " " "12" "0" "20" "01MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102MID-AREA" "X" "12" "27" "8" " " "02MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202MID-AREA" "N" "12" "38" "6" "0102MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302MID-AREA" "N" "12" "47" "6" "0202MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03MID-AREA" " " "14" "0" "16" "02MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103MID-AREA" "N" "14" "18" "8" " " "03MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203MID-AREA" "X" "14" "46" "8" "0103MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04MID-AREA" " " "16" "0" "12" "03MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104MID-AREA" "N" "16" "18" "4" " " "04MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204MID-AREA" "X" "16" "46" "8" "0104MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05MID-AREA" "X" "23" "48" "22" "04MID-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "23" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-AREA" " " "14" "0" "11" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FSK" "9" "14" "30" "1" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FSK" BY REFERENCE W-FSK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FTCD" "9" "14" "39" "4" "A-FSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FTCD" BY REFERENCE W-FTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FNEN" "9" "14" "46" "2" "A-FTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FNEN" BY REFERENCE W-FNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FGET" "9" "14" "49" "2" "A-FNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FPEY" "9" "14" "52" "2" "A-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "16" "0" "11" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TSK" "9" "16" "30" "1" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TSK" BY REFERENCE W-TSK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TTCD" "9" "16" "39" "4" "A-TSK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TTCD" BY REFERENCE W-TTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNEN" "9" "16" "46" "2" "A-TTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNEN" BY REFERENCE W-TNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TGET" "9" "16" "49" "2" "A-TNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPEY" "9" "16" "52" "2" "A-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "145" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATA" " " "0" "0" "103" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DATA" " " "W-L1" "0" "74" " " "DSP-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-DATA" "9" "W-L1" "1" "6" " " "01DSP-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101DSP-DATA" BY REFERENCE TDI-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-DATA" "9" "W-L1" "8" "4" "0101DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201DSP-DATA" BY REFERENCE TDI-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-DATA" "N" "W-L1" "13" "50" "0201DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-DATA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-DATA" "N" "W-L1" "64" "12" "0301DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-DATA" BY REFERENCE W-SNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-DATA" "N" "W-L1" "78" "2" "0401DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501DSP-DATA" BY REFERENCE W-HK "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATA" " " "W-L2" "0" "29" "01DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-DATA" "Z9" "W-L2" "2" "2" " " "02DSP-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102DSP-DATA" BY REFERENCE TDI-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-DATA" "X" "W-L2" "4" "1" "0102DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-DATA" "Z9" "W-L2" "5" "2" "0202DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302DSP-DATA" BY REFERENCE TDI-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-DATA" "X" "W-L2" "53" "10" "0302DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402DSP-DATA" BY REFERENCE TDI-HNO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502DSP-DATA" "N" "W-L2" "64" "12" "0402DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502DSP-DATA" BY REFERENCE W-UNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602DSP-DATA" "N" "W-L2" "78" "2" "0502DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602DSP-DATA" BY REFERENCE W-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CNA" " " "W-L2" "0" "42" "DSP-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CNA" "X" "W-L2" "8" "4" " " "DSP-CNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-CNA" BY REFERENCE W-CCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CNA" "N" "W-L2" "14" "38" "01DSP-CNA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-CNA" BY REFERENCE W-CNA "38" "0" RETURNING RESU.
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
      *
       M-05.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN  NOT  =  0  AND  2  AND  3
               CALL "DB_Close"
               STOP  RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "MID-AREA" MID-AREA "p" RETURNING RESU.
           MOVE  ZERO      TO  WORK-AREA.
           IF  JS-SIGN       =  2
               MOVE  6       TO  W-FSK  W-TSK
           END-IF
           IF  JS-SIGN       =  3
               MOVE  4       TO  W-FSK  W-TSK
           END-IF
           IF  JS-SIGN       =  2  OR  3
               CALL "SD_Output" USING "A-FSK" A-FSK "p" RETURNING RESU
               CALL "SD_Output" USING "A-TSK" A-TSK "p" RETURNING RESU
               GO  TO  M-16
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
       M-16.
           CALL "SD_Accept" USING BY REFERENCE A-FTCD "A-FTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO  TO  M-95
           END-IF
           IF  ESTAT       =  "09"
               IF  JS-SIGN   NOT =  2  AND 3
                   GO  TO  M-15
               END-IF
           END-IF
           IF  ESTAT   NOT =  "01" AND  "06"
               GO  TO  M-16
           END-IF.
       M-17.
           CALL "SD_Accept" USING BY REFERENCE A-TTCD "A-TTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-16
           END-IF
           IF  ESTAT   NOT =  "01" AND  "06"
               GO  TO  M-17
           END-IF
           IF  W-FTCD      >  W-TTCD
               GO  TO  M-17
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-FNEN "A-FNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-17
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
                   MOVE  ZERO      TO  W-FDT
                   GO  TO  M-35
               END-IF
           END-IF
           IF  W-FPEY      <  1  OR  >  31
               GO  TO  M-30
           END-IF.
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
                   MOVE  99999999  TO  W-TDT
                   GO  TO  M-55
               END-IF
           END-IF
           IF  W-TPEY      <  1  OR  >  31
               GO  TO  M-45
           END-IF
           IF  W-FDT       >  W-TDT
               GO  TO  M-35
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-35
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJT08R" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
       M-60.
      *           READ  TDIF    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  M-80
           END-IF
           IF  TDI-SOK       <  W-FSK  OR  >  W-TSK
               GO  TO  M-60
           END-IF
           IF  TDI-TCD       <  W-FTCD OR  >  W-TTCD
               GO  TO  M-60
           END-IF
           IF  TDI-DATE      <  W-FDT  OR  >  W-TDT
               GO  TO  M-60
           END-IF
      *
           IF  TDI-DNO       =  W-DNO
               GO  TO  M-60
           END-IF
           MOVE  TDI-DNO   TO  W-DNO.
           IF  W-END         =  0
               MOVE  1         TO  W-END
           END-IF
           MOVE  SPACE     TO  W-CNA  W-SNA  W-UNA  W-HK   W-SS.
      *
           MOVE  TDI-TCD   TO  TC-TCD.
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
           MOVE  TDI-SOK   TO  JCON1-02.
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
           MOVE  TDI-UNS   TO  JCON1-02.
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
           IF  9   =  TDI-UPC  OR  TDI-PRC
               MOVE  "çœ"    TO  W-HK
           ELSE
               MOVE  "ñ¢"    TO  W-HK
           END-IF
           IF  TDI-UPC       =  1  OR  9
               MOVE  "çœ"    TO  W-SS
           ELSE
               MOVE  "ñ¢"    TO  W-SS
           END-IF.
       M-65.
           ADD   2         TO  W-L1  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1          =  23
               GO  TO  M-75
           END-IF
           CALL "SD_Output" USING
            "DSP-DATA" DSP-DATA "p" RETURNING RESU.
           IF  TDI-TPC  NOT  = ZERO
               GO  TO  M-70
           END-IF
           IF  TDI-CCD       = 001
               GO  TO  M-60
           END-IF
           MOVE  TDI-TCD   TO  TC-TCD.
           MOVE  TDI-CCD   TO  TC-CCD.
      *           READ  TC-M     WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF
           MOVE  SPACE     TO  W-CCD.
           MOVE  TDI-CCD   TO  W-CCD2.
           MOVE  TC-NAME   TO  W-CNA.
           CALL "SD_Output" USING "DSP-CNA" DSP-CNA "p" RETURNING RESU.
           GO  TO  M-60.
       M-70.
           MOVE  TDI-TPC   TO  WTNA-KEY.
      *           READ  WTNAF    WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  WTNA-NAME
           END-IF
           MOVE  TDI-TPC   TO  W-CCD.
           MOVE  WTNA-NAME TO  W-CNA.
           CALL "SD_Output" USING "DSP-CNA" DSP-CNA "p" RETURNING RESU
           GO  TO  M-60.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   NOT =  "01"  AND  "06"
               GO  TO  M-75
           END-IF
           IF  W-DMM       =  9
               CALL "DB_F_Close" USING
                BY REFERENCE TDIF_IDLST TDIF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE WTNAF_IDLST WTNAF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               GO  TO  M-95
           END-IF
           IF  W-DMM   NOT =  1
               GO  TO  M-75
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJT08R" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           GO  TO  M-65.
       M-80.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
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
