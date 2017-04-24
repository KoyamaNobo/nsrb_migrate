       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JSS75R.
      ********************************************
      ******    î≠íçáÇÅ@ñ‚çáÇπÅ@Åiê∂ã¶Åj    ******
      ******    SCREEN : SJS75R             ******
      ********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)    VALUE  SPACE.
       77  JS-SIGN                 PIC 9(01).
       01  WORK-AREA.
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
           02  W-NNDD.
               03  F               PIC 9(02).
               03  W-NND           PIC 9(06).
           02  W-SZDD.
               03  F               PIC 9(02).
               03  W-SZD           PIC 9(06).
           02  W-UNA               PIC N(06).
           02  W-HK                PIC N(01).
           02  W-L.
               03  W-L1            PIC 9(02).
               03  W-L2            PIC 9(02).
           02  W-DMM               PIC 9(01).
           02  W-END               PIC 9(01).
       COPY  LWMSG.
      *
           COPY  LSKHAT.
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
           02  FILLER  PIC X(12)  VALUE "CLEAR SCREEN".
       01  MID-AREA.
           02  FILLER  PIC N(09)  VALUE
                "ê∂ã¶î≠íçáÇÅ@ñ‚çáÇπ".
           02  FILLER.
               03  FILLER  PIC N(03)  VALUE "î[ì¸ì˙".
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
               03  A-FNEN  PIC 9(02).
               03  A-FGET  PIC 9(02).
               03  A-FPEY  PIC 9(02).
           02  FILLER.
               03  A-TNEN  PIC 9(02).
               03  A-TGET  PIC 9(02).
               03  A-TPEY  PIC 9(02).
           02  A-DMM  PIC 9(01).
       01  DSP-AREA.
           02  DSP-DATA.
               03  FILLER.
                   04  FILLER  PIC 9(06).
                   04  FILLER  PIC 9(04).
                   04  FILLER  PIC N(23).
                   04  FILLER  PIC 9(06).
                   04  FILLER  PIC N(06).
                   04  FILLER  PIC 9(01).
               03  FILLER.
                   04  FILLER  PIC 9(06).
                   04  FILLER  PIC N(09).
                   04  FILLER  PIC N(01).
           02  DSP-CNA.
               03  FILLER  PIC 9(03).
               03  FILLER  PIC N(20).
       01  ERR-MSG-AREA.
           02  FILLER.
               03  E-STAT   PIC X(02).
               03  E-ME1    PIC N(07)   VALUE
                     "äYìñÉfÅ[É^Ç»Çµ".
               03  E-ME2    PIC N(05)   VALUE
                     "ÉfÅ[É^èIóπ".
               03  E-ME98   PIC X(05)   VALUE X"1B4A05".
               03  E-ME99   PIC X(05)   VALUE X"1B4205".
               03  E-CL     PIC X(40)   VALUE
                     "                                        ".
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
            "MID-AREA" " " "0" "0" "74" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MID-AREA" "N" "1" "32" "18" " " "MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02MID-AREA" " " "12" "0" "6" "01MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102MID-AREA" "N" "12" "37" "6" " " "02MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03MID-AREA" " " "14" "0" "16" "02MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103MID-AREA" "N" "14" "18" "8" " " "03MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203MID-AREA" "X" "14" "36" "8" "0103MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04MID-AREA" " " "16" "0" "12" "03MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104MID-AREA" "N" "16" "18" "4" " " "04MID-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204MID-AREA" "X" "16" "36" "8" "0104MID-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05MID-AREA" "X" "23" "48" "22" "04MID-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "14" "0" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FNEN" "9" "14" "36" "2" " " "01ACP-AREA" RETURNING RESU.
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
            "02ACP-AREA" " " "16" "0" "6" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNEN" "9" "16" "36" "2" " " "02ACP-AREA" RETURNING RESU.
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
            "A-DMM" "9" "23" "65" "1" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "144" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATA" " " "0" "0" "101" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DATA" " " "W-L1" "0" "75" " " "DSP-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-DATA" "9" "W-L1" "1" "6" " " "01DSP-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101DSP-DATA" BY REFERENCE HAT-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-DATA" "9" "W-L1" "9" "4" "0101DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201DSP-DATA" BY REFERENCE HAT-041 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-DATA" "N" "W-L1" "13" "46" "0201DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-DATA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-DATA" "9" "W-L1" "60" "6" "0301DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-DATA" BY REFERENCE W-SZD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-DATA" "N" "W-L1" "67" "12" "0401DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501DSP-DATA" BY REFERENCE W-UNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0601DSP-DATA" "9" "W-L1" "80" "1" "0501DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0601DSP-DATA" BY REFERENCE HAT-24 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATA" " " "W-L2" "0" "26" "01DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-DATA" "9" "W-L2" "2" "6" " " "02DSP-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102DSP-DATA" BY REFERENCE W-NND "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-DATA" "N" "W-L2" "55" "18" "0102DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202DSP-DATA" BY REFERENCE HAT-19 "46" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-DATA" "N" "W-L2" "78" "2" "0202DSP-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302DSP-DATA" BY REFERENCE W-HK "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CNA" " " "W-L2" "0" "43" "DSP-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CNA" "9" "W-L2" "9" "3" " " "DSP-CNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-CNA" BY REFERENCE HAT-042 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CNA" "N" "W-L2" "13" "40" "01DSP-CNA" " "
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
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "MID-AREA" MID-AREA "p" RETURNING RESU.
           MOVE  ZERO      TO  WORK-AREA.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-FNEN "A-FNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO  TO  M-95
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
                   GO  TO  M-55
               END-IF
           END-IF
           IF  W-TPEY      <  1  OR  >  31
               GO  TO  M-45
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  M-45
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
           CALL "SD_Screen_Output" USING "SJS75R" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SK-HAT_PNAME1 "SHARED" BY REFERENCE SK-HAT_IDLST "1"
            "HAT-KEY" BY REFERENCE HAT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE  SPACE     TO  HAT-KEY.
      *           START  SK-HAT     KEY  NOT <  HAT-KEY   INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SK-HAT_PNAME1 "HAT-KEY" " NOT < " HAT-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  M-75
           END-IF.
       M-60.
      *           READ  SK-HAT    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  M-75
           END-IF
           MOVE  HAT-03    TO  W-NNDD.
           IF  W-NND        <  W-FDT OR  >  W-TDT
               GO  TO  M-60
           END-IF
      *
           IF  W-END         =  0
               MOVE  1         TO  W-END
           END-IF
           MOVE  SPACE     TO  W-UNA  W-HK.
      *
           MOVE  HAT-041  TO  TC-TCD.
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
           MOVE  2         TO  JCON1-01.
           MOVE  HAT-16    TO  JCON1-02.
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
           IF  HAT-97       =  0
               MOVE  "ñ¢"    TO  W-HK
           ELSE
               MOVE  "çœ"    TO  W-HK
           END-IF
      *
           MOVE  HAT-25    TO  W-SZDD.
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
           IF  HAT-042      = 001
               GO  TO  M-60
           END-IF
           MOVE  HAT-041  TO  TC-TCD.
           MOVE  HAT-042  TO  TC-CCD.
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
                BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1
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
           CALL "SD_Screen_Output" USING "SJS75R" RETURNING RESU.
           MOVE  3         TO  W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  4         TO  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           GO  TO  M-65.
       M-75.
           CALL "DB_F_Close" USING
            BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1.
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
