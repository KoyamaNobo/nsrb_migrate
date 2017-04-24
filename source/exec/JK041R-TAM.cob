       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK041R.
      ****************************************************************
      *    ñ¢èàóùÉfÅ[É^ñ‚çáÇπ                                        *
      *    SCREEN   :  SJK041              89/ 8/17   NO.205 H.K     *
      *    W-SYSIN  :  ëSé–=0 , ëIë=1 , ëqå…=9                      *
      ****************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ‹∞∏ ¥ÿ± >----*
       77  ERR-STAT           PIC X(02).
       77  END-SW             PIC 9(01).
       77  NN                 PIC 9(02).
       77  KAKU-W             PIC X(01).
       77  W-SYSIN            PIC 9(01).
       77  INV-SW             PIC 9(01).
       77  W-SOK              PIC N(06) VALUE SPACE.
       01  GAMEN-WK.
         02  W-01             PIC 9(06).
         02  W-02             PIC 9(06).
         02  W-03             PIC 9(06).
         02  W-04             PIC 9(06).
         02  W-11             PIC 9(06).
         02  W-12             PIC 9(06).
         02  W-13             PIC 9(06).
         02  W-14             PIC 9(06).
         02  W-15             PIC 9(06).
         02  W-16             PIC 9(06).
         02  W-17             PIC 9(06).
         02  W-GOKEI          PIC 9(06).
         02  W2-01            PIC 9(06).
         02  W2-02            PIC 9(06).
         02  W2-03            PIC 9(06).
         02  W2-04            PIC 9(06).
         02  W2-11            PIC 9(06).
         02  W2-12            PIC 9(06).
         02  W2-13            PIC 9(06).
         02  W2-14            PIC 9(06).
         02  W2-15            PIC 9(06).
         02  W2-16            PIC 9(06).
         02  W2-17            PIC 9(06).
         02  W2-GOKEI         PIC 9(06).
         02  W-MEI.
           03  W-NGP.
             04  W-NEN        PIC 9(02).
             04  W-GET        PIC 9(02).
             04  W-PEY        PIC 9(02).
           03  W-SD.
             04  W-SKS        PIC 9(05).
             04  W-SIS        PIC 9(05).
           03  W-ND.
             04  W-NKS        PIC 9(05).
             04  W-NIS        PIC 9(05).
           03  W-OD.
             04  W-OKS        PIC 9(05).
             04  W-OIS        PIC 9(05).
           03  W-WD.
             04  W-WKS        PIC 9(05).
             04  W-WIS        PIC 9(05).
           03  W-FD.
             04  W-FKS        PIC 9(05).
             04  W-FIS        PIC 9(05).
           03  W-TD.
             04  W-TKS        PIC 9(05).
             04  W-TIS        PIC 9(05).
           03  W-AD.
             04  W-AKS        PIC 9(05).
             04  W-AIS        PIC 9(05).
         02  CNT              PIC 9(02).
         02  W-GD.
           03  W-STS          PIC 9(05).
           03  W-NTS          PIC 9(05).
           03  W-OTS          PIC 9(05).
           03  W-WTS          PIC 9(05).
           03  W-FTS          PIC 9(05).
           03  W-TTS          PIC 9(05).
           03  W-ATS          PIC 9(05).
       01  W-HIZUKE.
           02  W-DATE         PIC 9(06).
       77  JSTR-WK            PIC 9(06).
       77  JNIF-WK            PIC 9(06).
       01  TDNW-WK.
           02  WK-WSTC        PIC 9(09).
           02  WK-WDNO        PIC 9(09).
       01  TDNN-WK.
           02  WK-NSTC        PIC 9(09).
           02  WK-NDNO        PIC 9(09).
       01  TDIF-WK            PIC 9(06).
       01  TDNA-WK.
           02  WK-ASTC        PIC 9(07).
           02  WK-ADNO        PIC 9(06).
           COPY  LWMSG.
           COPY  LSTAT.
      *
           COPY  L-JCON.
           COPY  L-JOJF.
           COPY  L-JSTR-TAM.
           COPY  L-JNIF-TAM.
           COPY  LOKJF-TAM.
           COPY  LITDNW-TAM.
           COPY  LITDNN-TAM.
           COPY  L-TDIF-TAM.
           COPY  LITDNA-TAM.
           COPY  L-JOSF-TAM.
           COPY  LJOLJF-TAM.
      *
      *FD  DATEW
       01  DATEW_JK041R.
           02  DATEW_PNAME1   PIC  X(005) VALUE "DATEW".
           02  F              PIC  X(001).
           02  DATEW_LNAME    PIC  X(012) VALUE "DATEW_JK041R".
           02  F              PIC  X(001).
           02  DATEW_KEY1     PIC  X(100) VALUE SPACE.
           02  DATEW_KEY2     PIC  X(100) VALUE SPACE.
           02  DATEW_SORT     PIC  X(100) VALUE SPACE.
           02  DATEW_IDLST    PIC  X(100) VALUE SPACE.
           02  DATEW_RES      USAGE  POINTER.
       01  DATEW-R.
           02  DATEW-KEY      PIC X(08).
           02  DATEW-KEY1     REDEFINES  DATEW-KEY.
             03  DATEW-NEN    PIC 9(04).
             03  DATEW-GP     PIC 9(04).
           02  DATEW-KEY2     REDEFINES  DATEW-KEY.
             03  DATEW-F      PIC 9(02).
             03  DATEW-NGP    PIC 9(06).
           02  DATEW-SD.
             03  DATEW-SKS    PIC 9(05).
             03  DATEW-SIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-ND.
             03  DATEW-NKS    PIC 9(05).
             03  DATEW-NIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-OD.
             03  DATEW-OKS    PIC 9(05).
             03  DATEW-OIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-WD.
             03  DATEW-WKS    PIC 9(05).
             03  DATEW-WIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-FD.
             03  DATEW-FKS    PIC 9(05).
             03  DATEW-FIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-TD.
             03  DATEW-TKS    PIC 9(05).
             03  DATEW-TIS    PIC 9(05).
             03  F            PIC 9(05).
           02  DATEW-AD.
             03  DATEW-AKS    PIC 9(05).
             03  DATEW-AIS    PIC 9(05).
             03  F            PIC 9(05).
           02  F              PIC X(15).
       77  F                  PIC X(01).
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-GAMEN1.
         02  DSP-CLE.
           03  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
         02  DSP-MID.
           03  FILLER  PIC N(15)    VALUE
                "ÅñÅñÅ@ñ¢èàóùÉfÅ[É^ñ‚çáÇπÅ@ÅñÅñ".
           03  FILLER  PIC X(40)    VALUE
                "ñ{é–=0 , ã ìá=1 , í√éR=2 , ëÅìá=3   ÿ¿∞›".
         02  DSP-SOK   PIC N(06).
         02  DSP-SJD.
           03  FILLER.
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZ9 .
           03  FILLER.
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZZ .
             04  FILLER  PIC ZZZZ9 .
         02  FILLER.
           03  DSP-NGP1.
             04  FILLER  PIC 9(02).
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
           03  DSP-NGP2.
             04  FILLER  PIC 9(02).
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
           03  DSP-NGP3.
             04  FILLER  PIC 9(02).
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
             04  FILLER  PIC X(01)    VALUE  "/".
             04  FILLER  PIC Z9 .
           03  DSP-NGP4.
             04  FILLER  PIC N(01)    VALUE  "à»".
             04  FILLER  PIC N(01)    VALUE  "ç~".
         02  DSP-KD1.
           03  FILLER  PIC N(02)    VALUE "ã≥àÁ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-ID1.
           03  FILLER  PIC N(02)    VALUE "àÍî ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-KD2.
           03  FILLER  PIC N(02)    VALUE "ã≥àÁ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-ID2.
           03  FILLER  PIC N(02)    VALUE "àÍî ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-KD3.
           03  FILLER  PIC N(02)    VALUE "ã≥àÁ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-ID3.
           03  FILLER  PIC N(02)    VALUE "àÍî ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-KD4.
           03  FILLER  PIC N(02)    VALUE "ã≥àÁ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-ID4.
           03  FILLER  PIC N(02)    VALUE "àÍî ".
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
           03  FILLER  PIC ZZZZZ .
         02  DSP-TD.
           03  FILLER  PIC N(02)    VALUE "çáåv".
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
           03  FILLER  PIC ZZZZ9 .
       01  ACP-SYSIN.
         02  INP-SYSIN   PIC 9 .
       01  ACP-KAKU.
         02  INP-KAKU    PIC X .
         COPY  LSMSG.
      *==============================================================*
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-GAMEN1
       CALL "SD_Init" USING 
            "DSP-GAMEN1" " " "0" "0" "593" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLE" " " "1" "0" "12" " " "DSP-GAMEN1" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID" " " "0" "0" "70" "DSP-CLE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID" "N" "1" "21" "30" " " "DSP-MID" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-MID" "X" "2" "21" "40" "01DSP-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SOK" "N" "1" "68" "12" "DSP-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SOK" BY REFERENCE W-SOK "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SJD" " " "0" "0" "120" "DSP-SOK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SJD" " " "6" "0" "60" " " "DSP-SJD" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-SJD" "ZZZZZ" "6" "10" "5" " " "01DSP-SJD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101DSP-SJD" BY REFERENCE W-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-SJD" "ZZZZZ" "6" "16" "5" "0101DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201DSP-SJD" BY REFERENCE W-02 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-SJD" "ZZZZZ" "6" "22" "5" "0201DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-SJD" BY REFERENCE W-03 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-SJD" "ZZZZZ" "6" "28" "5" "0301DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-SJD" BY REFERENCE W-04 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-SJD" "ZZZZZ" "6" "34" "5" "0401DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501DSP-SJD" BY REFERENCE W-11 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0601DSP-SJD" "ZZZZZ" "6" "40" "5" "0501DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0601DSP-SJD" BY REFERENCE W-12 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0701DSP-SJD" "ZZZZZ" "6" "46" "5" "0601DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0701DSP-SJD" BY REFERENCE W-13 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0801DSP-SJD" "ZZZZZ" "6" "52" "5" "0701DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0801DSP-SJD" BY REFERENCE W-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0901DSP-SJD" "ZZZZZ" "6" "58" "5" "0801DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0901DSP-SJD" BY REFERENCE W-15 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1001DSP-SJD" "ZZZZZ" "6" "64" "5" "0901DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1001DSP-SJD" BY REFERENCE W-16 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1101DSP-SJD" "ZZZZZ" "6" "70" "5" "1001DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1101DSP-SJD" BY REFERENCE W-17 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1201DSP-SJD" "ZZZZ9" "6" "76" "5" "1101DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1201DSP-SJD" BY REFERENCE W-GOKEI "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-SJD" " " "7" "0" "60" "01DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-SJD" "ZZZZZ" "7" "10" "5" " " "02DSP-SJD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102DSP-SJD" BY REFERENCE W2-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-SJD" "ZZZZZ" "7" "16" "5" "0102DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202DSP-SJD" BY REFERENCE W2-02 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-SJD" "ZZZZZ" "7" "22" "5" "0202DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302DSP-SJD" BY REFERENCE W2-03 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-SJD" "ZZZZZ" "7" "28" "5" "0302DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402DSP-SJD" BY REFERENCE W2-04 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502DSP-SJD" "ZZZZZ" "7" "34" "5" "0402DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502DSP-SJD" BY REFERENCE W2-11 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602DSP-SJD" "ZZZZZ" "7" "40" "5" "0502DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602DSP-SJD" BY REFERENCE W2-12 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0702DSP-SJD" "ZZZZZ" "7" "46" "5" "0602DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702DSP-SJD" BY REFERENCE W2-13 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0802DSP-SJD" "ZZZZZ" "7" "52" "5" "0702DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802DSP-SJD" BY REFERENCE W2-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0902DSP-SJD" "ZZZZZ" "7" "58" "5" "0802DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902DSP-SJD" BY REFERENCE W2-15 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1002DSP-SJD" "ZZZZZ" "7" "64" "5" "0902DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002DSP-SJD" BY REFERENCE W2-16 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1102DSP-SJD" "ZZZZZ" "7" "70" "5" "1002DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1102DSP-SJD" BY REFERENCE W2-17 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1202DSP-SJD" "ZZZZ9" "7" "76" "5" "1102DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1202DSP-SJD" BY REFERENCE W2-GOKEI "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN1" " " "10" "0" "28" "DSP-SJD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGP1" " " "10" "0" "8" " " "05DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-NGP1" "9" "10" "19" "2" " " "DSP-NGP1" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-NGP1" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-NGP1" "X" "10" "21" "1" "01DSP-NGP1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-NGP1" "Z9" "10" "22" "2" "02DSP-NGP1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-NGP1" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-NGP1" "X" "10" "24" "1" "03DSP-NGP1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-NGP1" "Z9" "10" "25" "2" "04DSP-NGP1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-NGP1" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGP2" " " "10" "0" "8" "DSP-NGP1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-NGP2" "9" "10" "33" "2" " " "DSP-NGP2"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-NGP2" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-NGP2" "X" "10" "35" "1" "01DSP-NGP2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-NGP2" "Z9" "10" "36" "2" "02DSP-NGP2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-NGP2" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-NGP2" "X" "10" "38" "1" "03DSP-NGP2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-NGP2" "Z9" "10" "39" "2" "04DSP-NGP2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-NGP2" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGP3" " " "10" "0" "8" "DSP-NGP2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-NGP3" "9" "10" "47" "2" " " "DSP-NGP3"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-NGP3" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-NGP3" "X" "10" "49" "1" "01DSP-NGP3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-NGP3" "Z9" "10" "50" "2" "02DSP-NGP3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-NGP3" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-NGP3" "X" "10" "52" "1" "03DSP-NGP3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-NGP3" "Z9" "10" "53" "2" "04DSP-NGP3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-NGP3" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGP4" " " "10" "0" "4" "DSP-NGP3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-NGP4" "N" "10" "63" "2" " " "DSP-NGP4"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-NGP4" "N" "10" "66" "2" "01DSP-NGP4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-KD1" " " "0" "0" "39" "05DSP-GAMEN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-KD1" "N" "11" "19" "4" " " "DSP-KD1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-KD1" "ZZZZZ" "12" "18" "5" "01DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-KD1" BY REFERENCE W-SKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-KD1" "ZZZZZ" "13" "18" "5" "02DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-KD1" BY REFERENCE W-NKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-KD1" "ZZZZZ" "14" "18" "5" "03DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-KD1" BY REFERENCE W-OKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-KD1" "ZZZZZ" "15" "18" "5" "04DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-KD1" BY REFERENCE W-WKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-KD1" "ZZZZZ" "16" "18" "5" "05DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-KD1" BY REFERENCE W-FKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-KD1" "ZZZZZ" "17" "18" "5" "06DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-KD1" BY REFERENCE W-TKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-KD1" "ZZZZZ" "18" "18" "5" "07DSP-KD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-KD1" BY REFERENCE W-AKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ID1" " " "0" "0" "39" "DSP-KD1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ID1" "N" "11" "25" "4" " " "DSP-ID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ID1" "ZZZZZ" "12" "24" "5" "01DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ID1" BY REFERENCE W-SIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ID1" "ZZZZZ" "13" "24" "5" "02DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ID1" BY REFERENCE W-NIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-ID1" "ZZZZZ" "14" "24" "5" "03DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-ID1" BY REFERENCE W-OIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-ID1" "ZZZZZ" "15" "24" "5" "04DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-ID1" BY REFERENCE W-WIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-ID1" "ZZZZZ" "16" "24" "5" "05DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-ID1" BY REFERENCE W-FIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-ID1" "ZZZZZ" "17" "24" "5" "06DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-ID1" BY REFERENCE W-TIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-ID1" "ZZZZZ" "18" "24" "5" "07DSP-ID1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-ID1" BY REFERENCE W-AIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KD2" " " "0" "0" "39" "DSP-ID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-KD2" "N" "11" "33" "4" " " "DSP-KD2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-KD2" "ZZZZZ" "12" "32" "5" "01DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-KD2" BY REFERENCE W-SKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-KD2" "ZZZZZ" "13" "32" "5" "02DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-KD2" BY REFERENCE W-NKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-KD2" "ZZZZZ" "14" "32" "5" "03DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-KD2" BY REFERENCE W-OKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-KD2" "ZZZZZ" "15" "32" "5" "04DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-KD2" BY REFERENCE W-WKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-KD2" "ZZZZZ" "16" "32" "5" "05DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-KD2" BY REFERENCE W-FKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-KD2" "ZZZZZ" "17" "32" "5" "06DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-KD2" BY REFERENCE W-TKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-KD2" "ZZZZZ" "18" "32" "5" "07DSP-KD2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-KD2" BY REFERENCE W-AKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ID2" " " "0" "0" "39" "DSP-KD2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ID2" "N" "11" "39" "4" " " "DSP-ID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ID2" "ZZZZZ" "12" "38" "5" "01DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ID2" BY REFERENCE W-SIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ID2" "ZZZZZ" "13" "38" "5" "02DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ID2" BY REFERENCE W-NIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-ID2" "ZZZZZ" "14" "38" "5" "03DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-ID2" BY REFERENCE W-OIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-ID2" "ZZZZZ" "15" "38" "5" "04DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-ID2" BY REFERENCE W-WIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-ID2" "ZZZZZ" "16" "38" "5" "05DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-ID2" BY REFERENCE W-FIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-ID2" "ZZZZZ" "17" "38" "5" "06DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-ID2" BY REFERENCE W-TIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-ID2" "ZZZZZ" "18" "38" "5" "07DSP-ID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-ID2" BY REFERENCE W-AIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KD3" " " "0" "0" "39" "DSP-ID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-KD3" "N" "11" "47" "4" " " "DSP-KD3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-KD3" "ZZZZZ" "12" "46" "5" "01DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-KD3" BY REFERENCE W-SKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-KD3" "ZZZZZ" "13" "46" "5" "02DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-KD3" BY REFERENCE W-NKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-KD3" "ZZZZZ" "14" "46" "5" "03DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-KD3" BY REFERENCE W-OKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-KD3" "ZZZZZ" "15" "46" "5" "04DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-KD3" BY REFERENCE W-WKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-KD3" "ZZZZZ" "16" "46" "5" "05DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-KD3" BY REFERENCE W-FKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-KD3" "ZZZZZ" "17" "46" "5" "06DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-KD3" BY REFERENCE W-TKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-KD3" "ZZZZZ" "18" "46" "5" "07DSP-KD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-KD3" BY REFERENCE W-AKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ID3" " " "0" "0" "39" "DSP-KD3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ID3" "N" "11" "53" "4" " " "DSP-ID3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ID3" "ZZZZZ" "12" "52" "5" "01DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ID3" BY REFERENCE W-SIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ID3" "ZZZZZ" "13" "52" "5" "02DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ID3" BY REFERENCE W-NIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-ID3" "ZZZZZ" "14" "52" "5" "03DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-ID3" BY REFERENCE W-OIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-ID3" "ZZZZZ" "15" "52" "5" "04DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-ID3" BY REFERENCE W-WIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-ID3" "ZZZZZ" "16" "52" "5" "05DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-ID3" BY REFERENCE W-FIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-ID3" "ZZZZZ" "17" "52" "5" "06DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-ID3" BY REFERENCE W-TIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-ID3" "ZZZZZ" "18" "52" "5" "07DSP-ID3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-ID3" BY REFERENCE W-AIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KD4" " " "0" "0" "39" "DSP-ID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-KD4" "N" "11" "61" "4" " " "DSP-KD4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-KD4" "ZZZZZ" "12" "60" "5" "01DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-KD4" BY REFERENCE W-SKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-KD4" "ZZZZZ" "13" "60" "5" "02DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-KD4" BY REFERENCE W-NKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-KD4" "ZZZZZ" "14" "60" "5" "03DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-KD4" BY REFERENCE W-OKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-KD4" "ZZZZZ" "15" "60" "5" "04DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-KD4" BY REFERENCE W-WKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-KD4" "ZZZZZ" "16" "60" "5" "05DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-KD4" BY REFERENCE W-NKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-KD4" "ZZZZZ" "17" "60" "5" "06DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-KD4" BY REFERENCE W-TKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-KD4" "ZZZZZ" "18" "60" "5" "07DSP-KD4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-KD4" BY REFERENCE W-AKS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ID4" " " "0" "0" "39" "DSP-KD4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ID4" "N" "11" "67" "4" " " "DSP-ID4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ID4" "ZZZZZ" "12" "66" "5" "01DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ID4" BY REFERENCE W-SIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ID4" "ZZZZZ" "13" "66" "5" "02DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ID4" BY REFERENCE W-NIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-ID4" "ZZZZZ" "14" "66" "5" "03DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-ID4" BY REFERENCE W-OIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-ID4" "ZZZZZ" "15" "66" "5" "04DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-ID4" BY REFERENCE W-WIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-ID4" "ZZZZZ" "16" "66" "5" "05DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-ID4" BY REFERENCE W-FIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-ID4" "ZZZZZ" "17" "66" "5" "06DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-ID4" BY REFERENCE W-TIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-ID4" "ZZZZZ" "18" "66" "5" "07DSP-ID4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-ID4" BY REFERENCE W-AIS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TD" " " "0" "0" "39" "DSP-ID4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-TD" "N" "11" "75" "4" " " "DSP-TD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-TD" "ZZZZ9" "12" "74" "5" "01DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-TD" BY REFERENCE W-STS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-TD" "ZZZZ9" "13" "74" "5" "02DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-TD" BY REFERENCE W-NTS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-TD" "ZZZZ9" "14" "74" "5" "03DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-TD" BY REFERENCE W-OTS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-TD" "ZZZZ9" "15" "74" "5" "04DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-TD" BY REFERENCE W-WTS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-TD" "ZZZZ9" "16" "74" "5" "05DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-TD" BY REFERENCE W-FTS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-TD" "ZZZZ9" "17" "74" "5" "06DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-TD" BY REFERENCE W-TTS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-TD" "ZZZZ9" "18" "74" "5" "07DSP-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-TD" BY REFERENCE W-ATS "5" "0" RETURNING RESU.
      *ACP-SYSIN
       CALL "SD_Init" USING 
            "ACP-SYSIN" " " "2" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INP-SYSIN" "9" "2" "56" "1" " " "ACP-SYSIN" RETURNING RESU.
       CALL "SD_Using" USING 
            "INP-SYSIN" BY REFERENCE W-SYSIN "1" "0" RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INP-KAKU" "X" "24" "51" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "INP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *--------------------------------------------------------------*
      *    “ ≤ ›  Ÿ ∞ ¡ ›                                            *
      *--------------------------------------------------------------*
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           IF  END-STS   =  PF9
               GO  TO  MAINLINE-END
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT.
           PERFORM  END-RTN   THRU  END-EX.
           PERFORM  MESG-RTN  THRU  MESG-RTN-EXIT.
       MAINLINE-END.
           CALL "DB_Close".
           STOP RUN.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
           PERFORM  DETL-RTN  THRU  DETL-RTN-EXIT.
           PERFORM  DISP-RTN  THRU  DISP-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    D E T L  -  R T N                                         *
      *--------------------------------------------------------------*
       DETL-RTN.
           IF  W-SYSIN    =  0
               PERFORM  JORD-RTN  THRU  JORD-RTN-EXIT
               PERFORM  JORD2-RTN  THRU  JORD2-RTN-EXIT
           END-IF
           IF  W-SYSIN    =  9
               PERFORM  JOLS-RTN  THRU  JOLS-RTN-EXIT
               PERFORM  JOLR-RTN  THRU  JOLR-RTN-EXIT
           END-IF
           PERFORM  JSRD-RTN  THRU  JSRD-RTN-EXIT.
           PERFORM  JNRD-RTN  THRU  JNRD-RTN-EXIT.
           PERFORM  OKRD-RTN  THRU  OKRD-RTN-EXIT.
           PERFORM  TWRD-RTN  THRU  TWRD-RTN-EXIT.
           PERFORM  TNRD-RTN  THRU  TNRD-RTN-EXIT.
           PERFORM  TDRD-RTN  THRU  TDRD-RTN-EXIT.
           PERFORM  TARD-RTN  THRU  TARD-RTN-EXIT.
       DETL-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           MOVE  ZERO  TO  NN   JSTR-WK   JNIF-WK  TDNW-WK  TDNN-WK
                                                   TDIF-WK  TDNA-WK.
           INITIALIZE      GAMEN-WK.
           ACCEPT      W-SYSIN FROM ARGUMENT-VALUE.
           IF  W-SYSIN     =  0  OR  9
               GO  TO  INIT-020
           END-IF
           CALL "SD_Output" USING "DSP-CLE" DSP-CLE "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-MID" DSP-MID "p" RETURNING RESU.
       INIT-010.
           CALL "SD_Accept" USING BY REFERENCE INP-SYSIN "INP-SYSIN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS    =  PF9
               GO  TO  INIT-RTN-EXIT
           END-IF
           IF  END-STS  NOT =  HTB  AND  SKP
               GO  TO  INIT-010
           END-IF
           IF  W-SYSIN    >  3
               GO  TO  INIT-010
           END-IF
           IF  W-SYSIN    =  0
               GO  TO  INIT-020
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE  SPACE     TO  JCON3-KEY.
           MOVE  3         TO  JCON3-01.
           IF  W-SYSIN    =  1
               MOVE  6         TO  JCON3-02
           END-IF
           IF  W-SYSIN    =  2
               MOVE  7         TO  JCON3-02
           END-IF
           IF  W-SYSIN    =  3
               MOVE  4         TO  JCON3-02
           END-IF
      *           READ  JCON       WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  JCON3-03
           END-IF
           MOVE  JCON3-03  TO  W-SOK.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       INIT-020.
           CALL "SD_Screen_Output" USING "SJK041" RETURNING RESU.
           IF  W-SYSIN    NOT =  0  AND  9
               CALL "SD_Output" USING
                "DSP-SOK" DSP-SOK "p" RETURNING RESU
           END-IF
           ACCEPT  W-DATE    FROM  DATE.
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇ`ÇdÇcÅ@(JORD-RTN)                        *
      *--------------------------------------------------------------*
       JORD-RTN.
           MOVE  "0002"  TO   JOJF-KEY.
      *           READ   JOJF  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO JORD-RTN-EXIT
           END-IF
           IF  JOJF-061  =  6
               GO TO JORD-RTN-EXIT
           END-IF.
       JORD-010.
           ADD   1  TO  NN.
           IF  JOJF-08(NN)  =  01
               MOVE  JOJF-10(NN)  TO  W-01
           ELSE
               IF  JOJF-08(NN)  =  02
                   MOVE  JOJF-10(NN)  TO  W-02
               ELSE
                   IF  JOJF-08(NN)  =  03
                       MOVE  JOJF-10(NN)  TO  W-03
                   ELSE
                       IF  JOJF-08(NN)  =  04
                           MOVE  JOJF-10(NN)  TO  W-04
                       ELSE
                           IF  JOJF-08(NN)  =  11
                               MOVE  JOJF-10(NN)  TO  W-11
                           ELSE
                               IF  JOJF-08(NN)  =  12
                                   MOVE  JOJF-10(NN)  TO  W-12
                               ELSE
                                   IF  JOJF-08(NN)  =  13
                                       MOVE  JOJF-10(NN)  TO  W-13
                                   ELSE
                                       IF  JOJF-08(NN)  =  14
                                           MOVE  JOJF-10(NN)  TO  W-14
                                       ELSE
                                           IF  JOJF-08(NN)  =  15
                                           MOVE  JOJF-10(NN)  TO  W-15
                                           ELSE
                                           IF  JOJF-08(NN)  =  16
                                           MOVE  JOJF-10(NN)  TO  W-16
                                           ELSE
                                           IF  JOJF-08(NN)  =  17
                                           MOVE  JOJF-10(NN)  TO  W-17
                                           END-IF
                                           END-IF
                                           END-IF
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  NN  NOT  =  10
               GO TO JORD-010
           END-IF
           COMPUTE  W-GOKEI  =  W-01  +  W-02  +  W-03  +  W-04  +
                       W-11 + W-12 + W-13 + W-14 + W-15 + W-16 + W-17.
       JORD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇ`ÇdÇcÅ@(JORD2-RTN)        [ADD:90.04.11] *
      *--------------------------------------------------------------*
       JORD2-RTN.
           MOVE  "0001"  TO   JOJF-KEY.
      *           READ   JOJF  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO JORD2-RTN-EXIT
           END-IF
           IF  JOJF-061  =  6
               GO TO JORD2-RTN-EXIT
           END-IF
           MOVE   ZERO   TO   NN.
       JORD2-010.
           ADD   1  TO  NN.
           IF  JOJF-08(NN)  =  01
               MOVE  JOJF-09(NN)  TO  W2-01
           ELSE
               IF  JOJF-08(NN)  =  02
                   MOVE  JOJF-09(NN)  TO  W2-02
               ELSE
                   IF  JOJF-08(NN)  =  03
                       MOVE  JOJF-09(NN)  TO  W2-03
                   ELSE
                       IF  JOJF-08(NN)  =  04
                           MOVE  JOJF-09(NN)  TO  W2-04
                       ELSE
                           IF  JOJF-08(NN)  =  11
                               MOVE  JOJF-09(NN)  TO  W2-11
                           ELSE
                               IF  JOJF-08(NN)  =  12
                                   MOVE  JOJF-09(NN)  TO  W2-12
                               ELSE
                                   IF  JOJF-08(NN)  =  13
                                       MOVE  JOJF-09(NN)  TO  W2-13
                                   ELSE
                                       IF  JOJF-08(NN)  =  14
                                           MOVE  JOJF-09(NN)  TO  W2-14
                                       ELSE
                                           IF  JOJF-08(NN)  =  15
                                           MOVE  JOJF-09(NN)  TO  W2-15
                                           ELSE
                                           IF  JOJF-08(NN)  =  16
                                           MOVE  JOJF-09(NN)  TO  W2-16
                                           ELSE
                                           IF  JOJF-08(NN)  =  17
                                           MOVE  JOJF-09(NN)  TO  W2-17
                                           END-IF
                                           END-IF
                                           END-IF
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  NN  NOT  =  10
               GO TO JORD2-010
           END-IF
           COMPUTE  W2-GOKEI  =  W2-01  +  W2-02  +  W2-03  +  W2-04  +
               W2-11 + W2-12 + W2-13 + W2-14 + W2-15 + W2-16 + W2-17.
       JORD2-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ÇnÅ^ÇkëóêMÇeÅ@ÇqÇ`ÇdÇcÅ@(JOLS-RTN)    ëqå…ë§              *
      *--------------------------------------------------------------*
       JOLS-RTN.
      *           READ   JOLSF     AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSF_PNAME1 BY REFERENCE JOLSF-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO JOLS-010
           END-IF
           IF  JOLSF1-01    =  11
               ADD   1            TO  W-11
           ELSE
               IF  JOLSF1-01    =  12
                   ADD   1            TO  W-12
               ELSE
                   IF  JOLSF1-01    =  13
                       ADD   1            TO  W-13
                   ELSE
                       IF  JOLSF1-01    =  14
                           ADD   1            TO  W-14
                       ELSE
                           IF  JOLSF1-01    =  15
                               ADD   1            TO  W-15
                           ELSE
                               IF  JOLSF1-01    =  16
                                   ADD   1            TO  W-16
                               ELSE
                                   IF  JOLSF1-01    =  17
                                       ADD   1            TO  W-17
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           GO TO JOLS-RTN.
       JOLS-010.
           COMPUTE  W-GOKEI  =  W-01  +  W-02  +  W-03  +  W-04  +
                       W-11 + W-12 + W-13 + W-14 + W-15 + W-16 + W-17.
       JOLS-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ÇnÅ^ÇkéÛêMÇeÅ@ÇqÇ`ÇdÇcÅ@(JOLR-RTN)    ëqå…ë§              *
      *--------------------------------------------------------------*
       JOLR-RTN.
      *           READ   JOLJF     AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSF_PNAME1 BY REFERENCE JOLSF-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO JOLR-010
           END-IF
           IF  JOLJF11-01   =  11
               ADD   1            TO  W2-11
           END-IF
           GO TO JOLR-RTN.
       JOLR-010.
           COMPUTE  W2-GOKEI  =  W2-01 +  W2-02 +  W2-03 +  W2-04 +
                 W2-11 + W2-12 + W2-13 + W2-14 + W2-15 + W2-16 + W2-17.
       JOLR-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    èoâ◊éwê}ÉgÉâÉìÅ@ÇqÇdÇ`ÇcÅ@(JSRD-RTN)                      *
      *--------------------------------------------------------------*
       JSRD-RTN.
      *           READ  JSTR  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO JSRD-RTN-EXIT
           END-IF
           IF  JSTR-17  =  0
               GO TO JSRD-RTN
           END-IF
           IF  JSTR-05  =  ZERO
               GO TO JSRD-RTN
           END-IF
           IF  JSTR-03  =  5  OR  6
               GO TO JSRD-RTN
           END-IF
           IF  JSTR-158 NOT = 0
               GO TO JSRD-RTN
           END-IF
           IF  JSTR-WK  =  JSTR-01
               GO TO JSRD-RTN
           END-IF
           MOVE  JSTR-01    TO  JSTR-WK.
           IF  W-SYSIN  =  9
               GO TO JSRD-010
           END-IF
           IF  W-SYSIN  =  0
               IF  JSTR-07  NOT  =  2 AND 3 AND 4 AND 5 AND 6 AND 7
                   GO TO JSRD-010
               ELSE
                   GO TO JSRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  1
               IF  JSTR-07  =  6
                   GO  TO  JSRD-010
               ELSE
                   GO  TO  JSRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  2
               IF  JSTR-07  =  7
                   GO  TO  JSRD-010
               ELSE
                   GO  TO  JSRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  3
               IF  JSTR-07  =  4
                   GO  TO  JSRD-010
               ELSE
                   GO  TO  JSRD-RTN
               END-IF
           END-IF
           GO TO JSRD-RTN.
       JSRD-010.
           MOVE  0        TO  INV-SW.
           MOVE  JSTR-04  TO  DATEW-KEY.
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1        TO  INV-SW
               INITIALIZE  DATEW-R
               MOVE  JSTR-04  TO  DATEW-KEY
           END-IF
           IF  JSTR-01 < 100000
               ADD  1  TO  DATEW-SKS
           ELSE
               ADD  1  TO  DATEW-SIS
           END-IF
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO JSRD-RTN.
       JSRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    â◊éDÉgÉâÉìÅ@ÇqÇdÇ`ÇcÅ@(JNRD-RTN)                          *
      *--------------------------------------------------------------*
       JNRD-RTN.
      *           READ  JNIF  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO JNRD-RTN-EXIT
           END-IF
           IF  JNIF-WK   =  JNIF1-01
               GO TO JNRD-RTN
           END-IF
           MOVE JNIF1-01     TO  JNIF-WK.
           IF  JNIF1-02  =  7
               GO  TO  JNRD-RTN
           END-IF
           IF  JNIF1-10  NOT =  0
               GO  TO  JNRD-RTN
           END-IF
           IF  W-SYSIN  =  9
               GO TO JNRD-010
           END-IF
           IF  W-SYSIN  =  0
               IF  JNIF1-07 NOT = 2 AND 3 AND 4 AND 5 AND 6 AND 7
                   GO TO JNRD-010
               ELSE
                   GO TO JNRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  1
               IF  JNIF1-07     = 6
                   GO TO JNRD-010
               ELSE
                   GO TO JNRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  2
               IF  JNIF1-07     = 7
                   GO TO JNRD-010
               ELSE
                   GO TO JNRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  3
               IF  JNIF1-07     = 4
                   GO TO JNRD-010
               ELSE
                   GO TO JNRD-RTN
               END-IF
           END-IF
           GO TO JNRD-RTN.
       JNRD-010.
           MOVE  0         TO  INV-SW.
           MOVE  ZERO      TO  DATEW-KEY.
           MOVE  JNIF1-04  TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1  TO  INV-SW
               GO  TO  JNRD-020
           END-IF
           GO  TO  JNRD-030.
       JNRD-020.
           INITIALIZE  DATEW-R.
           MOVE  ZERO      TO  DATEW-KEY.
           MOVE  JNIF1-04  TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF.
       JNRD-030.
           IF  JNIF1-01 < 100000
               ADD  1  TO  DATEW-NKS
           ELSE
               ADD  1  TO  DATEW-NIS
           END-IF
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO JNRD-RTN.
       JNRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ëóÇËèÛÇeÅ@ÇqÇdÇ`ÇcÅ@(OKRD-RTN)                            *
      *--------------------------------------------------------------*
       OKRD-RTN.
      *           READ  OKJF  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO OKRD-RTN-EXIT
           END-IF
           IF  OKJF-08  NOT = 0
               GO TO OKRD-RTN
           END-IF
           IF  OKJF-10      = 0
               GO TO OKRD-RTN
           END-IF
           IF  W-SYSIN  =  9
               GO TO OKRD-010
           END-IF
           IF  W-SYSIN  =  0
               IF  OKJF-04  NOT  =  2 AND 3 AND 4 AND 5 AND 6 AND 7
                   GO TO OKRD-010
               ELSE
                   GO TO OKRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  1
               IF  OKJF-04       =  6
                   GO TO OKRD-010
               ELSE
                   GO TO OKRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  2
               IF  OKJF-04       =  7
                   GO TO OKRD-010
               ELSE
                   GO TO OKRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  3
               IF  OKJF-04       = 4
                   GO TO OKRD-010
               ELSE
                   GO TO OKRD-RTN
               END-IF
           END-IF
           GO TO OKRD-RTN.
       OKRD-010.
           MOVE  0        TO  INV-SW.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  OKJF-03  TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1  TO  INV-SW
               GO  TO  OKRD-020
           END-IF
           GO  TO  OKRD-030.
       OKRD-020.
           INITIALIZE  DATEW-R.
           MOVE  ZERO  TO  DATEW-KEY.
           MOVE  OKJF-03  TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF.
       OKRD-030.
           IF  OKJF-01 < 100000
               ADD  1  TO  DATEW-OKS
           ELSE
               ADD 1  TO  DATEW-OIS
           END-IF
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO OKRD-RTN.
       OKRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ìùàÍì`ï[ÇeÅi‹∞∏œ›)  ÇqÇdÇ`ÇcÅ@(TWRD-RTN)                  *
      *--------------------------------------------------------------*
       TWRD-RTN.
      *           READ  TDNWF NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TWRD-RTN-EXIT
           END-IF
           IF  TDNW1-MHC = SPACE
               GO TO TWRD-RTN
           END-IF
           IF  TDNW1-PC =  0  OR  9
               GO TO TWRD-RTN
           END-IF
           IF  (TDNW1-STC  =  WK-WSTC)  AND  (TDNW1-DNO  =  WK-WDNO)
               GO TO TWRD-RTN
           END-IF
           MOVE  TDNW1-STC  TO  WK-WSTC.
           MOVE  TDNW1-DNO  TO  WK-WDNO.
           IF  W-SYSIN  NOT  =  1  AND  9
               GO TO TWRD-RTN
           END-IF
           MOVE  0        TO  INV-SW.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  W-DATE   TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1  TO  INV-SW
               GO  TO  TWRD-010
           END-IF
           GO TO TWRD-020.
       TWRD-010.
           INITIALIZE  DATEW-R.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  W-DATE   TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF.
       TWRD-020.
           ADD  1  TO  DATEW-WIS.
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO TWRD-RTN.
       TWRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ìùàÍì`ï[ÇeÅi≈Ã∫)  ÇqÇdÇ`ÇcÅ@(TNRD-RTN)                    *
      *--------------------------------------------------------------*
       TNRD-RTN.
      *           READ  TDNNF NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TNRD-RTN-EXIT
           END-IF
           IF  TDNN1-PC =  0  OR  9
               GO TO TNRD-RTN
           END-IF
           IF  (TDNN1-STC  =  WK-NSTC)  AND  (TDNN1-DNO  =  WK-NDNO)
               GO TO TNRD-RTN
           END-IF
           MOVE  TDNN1-STC  TO  WK-NSTC.
           MOVE  TDNN1-DNO  TO  WK-NDNO.
           IF  W-SYSIN  NOT  =  1  AND  9
               GO TO TNRD-RTN
           END-IF
      *
           MOVE  0        TO  INV-SW.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  W-DATE   TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1  TO  INV-SW
               GO  TO  TNRD-010
           END-IF
           GO TO TNRD-020.
       TNRD-010.
           INITIALIZE  DATEW-R.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  W-DATE   TO  DATEW-NGP.
           IF  DATEW-NEN > 90
               ADD  1900  TO  DATEW-NEN
           ELSE
               ADD  2000  TO  DATEW-NEN
           END-IF.
       TNRD-020.
           ADD  1  TO  DATEW-FIS.
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO TNRD-RTN.
       TNRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ÉgÉâÉXÉRëºìùàÍì`ï[Å@ÇqÇdÇ`ÇcÅ@(TDIF-RTN)                  *
      *--------------------------------------------------------------*
       TDRD-RTN.
      *           READ  TDIF  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDIF_PNAME1 BY REFERENCE TDI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TDRD-RTN-EXIT
           END-IF
           IF  TDI-UPC  NOT =  1
               GO TO TDRD-RTN
           END-IF
           IF  TDIF-WK  =  TDI-DNO
               GO TO TDRD-RTN
           END-IF
           MOVE  TDI-DNO    TO  TDIF-WK.
           IF  W-SYSIN  =  9
               IF  TDI-PRC  =  9
                   GO TO TDRD-RTN
               ELSE
                   GO TO TDRD-010
               END-IF
           END-IF
           IF  W-SYSIN  NOT =  9
               IF  TDI-UPC  =  9
                   GO TO TDRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  0
               IF  TDI-SOK  NOT  =  2 AND 3 AND 4 AND 5 AND 6 AND 7
                   GO TO TDRD-010
               ELSE
                   GO TO TDRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  1
               IF  TDI-SOK  =  6
                   GO  TO  TDRD-010
               ELSE
                   GO  TO  TDRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  2
               IF  TDI-SOK  =  7
                   GO  TO  TDRD-010
               ELSE
                   GO  TO  TDRD-RTN
               END-IF
           END-IF
           IF  W-SYSIN  =  3
               IF  TDI-SOK  =  4
                   GO  TO  TDRD-010
               ELSE
                   GO  TO  TDRD-RTN
               END-IF
           END-IF
           GO TO TDRD-RTN.
       TDRD-010.
           MOVE  0        TO  INV-SW.
           MOVE  ZERO     TO  DATEW-KEY.
           MOVE  TDI-DATE TO  DATEW-NGP.
           ADD   2000     TO  DATEW-NEN.
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1        TO  INV-SW
               INITIALIZE  DATEW-R
               MOVE  ZERO     TO  DATEW-KEY
               MOVE  TDI-DATE TO  DATEW-NGP
               ADD   2000     TO  DATEW-NEN
           END-IF
           ADD  1  TO  DATEW-TIS.
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO TDRD-RTN.
       TDRD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ìùàÍì`ï[ÇeÅi±∂¡¨›Œ›Œﬂ)  ÇqÇdÇ`ÇcÅ@(TARD-RTN)              *
      *--------------------------------------------------------------*
       TARD-RTN.
      *           READ  TDNAF NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TARD-RTN-EXIT
           END-IF
           IF  TDNA-PC =  9
               GO TO TARD-RTN
           END-IF
           IF  (TDNA-STC  =  WK-ASTC)  AND  (TDNA-DNO  =  WK-ADNO)
               GO TO TARD-RTN
           END-IF
           MOVE  TDNA-STC   TO  WK-ASTC.
           MOVE  TDNA-DNO   TO  WK-ADNO.
           IF  W-SYSIN  NOT  =  3  AND  9
               GO TO TARD-RTN
           END-IF
      *
           MOVE  0        TO  INV-SW.
           MOVE  TDNA-DNGP TO  DATEW-KEY.
      *           READ  DATEW  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1  TO  INV-SW
               GO  TO  TARD-010
           END-IF
           GO TO TARD-020.
       TARD-010.
           INITIALIZE  DATEW-R.
           MOVE  TDNA-DNGP TO  DATEW-KEY.
       TARD-020.
           ADD  1  TO  DATEW-AKS.
           IF  INV-SW = 1
               PERFORM  WRI-RTN  THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO TO TARD-RTN.
       TARD-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    M E S G  -  R T N                                         *
      *--------------------------------------------------------------*
       MESG-RTN.
           CALL "SD_Accept" USING BY REFERENCE INP-KAKU "INP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
       MESG-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    D I S P  -  R T N                                         *
      *--------------------------------------------------------------*
       DISP-RTN.
           CALL "SD_Output" USING "DSP-SJD" DSP-SJD "p" RETURNING RESU.
           MOVE  ZERO  TO  CNT  W-GD.
           CALL "DB_F_Close" USING
            BY REFERENCE DATEW_IDLST DATEW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" DATEW_PNAME1 " " BY REFERENCE DATEW_IDLST "1"
            "DATEW-KEY" BY REFERENCE DATEW-KEY.
       DISP-010.
      *           READ  DATEW  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" DATEW_PNAME1 BY REFERENCE DATEW-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  DISP-040
           END-IF
           ADD  DATEW-SKS  TO  W-STS.
           ADD  DATEW-SIS  TO  W-STS.
           ADD  DATEW-NKS  TO  W-NTS.
           ADD  DATEW-NIS  TO  W-NTS.
           ADD  DATEW-OKS  TO  W-OTS.
           ADD  DATEW-OIS  TO  W-OTS.
           ADD  DATEW-WKS  TO  W-WTS.
           ADD  DATEW-WIS  TO  W-WTS.
           ADD  DATEW-FKS  TO  W-FTS.
           ADD  DATEW-FIS  TO  W-FTS.
           ADD  DATEW-TKS  TO  W-TTS.
           ADD  DATEW-TIS  TO  W-TTS.
           ADD  DATEW-AKS  TO  W-ATS.
           ADD  DATEW-AIS  TO  W-ATS.
           ADD  1          TO  CNT.
           IF  CNT > 3
               ADD  DATEW-SKS  TO  W-SKS
               ADD  DATEW-SIS  TO  W-SIS
               ADD  DATEW-NKS  TO  W-NKS
               ADD  DATEW-NIS  TO  W-NIS
               ADD  DATEW-OKS  TO  W-OKS
               ADD  DATEW-OIS  TO  W-OIS
               ADD  DATEW-WKS  TO  W-WKS
               ADD  DATEW-WIS  TO  W-WIS
               ADD  DATEW-FKS  TO  W-FKS
               ADD  DATEW-FIS  TO  W-FIS
               ADD  DATEW-TKS  TO  W-TKS
               ADD  DATEW-TIS  TO  W-TIS
               ADD  DATEW-AKS  TO  W-AKS
               ADD  DATEW-AIS  TO  W-AIS
               GO  TO  DISP-010
           END-IF
           MOVE  ZERO       TO  W-MEI.
           MOVE  DATEW-NGP  TO  W-NGP.
           MOVE  DATEW-SD   TO  W-SD.
           MOVE  DATEW-ND   TO  W-ND.
           MOVE  DATEW-OD   TO  W-OD.
           MOVE  DATEW-WD   TO  W-WD.
           MOVE  DATEW-FD   TO  W-FD.
           MOVE  DATEW-TD   TO  W-TD.
           MOVE  DATEW-AD   TO  W-AD.
           IF  CNT = 4
               GO  TO  DISP-010
           END-IF
           IF  CNT = 1
               CALL "SD_Output" USING
                "DSP-NGP1" DSP-NGP1 "p" RETURNING RESU
           END-IF
           IF  CNT = 2
               CALL "SD_Output" USING
                "DSP-NGP2" DSP-NGP2 "p" RETURNING RESU
           END-IF
           IF  CNT = 3
               CALL "SD_Output" USING
                "DSP-NGP3" DSP-NGP3 "p" RETURNING RESU
           END-IF
           IF  ZERO = W-SKS  AND  W-NKS  AND  W-OKS  AND
                      W-WKS  AND  W-FKS  AND  W-TKS  AND  W-AKS
               GO  TO  DISP-020
           END-IF
      *
           IF  CNT = 1
               CALL "SD_Output" USING
                "DSP-KD1" DSP-KD1 "p" RETURNING RESU
           END-IF
           IF  CNT = 2
               CALL "SD_Output" USING
                "DSP-KD2" DSP-KD2 "p" RETURNING RESU
           END-IF
           IF  CNT = 3
               CALL "SD_Output" USING
                "DSP-KD3" DSP-KD3 "p" RETURNING RESU
           END-IF.
       DISP-020.
           IF  ZERO = W-SIS  AND  W-NIS  AND  W-OIS  AND
                      W-WIS  AND  W-FIS  AND  W-TIS  AND  W-AIS
               GO  TO  DISP-010
           END-IF
           IF  CNT = 1
               CALL "SD_Output" USING
                "DSP-ID1" DSP-ID1 "p" RETURNING RESU
           END-IF
           IF  CNT = 2
               CALL "SD_Output" USING
                "DSP-ID2" DSP-ID2 "p" RETURNING RESU
           END-IF
           IF  CNT = 3
               CALL "SD_Output" USING
                "DSP-ID3" DSP-ID3 "p" RETURNING RESU
           END-IF
           GO  TO  DISP-010.
       DISP-040.
           IF  CNT > 3
               IF  (W-SKS NOT = ZERO)  OR  (W-NKS NOT = ZERO)
               OR  (W-OKS NOT = ZERO)  OR  (W-WKS NOT = ZERO)
               OR  (W-FKS NOT = ZERO)  OR  (W-TKS NOT = ZERO)
               OR  (W-AKS NOT = ZERO)
                   CALL "SD_Output" USING
                    "DSP-KD4" DSP-KD4 "p" RETURNING RESU
               END-IF
           END-IF
           IF  CNT > 3
               IF  (W-SIS NOT = ZERO)  OR  (W-NIS NOT = ZERO)
               OR  (W-OIS NOT = ZERO)  OR  (W-WIS NOT = ZERO)
               OR  (W-FIS NOT = ZERO)  OR  (W-TIS NOT = ZERO)
               OR  (W-AIS NOT = ZERO)
                   CALL "SD_Output" USING
                    "DSP-ID4" DSP-ID4 "p" RETURNING RESU
               END-IF
           END-IF
           CALL "SD_Output" USING "DSP-TD" DSP-TD "p" RETURNING RESU.
       DISP-RTN-EXIT.
           EXIT.
       WRI-RTN.
      *           WRITE  DATEW-R  INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            DATEW_PNAME1 DATEW_LNAME DATEW-R RETURNING RET.
           IF  RET = 1
               MOVE  "DATEW"  TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
       REW-RTN.
      *           REWRITE  DATEW-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            DATEW_PNAME1 DATEW_LNAME DATEW-R RETURNING RET.
           IF  RET = 1
               MOVE  "DATEW"  TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       REW-EX.
           EXIT.
      *--------------------------------------------------------------*
      *    O P E N  -  R T N                                         *
      *--------------------------------------------------------------*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "OUTPUT" DATEW_PNAME1 " " BY REFERENCE DATEW_IDLST "1"
            "DATEW-KEY" BY REFERENCE DATEW-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE DATEW_IDLST DATEW_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" DATEW_PNAME1 " " BY REFERENCE DATEW_IDLST "1"
            "DATEW-KEY" BY REFERENCE DATEW-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JSTR_PNAME1 
            "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JNIF_PNAME1 
            "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" OKJF_PNAME1 
            "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDNWF_PNAME1 
            "SHARED" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDNNF_PNAME1 
            "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDIF_PNAME1 
            "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDNAF_PNAME1 
            "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           IF  W-SYSIN  =  9
               CALL "DB_F_Open" USING
                "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
               CALL "DB_F_Open" USING
                "INPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0"
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
                "JOJF-KEY" BY REFERENCE JOJF-KEY
           END-IF.
       OPEN-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    E N D - R T N                                             *
      *--------------------------------------------------------------*
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE DATEW_IDLST DATEW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           IF  W-SYSIN  =  9
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JOLJF_IDLST JOLJF_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE JOJF_IDLST JOJF_PNAME1
           END-IF.
       END-EX.
           EXIT.
           COPY  LPMSG.
