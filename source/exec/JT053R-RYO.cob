       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT053R.
       AUTHOR.                        M-OOSAKO.
      ***************************************************
      *    PROGRAM        : ëóÇËèÛñ‚çáÇπ Åiâ^ëóã∆é“ï Åj *
      *    DATA WRITTEN   : 88/09/29                    *
      *    SCREEN USED    : SJ053R                      *
      *    FORM   USED    : UNUSED                      *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  END-SW                    PIC 9(01)    VALUE 0.
       77  DSP-SW                    PIC 9(01)    VALUE 0.
       77  REC-CNT                   PIC X(02).
       77  JS-SIGN                   PIC X(01).
       01  WORK-AREA.
           02  INP-WORK.
               03  W-KUBUN           PIC 9(01).
               03  W-OKC             PIC 9(01).
               03  W-CD              PIC 9(02).
           02  MAISU-WORK.
               03  W-MAISU        OCCURS     9.
                   04  W-MAISU1      PIC 9(04).
                   04  W-MAISU2      PIC 9(04).
                   04  W-MAISUKEI    PIC 9(04).
               03  W-KEI1            PIC 9(04).
               03  W-KEI2            PIC 9(04).
               03  W-KEI3            PIC 9(04).
           02  MEISAI-WORK.
               03  W-YMD             PIC 9(06).
               03  W-YMDR     REDEFINES    W-YMD.
                   04  W-YY          PIC 9(02).
                   04  W-MM          PIC 9(02).
                   04  W-DD          PIC 9(02).
               03  W-INJI            PIC N(01).
           02  SOEJI.
               03  W-L               PIC 9(02).
      * * * * * * * * * *
       COPY    LWMSG.
      * * * * * * * * * *
           COPY      LOKJF-RYO.
           COPY      L-JCON.
           COPY      LITCM.
      * * * * * * * * * *
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-AREA.
           02  CLE-02      PIC  X(11) VALUE "CLEAR  DATA".
           02  CLE-03      PIC  X(11) VALUE "CLEAR  DATA".
           02  CLE-04      PIC  X(11) VALUE "CLEAR  DATA".
           02  CLE-05      PIC  X(01) VALUE " ".
           02  CLE-06.
               03  FILLER  PIC  X(01)      VALUE   " ".
               03  FILLER  PIC  N(09)      VALUE "Å@".
           02  CLE-07      PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(08) VALUE   "â^ëóã∆é“".
               03  FILLER  PIC  X(01) VALUE     "=".
               03  FILLER  PIC  X(01) VALUE     "(".
               03  FILLER  PIC  X(01) VALUE     ")".
           02  DSP-02.
               03  FILLER  PIC  X(05) VALUE     "ã∆é“C".
               03  FILLER  PIC  X(10) VALUE   "â^ëóã∆é“ñº".
               03  FILLER  PIC  X(10) VALUE   "ñ¢î≠çsñáêî".
               03  FILLER  PIC  X(08) VALUE   "î≠çsñáêî".
               03  FILLER  PIC  X(06) VALUE   "ëçñáêî".
           02  DSP-03.
               03  FILLER  PIC  X(06) VALUE   "ëóÇËèÛ".
               03  FILLER  PIC  X(08) VALUE     "îN åé ì˙".
               03  FILLER  PIC  X(14) VALUE   "íºÅ@ëóÅ@êÊÅ@ñº".
               03  FILLER  PIC  X(10) VALUE   "îzÅ@íBÅ@ì˙".
               03  FILLER  PIC  X(08) VALUE   "ëqÅ@Å@å…".
               03  FILLER  PIC  X(04) VALUE   "å¬êî".
               03  FILLER  PIC  X(04) VALUE   "î≠çs".
           02  DSP-04.
               03  FILLER  PIC  9(01).
               03  FILLER  PIC  N(09).
               03  FILLER  PIC  ZZZ9 .
               03  FILLER  PIC  ZZZ9 .
               03  FILLER  PIC  ZZZ9 .
           02  DSP-05.
               03  FILLER  PIC  X(02) VALUE   "åv".
               03  FILLER  PIC  ZZZ9 .
               03  FILLER  PIC  ZZZ9 .
               03  FILLER  PIC  ZZZ9 .
           02  DSP-06      PIC  N(09).
           02  DSP-07.
               03  FILLER  PIC  9(06).
               03  FILLER  PIC  Z9 .
               03  FILLER  PIC  X(01) VALUE     "/".
               03  FILLER  PIC  Z9 .
               03  FILLER  PIC  X(01) VALUE     "/".
               03  FILLER  PIC  Z9 .
               03  FILLER  PIC  N(10).
               03  FILLER  PIC  N(09).
               03  FILLER  PIC  N(06).
               03  FILLER  PIC  ZZ9 .
               03  FILLER  PIC  N(01).
           02  DSP-ERR.
               03  INV-01  PIC  X(18) VALUE
                                     "ÅñÅ@ã∆é“ñ¢ìoò^Å@Åñ".
               03  INV-02  PIC  X(22) VALUE
                                     "ÅñÅ@äYìñÉfÅ[É^Ç»ÇµÅ@Åñ".
       01  ACP-AREA.
           02  ACP-KUBUN   PIC 9(01).
           02  ACP-CD      PIC 9(01).
           02  ACP-OKC     PIC 9(01).
       COPY    LSMSG.
      * * * * * * * * * *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" "X" "2" "0" "11" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "3" "0" "11" "CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" "X" "4" "23" "11" "CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-05" "X" "24" "64" "1" "CLE-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-06" " " "2" "0" "19" "CLE-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-06" "X" "2" "12" "1" " " "CLE-06" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-06" "N" "2" "15" "18" "01CLE-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-07" "X" "1" "74" "1" "CLE-06" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "276" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "2" "0" "11" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "2" "2" "8" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "2" "10" "1" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "X" "2" "14" "1" "02DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "2" "33" "1" "03DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "3" "0" "39" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "3" "11" "5" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "3" "21" "10" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-02" "X" "3" "41" "10" "02DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-02" "X" "3" "56" "8" "03DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-02" "X" "3" "71" "6" "04DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "3" "0" "54" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "3" "2" "6" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-03" "X" "3" "9" "8" "01DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-03" "X" "3" "18" "14" "02DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-03" "X" "3" "39" "10" "03DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-03" "X" "3" "58" "8" "04DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-03" "X" "3" "71" "4" "05DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-03" "X" "3" "77" "4" "06DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "W-L" "0" "31" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "9" "W-L" "13" "1" " " "DSP-04" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-04" BY REFERENCE W-CD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-04" "N" "W-L" "21" "18" "01DSP-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-04" BY REFERENCE JCON2-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-04" "ZZZ9" "W-L" "47" "4" "02DSP-04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-04" BY REFERENCE W-MAISU1(1) "4" "1" BY REFERENCE
             W-CD 12 RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-04" "ZZZ9" "W-L" "60" "4" "03DSP-04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-04" BY REFERENCE W-MAISU2(1) "4" "1" BY REFERENCE
             W-CD 12 RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-04" "ZZZ9" "W-L" "73" "4" "04DSP-04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-04" BY REFERENCE W-MAISUKEI(1) "4" "1" BY REFERENCE
             W-CD 12 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" " " "15" "0" "14" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-05" "X" "15" "25" "2" " " "DSP-05" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-05" "ZZZ9" "15" "47" "4" "01DSP-05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-05" BY REFERENCE W-KEI1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-05" "ZZZ9" "15" "60" "4" "02DSP-05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-05" BY REFERENCE W-KEI2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-05" "ZZZ9" "15" "73" "4" "03DSP-05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-05" BY REFERENCE W-KEI3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06" "N" "2" "15" "18" "DSP-05" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-06" BY REFERENCE JCON2-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "W-L" "0" "69" "DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "9" "W-L" "2" "6" " " "DSP-07" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-07" BY REFERENCE OKJF-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "Z9" "W-L" "9" "2" "01DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-07" BY REFERENCE W-YY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "W-L" "11" "1" "02DSP-07" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-07" "Z9" "W-L" "12" "2" "03DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-07" BY REFERENCE W-MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-07" "X" "W-L" "14" "1" "04DSP-07" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-07" "Z9" "W-L" "15" "2" "05DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06DSP-07" BY REFERENCE W-DD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-07" "N" "W-L" "18" "20" "06DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07DSP-07" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-07" "N" "W-L" "39" "18" "07DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08DSP-07" BY REFERENCE OKJF-06 "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-07" "N" "W-L" "58" "12" "08DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09DSP-07" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-07" "ZZ9" "W-L" "72" "3" "09DSP-07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10DSP-07" BY REFERENCE OKJF-07 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-07" "N" "W-L" "78" "2" "10DSP-07" " " RETURNING RESU.
       CALL "SD_From" USING 
            "11DSP-07" BY REFERENCE W-INJI "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "40" "DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-01" "X" "24" "1" "18" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-02" "X" "24" "1" "22" "INV-01" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "3" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KUBUN" "9" "1" "74" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KUBUN" BY REFERENCE W-KUBUN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CD" "9" "2" "12" "1" "ACP-KUBUN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CD" BY REFERENCE W-CD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "64" "1" "ACP-CD" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INI-RTN   THRU  INI-EX.
           GO  TO    PRO-010.
       PRO-000.
           CALL "SD_Output" USING "CLE-04" CLE-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-05" CLE-05 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-06" CLE-06 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-07" CLE-07 "p" RETURNING RESU.
       PRO-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-KUBUN "ACP-KUBUN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACES" DISP-MSG-SPACES "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  OWARI
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  PRO-000
           END-IF
           IF  W-KUBUN  NOT  =   1   AND     2
               GO  TO  PRO-000
           END-IF
           CALL "SD_Output" USING
            "ACP-KUBUN" ACP-KUBUN "p" RETURNING RESU.
           IF  W-KUBUN    =    1
               PERFORM    MAI-RTN    THRU    MAI-EX
               GO  TO     PRO-000
           END-IF
           IF  DSP-SW     =    1
               CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU
               CALL "SD_Output" USING "DSP-01" DSP-01 "p" RETURNING RESU
               CALL "SD_Output" USING "DSP-03" DSP-03 "p" RETURNING RESU
               MOVE   2   TO   DSP-SW
           END-IF.
       PRO-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-CD "ACP-CD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACES" DISP-MSG-SPACES "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  PRO-000
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  PRO-020
           END-IF
           CALL "SD_Output" USING "ACP-CD" ACP-CD "p" RETURNING RESU.
      *
           MOVE    2     TO     JCON2-01.
           MOVE    W-CD  TO     JCON2-02.
      *           READ    JCON    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-01" INV-01 "p" RETURNING RESU
               GO  TO    PRO-020
           END-IF
           CALL "SD_Output" USING "DSP-06" DSP-06 "p" RETURNING RESU.
       PRO-030.
           PERFORM    MEI-RTN    THRU    MEI-EX.
       PRO-040.
           IF  REC-CNT  =  "ON"
               CALL "SD_Output" USING "INV-02" INV-02 "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACES" DISP-MSG-SPACES "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
               GO  TO  PRO-020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  PRO-040
           END-IF
           IF  W-OKC  NOT  =   1     AND     9
               GO  TO  PRO-040
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  END-SW NOT  =   1
               IF  W-OKC   =   1
                   PERFORM    MEI-000    THRU    MEI-EX
                   GO  TO  PRO-040
               END-IF
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
      *
           MOVE     0    TO    END-SW.
           GO  TO   PRO-000.
       OWARI.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INI-RTN.
           ACCEPT         JS-SIGN FROM ARGUMENT-VALUE.
      *
           INITIALIZE     WORK-AREA.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ053R" RETURNING RESU.
           CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           MOVE  1        TO     DSP-SW.
       INI-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***************
      *  ãÊï™ ñáêî  *
      ***************
       MAI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           INITIALIZE     MAISU-WORK.
           IF  DSP-SW     =    2
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU
               CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU
               MOVE   1   TO   DSP-SW
           END-IF.
       MAI-000.
      *           READ    OKJF   NEXT    UNLOCK    AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO    MAI-010
           END-IF
           IF  JS-SIGN     =     1  AND    OKJF-04    NOT =  2 AND 3
               GO  TO  MAI-000
           END-IF
           IF  JS-SIGN     =     2  AND    OKJF-04    NOT =  5  AND  6
               GO  TO  MAI-000
           END-IF
           IF  JS-SIGN     =     3  
               AND    OKJF-04    NOT =  2  AND  3  AND  5  AND  6
               GO  TO  MAI-000
           END-IF
           IF  JS-SIGN     =     4  AND    OKJF-04    NOT =  7
               GO  TO  MAI-000
           END-IF
           IF    (OKJF-10   NOT   =   1)  OR  (OKJF-07   NOT   >   0)
               GO  TO  MAI-000
           END-IF
           MOVE     OKJF-02    TO    W-CD.
           IF  OKJF-08    =    0
               ADD     1    TO    W-MAISU1(W-CD)
           END-IF
           IF  OKJF-08    =    1
               ADD     1    TO    W-MAISU2(W-CD)
           END-IF
           GO  TO   MAI-000.
       MAI-010.
           MOVE     5    TO    W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE     1    TO    W-CD.
       MAI-020.
           IF  W-CD   >   9
               GO  TO    MAI-030
           END-IF
           COMPUTE    W-MAISUKEI(W-CD)  =   W-MAISU1(W-CD)
                                                    +  W-MAISU2(W-CD).
           IF  W-MAISUKEI(W-CD)   =   ZERO
               ADD     1    TO    W-CD
               GO  TO    MAI-020
           END-IF
      *
           MOVE     2    TO    JCON2-01.
           MOVE    W-CD  TO    JCON2-02.
      *           READ    JCON    UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    ALL "Å@"   TO   JCON2-03
           END-IF
           CALL "SD_Output" USING "DSP-04" DSP-04 "p" RETURNING RESU.
           ADD     W-MAISU1(W-CD)   TO     W-KEI1.
           ADD     W-MAISU2(W-CD)   TO     W-KEI2.
           ADD     W-MAISUKEI(W-CD) TO     W-KEI3.
           ADD     1          TO     W-L    W-CD.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO  TO   MAI-020.
       MAI-030.
           IF  W-KEI3  =  ZERO
               CALL "SD_Output" USING "INV-02" INV-02 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DSP-05" DSP-05 "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACES" DISP-MSG-SPACES "p" RETURNING RESU.
           IF  ESTAT   NOT   =   "01" AND "06"
               GO  TO  MAI-030
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
       MAI-EX.
           EXIT.
      ***************
      *  ãÊï™ ñæç◊  *
      ***************
       MEI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           MOVE     "ON"  TO   REC-CNT.
       MEI-000.
           CALL "SD_Output" USING "CLE-04" CLE-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-05" CLE-05 "p" RETURNING RESU.
           MOVE      4    TO   W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       MEI-010.
      *           READ    OKJF   NEXT    UNLOCK    AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      1    TO   END-SW
               GO  TO    MEI-EX
           END-IF
           IF  (W-CD      NOT   =    OKJF-02)
               OR  (OKJF-10   NOT   =   1)
               OR  (OKJF-07   NOT   >   0)
               GO  TO  MEI-010
           END-IF
           IF  JS-SIGN     =     1
               IF  OKJF-04    NOT =  2 AND 3
                   GO  TO  MEI-010
               END-IF
           END-IF
           IF  JS-SIGN     =     2
               IF  OKJF-04    NOT =  5 AND 6
                   GO  TO  MEI-010
               END-IF
           END-IF
           IF  JS-SIGN     =     3
               IF  OKJF-04    NOT =  2 AND 3 AND 5 AND 6
                   GO  TO  MEI-010
               END-IF
           END-IF
           IF  JS-SIGN     =     4
               IF  OKJF-04    NOT =  7
                   GO  TO  MEI-010
               END-IF
           END-IF
           MOVE     "OF"  TO   REC-CNT.
           INITIALIZE     MEISAI-WORK.
           MOVE     OKJF-03    TO    W-YMD.
           IF  OKJF-08   =   0
               MOVE    "ñ¢"   TO   W-INJI
           END-IF
           IF  OKJF-08   =   1
               MOVE    "çœ"   TO   W-INJI
           END-IF
      *
           MOVE     OKJF-05    TO    TC-KEY.
      *           READ     TC-M    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    ALL "Å@"   TO    TC-NAME
           END-IF
      *
           MOVE       3        TO    JCON3-01.
           MOVE     OKJF-04    TO    JCON3-02.
      *           READ     JCON    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    ALL "Å@"   TO    JCON3-03
           END-IF
           CALL "SD_Output" USING "DSP-07" DSP-07 "p" RETURNING RESU.
      *
           ADD     1   TO   W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L   <   24
               GO  TO  MEI-010
           END-IF.
       MEI-EX.
           EXIT.
      * * * * * * * * * *
       COPY LPMSG.
      * * * * * * * * * *
