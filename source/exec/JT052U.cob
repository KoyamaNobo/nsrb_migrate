       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      JT052U.
      ******************************************************
      *    PROGRAM..........  送り状ファイルのメンテナンス *
      *    AUTHOR...........  O.OOSAKO.                    *
      *    COMPILE MODE.....  NORMAL                       *
      *    SCREEN...........  SJ052U                       *
      *    RELEASE..........  63/07/19      [REV.001]      *
      ******************************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       SOURCE-COMPUTER.                 SYSTEM100.
       OBJECT-COMPUTER.                 SYSTEM100.
       DATA                             DIVISION.
       WORKING-STORAGE SECTION.
      *****
       77  ERR-STAT               PIC  X(02).
       77  W-ACT                  PIC  9(01).
       77  W-OKC                  PIC  9(01).
       77  W-JS                   PIC  X(01).
       77  W-JSD                  PIC  9(01).
       01  W-MID                  PIC  N(05).
      *
       01  DATE-AREA.
           03  WYMD               PIC  9(06).
           03  WYMD-R  REDEFINES  WYMD.
               05  WYY            PIC  9(02).
               05  WMM            PIC  9(02).
               05  WDD            PIC  9(02).
       01  WORK-AREA.
           03  W-01               PIC  9(06).
           03  W-02               PIC  9(01).
           03  W-03.
               05  W-031          PIC  9(02).
               05  W-032          PIC  9(02).
               05  W-033          PIC  9(02).
           03  W-04               PIC  9(01).
           03  W-05               PIC  9(07).
           03  W-06               PIC  N(09).
           03  W-07               PIC  9(03).
           03  W-11               PIC  9(05).
           03  W-12               PIC  9(06).
           03  W-08               PIC  9(01).
      *
           03  W-DSP.
               05  WD-ACT         PIC  N(02).
               05  WD-01          PIC  N(06).
               05  WD-02          PIC  N(06).
               05  WD-03          PIC  N(26).
               05  WD-MSG         PIC  N(15).
       01  ERR-MSG.
           03  MSG-01             PIC  N(11)    VALUE
                   "＊　送り状№未登録　＊".
           03  MSG-02             PIC  N(14)    VALUE
                   "＊　運送業者コード未登録　＊".
           03  MSG-03             PIC  N(11)    VALUE
                   "＊　倉コード未登録　＊".
           03  MSG-04             PIC  N(13)    VALUE
                   "＊　直送先コード未登録　＊".
      * * * * * * * * * * * *
           COPY     LWMSG.
      * * * * * * * * * * * *
           COPY     L-JCON.
           COPY     LITCM.
           COPY     LOKJF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-GAMEN.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-MID.
           03  FILLER  PIC N(16) VALUE
               "＊＊＊　　送り状　入力　　＊＊＊".
           03  FILLER.
               04  FILLER  PIC N(03) VALUE "教　育".
               04  FILLER  PIC X(05) VALUE   "=0 , ".
               04  FILLER  PIC N(03) VALUE "一　般".
               04  FILLER  PIC X(06) VALUE   "=1    ".
           03  FILLER  PIC X(25) VALUE
                 "確認 (OK=1,NO=9) --> ﾘﾀｰﾝ".
       01  ACP-JS.
           03     01ACP-JS   PIC  9(01).
       01  ACP-ACT.
           03     01ACP-ACT  PIC  9(01).
           03  DSP-ACT    PIC   N(02).
       01  ACP-OKC.
           03     01ACP-OKC  PIC  9(01).
      ***** ﾒﾝﾃﾅﾝｽ ******
       01  ACP-AREA.
           03  ACP-01      PIC  9(06).
           03  ACP-02      PIC  9(01).
           03  ACP-031     PIC  9(02).
           03  ACP-032     PIC  9(02).
           03  ACP-033     PIC  9(02).
           03  ACP-04      PIC  9(01).
           03  ACP-05      PIC  9(07).
           03  ACP-06      PIC  N(09).
           03  ACP-07      PIC  9(03).
           03  ACP-11      PIC  9(05).
           03  ACP-12      PIC  9(06).
           03  ACP-08      PIC  9(01).
       01  DSP-AREA.
           03  DSP-02    PIC   N(06).
           03  DSP-032   PIC   Z9 .
           03  DSP-033   PIC   Z9 .
           03  DSP-04    PIC   N(06).
           03  DSP-05    PIC   N(26).
           03  DSP-07    PIC   ZZ9 .
           03  DSP-11    PIC   Z(05).
           03  DSP-12    PIC   Z(06).
       01  MSG-AREA.
           03  DSP-MSG   PIC   N(15).
           03  DSP-KERR  PIC   N(05)
               VALUE  "区分エラー".
       01  CLE-ACT.
           03  CLE-ACT1  PIC   X(01)   VALUE   " ".
           03  CLE-ACT2  PIC   N(02)   VALUE "　".
       01  CLE-AREA.
           03  CLE-01    PIC   X(06)   VALUE   " ".
           03  CLE-021   PIC   X(01)   VALUE   " ".
           03  CLE-022   PIC   N(06)   VALUE "　".
           03  CLE-031   PIC   X(02)   VALUE   " ".
           03  CLE-032   PIC   X(02)   VALUE   " ".
           03  CLE-033   PIC   X(02)   VALUE   " ".
           03  CLE-041   PIC   X(01)   VALUE   " ".
           03  CLE-042   PIC   N(06)   VALUE "　".
           03  CLE-051   PIC   X(07)   VALUE   " ".
           03  CLE-052   PIC   N(26)   VALUE "　".
           03  CLE-06    PIC   N(09)   VALUE "　".
           03  CLE-07    PIC   X(03)   VALUE   " ".
           03  CLE-11    PIC   X(05)   VALUE   " ".
           03  CLE-12    PIC   X(06)   VALUE   " ".
           03  CLE-08    PIC   X(01)   VALUE   " ".
           03  CLE-OKC   PIC   X(01)   VALUE   " ".
           03  DSP-MSG-SPACE
                         PIC   X(40)   VALUE   " ".
       01  DSP-MIDASHI.
           03  DSP-MID1  PIC   N(05).
      * * * * * * * * * * * *
           COPY     LSMSG.
      * * * * * * * * * * * *
      ********************************************
       PROCEDURE                        DIVISION.
      ********************************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-GAMEN
       CALL "SD_Init" USING
           "CLE-GAMEN" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
          "01CLE-GAMEN" "X" "1" "0" "12" " " "CLE-GAMEN" RETURNING RESU.
      *DSP-MID
       CALL "SD_Init" USING 
            "DSP-MID" " " "0" "0" "80" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID" "N" "1" "24" "32" " " "DSP-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID" " " "5" "0" "23" "01DSP-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-MID" "N" "5" "21" "6" " " "02DSP-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-MID" "X" "5" "27" "5" "0102DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-MID" "N" "5" "32" "6" "0202DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-MID" "X" "5" "38" "6" "0302DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MID" "X" "24" "41" "25" "02DSP-MID" " "
            RETURNING RESU.
      *ACP-JS
       CALL "SD_Init" USING 
            "ACP-JS" " " "5" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-JS" "9" "5" "43" "1" " " "ACP-JS"  RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
      *ACP-ACT
       CALL "SD_Init" USING 
            "ACP-ACT" " " "3" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-ACT" "9" "3" "46" "1" " " "ACP-ACT" RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ACT" "N" "3" "48" "4" "01ACP-ACT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ACT" BY REFERENCE WD-ACT "4" "0" RETURNING RESU.
      *ACP-OKC
       CALL "SD_Init" USING 
            "ACP-OKC" " " "24" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-OKC" "9" "24" "61" "1" " " "ACP-OKC" RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "54" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-01" "9" "6" "17" "6" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-01" BY REFERENCE W-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-02" "9" "8" "17" "1" "ACP-01" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-02" BY REFERENCE W-02 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-031" "9" "10" "17" "2" "ACP-02" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-031" BY REFERENCE W-031 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-032" "9" "10" "20" "2" "ACP-031" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-032" BY REFERENCE W-032 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-033" "9" "10" "23" "2" "ACP-032" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-033" BY REFERENCE W-033 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-04" "9" "12" "17" "1" "ACP-033" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-04" BY REFERENCE W-04 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-05" "9" "14" "17" "7" "ACP-04" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-05" BY REFERENCE W-05 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-06" "N" "16" "17" "18" "ACP-05" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-06" BY REFERENCE W-06 "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-07" "9" "18" "17" "3" "ACP-06" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-07" BY REFERENCE W-07 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-11" "9" "20" "17" "5" "ACP-07" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-11" BY REFERENCE W-11 "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-12" "9" "20" "33" "6" "ACP-11" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-12" BY REFERENCE W-12 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-08" "9" "22" "17" "1" "ACP-12" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-08" BY REFERENCE W-08 "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "94" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "N" "8" "19" "12" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-02" BY REFERENCE WD-01 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-032" "Z9" "10" "20" "2" "DSP-02" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-032" BY REFERENCE W-032 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-033" "Z9" "10" "23" "2" "DSP-032" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-033" BY REFERENCE W-033 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" "N" "12" "19" "12" "DSP-033" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-04" BY REFERENCE WD-02 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" "N" "14" "25" "52" "DSP-04" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-05" BY REFERENCE WD-03 "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" "ZZ9" "18" "17" "3" "DSP-05" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-07" BY REFERENCE W-07 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "Z" "20" "17" "5" "DSP-07" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-11" BY REFERENCE W-11 "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" "Z" "20" "33" "6" "DSP-11" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-12" BY REFERENCE W-12 "6" "0" RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING 
            "MSG-AREA" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG" "N" "24" "2" "30" " " "MSG-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MSG" BY REFERENCE WD-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KERR" "N" "24" "2" "10" "DSP-MSG" " " RETURNING RESU.
      *CLE-ACT
       CALL "SD_Init" USING 
            "CLE-ACT" " " "0" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-ACT1" "X" "3" "46" "1" " " "CLE-ACT"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-ACT2" "N" "3" "48" "4" "CLE-ACT1" " " RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "171" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" "X" "6" "17" "6" " " "CLE-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-021" "X" "8" "17" "1" "CLE-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-022" "N" "8" "19" "12" "CLE-021" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-031" "X" "10" "17" "2" "CLE-022" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-032" "X" "10" "20" "2" "CLE-031" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-033" "X" "10" "23" "2" "CLE-032" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-041" "X" "12" "17" "1" "CLE-033" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-042" "N" "12" "19" "12" "CLE-041" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-051" "X" "14" "17" "7" "CLE-042" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-052" "N" "14" "25" "52" "CLE-051" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-06" "N" "16" "17" "18" "CLE-052" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-07" "X" "18" "17" "3" "CLE-06" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-11" "X" "20" "17" "5" "CLE-07" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-12" "X" "20" "33" "6" "CLE-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-08" "X" "22" "17" "1" "CLE-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-OKC" "X" "24" "61" "1" "CLE-08" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG-SPACE" "X" "24" "1" "40" "CLE-OKC" " "
            RETURNING RESU.
      *DSP-MIDASHI
       CALL "SD_Init" USING 
            "DSP-MIDASHI" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-MID1" "N" "1" "1" "10" " " "DSP-MIDASHI" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MID1" BY REFERENCE W-MID "10" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       HAJIME.
           PERFORM  INI-RTN  THRU  INI-EX.
       PRO-ACT.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-ACT "01ACP-ACT"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  OWARI
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  PRO-ACT
           END-IF
           IF  W-ACT   NOT  =  1 AND 2 AND 3 AND "P9"
               GO  TO  PRO-ACT
           END-IF
      *
           CALL "SD_Output" USING
            "CLE-AREA" CLE-AREA "p" RETURNING RESU.
           INITIALIZE         WORK-AREA.
           IF  W-ACT  =  1 OR 2 OR 3
               PERFORM   INP-RTN  THRU  INP-EX
           END-IF
           GO  TO   PRO-ACT.
      *
       OWARI.
           PERFORM  END-RTN  THRU  END-EX.
           CALL "SD_Output" USING
            "CLE-GAMEN" CLE-GAMEN "p" RETURNING RESU.
           CALL "DB_Close".
           STOP     RUN.
      ******************************************************************
      *    INI-RTN            初期処理
      ******************************************************************
       INI-RTN.
           CALL "SD_Output" USING
            "CLE-GAMEN" CLE-GAMEN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-MID" DSP-MID "p" RETURNING RESU.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-JS "01ACP-JS"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-OKC" CLE-OKC "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               CALL "SD_Output" USING
                "CLE-GAMEN" CLE-GAMEN "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU.
           IF  W-JS   NOT =  0  AND  1
               GO  TO  INI-010
           END-IF.
       INI-090.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-OKC "01ACP-OKC"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-090
           END-IF
           IF  W-OKC  NOT =   1   AND  9
               GO  TO  INI-090
           END-IF
           IF  W-OKC  =  9
               CALL "SD_Output" USING
                "CLE-GAMEN" CLE-GAMEN "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF
      *
           ACCEPT   WYMD  FROM  DATE.
           CALL "SD_Output" USING
            "CLE-GAMEN" CLE-GAMEN "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ052U" RETURNING RESU.
           IF  W-JS             =  0
               MOVE  "［教　育］" TO W-MID
           END-IF
           IF  W-JS             =  1
               MOVE  "［一　般］" TO W-MID
           END-IF
           CALL "SD_Output" USING
            "DSP-MID1" DSP-MID1 "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       INI-EX.
           EXIT.
      ******************************************************************
      *    END-RTN            終了処理
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
      ******************************************************************
      *    INP-RTN            メンテナンス処理
      ******************************************************************
       INP-RTN.
           IF  W-ACT      =      1
               MOVE     "追加"   TO   WD-ACT
               CALL "SD_Output" USING
                "DSP-ACT" DSP-ACT "p" RETURNING RESU
               GO  TO  INP-010
           END-IF
           IF  W-ACT      =      2
               MOVE     "変更"   TO   WD-ACT
               CALL "SD_Output" USING
                "DSP-ACT" DSP-ACT "p" RETURNING RESU
           END-IF
           IF  W-ACT      =      3
               MOVE     "取消"   TO   WD-ACT
               CALL "SD_Output" USING
                "DSP-ACT" DSP-ACT "p" RETURNING RESU
           END-IF.
       INP-000.
           CALL "SD_Accept" USING BY REFERENCE ACP-01 "ACP-01" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-OKC" CLE-OKC "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-000
           END-IF
           CALL "SD_Output" USING "ACP-01" ACP-01 "p" RETURNING RESU.
      *
           MOVE     W-01      TO    OKJF-KEY.
      *           READ     OKJF      INVALID    KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE      MSG-01    TO    WD-MSG
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-000
           END-IF
           MOVE     OKJF-09   TO    W-JSD.
           IF  W-JSD           =      2
               MOVE     1         TO    W-JSD
           END-IF
           IF  W-JS        NOT =      W-JSD
               CALL "SD_Output" USING
                "DSP-KERR" DSP-KERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-000
           END-IF
      *
           PERFORM   DSP-RTN   THRU   DSP-EX.
           IF  W-ACT      =      3
               GO  TO  INP-OKC
           END-IF.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-02 "ACP-02" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-OKC" CLE-OKC "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT      =   1
                   GO  TO  INP-EX
               ELSE
                   GO  TO  INP-000
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-010
           END-IF
           CALL "SD_Output" USING "ACP-02" ACP-02 "p" RETURNING RESU.
      *
           MOVE     2      TO    JCON2-01.
           MOVE     W-02   TO    JCON2-02.
      *
      *           READ     JCON      WITH  UNLOCK    INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      MSG-02    TO    WD-MSG
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-010
           END-IF
      *
           MOVE     JCON2-03    TO    WD-01.
           CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU.
       INP-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-031 "ACP-031" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-010
           END-IF
           CALL "SD_Output" USING "ACP-031" ACP-031 "p" RETURNING RESU.
           IF  W-031   =   0
               MOVE       WYY   TO   W-031
               MOVE       WMM   TO   W-032
               MOVE       WDD   TO   W-033
               CALL "SD_Output" USING
                "ACP-031" ACP-031 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-032" DSP-032 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-033" DSP-033 "p" RETURNING RESU
               GO  TO   INP-050
           END-IF.
       INP-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-032 "ACP-032" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-020
           END-IF
           CALL "SD_Output" USING "ACP-032" ACP-032 "p" RETURNING RESU.
       INP-040.
           CALL "SD_Accept" USING BY REFERENCE ACP-033 "ACP-033" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-030
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-040
           END-IF
           CALL "SD_Output" USING "ACP-033" ACP-033 "p" RETURNING RESU.
      *
      *
           IF  (W-032 < 1)  OR  (W-032 > 12)
                OR  (W-033 < 1)  OR  (W-033 > 31)
               GO  TO  INP-020
           END-IF
           CALL "SD_Output" USING "DSP-032" DSP-032 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-033" DSP-033 "p" RETURNING RESU.
       INP-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-04 "ACP-04" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-050
           END-IF
           CALL "SD_Output" USING "ACP-04" ACP-04 "p" RETURNING RESU.
      *
           MOVE     3      TO    JCON3-01.
           MOVE     W-04   TO    JCON3-02.
      *
      *           READ     JCON      WITH  UNLOCK    INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      MSG-03    TO    WD-MSG
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-050
           END-IF
      *
           MOVE     JCON3-03    TO    WD-02.
           CALL "SD_Output" USING "DSP-04" DSP-04 "p" RETURNING RESU.
       INP-060.
           CALL "SD_Accept" USING BY REFERENCE ACP-05 "ACP-05" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-050
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-060
           END-IF
           CALL "SD_Output" USING "ACP-05" ACP-05 "p" RETURNING RESU.
      *
           MOVE     W-05   TO    TC-KEY.
      *
      *           READ     TC-M      WITH  UNLOCK    INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      MSG-04    TO    WD-MSG
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-060
           END-IF
      *
           MOVE     TC-NAME     TO    WD-03.
           CALL "SD_Output" USING "DSP-05" DSP-05 "p" RETURNING RESU.
       INP-070.
           IF  W-ACT      =    1
               CALL "SD_Output" USING
                "CLE-06" CLE-06 "p" RETURNING RESU
           END-IF.
       INP-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-06 "ACP-06" "N" "18"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-060
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-080
           END-IF.
       INP-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-07 "ACP-07" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-080
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-090
           END-IF
           CALL "SD_Output" USING "DSP-07" DSP-07 "p" RETURNING RESU.
       INP-095.
           CALL "SD_Accept" USING BY REFERENCE ACP-11 "ACP-11" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-090
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-095
           END-IF
           CALL "SD_Output" USING "DSP-11" DSP-11 "p" RETURNING RESU.
           IF  W-02   NOT =  6
               MOVE  ZERO       TO  W-12
               CALL "SD_Output" USING
                "DSP-12" DSP-12 "p" RETURNING RESU
               GO  TO  INP-100
           END-IF.
       INP-097.
           CALL "SD_Accept" USING BY REFERENCE ACP-12 "ACP-12" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INP-095
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-097
           END-IF
           CALL "SD_Output" USING "DSP-12" DSP-12 "p" RETURNING RESU.
           IF  W-12       =  ZERO
               GO  TO  INP-097
           END-IF.
       INP-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-08 "ACP-08" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-02       =  6
                   GO  TO  INP-097
               ELSE
                   GO  TO  INP-095
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-100
           END-IF
           CALL "SD_Output" USING "ACP-08" ACP-08 "p" RETURNING RESU.
           IF  W-08   NOT =   0  AND  1
               GO  TO  INP-100
           END-IF.
       INP-OKC.
           CALL "SD_Output" USING "CLE-OKC" CLE-OKC "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-OKC "01ACP-OKC"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG-SPACE" DSP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT =   3
                   GO  TO  INP-000
               ELSE
                   GO  TO  INP-100
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INP-OKC
           END-IF
           IF  W-OKC  NOT =   1   AND  9
               GO  TO  INP-OKC
           END-IF
           IF  W-OKC  =  9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  INP-999
           END-IF
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           PERFORM    WRD-RTN   THRU   WRD-EX.
       INP-999.
           INITIALIZE                 WORK-AREA.
           CALL "SD_Output" USING
            "CLE-AREA" CLE-AREA "p" RETURNING RESU.
           MOVE     ZERO       TO     W-OKC.
           IF  W-ACT   =   1
               GO  TO  INP-010
           ELSE
               GO  TO  INP-000
           END-IF.
       INP-EX.
           EXIT.
      *    * * * * * * * * * * * * * * * * * * * *
      *    * * * * * DSP-RTN * * 表示* * * * * * *
      *    * * * * * * * * * * * * * * * * * * * *
       DSP-RTN.
           MOVE     OKJF-02   TO    W-02.
           MOVE     OKJF-03   TO    W-03.
           MOVE     OKJF-04   TO    W-04.
           MOVE     OKJF-05   TO    W-05.
           MOVE     OKJF-06   TO    W-06.
           MOVE     OKJF-07   TO    W-07.
           MOVE     OKJF-08   TO    W-08.
           MOVE     OKJF-11   TO    W-11.
           MOVE     OKJF-12   TO    W-12.
      *
           MOVE     2         TO    JCON2-01.
           MOVE     OKJF-02   TO    JCON2-02.
      *           READ     JCON      WITH  UNLOCK     INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"   TO   JCON2-03
           END-IF
           MOVE     JCON2-03  TO    WD-01.
      *
           MOVE     3         TO    JCON3-01.
           MOVE     OKJF-04   TO    JCON3-02.
      *           READ     JCON      WITH  UNLOCK     INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"   TO   JCON3-03
           END-IF
           MOVE     JCON3-03  TO    WD-02.
      *
           MOVE     OKJF-05   TO    TC-KEY.
      *           READ     TC-M      WITH  UNLOCK     INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"   TO   TC-NAME
           END-IF
           MOVE     TC-NAME   TO    WD-03.
      *
           CALL "SD_Output" USING "ACP-02" ACP-02 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-031" ACP-031 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-04" ACP-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-05" ACP-05 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-06" ACP-06 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-08" ACP-08 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-11" ACP-11 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-12" ACP-12 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       DSP-EX.
           EXIT.
      *    * * * * * * * * * * * * * * * * * * * *
      *     * * * *  WRD-RTN  *  更新 * * * * * *
      *    * * * * * * * * * * * * * * * * * * * *
       WRD-RTN.
           IF  W-ACT     =    2
               GO  TO  WRD-000
           END-IF
           IF  W-ACT     =    3
               GO  TO  WRD-111
           END-IF
           PERFORM    CNT-RTN    THRU    CNT-EX.
      *
           MOVE     SPACE     TO    OKJF-R.
           INITIALIZE               OKJF-R.
      *
           MOVE     W-01      TO    OKJF-01.
           MOVE     W-02      TO    OKJF-02.
           MOVE     W-03      TO    OKJF-03.
           MOVE     W-04      TO    OKJF-04.
           MOVE     W-05      TO    OKJF-05.
           MOVE     W-06      TO    OKJF-06.
           MOVE     W-07      TO    OKJF-07.
           MOVE     W-08      TO    OKJF-08.
           MOVE     W-JS      TO    OKJF-09.
           MOVE     1         TO    OKJF-10.
           MOVE     W-11      TO    OKJF-11.
           MOVE     W-12      TO    OKJF-12.
      *
      *           WRITE    OKJF-R    INVALID    KEY
      *//////////////
           CALL "DB_Insert" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"     TO  ERR-F
               MOVE     "W"        TO  ERR-M
               MOVE     OKJF-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           PERFORM  CBL-RTN   THRU  CBL-EX.
           GO   TO  WRD-EX.
       WRD-000.
           MOVE     W-02      TO    OKJF-02.
           MOVE     W-03      TO    OKJF-03.
           MOVE     W-04      TO    OKJF-04.
           MOVE     W-05      TO    OKJF-05.
           MOVE     W-06      TO    OKJF-06.
           MOVE     W-07      TO    OKJF-07.
           MOVE     W-08      TO    OKJF-08.
           MOVE     W-11      TO    OKJF-11.
           MOVE     W-12      TO    OKJF-12.
      *
      *           REWRITE  OKJF-R    INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"     TO  ERR-F
               MOVE     "R"        TO  ERR-M
               MOVE     OKJF-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO   TO  WRD-EX.
       WRD-111.
      *           DELETE   OKJF      INVALID    KEY
      *///////////////
           CALL "DB_Delete" USING OKJF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"     TO  ERR-F
               MOVE     "D"        TO  ERR-M
               MOVE     OKJF-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           PERFORM  CBL-RTN   THRU  CBL-EX.
      *
       WRD-EX.
           EXIT.
       CNT-RTN.
           MOVE     1        TO      JCON1-01.
           MOVE     4        TO      JCON1-02.
      *
      *           READ     JCON        INVALID     KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE      MSG-01    TO    WD-MSG
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               PERFORM   END-RTN   THRU  END-EX
               CALL "DB_Close"
               STOP      RUN
           END-IF
      *
           IF  W-JS        =     0
               IF  JCON1-03    =     099999
                   MOVE      000001    TO    JCON1-03
               ELSE
                   ADD       1         TO    JCON1-03
               END-IF
           END-IF
      *
           IF  W-JS        =     1
               IF  JCON1-04    =     199999
                   MOVE      100000    TO    JCON1-04
               ELSE
                   ADD       1         TO    JCON1-04
               END-IF
           END-IF
      *
      *           REWRITE  JCON1-R   INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"     TO  ERR-F
               MOVE     "R"        TO  ERR-M
               MOVE     JCON1-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           IF  W-JS        =     0
               MOVE      JCON1-03  TO    W-01
           ELSE
               MOVE      JCON1-04  TO    W-01
           END-IF.
      *
       CNT-EX.
           EXIT.
      ******************************************************************
      *    CBL-RTN            仮クローズ処理
      ******************************************************************
       CBL-RTN.
       CBL-EX.
           EXIT.
      *****
           COPY     LPMSG.
      *******************    E N D    O F    P R O G R A M    **********
