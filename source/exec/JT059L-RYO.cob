       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT059L.
      **************************************************
      **************************************************
      **                                              **
      **     送り状発行（西濃運輸用）                 **
      **                                              **
      **        USER  NAME : 日進ゴム.                **
      **        DATE       : 1999･07･27               **
      **        TYPE       : COBOL                    **
      **        PROGRAM-ID : JT059L                   **
      **        SCREEN-ID  : ------.                  **
      **        AUTHOR     :                          **
      **        JS-SIGN    :  本社=0 , 選択=1 , 玉島=2 , 早島=3  **
      **************************************************
      **************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM150.
       OBJECT-COMPUTER.    SYSTEM150.
      *********************************************
      **                                          *
      **        DATA             DIVISION         *
      **                                          *
      *********************************************
       DATA                DIVISION.
      *********************************************
      **                                          *
      **        WORKING  STORAGE  SECTION         *
      **                                          *
      *********************************************
       WORKING-STORAGE     SECTION.
       01  ERR-STAT        PIC X(02).
       01  JS-SIGN         PIC X(01).
       01  W-JS            PIC X(01).
       01  W-15K           PIC X(05) VALUE X"1A24212078".
       01  W-20K           PIC X(05) VALUE X"1A24212474".
      ******************************
      *     WORK         AREA      *
      ******************************
       01  WORK-AREA.
           03  ENDFLG        PIC     X(03).
           03  ENDFLG2       PIC     X(03).
           03  ACT1          PIC     9.
           03  ACT2          PIC     9.
           03  ACT3          PIC     9(06).
           03  ACTK          PIC     9(01).
           03  LCNT          PIC     9(02).
           03  DSW           PIC     9(01).
           03  DATE-WORK.
               05  YY        PIC     9(02).
               05  MM        PIC     9(02).
               05  DD        PIC     9(02).
           03  DATE-GP.
               05  W-MM      PIC     Z9.
               05  W-DD      PIC     Z9.
           03  W-NGP.
             04  F              PIC 9(02).
             04  W-NGPS         PIC 9(06).
           03  W-NGPL         REDEFINES  W-NGP.
             04  W-NEN          PIC 9(04).
             04  W-NENL       REDEFINES  W-NEN.
               05  W-NEN1       PIC 9(02).
               05  W-NEN2       PIC 9(02).
             04  F              PIC 9(04).
           03  W-SDATE          PIC 9(08).
           03  W-EDATE          PIC 9(08).
           03  W-SNGP.
             04  W-SNEN       PIC 9(04).
             04  W-SNENL      REDEFINES  W-SNEN.
               05  W-SNEN1      PIC 9(02).
               05  W-SNEN2      PIC 9(02).
             04  W-SGET         PIC 9(02).
             04  W-SPEY         PIC 9(02).
           03  W-ENGP.
             04  W-ENEN       PIC 9(04).
             04  W-ENENL      REDEFINES  W-ENEN.
               05  W-ENEN1      PIC 9(02).
               05  W-ENEN2      PIC 9(02).
             04  W-EGET         PIC 9(02).
             04  W-EPEY         PIC 9(02).
           03  W-JU1.
               05  W-JUFC       PIC    N(16).
               05  W-JUR        PIC    N(04).
           03  W-JU2   REDEFINES  W-JU1.
               05  W-JUF        PIC    N(04).
               05  W-JUCR       PIC    N(16).
           03  W-JS1.
               05  W-JSFC       PIC    N(16).
               05  W-JSR        PIC    N(04).
           03  W-JS2   REDEFINES  W-JS1.
               05  W-JSF        PIC    N(04).
               05  W-JSCR       PIC    N(16).
           03  W-JPC            PIC    9(01).
           03  W-JUS1           PIC    N(16).
           03  W-JUS2.
             05  W-JUS2F        PIC    N(04).
             05  W-JUS2C        PIC    N(08).
             05  W-JUS2R        PIC    N(04).
           03  W-JUS3.
             05  W-JUS3F        PIC    N(04).
             05  W-JUS3C        PIC    N(08).
             05  W-JUS3R        PIC    N(04).
           03  W-POC            PIC    9(01).
           03  W-FOC            PIC    9(01).
           03  W-SOK            PIC    9(01).
       01  SOM.
           03  F1        PIC     N(01)     VALUE    "（".
           03  F2        PIC     N(06).
           03  F3        PIC     N(01)     VALUE    "）".
           COPY    LNAMW5.
           COPY    LWMSG.
      *
           COPY  LIBFDD.
           COPY  LOKJF-RYO.
           COPY  L-JCON.
           COPY  LITCM.
      *FD  P-F
       01  P-R.
      *
           02  P-R1.
               03  R1-15K              PIC     X(05).
               03  F                   PIC     X(16).
               03  R1-01               PIC     N(02).
               03  F                   PIC     X(04).
               03  R1-02               PIC     N(02).
               03  F                   PIC     X(04).
               03  R1-03               PIC     N(02).
               03  F                   PIC     X(95).
           02  P-R2   REDEFINES P-R1.
               03  F                   PIC     X(13).
               03  R2-01               PIC     N(12).
               03  F                   PIC     X(99).
           02  P-R3   REDEFINES P-R1.
               03  F                   PIC     X(17).
               03  R3-01               PIC     9(06).
               03  F                   PIC     X(01).
               03  R3-02               PIC     N(02).
               03  F                   PIC     X(108).
           02  P-R4   REDEFINES P-R1.
               03  F                   PIC     X(17).
               03  R4-01               PIC     X(14).
               03  F                   PIC     X(11).
               03  R4-02               PIC     X(14).
               03  F                   PIC     X(76).
           02  P-R5   REDEFINES P-R1.
               03  F                   PIC     X(09).
               03  R5-01               PIC     N(16).
               03  F                   PIC     X(07).
               03  R5-02               PIC     N(14).
               03  F                   PIC     X(60).
           02  P-R6   REDEFINES P-R1.
               03  F                   PIC     X(09).
               03  R6-01               PIC     N(16).
               03  F                   PIC     X(05).
               03  R6-02               PIC     N(14).
               03  F                   PIC     X(04).
               03  R6-03               PIC     ZZ9.
               03  R6-20K              PIC     X(05).
               03  F                   PIC     X(50).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-CLEAR.
           03  CLE-01   PIC  X(12) VALUE "CLEAR SCREEN".
      ******************************
      *     DISPLAY      SECTION.  *
      ******************************
       01  DISP-AREA1.
           03  DSP-RVE  PIC  X(12)  VALUE
                            " ".
           03  DISP-01  PIC  N(05)  VALUE
                          "送り状発行".
           03  DISP-02  PIC  N(05)  VALUE
                                           "西濃運輸　".
           03  DISP-03  PIC  N(09)  VALUE
                                       "テストプリント印字".
           03  DISP-04  PIC  X(12)  VALUE
                                             "(YES=1,NO=2)".
           03  DISP-04A.
               04  FILLER  PIC  N(03)  VALUE  "教　育".
               04  FILLER  PIC  X(05)  VALUE    "=0 , ".
               04  FILLER  PIC  N(03)  VALUE  "一　般".
               04  FILLER  PIC  X(05)  VALUE    "=1 , ".
               04  FILLER  PIC  N(03)  VALUE  "ＡＬＬ".
               04  FILLER  PIC  X(06)  VALUE    "=9    ".
           03  DISP-05  PIC  N(06)  VALUE
                        "１．新規発行".
           03  DISP-06  PIC  N(05)  VALUE
                        "２．再発行".
           03  DISP-07  PIC  N(02)  VALUE
                        "選択".
           03  DISP-08  PIC  X(03)  VALUE
                        "[ ]".
           03  DISP-09  PIC  N(03)  VALUE
                        "確認（".
           03  DISP-10  PIC  X(09)  VALUE
                        "OK=1,NO=9".
           03  DISP-11  PIC  N(01)  VALUE
                        "）".
           03  DISP-12  PIC  X(08)  VALUE
                                             "--> ﾘﾀｰﾝ".
       01  DISP-AREA2.
           03  DISP-15  PIC  X(39)  VALUE
                "本社=0 , 玉島=1 , 津山=2 , 早島=3 ...  ".
           03  DISP-21  PIC  X(09)  VALUE
                                             "送り状NO:".
           03  DISP-DATM.
             04  FILLER  PIC  X(06)  VALUE  "日　付".
             04  FILLER.
               05  FILLER  PIC  X(08)  VALUE  "ＦＲＯＭ".
               05  FILLER  PIC  X(08)  VALUE  "  /  /  ".
             04  FILLER.
               05  FILLER  PIC  X(04)  VALUE  "ＴＯ".
               05  FILLER  PIC  X(08)  VALUE  "  /  /  ".
       01  DISP-AREA3.
           03  DISP-22  PIC  N(09)  VALUE
                                             "該当データ無".
      ******************************
      *     ACCEPT       SECTION.  *
      ******************************
       01  ACT-AREA.
           03  ACT-01  PIC  9 .
           03  ACT-SOK PIC  9 .
           03  ACT-JS  PIC  9 .
           03  ACT-02  PIC  9 .
           03  ACT-03  PIC  9(06).
           03  ACP-SNGP.
             04  ACP-SNEN  PIC 9(02).
             04  ACP-SGET  PIC 9(02).
             04  ACP-SPEY  PIC 9(02).
           03  ACP-ENGP.
             04  ACP-ENEN  PIC 9(02).
             04  ACP-EGET  PIC 9(02).
             04  ACP-EPEY  PIC 9(02).
           03  ACT-04  PIC  9(01).
      *
      ******************************
      *     CLEAR        SECTION.  *
      ******************************
       01  CLR-AREA.
           03  CLR-01  PIC  X     VALUE   " ".
           03  CLR-JS  PIC  X     VALUE   " ".
           03  CLR-02  PIC  X     VALUE   " ".
           03  CLR-03  PIC  X(06) VALUE   " ".
           03  CLR-04  PIC  X     VALUE   " ".
           03  CLR-05  PIC  X(15) VALUE   " ".
           03  CLR-DATM.
             04  FILLER  PIC X(06)    VALUE  " ".
             04  FILLER.
               05  FILLER  PIC X(08)    VALUE  " ".
               05  FILLER  PIC X(08)    VALUE  " ".
             04  FILLER.
               05  FILLER  PIC X(04)    VALUE  " ".
               05  FILLER  PIC X(08)    VALUE  " ".
      *
           COPY  LSMSG.
           COPY  LIBSCR.
      **
      *********************************************
      **                                          *
      **        PROCEDURE        DIVISION         *
      **                                          *
      *********************************************
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DISP-CLEAR
       CALL "SD_Init" USING
           "DISP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-01" "X" "1" "0" "12" " " "DISP-CLEAR" RETURNING RESU.
      *DISP-AREA1
       CALL "SD_Init" USING 
            "DISP-AREA1" " " "0" "0" "150" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-RVE" "RX" "1" "20" "12" " " "DISP-AREA1" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-01" "N" "1" "21" "10" "DSP-RVE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-02" "N" "1" "11" "10" "DISP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-03" "N" "5" "13" "18" "DISP-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-04" "X" "5" "32" "12" "DISP-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-04A" " " "9" "0" "34" "DISP-04" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-04A" "N" "9" "13" "6" " " "DISP-04A"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DISP-04A" "X" "9" "19" "5" "01DISP-04A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DISP-04A" "N" "9" "24" "6" "02DISP-04A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DISP-04A" "X" "9" "30" "5" "03DISP-04A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DISP-04A" "N" "9" "35" "6" "04DISP-04A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DISP-04A" "X" "9" "41" "6" "05DISP-04A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-05" "N" "11" "13" "12" "DISP-04A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-06" "N" "13" "13" "10" "DISP-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-07" "N" "13" "30" "4" "DISP-06" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-08" "X" "13" "35" "3" "DISP-07" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-09" "N" "23" "41" "6" "DISP-08" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-10" "X" "23" "47" "9" "DISP-09" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-11" "N" "23" "56" "2" "DISP-10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-12" "X" "23" "58" "8" "DISP-11" " "  RETURNING RESU.
      *DISP-AREA2
       CALL "SD_Init" USING 
            "DISP-AREA2" " " "0" "0" "82" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-15" "X" "7" "13" "39" " " "DISP-AREA2" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-21" "X" "16" "15" "9" "DISP-15" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-DATM" " " "0" "0" "34" "DISP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-DATM" "X" "15" "31" "6" " " "DISP-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DISP-DATM" " " "17" "13" "16" "01DISP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DISP-DATM" "X" "17" "21" "8" " " "02DISP-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DISP-DATM" "X" "17" "31" "8" "0102DISP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DISP-DATM" " " "19" "13" "12" "02DISP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DISP-DATM" "X" "19" "21" "4" " " "03DISP-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203DISP-DATM" "X" "19" "31" "8" "0103DISP-DATM" " "
            RETURNING RESU.
      *DISP-AREA3
       CALL "SD_Init" USING 
            "DISP-AREA3" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-22" "N" "24" "1" "18" " " "DISP-AREA3" RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "23" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-01" "9" "5" "46" "1" " " "ACT-AREA"  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-01" BY REFERENCE ACT1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SOK" "9" "7" "51" "1" "ACT-01" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SOK" BY REFERENCE W-SOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JS" "9" "9" "46" "1" "ACT-SOK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-02" "9" "13" "36" "1" "ACT-JS" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-02" BY REFERENCE ACT2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-03" "9" "16" "24" "6" "ACT-02" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-03" BY REFERENCE ACT3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" " " "17" "0" "6" "ACT-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNEN" "9" "17" "31" "2" " " "ACP-SNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SGET" "9" "17" "34" "2" "ACP-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SPEY" "9" "17" "37" "2" "ACP-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENGP" " " "19" "0" "6" "ACP-SNGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENEN" "9" "19" "31" "2" " " "ACP-ENGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EGET" "9" "19" "34" "2" "ACP-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EPEY" "9" "19" "37" "2" "ACP-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-04" "9" "23" "61" "1" "ACP-ENGP" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-04" BY REFERENCE ACTK "1" "0" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "59" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-01" "X" "6" "46" "1" " " "CLR-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-JS" "X" "9" "46" "1" "CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-02" "X" "13" "36" "1" "CLR-JS" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-03" "X" "16" "24" "6" "CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-04" "X" "23" "61" "1" "CLR-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-05" "X" "16" "15" "15" "CLR-04" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-DATM" " " "0" "0" "34" "CLR-05" " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01CLR-DATM" "X" "15" "31" "6" " " "CLR-DATM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-DATM" " " "17" "46" "16" "01CLR-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102CLR-DATM" "X" "17" "21" "8" " " "02CLR-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202CLR-DATM" "X" "17" "31" "8" "0102CLR-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-DATM" " " "19" "46" "12" "02CLR-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103CLR-DATM" "X" "19" "21" "4" " " "03CLR-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203CLR-DATM" "X" "19" "31" "8" "0103CLR-DATM" " "
            RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MAIN-LOGIC.
           PERFORM   INITIAL-RTN  THRU  INITIAL-EXT.
           PERFORM      MAIN-RTN  THRU  MAIN-EXT
                        UNTIL     ENDFLG = "END".
           PERFORM     END-RTN THRU    END-EX.
           CALL "DB_Close".
           STOP     RUN.
      *********************************************
      **                                          *
      **        INITIAL            ROUTIN         *
      **                                          *
      *********************************************
       INITIAL-RTN.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
      *
           MOVE    SPACE      TO    WORK-AREA.
           INITIALIZE               WORK-AREA.
      *
           CALL "SD_Output" USING
            "DISP-CLEAR" DISP-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-AREA1" DISP-AREA1 "p" RETURNING RESU.
           IF  JS-SIGN      =  1
               CALL "SD_Output" USING
                "DISP-15" DISP-15 "p" RETURNING RESU
           END-IF
           COPY  LIBCPR.
       INITIAL-EXT.
           EXIT.
      *********************************************
      **                                          *
      **        MAIN               ROUTIN         *
      **                                          *
      *********************************************
       MAIN-RTN.
       MAIN-10.
           CALL "SD_Accept" USING BY REFERENCE ACT-01 "ACT-01" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT   =   "P9"
               MOVE  "END"  TO  ENDFLG
               GO     TO        MAIN-EXT
           END-IF
           IF  ESTAT   =   "01"  OR  "06"
               CONTINUE
           ELSE
               GO     TO  MAIN-10
           END-IF
           IF  ACT1  NOT =   1  AND 2
               GO     TO  MAIN-10
           END-IF
           IF  ACT1      =   1
               PERFORM    TEST-RTN    THRU    TEST-EXT
               GO   TO    MAIN-10
           END-IF
           IF  JS-SIGN   =   0
               MOVE  0     TO  W-SOK
               GO     TO  MAIN-15
           END-IF
           IF  JS-SIGN   =   2
               MOVE  1     TO  W-SOK
               GO     TO  MAIN-13
           END-IF
           IF  JS-SIGN   =   3
               MOVE  3     TO  W-SOK
               GO     TO  MAIN-13
           END-IF.
       MAIN-12.
           CALL "SD_Accept" USING BY REFERENCE ACT-SOK "ACT-SOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT   =   "09"
               GO     TO  MAIN-10
           END-IF
           IF  ESTAT   =   "01"  OR  "06"
               CONTINUE
           ELSE
               GO     TO  MAIN-12
           END-IF
           IF  W-SOK NOT =   0  AND  1  AND  2  AND  3
               GO     TO  MAIN-12
           END-IF.
       MAIN-13.
           IF  W-SOK NOT =   0
               MOVE  9        TO  W-JS
               CALL "SD_Output" USING "ACT-JS" ACT-JS "p" RETURNING RESU
               GO  TO  MAIN-20
           END-IF.
       MAIN-15.
           CALL "SD_Accept" USING BY REFERENCE ACT-JS "ACT-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT   =   "09"
               IF  JS-SIGN       =   1
                   GO     TO  MAIN-12
               ELSE
                   GO     TO  MAIN-10
               END-IF
           END-IF
           IF  ESTAT   =   "01"  OR  "06"
               CONTINUE
           ELSE
               GO     TO  MAIN-15
           END-IF
           IF  W-JS  NOT =   0  AND  1  AND  9
               GO     TO  MAIN-15
           END-IF.
       MAIN-20.
           CALL "SD_Accept" USING BY REFERENCE ACT-02 "ACT-02" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT   =   "09"
               IF  W-SOK    =   0
                   GO  TO  MAIN-15
               ELSE
                   IF  JS-SIGN  =   1
                       GO  TO  MAIN-12
                   ELSE
                       GO  TO  MAIN-10
                   END-IF
               END-IF
           END-IF
           IF  ESTAT   =   "01"  OR  "06"
               CONTINUE
           ELSE
               GO     TO  MAIN-20
           END-IF
           IF  ACT2  NOT =   1  AND 2
               GO     TO  MAIN-20
           END-IF
           IF  ACT2 = 2
               CALL "SD_Output" USING
                "CLR-DATM" CLR-DATM "p" RETURNING RESU
               GO  TO  MAIN-30
           END-IF
           PERFORM  SEL-RTN  THRU  SEL-EX.
           CALL "SD_Output" USING "CLR-05" CLR-05 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-DATM" DISP-DATM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
           IF  W-SNGP = ZERO
               CALL "SD_Output" USING
                "DISP-22" DISP-22 "p" RETURNING RESU
               GO  TO  MAIN-20
           END-IF
           GO  TO  MAIN-40.
       MAIN-30.
           CALL "SD_Output" USING "DISP-21" DISP-21 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACT-03 "ACT-03" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "P9"
               MOVE  "END"  TO  ENDFLG
               GO  TO  MAIN-EXT
           END-IF
           IF  ESTAT = "09"
               GO  TO  MAIN-20
           END-IF
           IF  ESTAT = "01"  OR  "06"
               GO  TO  MAIN-40
           ELSE
               GO  TO  MAIN-30
           END-IF.
       MAIN-32.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNEN "ACP-SNEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-20
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-32
           END-IF
           MOVE  ZERO  TO  W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1  AND  <= DATE-NT1
               ADD  DATE-NC1  TO  W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2  AND  <= DATE-NT2
               ADD  DATE-NC2  TO  W-SNEN
           END-IF.
       MAIN-33.
           CALL "SD_Accept" USING BY REFERENCE ACP-SGET "ACP-SGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-32
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-33
           END-IF
           IF  W-SGET < 1  OR  > 12
               GO  TO  MAIN-33
           END-IF.
       MAIN-34.
           CALL "SD_Accept" USING BY REFERENCE ACP-SPEY "ACP-SPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-33
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-34
           END-IF
           IF  W-SGET < 1  OR  > 31
               GO  TO  MAIN-34
           END-IF
           IF  W-SNGP < W-SDATE  OR  > W-EDATE
               GO  TO  MAIN-32
           END-IF.
       MAIN-36.
           CALL "SD_Accept" USING BY REFERENCE ACP-ENEN "ACP-ENEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-34
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-36
           END-IF
           MOVE  ZERO  TO  W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1  AND  <= DATE-NT1
               ADD  DATE-NC1  TO  W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2  AND  <= DATE-NT2
               ADD  DATE-NC2  TO  W-ENEN
           END-IF.
       MAIN-37.
           CALL "SD_Accept" USING BY REFERENCE ACP-EGET "ACP-EGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-36
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-37
           END-IF
           IF  W-EGET < 1  OR  > 12
               GO  TO  MAIN-37
           END-IF.
       MAIN-38.
           CALL "SD_Accept" USING BY REFERENCE ACP-EPEY "ACP-EPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-37
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MAIN-38
           END-IF
           IF  W-SGET < 1  OR  > 31
               GO  TO  MAIN-38
           END-IF
           IF  W-SNGP > W-ENGP
               GO  TO  MAIN-36
           END-IF
           IF  W-SNGP < W-SDATE  OR  > W-EDATE
               GO  TO  MAIN-36
           END-IF.
       MAIN-40.
           CALL "SD_Accept" USING BY REFERENCE ACT-04 "ACT-04" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT   =   "09"
               IF  ACT2    =   1
                   GO     TO  MAIN-32
               ELSE
                   GO     TO  MAIN-30
               END-IF
           END-IF
           IF  ESTAT   =   "01"  OR  "06"
               CONTINUE
           ELSE
               GO     TO  MAIN-40
           END-IF
           IF  ACTK  NOT =   1  AND 9
               GO     TO  MAIN-40
           END-IF
      *
           IF  ACTK      =   1
               MOVE       ZERO         TO     DSW
               PERFORM    PRINT-RTN    THRU   PRINT-EXT
           ELSE
               CALL "SD_Output" USING
                "CLR-05" CLR-05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-01" CLR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-02" CLR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-04" CLR-04 "p" RETURNING RESU
               GO   TO    MAIN-10
           END-IF
           IF  ACT2      =   2
               GO   TO    MAIN-30
           END-IF
           IF  DSW       =   0
               GO   TO    MAIN-20
           END-IF
           MOVE  "END"  TO  ENDFLG.
       MAIN-EXT.
           EXIT.
      *********************************************
      **                                          *
      **        FAINAL             ROUTIN         *
      **                                          *
      *********************************************
       END-RTN.
           IF  W-FOC     =  1
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
           END-IF
           IF  W-POC     =  1
               CALL "PR_Close" RETURNING RESP
           END-IF.
       END-EX.
           EXIT.
      *********************************************
      **                                          *
      **        SUB    ROUTIN     SECTION         *
      **                                          *
      *********************************************
      *
      ******************************
      *     TEST         ROUTIN    *
      ******************************
       TEST-RTN.
           IF  W-POC     =  0
               MOVE  1       TO  W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           IF  LCNT  =  23
               MOVE       ZERO     TO        LCNT
               MOVE       SPACE    TO        P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE       SPACE    TO        P-R.
           MOVE       W-15K    TO        R1-15K.
           MOVE       99       TO        R1-01  R1-02  R1-03.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE  ALL "Ｎ"    TO        R2-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE  ALL  "9"      TO        R3-01.
           MOVE       SPACE    TO        R3-02.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE  ALL  "X"      TO        R4-01  R4-02.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R5-01  R5-02.
           MOVE  ALL "Ｎ"    TO        R5-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  ALL "Ｎ"    TO        R5-02.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R6-01  R6-02.
           MOVE  ALL "Ｎ"    TO        R6-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  ALL "Ｎ"    TO        R6-02.
           MOVE        "999"   TO        R6-03.
           MOVE       W-20K    TO        R6-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       23       TO        LCNT.
       TEST-EXT.
           EXIT.
      *********************************************
      **                                          *
      **        PRINT              ROUTIN         *
      **                                          *
      *********************************************
       PRINT-RTN.
           IF  W-POC     =  0
               MOVE  1       TO  W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           IF  W-FOC     =  0
               MOVE  1       TO  W-FOC
               CALL "DB_F_Open" USING
                "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
                "JCON3-KEY" BY REFERENCE JCON3-KEY
               CALL "DB_F_Open" USING
                "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
                "OKJF-KEY" BY REFERENCE OKJF-KEY
           END-IF
           IF  LCNT  =  23
               MOVE       ZERO     TO        LCNT
               MOVE       SPACE    TO        P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
      *********  明  細  部  ********
           IF  ACT2      =   1
               CONTINUE
           ELSE
               GO   TO    PRT-20.
       PRT-10.
           IF  ENDFLG2  =  "END"
               GO   TO  PRT-15
           END-IF
      *           READ    OKJF  NEXT          AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE      "END"  TO  ENDFLG2
               GO  TO    PRT-15
           END-IF
           IF  W-JS     NOT =  9
               IF  OKJF-09  NOT =  W-JS
                   GO   TO  PRT-10
               END-IF
           END-IF
           IF  OKJF-08  =  1
               GO   TO  PRT-10
           END-IF
           IF  OKJF-02  NOT = 2
               GO   TO  PRT-10
           END-IF
           IF (OKJF-07  >  ZERO)  AND  (OKJF-10  =  1)
               CONTINUE
           ELSE
               GO  TO  PRT-10
           END-IF
           IF  W-SOK       =   0
               IF  OKJF-04    =    2 OR 3 OR 4 OR 5 OR 6 OR 7
                   GO  TO  PRT-10
               END-IF
           END-IF
           IF  W-SOK       =   1
               IF  OKJF-04    NOT =   6
                   GO  TO  PRT-10
               END-IF
           END-IF
           IF  W-SOK       =   2
               IF  OKJF-04    NOT =   7
                   GO  TO  PRT-10
               END-IF
           END-IF
           IF  W-SOK       =   3
               IF  OKJF-04    NOT =   4
                   GO  TO  PRT-10
               END-IF
           END-IF
           MOVE  ZERO     TO  W-NGP.
           MOVE  OKJF-03  TO  W-NGPS.
           IF  W-NEN2 >= DATE-NF1  AND  <= DATE-NT1
               ADD  DATE-NC1  TO  W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2  AND  <= DATE-NT2
               ADD  DATE-NC2  TO  W-NEN
           END-IF
           IF  W-NGP < W-SNGP  OR  > W-ENGP
               GO  TO  PRT-10
           END-IF
           GO      TO   PRT-30.
       PRT-15.
           IF  DSW  =  ZERO
               CALL "SD_Output" USING
                "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
               GO   TO    PRINT-EXT
           ELSE
               GO   TO    PRINT-EXT
           END-IF.
       PRT-20.
           MOVE    ACT3     TO  OKJF-01.
      *           READ    OKJF                   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
               GO   TO    PRINT-EXT
           END-IF
           IF  OKJF-02  NOT = 2
               CALL "SD_Output" USING
                "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
               GO   TO    PRINT-EXT
           END-IF
           IF (OKJF-07  >  ZERO)  AND  (OKJF-10  =  1)
               CONTINUE
           ELSE
               CALL "SD_Output" USING
                "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
               GO   TO    PRINT-EXT
           END-IF
           IF  W-JS     NOT =  9
               IF  OKJF-09  NOT =  W-JS
                   CALL "SD_Output" USING
                    "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
                   GO   TO    PRINT-EXT
               END-IF
           END-IF
           IF  W-SOK       =   0
               IF  OKJF-04    =  2 OR 3 OR 4 OR 5 OR 6 OR 7
                   CALL "SD_Output" USING
                    "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
                   GO   TO    PRINT-EXT
               END-IF
           END-IF
           IF  W-SOK       =   1
               IF  OKJF-04    NOT =   6
                   CALL "SD_Output" USING
                    "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
                   GO   TO    PRINT-EXT
               END-IF
           END-IF
           IF  W-SOK       =   2
               IF  OKJF-04    NOT =   7
                   CALL "SD_Output" USING
                    "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
                   GO   TO    PRINT-EXT
               END-IF
           END-IF
           IF  W-SOK       =   3
               IF  OKJF-04    NOT =   4
                   CALL "SD_Output" USING
                    "DISP-AREA3" DISP-AREA3 "p" RETURNING RESU
                   GO   TO    PRINT-EXT
               END-IF
           END-IF.
       PRT-30.
           MOVE    OKJF-05  TO  TC-KEY.
      *           READ    TC-M        UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE       SPACE  TO  TC-R
               INITIALIZE            TC-R
           END-IF.
       PRT-40.
           MOVE    3        TO  JCON3-01.
           MOVE    OKJF-04  TO  JCON3-02.
      *           READ    JCON        UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE       SPACE  TO  JCON3-R
               INITIALIZE            JCON3-R
           END-IF
           IF  OKJF-04     =   1
               IF   (OKJF-05     >=  2654000  AND  <=  2654999)
                OR  (OKJF-05     >=  2656000  AND  <=  2657999)
                OR  (OKJF-05     >=  4038000  AND  <=  4038999)
                OR  (OKJF-05     >=  4054000  AND  <=  4054999)
                OR  (OKJF-05     >=  4056000  AND  <=  4057999)
                   MOVE      "富　士　　　"  TO  JCON3-03
               END-IF
           END-IF
           MOVE       1        TO  DSW.
       PRT-50.
      *
           MOVE       SPACE    TO        P-R.
           MOVE       OKJF-03  TO  DATE-WORK.
           MOVE       MM       TO  W-MM.
           MOVE       DD       TO  W-DD.
           MOVE       SPACE    TO  R1-01  R1-02  R1-03.
           MOVE       W-15K    TO  R1-15K.
           MOVE       YY       TO  R1-01.
           MOVE       W-MM     TO  R1-02.
           MOVE       W-DD     TO  R1-03.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE  "地下・布靴・長靴・その他"  TO  R2-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R2-01.
           MOVE  OKJF-06       TO        R2-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R2-01.
           MOVE       JCON3-03 TO        F2.
           MOVE       SOM      TO        R2-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       OKJF-01    TO      R3-01.
           IF    ACT2          =   2
                      MOVE  "再"     TO   R3-02
           END-IF
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE  TC-TEL        TO        R4-01.
           IF  (OKJF-05  NOT <  2654000  AND NOT >  2654999)
               OR  (OKJF-05  NOT <  2656000  AND NOT >  2657999)
               MOVE  "077-543-1331  " TO      R4-02
           ELSE
               IF  (OKJF-05  NOT <  4038000  AND NOT >  4038999)
                   OR (OKJF-05  NOT <  4054000  AND NOT >  4054999)
                   OR (OKJF-05  NOT <  4056000  AND NOT >  4057999)
                   MOVE  "06-6268-4561  " TO      R4-02
               ELSE
                   IF  (OKJF-05  NOT <  0459002  AND NOT >  0459999)
                       MOVE  "0798-64-5111  " TO      R4-02
                   ELSE
                       IF  (OKJF-05  NOT <  0460002  AND NOT >  0460999)
                       MOVE  "0798-67-0810"  TO      R4-02
                       ELSE
                       IF  (OKJF-05  NOT <  3966002  AND NOT >  3966999)
                       MOVE  "03-5373-6711"  TO      R4-02
                       ELSE
                       IF  (OKJF-05  NOT <  3015001  AND NOT >  3015999)
                       MOVE  "03-3909-8883"  TO      R4-02
                       ELSE
                       MOVE  "086-243-2456  " TO      R4-02
                       END-IF
                       END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE TC-JSU TO W-JU1.
           MOVE TC-JSS TO W-JS1.
           MOVE ALL "　" TO W-JUS1 W-JUS2 W-JUS3.
           MOVE 0 TO W-JPC.
           IF  TC-JSS = SPACE
               IF  SPACE NOT = W-JUF AND W-JUR
                   MOVE W-JUFC TO W-JUS2
                   MOVE W-JUR TO W-JUS3R
               ELSE
                   IF  W-JUR = SPACE
                       MOVE W-JUFC TO W-JUS3
                   ELSE
                       IF  W-JUF = SPACE
                           MOVE W-JUCR TO W-JUS3
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TC-JSS = SPACE
               GO TO PRT-55
           END-IF
           IF  W-JSF = SPACE
               MOVE W-JSCR TO W-JUS3
           ELSE
               IF  W-JSR = SPACE
                   MOVE W-JSFC TO W-JUS3
               ELSE
                   MOVE W-JSCR TO W-JUS3
                   MOVE 1 TO W-JPC
               END-IF
           END-IF
           IF  W-JUF NOT = SPACE
               IF  W-JUR NOT = SPACE
                   MOVE W-JUFC TO W-JUS1
                   IF  W-JPC = 0
                       MOVE W-JUR TO W-JUS2R
                   ELSE
                       MOVE W-JUR TO W-JUS2F
                       MOVE W-JSF TO W-JUS2R
                   END-IF
               END-IF
           END-IF
           IF  W-JUR = SPACE
               IF  W-JUF NOT = SPACE
                   IF  W-JPC = 0
                       MOVE W-JUFC TO W-JUS2
                   ELSE
                       MOVE W-JUFC TO W-JUS1
                       MOVE W-JSF TO W-JUS2F
                   END-IF
               END-IF
           END-IF
           IF  W-JUF = SPACE
               IF  W-JUR NOT = SPACE
                   IF  W-JPC = 0
                       MOVE W-JUCR TO W-JUS2
                   ELSE
                       MOVE W-JUCR TO W-JUS1
                       MOVE W-JSF TO W-JUS2F
                   END-IF
               END-IF
           END-IF.
       PRT-55.
           MOVE       SPACE    TO        R5-01  R5-02.
           MOVE       W-JUS1   TO        R5-01.
           IF  (OKJF-05      <  0459002  OR      >  0459999)  AND
               (OKJF-05      <  0460002  OR      >  0460999)  AND
               (OKJF-05      <  3966002  OR      >  3966999)  AND
               (OKJF-05      <  2654000  OR      >  2654999)  AND
               (OKJF-05      <  2656000  OR      >  2657999)  AND
               (OKJF-05      <  4038000  OR      >  4038999)  AND
               (OKJF-05      <  4054000  OR      >  4054999)  AND
               (OKJF-05      <  4056000  OR      >  4057999)  AND
               (OKJF-05      <  3015000  OR      >  3015999)
               IF   OKJF-04      =  6
                   MOVE  "岡山県倉敷市玉島乙島字新湊　"   TO  R5-02
               ELSE
                   IF   OKJF-04      =  4
                       MOVE  "岡山県都窪郡早島町早島　　　"   TO  R5-02
                   END-IF
               END-IF
           END-IF
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R5-01  R5-02.
           MOVE       W-JUS2   TO        R5-01.
           IF   (OKJF-05      <  0459002  OR      >  0459999)  AND
                (OKJF-05      <  0460002  OR      >  0460999)  AND
                (OKJF-05      <  3015001  OR      >  3015999)  AND
                (OKJF-05      <  3966002  OR      >  3966999)  AND
                (OKJF-05      <  2654000  OR      >  2654999)  AND
                (OKJF-05      <  2656000  OR      >  2657999)
           IF   (OKJF-05      >= 4038000  AND     <= 4038999)
             OR (OKJF-05      >= 4054000  AND     <= 4054999)
             OR (OKJF-05      >= 4056000  AND     <= 4057999)
               MOVE  "大阪市中央区南船場　　　　　"   TO  R5-02
           ELSE
               IF  OKJF-04      =  4
                   MOVE  "　　　　　　　４５０７－３７"   TO  R5-02
               ELSE
                   IF  OKJF-04      =  7
                       MOVE  "岡山県久米郡　　　　　　　　"   TO  R5-02
                   ELSE
                       IF  OKJF-04      =  6
                       MOVE  "　　　　　　　　８２６２－１"   TO  R5-02
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R5-01  R5-02.
           MOVE       W-JUS3   TO        R5-01.
           IF  (OKJF-05  NOT <  2654000  AND NOT >  2654999)
                 OR   (OKJF-05  NOT <  2656000  AND NOT >  2657999)
               MOVE   "滋賀県大津市神領１－９－３　"  TO  R5-02
           ELSE
               IF  (OKJF-05  NOT <  4038000  AND NOT >  4038999)
                  OR (OKJF-05  NOT <  4054000  AND NOT >  4054999)
                  OR (OKJF-05  NOT <  4056000  AND NOT >  4057999)
                   MOVE   "　　　　　　　１丁目１１－９"  TO  R5-02
               ELSE
                   IF  (OKJF-05  NOT <  0459002  AND NOT >  0459999)
                   OR  (OKJF-05  NOT <  0460002  AND NOT >  0460999)
                       MOVE   "兵庫県西宮市高木東町１５－１"  TO  R5-02
                   ELSE
                     IF  (OKJF-05  NOT <  3966002  AND NOT >  3966999)
                     MOVE   "東京都中野区丸山２－５－１９"  TO  R5-02
                     ELSE
                     IF  (OKJF-05  NOT <  3015001  AND NOT >  3015999)
                     MOVE   "東京都北区上十条４－１－５　"  TO  R5-02
                     ELSE
                         IF  OKJF-04      =  4
                     MOVE   "両備ＨＤ中四国物流センター内"   TO  R5-02
                         ELSE
                             IF  OKJF-04      =  7
                     MOVE   "　　　美咲町原田３２２７－１"   TO  R5-02
                             ELSE
                                 IF  OKJF-04      =  6
                     MOVE   "　　水島港国際物流センター内"   TO  R5-02
                                 ELSE
                     MOVE   "岡山市北区今８丁目１６－１７"  TO  R5-02
                                 END-IF
                             END-IF
                         END-IF
                     END-IF
                     END-IF
                   END-IF
               END-IF
           END-IF
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       TC-NAME  TO        WN-NAME.
           COPY  LNAMP5.
           MOVE       SPACE    TO        R6-01  R6-02.
           MOVE       WN-ONAME TO        R6-01.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       SPACE    TO        R6-01  R6-02.
           MOVE       WN-UNAME TO        R6-01.
           IF   (OKJF-05  NOT <  2654000  AND NOT >  2654999)
            OR  (OKJF-05  NOT <  2656000  AND NOT >  2657999)
               MOVE      "株式会社新日本教育シューズ　"  TO  R6-02
           ELSE
               IF  (OKJF-05  NOT <  4038000  AND NOT >  4038999)
                  OR (OKJF-05  NOT <  4054000  AND NOT >  4054999)
                  OR (OKJF-05  NOT <  4056000  AND NOT >  4057999)
                   MOVE      "　　千曲産業株式会社　　　　"  TO  R6-02
               ELSE
                   IF  (OKJF-05  NOT <  0459002  AND NOT >  0459999)
                   MOVE      "　　株式会社上山實業　　　　"  TO  R6-02
                   ELSE
                       IF  (OKJF-05  NOT <  0460002  AND NOT >  0460999)
                       MOVE   "　　株式会社ウエヤマ　　　　"  TO  R6-02
                       ELSE
                       IF  (OKJF-05  NOT <  3966002  AND NOT >  3966999)
                       MOVE   "　　多比良株式会社　　　　　"  TO  R6-02
                       ELSE
                       IF  (OKJF-05  NOT <  3015001  AND NOT >  3015999)
                       MOVE   "　　株式会社スギヤマ　　　　"  TO  R6-02
                       ELSE
                           IF  OKJF-04      =  4
                       MOVE   "日進ゴム　早島配送センター　"  TO  R6-02
                           ELSE
                               IF  OKJF-04      =  7
                       MOVE   "日進ゴム㈱　津山物流センター"  TO  R6-02
                               ELSE
                                   IF  OKJF-04      =  6
                       MOVE   "日進ゴム㈱　玉島物流センター"  TO  R6-02
                                   ELSE
                       MOVE   "　　日進ゴム株式会社　　　　"  TO  R6-02
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                       END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE       OKJF-07  TO        R6-03.
           MOVE       W-20K    TO        R6-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE    TO        P-R.
      *
           MOVE       23       TO        LCNT.
      *
       PRT-60.
           MOVE          1     TO        OKJF-08.
      *           REWRITE  OKJF-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE    "OKJF"     TO    ERR-F
               MOVE    "R"        TO    ERR-M
               MOVE     OKJF-KEY  TO    ERR-K
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF
           IF  ACT2  =  1
               GO    TO    PRINT-RTN
           END-IF.
       PRINT-EXT.
           EXIT.
       SEL-RTN.
           MOVE  ZERO  TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           IF  W-FOC     =  1
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
       SEL-010.
      *           READ  OKJF  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  W-JS    NOT = 9
               IF  OKJF-09 NOT = W-JS
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  OKJF-08 = 1
               GO  TO  SEL-010
           END-IF
           IF  OKJF-02 NOT = 2
               GO  TO  SEL-010
           END-IF
           IF  (OKJF-07 > ZERO)  AND  (OKJF-10 = 1)
               CONTINUE
           ELSE
               GO  TO  SEL-010
           END-IF
           IF  W-SOK   = 0
               IF  OKJF-04 = 2  OR  3  OR  4  OR  5  OR  6  OR  7
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-SOK   = 1
               IF  OKJF-04 NOT = 6
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-SOK   = 2
               IF  OKJF-04 NOT = 7
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-SOK   = 3
               IF  OKJF-04 NOT = 4
                   GO  TO  SEL-010
               END-IF
           END-IF
           MOVE  ZERO  TO  W-NGP.
           MOVE  OKJF-03  TO  W-NGPS.
           IF  W-NEN2 >= DATE-NF1  AND  <= DATE-NT1
               ADD  DATE-NC1  TO  W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2  AND  <= DATE-NT2
               ADD  DATE-NC2  TO  W-NEN
           END-IF
           IF  W-SDATE = ZERO
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-SDATE > W-NGP
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-EDATE < W-NGP
               MOVE  W-NGP  TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           IF  W-FOC    =  1
               CALL "DB_F_Open" USING
                "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
                "OKJF-KEY" BY REFERENCE OKJF-KEY
           END-IF
           MOVE  W-SDATE  TO  W-SNGP.
           MOVE  W-EDATE  TO  W-ENGP.
       SEL-EX.
           EXIT.
           COPY    LPMSG.
      *********************  ＰＲＯＧＲＡＭ　　ＥＮＤ　*****************
