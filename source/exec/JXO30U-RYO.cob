       IDENTIFICATION   DIVISION.
      ******************************************************************
      *    ñ{é–ë§Å@Å@éÛêMämîFÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@      *
      *                                ÇOÇTÅ^ÇOÇXÅ^ÇOÇPÅ@              *
      *    JS-SIGN : ì`ëóêÊÅiã ìá=1,í√éR=2,óºîı=3Åj                    *
      *    [JXO30U]                    T.I                             *
      ******************************************************************
       PROGRAM-ID.            JXO30U.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       SYSTEM3100.
       OBJECT-COMPUTER.       SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN                     PIC  9(01).
       01  ERR-STAT                    PIC  X(02).
       01  KAKU-W                      PIC  X(01).
      *
       01  W-DATE.
           02  W-YY                PIC  9(02).
           02  W-MD                PIC  9(04).
           02  W-MDR               REDEFINES  W-MD.
               03  W-MM            PIC  9(02).
               03  W-DD            PIC  9(02).
       01  WORK-AREA.
           02  STR-TIME            PIC  9(08).
           02  STR-TIMER           REDEFINES  STR-TIME.
               03  STR-JF          PIC  9(04).
               03  F               PIC  X(04).
           02  END-TIME            PIC  9(08).
           02  END-TIMER           REDEFINES  END-TIME.
               03  END-JF          PIC  9(04).
               03  F               PIC  X(04).
           02  TAMECOMI-WORK.
               03  STR-CODE        PIC  X(10).
               03  END-CODE        PIC  X(10).
               03  W-KEN           PIC  9(06).
           02  W-SKSU              PIC  9(06).
           02  W-JKYO              PIC  N(04).
           02  W-EMGA.
               03  W-EMGA1         PIC  X(02).
               03  W-EMGA2         PIC  X(01).
           02  W-EMGN              PIC  N(10).
           02  W-CD                PIC  9(01).
           02  W-KAKU              PIC  9(01).
           02  NXT-NO              PIC  9(04).
           02  DATKN               PIC  9(06).
           02  JYUKN               PIC  9(06).
       01  SW-AREA.
           02  END-SW                  PIC  9(01).
      ***
       COPY    LWMSG.
      **
           COPY    L-JOJF.
           COPY    LJOLJF-RYO.
           COPY    L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           02  ACP-CD     PIC  9(01).
           02  ACP-KAKU   PIC  9(01).
           02  DSP-NM     PIC  N(06).
           02  DSP-DAT    PIC  ZZZ,ZZ9 .
           02  DSP-JYU    PIC  ZZZ,ZZ9 .
           02  ACP-01     PIC  X(01).
           02  DSP-04     PIC  N(04).
           02  DSP-04B    PIC  N(04).
           02  DSP-05.
               03  DSP-051     PIC  X(03).
               03  DSP-052     PIC  N(10).
           02  DSP-DATE.
               03  DSP-YY      PIC Z9 .
               03  DSP-MM      PIC Z9 .
               03  DSP-DD      PIC Z9 .
           02  DSP-INV    PIC  X(24)
                   VALUE  "ÅñÅñÅ@ëóêMå≥ñ¢ìoò^Å@ÅñÅñ".
           02  DSP-TEL    PIC  X(28)
                   VALUE  "ÅñÅñÅ@ÇsÇdÇkÅDáÇñ¢ìoò^Å@ÅñÅñ".
           02  DSP-ER1    PIC  X(26)
                   VALUE  "ÅñÅñÅ@éÛêMÉfÅ[É^ñ≥ÇµÅ@ÅñÅñ".
      *
      ***
       COPY    LSMSG.
      ***
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "152" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CD" "9" "1" "60" "1" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CD" BY REFERENCE W-CD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" "9" "24" "77" "1" "ACP-CD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NM" "N" "1" "63" "12" "ACP-KAKU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NM" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-DAT" "ZZZ,ZZ9" "5" "36" "7" "DSP-NM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DAT" BY REFERENCE DATKN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-JYU" "ZZZ,ZZ9" "5" "60" "7" "DSP-DAT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-JYU" BY REFERENCE JYUKN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-01" "X" "24" "1" "1" "DSP-JYU" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-01" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" "N" "14" "45" "8" "ACP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-04" BY REFERENCE W-JKYO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04B" "bN" "14" "45" "8" "DSP-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-04B" BY REFERENCE W-JKYO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" " " "17" "0" "23" "DSP-04B" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-051" "X" "17" "28" "3" " " "DSP-05" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-051" BY REFERENCE W-EMGA "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-052" "N" "17" "33" "20" "DSP-051" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-052" BY REFERENCE W-EMGN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATE" " " "1" "0" "6" "DSP-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-YY" "Z9" "1" "66" "2" " " "DSP-DATE" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-YY" BY REFERENCE W-YY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MM" "Z9" "1" "69" "2" "DSP-YY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MM" BY REFERENCE W-MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DD" "Z9" "1" "72" "2" "DSP-MM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DD" BY REFERENCE W-DD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-INV" "X" "23" "1" "24" "DSP-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TEL" "X" "23" "1" "28" "DSP-INV" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ER1" "X" "23" "1" "26" "DSP-TEL" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *    ÇlÇ`ÇhÇmÅ@ÇqÇnÇtÇsÇhÇmÇdÅ@Å@Å@Å@Å@Å@                        *
      ******************************************************************
       MAIN.
           PERFORM  INI-RTN        THRU  INI-EX.
           IF  END-SW     = 1
               GO  TO  MR999
           END-IF.
       MRUPD.
           PERFORM  UPD-RTN        THRU  UPD-EX.
       MROKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-KAKU "ACP-KAKU"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  NOT = "01" AND "06"
               GO  TO  MROKC
           END-IF
           IF  W-KAKU NOT = 1
               GO  TO  MROKC
           END-IF.
       MR999.
           PERFORM  END-RTN        THRU  END-EX.
           CALL "DB_Close".
           STOP  RUN.
      ******************************************************************
      *    ÇhÇmÇhÅ|ÇqÇsÇmÅ@Å@Åièâä˙èàóùÅj            Å@                *
      ******************************************************************
       INI-RTN.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
      *
           CALL "SD_Screen_Output" USING "SJTO30" RETURNING RESU.
      *
           ACCEPT W-DATE   FROM DATE.
           ACCEPT STR-TIME FROM TIME.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON4-KEY" BY REFERENCE JCON4-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
      *
      *           READ JOLJF NEXT AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT AT END" JOLJF_PNAME1 BY REFERENCE JOLJF11-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-ER1" DSP-ER1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               MOVE    1       TO  END-SW
               GO TO INI-EX
           END-IF
      *
           MOVE   "3"         TO  JCON3-01.
           MOVE   JOLJF11-08  TO  JCON3-02 W-CD.
      *           READ   JCON    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   SPACE       TO  JCON3-03
           END-IF
           CALL "SD_Output" USING "DSP-NM" DSP-NM "p" RETURNING RESU.
      *
           MOVE   0001         TO  JOJF-01.
      *           READ   JOJF   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  INI-010
           END-IF
           MOVE   JOJF-90      TO  NXT-NO.
           GO  TO  INI-EX.
       INI-010.
           MOVE   SPACE        TO  JOJF-REC.
           INITIALIZE              JOJF-REC.
           MOVE   0001         TO  JOJF-01.
           MOVE   10           TO  JOJF-90.
      *
      *           WRITE   JOJF-REC       INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"      TO  ERR-F
               MOVE  JOJF-KEY    TO  ERR-K
               MOVE  "W"         TO  ERR-M
               PERFORM ERR-RTN THRU ERR-EX
           END-IF.
       INI-EX.
           EXIT.
      ******************************************************************
      *    ÇtÇoÇcÅ|ÇqÇsÇmÅ@       ÅiÇnÅ^ÇkèÛãµÇeÅ@çXêVÅjÅ@Å@Å@Å@       *
      ******************************************************************
       UPD-RTN.
           ADD   1             TO  W-SKSU.
           ADD   1             TO  W-KEN.
           MOVE  JOLJF11-KEYW  TO  END-CODE.
           IF  STR-CODE     =  SPACE
               MOVE  JOLJF11-KEYW  TO  STR-CODE
           END-IF
           MOVE  W-KEN         TO  JYUKN.
           CALL "SD_Output" USING "DSP-JYU" DSP-JYU "p" RETURNING RESU.
       UPD-010.
      *           READ JOLJF NEXT AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT AT END" JOLJF_PNAME1 BY REFERENCE JOLJF11-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-100
           END-IF
           GO  TO  UPD-RTN.
       UPD-100.
           MOVE  NXT-NO           TO  JOJF-01.
           MOVE  W-MD             TO  JOJF-02.
           MOVE  STR-JF           TO  JOJF-03.
           ACCEPT  END-TIME       FROM  TIME.
           MOVE  END-JF           TO  JOJF-04.
           MOVE  2                TO  JOJF-05.
           MOVE  1                TO  JOJF-061.
           MOVE  ZERO             TO  JOJF-063.
           MOVE  SPACE            TO  JOJF-062.
           MOVE  JS-SIGN          TO  JOJF-07.
      *
           MOVE  11               TO  JOJF-08(1).
           MOVE  W-KEN            TO  JOJF-09(1)  JOJF-10(1).
           MOVE  STR-CODE         TO  JOJF-11(1).
           MOVE  END-CODE         TO  JOJF-12(1).
      *
      *           WRITE    JOJF-REC    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"   TO  ERR-F
               MOVE  JOJF-KEY TO  ERR-K
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           PERFORM  AFT-RTN  THRU  AFT-EX.
       UPD-EX.
           EXIT.
      **********************************************
      *    Ç`ÇeÇsÅ|ÇqÇsÇmÅ@Å@Åiå„èàóùÅj            *
      **********************************************
       AFT-RTN.
           MOVE   0001        TO   JOJF-01.
      *           READ   JOJF    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"      TO  ERR-F
               MOVE  JOJF-KEY    TO  ERR-K
               MOVE  "A"         TO  ERR-M
               PERFORM ERR-RTN THRU ERR-EX
           END-IF
      *
           ADD    1           TO   JOJF-90.
      *
      *           REWRITE  JOJF-REC   INVALID
      *///////////////
           CALL "DB_Update" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"      TO  ERR-F
               MOVE  JOJF-KEY    TO  ERR-K
               MOVE  "R"         TO  ERR-M
               PERFORM ERR-RTN THRU ERR-EX
           END-IF.
      *
       AFT-EX.
           EXIT.
      ******************************************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇmÅ@Å@ÅièIóπèàóùÅj                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
       END-EX.
           EXIT.
      *
      ***
       COPY    LPMSG.
      ***
