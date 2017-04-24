      *************************************************
      *    PROGRAM        :　経費相手科目別日計表     *
      *    AUTHOR         :  MAYUMI.I                 *
      *    DATE           :  90/12/26                 *
      *    COMPILE  TYPE  :  COBOL                    *
      *    PRINTER  TYPE  :  JIPS                     *
      *************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PRG110.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                    DIVISION.
      *
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  W-DMM               PIC 9(01).
       77  W-POC               PIC 9(01)  VALUE  0.
       01  HEAD1.
           02  W-20K           PIC X(05)  VALUE  X"1A24212474".
           02  F               PIC N(02)  VALUE  "【　".
           02  H-NEN           PIC 9(02).
           02  F               PIC N(01)  VALUE  "年".
           02  H-GET           PIC 9(02).
           02  F               PIC N(02)  VALUE  "月分".
           02  F               PIC N(02)  VALUE  "　】".
           02  F               PIC X(20)  VALUE  SPACE.
           02  F               PIC N(22)  VALUE
               "＊＊＊　　経費　相手科目別　日計表　　＊＊＊".
           02  F               PIC X(38)  VALUE  SPACE.
       01  HEAD2.
           02  F               PIC X(06)  VALUE  "   : I".
           02  F               PIC X(18)  VALUE  "------------------".
           02  F               PIC N(09)  VALUE  "　製　造　経　費　".
           02  F               PIC X(18)  VALUE  "------------------".
           02  F               PIC X(05)  VALUE  "I ･ I".
           02  F               PIC X(18)  VALUE  "------------------".
           02  F               PIC N(09)  VALUE  "　一般管理販売費　".
           02  F               PIC X(19)  VALUE  "------------------I".
       01  HEAD3.
           02  F               PIC N(01)  VALUE  "日".
           02  F               PIC X(02)  VALUE  " :".
           02  F               PIC X(09)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "現預金".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "手　形".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "振　替".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "合　計".
           02  F               PIC X(02)  VALUE  " ･".
           02  F               PIC X(09)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "現預金".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "手　形".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "振　替".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "合　計".
       01  W-P.
           02  P-PEY           PIC Z9.
           02  P-TMD           REDEFINES  P-PEY.
             03  P-TM          PIC N(01).
           02  F               PIC X(01).
           02  P-X1            PIC X(01).
           02  F               PIC X(01).
           02  P-GYKS          PIC --,---,---,--9.
           02  P-GYKSD         REDEFINES P-GYKS.
             03  F             PIC X(04).
             03  P-GYKSF       PIC X(01).
             03  P-GYKSP       PIC ---9.99.
             03  P-GYKSR       PIC X(02).
           02  P-TGTS          PIC --,---,---,--9.
           02  P-TGTSD         REDEFINES P-TGTS.
             03  F             PIC X(04).
             03  P-TGTSF       PIC X(01).
             03  P-TGTSP       PIC ---9.99.
             03  P-TGTSR       PIC X(02).
           02  P-ETCS          PIC --,---,---,--9.
           02  P-ETCSD         REDEFINES P-ETCS.
             03  F             PIC X(04).
             03  P-ETCSF       PIC X(01).
             03  P-ETCSP       PIC ---9.99.
             03  P-ETCSR       PIC X(02).
           02  P-KEIS          PIC --,---,---,--9.
           02  P-KEISD         REDEFINES P-KEIS.
             03  F             PIC X(04).
             03  P-KEISF       PIC X(01).
             03  P-KEISP       PIC ---9.99.
             03  P-KEISR       PIC X(02).
           02  F               PIC X(01).
           02  P-X2            PIC X(01).
           02  F               PIC X(01).
           02  P-GYKI          PIC --,---,---,--9.
           02  P-GYKID         REDEFINES P-GYKI.
             03  F             PIC X(04).
             03  P-GYKIF       PIC X(01).
             03  P-GYKIP       PIC ---9.99.
             03  P-GYKIR       PIC X(02).
           02  P-TGTI          PIC --,---,---,--9.
           02  P-TGTID         REDEFINES P-TGTI.
             03  F             PIC X(04).
             03  P-TGTIF       PIC X(01).
             03  P-TGTIP       PIC ---9.99.
             03  P-TGTIR       PIC X(02).
           02  P-ETCI          PIC --,---,---,--9.
           02  P-ETCID         REDEFINES P-ETCI.
             03  F             PIC X(04).
             03  P-ETCIF       PIC X(01).
             03  P-ETCIP       PIC ---9.99.
             03  P-ETCIR       PIC X(02).
           02  P-KEII          PIC --,---,---,--9.
           02  P-KEIID         REDEFINES P-KEII.
             03  F             PIC X(04).
             03  P-KEIIF       PIC X(01).
             03  P-KEIIP       PIC ---9.99.
             03  P-KEIIR       PIC X(02).
       01  W-DATA.
           02  W-D.
             03  W-DD          OCCURS  31.
               04  W-GYKS      PIC S9(10).
               04  W-TGTS      PIC S9(10).
               04  W-ETCS      PIC S9(10).
               04  W-KEIS      PIC S9(10).
               04  W-GYKI      PIC S9(10).
               04  W-TGTI      PIC S9(10).
               04  W-ETCI      PIC S9(10).
               04  W-KEII      PIC S9(10).
           02  WT-D.
             03  WT-GYKS       PIC S9(10).
             03  WT-TGTS       PIC S9(10).
             03  WT-ETCS       PIC S9(10).
             03  WT-KEIS       PIC S9(10).
             03  WT-GYKI       PIC S9(10).
             03  WT-TGTI       PIC S9(10).
             03  WT-ETCI       PIC S9(10).
             03  WT-KEII       PIC S9(10).
             03  WT-GYKSP      PIC S9(03)V9(02).
             03  WT-TGTSP      PIC S9(03)V9(02).
             03  WT-ETCSP      PIC S9(03)V9(02).
             03  WT-GYKIP      PIC S9(03)V9(02).
             03  WT-TGTIP      PIC S9(03)V9(02).
             03  WT-ETCIP      PIC S9(03)V9(02).
           02  W-NGP.
             03  W-NG.
               04  W-NEN       PIC 9(04).
               04  W-NENL      REDEFINES W-NEN.
                 05  W-NEN1    PIC 9(02).
                 05  W-NEN2    PIC 9(02).
               04  W-GET       PIC 9(02).
             03  W-PEY         PIC 9(02).
           02  W-NGPD.
             03  W-NGD         PIC 9(06).
             03  F             PIC 9(02).
           02  W-DATE.
             03  F             PIC 9(6).
             03  W-PEYD        PIC 9(2).
           02  W-DNO           PIC 9(06).
           02  W-SIWA.
             03  W-KRD.
               04  W-KR        OCCURS   5.
                 05  W-KRKM.
                   06  W-KRKA  PIC 9(04).
                   06  W-KRHO  PIC 9(04).
                 05  W-KRKH    PIC 9(01).
                 05  W-KRKN    PIC S9(10).
             03  W-KSD.
               04  W-KS        OCCURS   5.
                 05  W-KSKM.
                   06  W-KSKA  PIC 9(04).
                   06  W-KSHO  PIC 9(04).
                 05  W-KSKH    PIC 9(01).
                 05  W-KSKN    PIC S9(10).
           02  WD-SIWA.
             03  WD-KRD.
               04  WD-KR       OCCURS   5.
                 05  WD-KRKM.
                   06  WD-KRKA PIC 9(04).
                   06  WD-KRHO PIC 9(04).
                 05  WD-KRKH   PIC 9(01).
                 05  WD-KRKN   PIC S9(10).
             03  WD-KSD.
               04  WD-KS       OCCURS   5.
                 05  WD-KSKM.
                   06  WD-KSKA PIC 9(04).
                   06  WD-KSHO PIC 9(04).
                 05  WD-KSKH   PIC 9(01).
                 05  WD-KSKN   PIC S9(10).
           02  CNT.
             03  W-R           PIC 9(01).
             03  W-S           PIC 9(01).
           COPY LWMSG_PR.
      *       FD  SDH
       01  SDH_PRG110.
           02  SDH_PNAME1      PIC  X(009)  VALUE "SIWAKE-H1".
           02  F               PIC  X(001).
           02  SDH_LNAME       PIC  X(003)  VALUE "SDH".
           02  F               PIC  X(001).
           02  SDH_KEY1        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2        PIC  X(100)  VALUE SPACE.
           02  SDH_SORT        PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST       PIC  X(100)  VALUE SPACE.
           02  SDH_RES         USAGE  POINTER.
       COPY  SIWAKH.
       77  F                   PIC  X(001).
      ***
       COPY  FCTL.
      *       FD  SP-F
       01  SP-R                PIC X(206).
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER       PIC  9(003).
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RESP                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER          PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  C-MID.
           02  FILLER.
               03  FILLER      PIC N(12) VALUE
                   "経費　相手科目別　日計表".
           02  FILLER.
               03  FILLER      PIC  N(01) VALUE "年".
               03  FILLER      PIC  N(02) VALUE "月度".
           02  FILLER          PIC X(18) 
               VALUE  "確認 OK=1,NO=9 ( )".
      *
       01  D-NG.
           02  FILLER          PIC  N(02).
           02  FILLER          PIC  N(02).
       01  C-ACP.
           02  A-DMM           PIC 9(01).
       01  C-ERR.
           02  E-ME1           PIC  X(23)  VALUE
               "***  ｺﾝﾄﾛｰﾙﾏｽﾀｰ ﾅｼ  ***".
           02  E-ME2           PIC  X(18)  VALUE
               "***  DATA ｴﾗｰ  ***".
           02  E-KEY           PIC  X(17).
           02  E-ME98          PIC  X(05)  VALUE  X"1B4A05".
           02  E-ME99          PIC  X(05)  VALUE  X"1B4205".
           COPY LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
      *    01  C-CLEAR.
           CALL "SD_Init" USING
                "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR"
                RETURNING RESU.
      *    01  C-MID.
           CALL "SD_Init" USING
                "C-MID" " " "0" "0" "48" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01C-MID" " " "1" "0" "24" " " "C-MID"
                 RETURNING RESU.
           CALL "SD_Init" USING
                "02C-MID" "RN" "1" "29" "24" " " "01C-MID"
                 RETURNING RESU.
           CALL "SD_Init" USING
                "03C-MID" " " "5" "0" "6" "01C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "0103C-MID" "N" "5" "6" "2" " " "03C-MID"
                RETURNING RESU.
           CALL "SD_Init" USING
                "0203C-MID" "N" "5" "12" "4" "0103C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING
                "04C-MID" "X" "24" "61" "18" "03C-MID" " "
                RETURNING RESU.
      *    01  D-NG
           CALL "SD_Init" USING
                "D-NG" " " "5" "0" "8" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01D-NG" "N" "5" "2" "4" " " "D-NG"  RETURNING RESU.
           CALL "SD_From" USING
                "01D-NG" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
           CALL "SD_Init" USING
                "02D-NG" "N" "5" "8" "4" "01D-NG" " "  RETURNING RESU.
           CALL "SD_From" USING
                "02D-NG" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
      *    01  C-ACP.
           CALL "SD_Init" USING
                "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "A-DMM" "9" "24" "77" "1" " " "C-ACP"  RETURNING RESU.
           CALL "SD_Into" USING
                "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *    01  C-ERR
           CALL "SD_Init" USING 
                "C-ERR" " " "24" "0" "68" " " " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "E-ME1" "X" "24" "15" "23" " " "C-ERR"  RETURNING RESU.
           CALL "SD_Init" USING 
                "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "E-KEY" "X" "24" "40" "17" "E-ME2" " "  RETURNING RESU.
           CALL "SD_From" USING 
                "E-KEY" BY REFERENCE SH-KEY1 "17" "0" RETURNING RESU.
           CALL "SD_Init" USING 
                "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "E-ME99" "X" "24" "75" "5" "E-ME98" " " 
                 RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
        M-05.
           CALL "SD_Output" USING 
            "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING 
           "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING 
               "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE  FCTL-REC     TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING
            "D-NG" D-NG "p" RETURNING RESU.
           MOVE  Z-KONYMD     TO  ZYMD   W-NGP.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               CALL "SD_Output" USING 
               "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "SD_Output" USING 
               "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  ESTAT  NOT = "01" AND "06"
               GO  TO  M-10
           END-IF.
           IF  W-DMM  =  9
               CALL "SD_Output" USING 
               "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  W-DMM  NOT =  1
               GO  TO  M-10
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
      *
       M-15.
           MOVE    1         TO W-PEY.
           MOVE SPACE        TO SH-KEY1.
           MOVE W-NGP        TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT LESS " SH-KEY1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                           RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                           RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                           RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE HTRDATE      TO W-NGPD.
           IF  W-NGD   NOT = W-NG
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING 
               "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE ZERO         TO W-D.
       M-20.
           MOVE HTRDATE      TO W-DATE.
           MOVE HJUNLNO      TO W-DNO.
           MOVE ZERO         TO W-SIWA.
       M-25.
           IF  HDR-CR      =  1
               MOVE HKACD1     TO W-KRKM(HLINENO)
               MOVE HKEIHIKB   TO W-KRKH(HLINENO)
               MOVE HAMOUNT    TO W-KRKN(HLINENO)
           ELSE
               MOVE HKACD1     TO W-KSKM(HLINENO)
               MOVE HKEIHIKB   TO W-KSKH(HLINENO)
               MOVE HAMOUNT    TO W-KSKN(HLINENO)
           END-IF.
      *
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-50
           END-IF.
           MOVE HTRDATE      TO W-NGPD.
           IF  W-NGD   NOT = W-NG
               GO  TO  M-50
           END-IF.
           IF  HJUNLNO     = W-DNO
               GO  TO  M-25
           END-IF.
      *
           IF  0  = W-KRKH(1) AND W-KRKH(2) AND W-KRKH(3) AND
                    W-KRKH(4) AND W-KRKH(5) AND
                    W-KSKH(1) AND W-KSKH(2) AND W-KSKH(3) AND
                    W-KSKH(4) AND W-KSKH(5)
               GO  TO  M-20
           END-IF.
      *
           PERFORM  S-15  THRU S-65.
           GO  TO  M-20.
       M-50.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           MOVE ZERO         TO W-PEYD  WT-D.
       M-55.
           ADD   1           TO W-PEYD.
           IF  W-PEYD        >  31
               GO  TO  M-60
           END-IF.
           IF  ZERO  =  W-GYKS(W-PEYD) AND W-TGTS(W-PEYD)  AND
                        W-ETCS(W-PEYD) AND
                        W-GYKI(W-PEYD) AND W-TGTI(W-PEYD)  AND
                        W-ETCI(W-PEYD)
               GO  TO  M-55
           END-IF.
           IF  W-POC         =  0
               MOVE  5        TO  W-POC
               PERFORM  S-05  THRU S-10
           END-IF.
           COMPUTE  W-KEIS(W-PEYD)  = W-GYKS(W-PEYD) + W-TGTS(W-PEYD) +
                                      W-ETCS(W-PEYD).
           COMPUTE  W-KEII(W-PEYD)  = W-GYKI(W-PEYD) + W-TGTI(W-PEYD) +
                                      W-ETCI(W-PEYD).
           MOVE SPACE          TO W-P.
           MOVE ":"            TO P-X1.
           MOVE "･"            TO P-X2.
           MOVE W-PEYD         TO P-PEY.
           MOVE W-GYKS(W-PEYD) TO P-GYKS.
           MOVE W-TGTS(W-PEYD) TO P-TGTS.
           MOVE W-ETCS(W-PEYD) TO P-ETCS.
           MOVE W-KEIS(W-PEYD) TO P-KEIS.
           MOVE W-GYKI(W-PEYD) TO P-GYKI.
           MOVE W-TGTI(W-PEYD) TO P-TGTI.
           MOVE W-ETCI(W-PEYD) TO P-ETCI.
           MOVE W-KEII(W-PEYD) TO P-KEII.
           MOVE SPACE          TO SP-R.
           MOVE W-P            TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE          TO SP-R.
           MOVE SPACE          TO W-P.
           MOVE ":"            TO P-X1.
           MOVE "･"            TO P-X2.
           MOVE SPACE          TO SP-R.
           MOVE W-P            TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE          TO SP-R.
           ADD  W-GYKS(W-PEYD) TO WT-GYKS.
           ADD  W-TGTS(W-PEYD) TO WT-TGTS.
           ADD  W-ETCS(W-PEYD) TO WT-ETCS.
           ADD  W-KEIS(W-PEYD) TO WT-KEIS.
           ADD  W-GYKI(W-PEYD) TO WT-GYKI.
           ADD  W-TGTI(W-PEYD) TO WT-TGTI.
           ADD  W-ETCI(W-PEYD) TO WT-ETCI.
           ADD  W-KEII(W-PEYD) TO WT-KEII.
           GO  TO  M-55.
       M-60.
           MOVE SPACE          TO W-P.
           MOVE ":"            TO P-X1.
           MOVE "･"            TO P-X2.
           MOVE "計"           TO P-TM.
           MOVE WT-GYKS        TO P-GYKS.
           MOVE WT-TGTS        TO P-TGTS.
           MOVE WT-ETCS        TO P-ETCS.
           MOVE WT-KEIS        TO P-KEIS.
           MOVE WT-GYKI        TO P-GYKI.
           MOVE WT-TGTI        TO P-TGTI.
           MOVE WT-ETCI        TO P-ETCI.
           MOVE WT-KEII        TO P-KEII.
           MOVE SPACE          TO SP-R.
           MOVE W-P            TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE          TO SP-R.
      *
           IF  WT-KEIS         = ZERO
               GO  TO  M-65
           END-IF.
           IF  WT-GYKS     NOT = ZERO
               COMPUTE WT-GYKSP  ROUNDED  =  (WT-GYKS * 100) / WT-KEIS
           END-IF.
           IF  WT-TGTS     NOT = ZERO
               COMPUTE WT-TGTSP  ROUNDED  =  (WT-TGTS * 100) / WT-KEIS
           END-IF.
           IF  WT-ETCS     NOT = ZERO
               COMPUTE WT-ETCSP  ROUNDED  =  (WT-ETCS * 100) / WT-KEIS
           END-IF.
       M-65.
           IF  WT-KEII         = ZERO
               GO  TO  M-70
           END-IF.
           IF  WT-GYKI     NOT = ZERO
               COMPUTE WT-GYKIP  ROUNDED  =  (WT-GYKI * 100) / WT-KEII
           END-IF.
           IF  WT-TGTI     NOT = ZERO
               COMPUTE WT-TGTIP  ROUNDED  =  (WT-TGTI * 100) / WT-KEII
           END-IF.
           IF  WT-ETCI     NOT = ZERO
               COMPUTE WT-ETCIP  ROUNDED  =  (WT-ETCI * 100) / WT-KEII
           END-IF.
       M-70.
           MOVE SPACE          TO W-P.
           MOVE ":"            TO P-X1.
           MOVE "･"            TO P-X2.
           MOVE "("            TO P-GYKSF P-TGTSF P-ETCSF P-KEISF
                                  P-GYKIF P-TGTIF P-ETCIF P-KEIIF.
           MOVE "%)"           TO P-GYKSR P-TGTSR P-ETCSR P-KEISR
                                  P-GYKIR P-TGTIR P-ETCIR P-KEIIR.
           MOVE WT-GYKSP       TO P-GYKSP.
           MOVE WT-TGTSP       TO P-TGTSP.
           MOVE WT-ETCSP       TO P-ETCSP.
           MOVE 100            TO P-KEISP.
           MOVE WT-GYKIP       TO P-GYKIP.
           MOVE WT-TGTIP       TO P-TGTIP.
           MOVE WT-ETCIP       TO P-ETCIP.
           MOVE 100            TO P-KEIIP.
           MOVE SPACE          TO SP-R.
           MOVE W-P            TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING
            "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *
       S-05.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-NEN2   TO H-NEN.
           MOVE W-GET    TO H-GET.
           MOVE SPACE    TO SP-R.
           MOVE HEAD1    TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
           MOVE HEAD2    TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
           MOVE HEAD3    TO SP-R.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE    TO SP-R.
           MOVE SPACE    TO W-P.
           MOVE ":"      TO P-X1.
           MOVE "･"      TO P-X2.
           MOVE SPACE    TO SP-R.
           MOVE W-P      TO SP-R.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE    TO SP-R.
       S-10.
           EXIT.
       S-15.
           MOVE W-SIWA   TO WD-SIWA.
           MOVE ZERO     TO W-R.
       S-20.
           ADD  1        TO W-R.
           IF  W-R              = 6
               GO      TO  S-40
           END-IF.
           IF  WD-KRKH(W-R) NOT = 1
               GO      TO  S-20
           END-IF.
           IF  W-DNO > 299999 AND < 400000
               MOVE W-R  TO W-S
               GO TO S-30
           END-IF.
           MOVE ZERO     TO W-S.
       S-25.
           ADD  1        TO W-S.
           IF  W-S              = 6
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-95
           END-IF.
           IF  WD-KSKN(W-S)     = ZERO
               GO  TO  S-25
           END-IF.
           IF  WD-KRKN(W-R)     >  WD-KSKN(W-S)
               GO  TO  S-35
           END-IF.
       S-30.
           IF  WD-KSKA(W-S)     =  2110
               IF  WD-KRKA(W-R)     <  7000
                   ADD  WD-KRKN(W-R)  TO  W-TGTS(W-PEYD)
               ELSE
                   ADD  WD-KRKN(W-R)  TO  W-TGTI(W-PEYD)
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      >  0129)  AND
                   (WD-KSKA(W-S)  NOT =  0380)
                   IF  WD-KRKA(W-R)      <  7000
                       ADD  WD-KRKN(W-R)  TO  W-ETCS(W-PEYD)
                   ELSE
                       ADD  WD-KRKN(W-R)  TO  W-ETCI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      <  0130)  OR
                   (WD-KSKA(W-S)      =  0380)
                   IF  WD-KRKA(W-R)      <  7000
                       ADD  WD-KRKN(W-R)  TO  W-GYKS(W-PEYD)
                   ELSE
                       ADD  WD-KRKN(W-R)  TO  W-GYKI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           SUBTRACT  WD-KRKN(W-R)  FROM  WD-KSKN(W-S).
           MOVE ZERO         TO WD-KRKN(W-R).
           GO  TO  S-20.
       S-35.
           IF  WD-KSKA(W-S)     =  2110
               IF  WD-KRKA(W-R)     <  7000
                   ADD  WD-KSKN(W-S)  TO  W-TGTS(W-PEYD)
               ELSE
                   ADD  WD-KSKN(W-S)  TO  W-TGTI(W-PEYD)
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      >  0129)  AND
                   (WD-KSKA(W-S)  NOT =  0380)
                   IF  WD-KRKA(W-R)      <  7000
                       ADD  WD-KSKN(W-S)  TO  W-ETCS(W-PEYD)
                   ELSE
                       ADD  WD-KSKN(W-S)  TO  W-ETCI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      <  0130)  OR
                   (WD-KSKA(W-S)      =  0380)
                   IF  WD-KRKA(W-R)      <  7000
                       ADD  WD-KSKN(W-S)  TO  W-GYKS(W-PEYD)
                   ELSE
                       ADD  WD-KSKN(W-S)  TO  W-GYKI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           SUBTRACT  WD-KSKN(W-S)  FROM  WD-KRKN(W-R).
           MOVE ZERO         TO WD-KSKN(W-S).
           GO  TO  S-25.
      *
       S-40.
           MOVE W-SIWA       TO WD-SIWA.
           MOVE ZERO         TO W-S.
       S-45.
           ADD  1            TO W-S.
           IF  W-S              = 6
               GO  TO  S-65
           END-IF.
           IF  WD-KSKH(W-S) NOT = 1
               GO  TO  S-45
           END-IF.
           IF  W-DNO > 299999 AND < 400000
               MOVE W-S TO W-R
               GO TO S-55
           END-IF.
           MOVE ZERO         TO W-R.
       S-50.
           ADD  1            TO W-R.
           IF  W-R              = 6
               CALL "SD_Output" USING 
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING 
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING 
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-95
           END-IF.
           IF  WD-KRKN(W-R)     = ZERO
               GO  TO  S-50
           END-IF.
           IF  WD-KSKN(W-S)     >  WD-KRKN(W-R)
               GO  TO  S-60
           END-IF.
       S-55.
           IF  WD-KRKA(W-R)     =  2110
               IF  WD-KSKA(W-S)     <  7000
                   SUBTRACT  WD-KSKN(W-S)  FROM  W-TGTS(W-PEYD)
               ELSE
                   SUBTRACT  WD-KSKN(W-S)  FROM  W-TGTI(W-PEYD)
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      >  0129)  AND
                   (WD-KRKA(W-R)  NOT =  0380)
                   IF  WD-KSKA(W-S)     <  7000
                       SUBTRACT  WD-KSKN(W-S)  FROM W-ETCS(W-PEYD)
                   ELSE
                       SUBTRACT  WD-KSKN(W-S)  FROM W-ETCI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      <  0130)  OR
                   (WD-KRKA(W-R)      =  0380)
                   IF  WD-KSKA(W-S)     <  7000
                       SUBTRACT  WD-KSKN(W-S)  FROM W-GYKS(W-PEYD)
                   ELSE
                       SUBTRACT  WD-KSKN(W-S)  FROM W-GYKI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           SUBTRACT  WD-KSKN(W-S)  FROM  WD-KRKN(W-R).
           MOVE ZERO         TO WD-KSKN(W-S).
           GO  TO  S-45.
       S-60.
           IF  WD-KRKA(W-R)     =  2110
               IF  WD-KSKA(W-S)     <  7000
                   SUBTRACT  WD-KRKN(W-R)  FROM  W-TGTS(W-PEYD)
               ELSE
                   SUBTRACT  WD-KRKN(W-R)  FROM  W-TGTI(W-PEYD)
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      >  0129)  AND
                   (WD-KRKA(W-R)  NOT =  0380)
                   IF  WD-KSKA(W-S)     <  7000
                       SUBTRACT  WD-KRKN(W-R)  FROM W-ETCS(W-PEYD)
                   ELSE
                       SUBTRACT  WD-KRKN(W-R)  FROM W-ETCI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      <  0130)  OR
                   (WD-KRKA(W-R)      =  0380)
                   IF  WD-KSKA(W-S)     <  7000
                       SUBTRACT  WD-KRKN(W-R)  FROM W-GYKS(W-PEYD)
                   ELSE
                       SUBTRACT  WD-KRKN(W-R)  FROM W-GYKI(W-PEYD)
                   END-IF
               END-IF
           END-IF.
           SUBTRACT  WD-KRKN(W-R)  FROM  WD-KSKN(W-S).
           MOVE ZERO         TO WD-KRKN(W-R).
           GO  TO  S-50.
       S-65.
           EXIT.
       CLSE-ENT.
       CLSE-EXT.
           EXIT.
           COPY LPMSG_PR.
