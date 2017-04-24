      *************************************************
      *    PROGRAM        :　経費相手科目別日計表     *
      *    AUTHOR         :  MAYUMI.I                 *
      *    DATE           :  90/12/26                 *
      *    COMPILE  TYPE  :  COBOL                    *
      *    PRINTER  TYPE  :  JIPS                     *
      *************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PRG150.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA           DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  W-DMM               PIC 9(01).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NGP.
             03  W-NG          PIC 9(06).
             03  W-PEY         PIC 9(02).
           02  W-NGPD.
             03  W-NGD         PIC 9(06).
             03  F             PIC 9(02).
           02  W-DNO           PIC 9(06).
           02  W-SIWA.
             03  W-KRD.
               04  W-KR  OCCURS   5.
                 05  W-KRKM.
                   06  W-KRKA  PIC 9(04).
                   06  W-KRHO  PIC 9(04).
                 05  W-KRKH    PIC 9(01).
                 05  W-KRKN    PIC S9(10).
             03  W-KSD.
               04  W-KS  OCCURS   5.
                 05  W-KSKM.
                   06  W-KSKA  PIC 9(04).
                   06  W-KSHO  PIC 9(04).
                 05  W-KSKH    PIC 9(01).
                 05  W-KSKN    PIC S9(10).
           02  WD-SIWA.
             03  WD-KRD.
               04  WD-KR  OCCURS   5.
                 05  WD-KRKM.
                   06  WD-KRKA PIC 9(04).
                   06  WD-KRHO PIC 9(04).
                 05  WD-KRKH   PIC 9(01).
                 05  WD-KRKN   PIC S9(10).
             03  WD-KSD.
               04  WD-KS  OCCURS   5.
                 05  WD-KSKM.
                   06  WD-KSKA PIC 9(04).
                   06  WD-KSHO PIC 9(04).
                 05  WD-KSKH   PIC 9(01).
                 05  WD-KSKN   PIC S9(10).
           02  CNT.
             03  W-R           PIC 9(01).
             03  W-S           PIC 9(01).
           COPY LWMSG_PR.
       01  SDH_PRG150.
           02  SDH_PNAME1      PIC  X(009)  VALUE "SIWAKE-H1".
           02  F               PIC  X(001).
           02  SDH_LNAME       PIC  X(003)  VALUE "SDH".
           02  F               PIC  X(001).
           02  SDH_KEY1        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2        PIC  X(100)  VALUE SPACE.
           02  SDH_SORT        PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST       PIC  X(100)  VALUE SPACE.
           02  SDH_RES             USAGE  POINTER.
       COPY  SIWAKH.
       77  F                   PIC  X(001).
      ***
       COPY  FCTL.
      ***
       01  KEI-PRN_PRG150.
           02  KEI-PRN_PNAME1      PIC  X(009)  VALUE SPACE.
           02  F                   PIC  X(001).
           02  KEI-PRN_LNAME       PIC  X(014)  VALUE "KEI-PRN_PRG150".
           02  F                   PIC  X(001).
           02  KEI-PRN_KEY1        PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_KEY2        PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_SORT        PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_IDLST       PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_RES         USAGE  POINTER.
       01  KEI-REC.
           02  KEI-KACD        PIC 9(04).
           02  KEI-KIN         PIC S9(10).
           02  KEI-AKC         PIC 9(01).
           02  FILLER          PIC X(49).
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  C-MID.
           02 FILLER.
               03  FILLER   PIC N(15)  VALUE
                   "経費相手科目作表ファイル　生成".
           02 FILLER.
               03  FILLER     PIC  N(01) VALUE "年".
               03  FILLER     PIC  N(02) VALUE "月度".
           02  FILLER  PIC X(18) VALUE  "確認 OK=1,NO=9 ( )".
      *
       01  D-NG.
           02  FILLER         PIC  N(02).
           02  FILLER         PIC  N(02).
       01  C-ACP.
           02  A-DMM    PIC 9(01).
       01  C-ERR.
           02  E-ME1   PIC  X(23)  VALUE
               "***  ｺﾝﾄﾛｰﾙﾏｽﾀｰ ﾅｼ  ***".
           02  E-ME2   PIC  X(18)  VALUE
               "***  DATA ｴﾗｰ  ***".
           02  E-KEY   PIC  X(17).
           02  E-ME98   PIC  X(05)  VALUE  X"1B4A09".
           02  E-ME99   PIC  X(05)  VALUE  X"1B4209".
           COPY LSMSG_PR.
      *
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *       01  C-CLEAR.
           CALL "SD_Init" USING 
                "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR"
                RETURNING RESU.
      *                01  C-MID.          
           CALL "SD_Init" USING 
                "C-MID" " " "0" "0" "54" " " " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "01C-MID" " " "1" "0" "30" " " "C-MID"  
                                       RETURNING RESU.
           CALL "SD_Init" USING 
                "0101C-MID" "RN" "1" "25" "30" " " "01C-MID"
                RETURNING RESU.
           CALL "SD_Init" USING 
                "02C-MID" " " "5" "0" "6" "01C-MID" " "  
                                         RETURNING RESU.
           CALL "SD_Init" USING 
                "0102C-MID" "N" "5" "6" "2" " " "02C-MID"  
                                          RETURNING RESU.
           CALL "SD_Init" USING 
                "0202C-MID" "N" "5" "12" "4" "0102C-MID" " "
                RETURNING RESU.
           CALL "SD_Init" USING 
                "03C-MID" "X" "24" "61" "18" "02C-MID" " "
                RETURNING RESU.
      *           01  D-NG
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
      *                 01  C-ACP.
           CALL "SD_Init" USING
                "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "A-DMM" "9" "24" "77" "1" " " "C-ACP"  RETURNING RESU.
           CALL "SD_Into" USING
                "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *      01  C-ERR
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 000
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
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF.
           MOVE  FCTL-REC     TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "D-NG" D-NG "p"
                                         RETURNING RESU
           MOVE  Z-KONYMD     TO  ZYMD   W-NGP.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               GO  TO  M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING
           BY REFERENCE A-DMM "A-DMM" "9" "1"
           BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF.
           IF  ESTAT  NOT = "01" AND "06"
               GO  TO  M-10
           END-IF.
           IF  W-DMM  =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF.
           IF  W-DMM  NOT =  1
               GO  TO  M-10
           END-IF.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KEI-PRN_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "DB_F_Open" USING
            "OUTPUT" KEI-PRN_PNAME1 " " BY REFERENCE KEI-PRN_IDLST "0".
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
               CALL "DB_F_Close" USING
                BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF.
           MOVE HTRDATE      TO W-NGPD.
           IF  W-NGD   NOT = W-NG
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  M-95
           END-IF.
       M-20.
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
           IF  0           = W-KRKH(1) AND W-KRKH(2) AND W-KRKH(3) AND
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
           CALL "DB_F_Close" USING
            BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close"
           STOP RUN.
      *
       S-15.
           MOVE W-SIWA       TO WD-SIWA.
           MOVE ZERO         TO W-R.
       S-20.
           ADD  1            TO W-R.
           IF  W-R              = 6
               GO  TO  S-40
           END-IF.
           IF  WD-KRKH(W-R) NOT = 1
               GO  TO  S-20
           END-IF.
           IF  W-DNO > 299999 AND < 400000
               MOVE W-R TO W-S
               GO TO S-30
           END-IF.
           MOVE ZERO         TO W-S.
       S-25.
           ADD  1            TO W-S.
           IF  W-S              = 6
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               GO  TO  M-95
           END-IF.
           IF  WD-KSKN(W-S)     = ZERO
               GO  TO  S-25
           END-IF.
           IF  WD-KRKN(W-R)     >  WD-KSKN(W-S)
               GO  TO  S-35
           END-IF.
       S-30.
           INITIALIZE    KEI-REC.
           MOVE  WD-KRKA(W-R)  TO  KEI-KACD.
           MOVE  WD-KRKN(W-R)  TO  KEI-KIN.
           IF  WD-KSKA(W-S)     =  2110
               MOVE  2             TO  KEI-AKC
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      >  0129)  AND
                   (WD-KSKA(W-S)  NOT =  0380)
                    MOVE  3             TO  KEI-AKC
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      <  0130)  OR
                   (WD-KSKA(W-S)      =  0380)
                    MOVE  1             TO  KEI-AKC
               END-IF
           END-IF.
      *           WRITE   KEI-REC.
      *///////////////
           CALL "DB_Insert" USING
            KEI-PRN_PNAME1 KEI-PRN_LNAME KEI-REC RETURNING RET.
           GO TO M-25.
           SUBTRACT  WD-KRKN(W-R)  FROM  WD-KSKN(W-S).
           MOVE ZERO         TO WD-KRKN(W-R).
           GO  TO  S-20.
       S-35.
           INITIALIZE    KEI-REC.
           MOVE  WD-KRKA(W-R)  TO  KEI-KACD.
           MOVE  WD-KSKN(W-S)  TO  KEI-KIN.
           IF  WD-KSKA(W-S)     =  2110
               MOVE  2             TO  KEI-AKC
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      >  0129)  AND
                   (WD-KSKA(W-S)  NOT =  0380)
                    MOVE  3             TO  KEI-AKC
               END-IF
           END-IF.
           IF  WD-KSKA(W-S)  NOT =  2110
               IF  (WD-KSKA(W-S)      <  0130)  OR
                   (WD-KSKA(W-S)      =  0380)
                    MOVE  1             TO  KEI-AKC
               END-IF
           END-IF.
      *           WRITE   KEI-REC.
      *///////////////
           CALL "DB_Insert" USING
            KEI-PRN_PNAME1 KEI-PRN_LNAME KEI-REC RETURNING RET.
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
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               GO  TO  M-95
           END-IF.
           IF  WD-KRKN(W-R)     = ZERO
               GO  TO  S-50
           END-IF.
           IF  WD-KSKN(W-S)     >  WD-KRKN(W-R)
               GO  TO  S-60
           END-IF.
       S-55.
           INITIALIZE    KEI-REC.
           MOVE  WD-KSKA(W-S)  TO  KEI-KACD.
           COMPUTE  KEI-KIN  =  -1  *  WD-KSKN(W-S).
           IF  WD-KRKA(W-R)     =  2110
               MOVE  2             TO  KEI-AKC
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      >  0129)  AND
                   (WD-KRKA(W-R)  NOT =  0380)
                    MOVE  3             TO  KEI-AKC
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      <  0130)  OR
                   (WD-KRKA(W-R)      =  0380)
                    MOVE  1             TO  KEI-AKC
               END-IF
           END-IF.
      *           WRITE   KEI-REC.
      *///////////////
           CALL "DB_Insert" USING
            KEI-PRN_PNAME1 KEI-PRN_LNAME KEI-REC RETURNING RET.
           SUBTRACT  WD-KSKN(W-S)  FROM  WD-KRKN(W-R).
           MOVE ZERO         TO WD-KSKN(W-S).
           GO  TO  S-45.
       S-60.
           INITIALIZE    KEI-REC.
           MOVE  WD-KSKA(W-S)  TO  KEI-KACD.
           COMPUTE  KEI-KIN  =  -1  *  WD-KRKN(W-R).
           IF  WD-KRKA(W-R)     =  2110
               MOVE  2             TO  KEI-AKC
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      >  0129)  AND
                   (WD-KRKA(W-R)  NOT =  0380)
                    MOVE  3             TO  KEI-AKC
               END-IF
           END-IF.
           IF  WD-KRKA(W-R)  NOT =  2110
               IF  (WD-KRKA(W-R)      <  0130)  OR
                   (WD-KRKA(W-R)      =  0380)
                    MOVE  1             TO  KEI-AKC
               END-IF
           END-IF.
      *           WRITE   KEI-REC.
      *///////////////
           CALL "DB_Insert" USING
            KEI-PRN_PNAME1 KEI-PRN_LNAME KEI-REC RETURNING RET.
           SUBTRACT  WD-KRKN(W-R)  FROM  WD-KSKN(W-S).
           MOVE ZERO         TO WD-KRKN(W-R).
           GO  TO  S-50.
       S-65.
           EXIT.
       CLSE-ENT.
       CLSE-EXT.
           EXIT.
           COPY LPMSG_PR.
