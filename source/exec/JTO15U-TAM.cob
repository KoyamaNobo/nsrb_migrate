       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JTO15U.
      *================================================================*
      *                  Å@    ÇnÅ^ÇkëóêMÇeê∂ê¨ÅiçƒëóópÅj              *
      *    W-JS    : 1=ì°ìc , 3=ëÅìá                                   *
      *================================================================*
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
           COPY  LWMSG.
      *
       01  W-JS                       PIC  9(01).
       01  JS-SIGN                    PIC  9(01).
       01  WORK-AREA.
           02  WORK-01.
               03  HIZ1-W             PIC  9(06).
               03  HIZ1-WR            REDEFINES  HIZ1-W.
                   04  YY1-W          PIC  9(02).
                   04  MM1-W          PIC  9(02).
                   04  DD1-W          PIC  9(02).
               03  HIZ2-W             PIC  9(06).
               03  HIZ2-WR            REDEFINES  HIZ2-W.
                   04  YY2-W          PIC  9(02).
                   04  MM2-W          PIC  9(02).
                   04  DD2-W          PIC  9(02).
               03  TIM1-W             PIC  9(08).
               03  TIM1-WR            REDEFINES  TIM1-W.
                   04  JI1-W          PIC  9(02).
                   04  FU1-W          PIC  9(02).
                   04  SE1-W          PIC  9(04).
               03  TIM2-W             PIC  9(08).
               03  TIM2-WR            REDEFINES  TIM2-W.
                   04  JI2-W          PIC  9(02).
                   04  FU2-W          PIC  9(02).
                   04  SE2-W          PIC  9(04).
               03  INP-W              PIC  9(01).
               03  W-NO               PIC  9(02).
               03  W-NOD              PIC  9(02).
               03  KAKU-W             PIC  X(01).
               03  STS-W              PIC  9(01).
               03  ERR-CD             PIC  9(01).
               03  ERR-SW             PIC  9(01).
               03  ERR-STAT           PIC  X(02).
               03  DEN-CP             PIC  9(06).
               03  NXT-NO             PIC  9(04).
               03  NN                 PIC  9(02).
               03  NA                 PIC  9(02).
               03  W-KBN              PIC  9(01).
               03  W-SKB              PIC  9(01).
               03  W-SNM              PIC  N(06).
               03  W-DATE             PIC  9(08).
               03  W-NGP              REDEFINES  W-DATE.
                   04  F              PIC  9(02).
                   04  W-NGPS         PIC  9(06).
       01  STR-TMP.
           02  STR-01                 PIC  9(02).
           02  STR-02.
               03  STR-020            PIC  9(07).
               03  STR-021            PIC  X(243).
               03  STR-021A           PIC  X(01).
               03  STR-022            PIC  9(01).
               03  STR-023            PIC  9(01).
               03  STR-024            PIC  9(01).
       01  NIF-TMP.
           02  NIF-01                 PIC  9(02).
           02  NIF-02                 OCCURS  2.
               03  NIF-021            PIC  9(07).
               03  NIF-022            PIC  X(120).
       01  OKJ-TMP.
           02  OKJ-01                 PIC  9(02).
           02  OKJ-02                 OCCURS  4.
               03  OKJ-021            PIC  9(06).
               03  OKJ-022            PIC  X(57).
       01  TDNW-TMP.
           02  TDNW-01                PIC  9(02).
           02  TDNW-02.
               03  TDNW-02A           PIC  X(11).
               03  TDNW-020           PIC  X(07).
               03  TDNW-02B           PIC  X(02).
               03  TDNW-021           PIC  X(233).
               03  TDNW-022           PIC  9(01).
       01  TDNN-TMP.
           02  TDNN-01                PIC  9(02).
           02  TDNN-02.
               03  TDNN-02A           PIC  X(11).
               03  TDNN-020           PIC  X(07).
               03  TDNN-02B           PIC  X(02).
               03  TDNN-021           PIC  X(233).
               03  TDNN-022           PIC  9(01).
       01  TDI-TMP.
           02  TDI-01                 PIC  9(02).
           02  TDI-02.
               03  TDI-020            PIC  X(07).
               03  TDI-021            PIC  X(163).
               03  TDI-022            PIC  X(84).
       01  TDNA-TMP.
           02  TDNA-01                PIC  9(02).
           02  TDNA-02.
               03  TDNA-02A           PIC  X(07).
               03  TDNA-020           PIC  X(07).
               03  TDNA-02B           PIC  X(02).
               03  TDNA-021           PIC  X(218).
               03  TDNA-022           PIC  X(01).
               03  TDNA-023           PIC  9(08).
               03  TDNA-024           PIC  9(01).
               03  TDNA-025           PIC  X(08).
               03  TDNA-026           PIC  9(01).
               03  TDNA-027           PIC  9(01).
       01  CON-TMP.
           02  CON-01                 PIC  9(02).
           02  CON-02.
               03  CON-021            PIC  X(02).
               03  CON-022            PIC  X(30).
           02  CON-03                 PIC  X(222).
       01  TCM-TMP.
           02  TCM-01                 PIC  9(02).
           02  TCM-02.
               03  TCM-021            PIC  X(07).
               03  TCM-022            PIC  X(185).
           02  TCM-ACT                PIC  9(01).
           02  TCM-03                 PIC  X(61).
       01  HIM-TMP.
           02  HIM-01                 PIC  9(02).
           02  HIM-02.
               03  HIM-021            PIC  X(06).
               03  HIM-022            PIC  X(141).
           02  HIM-ACT                PIC  9(01).
           02  HIM-SCC                PIC  9(001).
           02  HIM-BMC                PIC  9(002).
           02  HIM-BMNO               PIC  9(001).
           02  HIM-YG                 PIC  9(005).
           02  HIM-HKB                PIC  9(001).
           02  HIM-HPV                PIC  9(001).
           02  HIM-BC4                PIC  9(001).
           02  F                      PIC  X(011).
           02  HIM-SMS                PIC  N(016).
           02  HIM-UNG                PIC  9(006).
           02  HIM-NNG                PIC  9(006).
           02  F                      PIC  X(001).
           02  HIM-CS                 PIC  N(010).
           02  F                      PIC  X(003).
           02  HIM-DNG                PIC  9(006).
           02  HIM-SNG                PIC  9(004).
           02  HIM-ENG                PIC  9(004).
           02  F                      PIC  X(001).
       01  WTN-TMP.
           02  WTN-01                 PIC  9(02).
           02  WTN-02.
               03  WTN-021            PIC  X(03).
               03  WTN-022            PIC  X(61).
           02  WTN-ACT                PIC  9(01).
           02  WTN-03                 PIC  X(189).
       01  DEFINE-WORK.
           02  FIL-DF                 PIC  X(10)  OCCURS  4.
           02  M-DF                   PIC  X(01)  OCCURS  4.
       01  KEY-WORK.
           02  KEY-01                 PIC  9(02).
           02  KEY-02                 PIC  X(07).
       01  SYS-NAME                   PIC  N(05).
       01  WJOJF-TBL.
           02  WJOJF-TBL1    OCCURS  12.
               03  WJOJF-08           PIC 9(02).
               03  WJOJF-09           PIC 9(06).
               03  WJOJF-10           PIC 9(06).
               03  WJOJF-11           PIC X(10).
               03  WJOJF-12           PIC X(10).
               03  F                  PIC X(06).
           COPY LSTAT.
      *
           COPY  L-JOJF.
           COPY  L-JOSF-TAM.
           COPY  L-JOSR.
           COPY  L-JCON.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-GMN.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-GMN.
           02  FILLER  PIC  N(20)  VALUE
             "ÅñÅñÅ@ÇnÅ^ÇkëóêMÇeê∂ê¨ÅiçƒëóêMópÅjÅ@ÅñÅñ".
           02  FILLER  PIC  X(39)  VALUE
               "ëóêMáÇ =     (ÉIÉìÉâÉCÉìèÛãµñ‚çáÇπÇÊÇË)".
           02  FILLER  PIC  X(22)  VALUE
               "èoâ◊ì˙ ÅÅ   îN  åé  ì˙".
           02  FILLER  PIC  X(14)  VALUE
               "ëóêMêÊ = ( ) :".
           02  FILLER  PIC  X(13)  VALUE
               "ämîF      ( )".
       01  SENTAKU.
           02  ACP-NO    PIC  9(02).
       01  SOUSIN.
           02  ACP-SKB   PIC  9(01).
           02  ACP-SNM   PIC  N(06).
       01  SOUSIN2.
           02  DSP-HIZ.
               03        FILLER  PIC  9(02).
               03        FILLER  PIC  Z9 .
               03        FILLER  PIC  Z9 .
       01  MESSEGE.
           02  DSP-MSG.
               03        FILLER  PIC  N(19)  VALUE
                 "ñ¢ëóêMÉfÅ[É^óLÇËÅAèàóùÇé¿çsÇµÇ‹Ç∑Ç©ÅH".
               03        FILLER  PIC  X(17)  VALUE
                 "YES=1,NO=9--->( )".
           02  ACP-INP   PIC  9(01).
       01  MESSEGE1.
           02  DSP-MSG1.
               03        FILLER  PIC  N(20)  VALUE
                 "ëºÇ≈ÉfÅ[É^ê∂ê¨íÜÇ≈Ç∑ÅBèàóùÇíÜífÇµÇ‹Ç∑ÅB".
       01  MESSEGE2.
           02  DSP-MSG2.
               03        FILLER  PIC  N(12)  VALUE
                 "ëóêMÉfÅ[É^Ç™Ç†ÇËÇ‹ÇπÇÒÅB".
       01  MESSEGE3.
           02  DSP-MSG3.
               03        FILLER  PIC  N(18)  VALUE
                 "ëOâÒÉfÅ[É^ñ¢èàóùÅBèàóùÇíÜífÇµÇ‹Ç∑ÅB".
       01  MESSEGE4.
           02  DSP-MSG4.
               03        FILLER  PIC  N(05)  VALUE
                 "ëqå…ÉGÉâÅ[".
       01  ACP-KAKU.
           02        01ACP-KAKU  PIC  X(01).
       01  DSP-SYSIN.
           02            FILLER  PIC  N(05).
           COPY  LSERR.
      *================================================================*
       PROCEDURE                      DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-GMN
       CALL "SD_Init" USING
           "CLR-GMN" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-GMN" "X" "1" "0" "12" " " "CLR-GMN" RETURNING RESU.
      *DSP-GMN
       CALL "SD_Init" USING 
            "DSP-GMN" " " "0" "0" "128" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GMN" "N" "1" "22" "40" " " "DSP-GMN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GMN" "X" "4" "20" "39" "01DSP-GMN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GMN" "X" "7" "20" "22" "02DSP-GMN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GMN" "X" "10" "20" "14" "03DSP-GMN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GMN" "X" "24" "40" "13" "04DSP-GMN" " "
            RETURNING RESU.
      *SENTAKU
       CALL "SD_Init" USING 
            "SENTAKU" " " "4" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NO" "9" "4" "29" "2" " " "SENTAKU" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NO" BY REFERENCE W-NO "2" "0" RETURNING RESU.
      *SOUSIN
       CALL "SD_Init" USING 
            "SOUSIN" " " "10" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SKB" "9" "10" "30" "1" " " "SOUSIN" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SKB" BY REFERENCE W-SKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNM" "N" "10" "35" "12" "ACP-SKB" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNM" BY REFERENCE W-SNM "12" "0" RETURNING RESU.
      *SOUSIN2
       CALL "SD_Init" USING 
            "SOUSIN2" " " "7" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HIZ" " " "7" "0" "6" " " "SOUSIN2" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HIZ" "9" "7" "30" "2" " " "DSP-HIZ" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-HIZ" BY REFERENCE YY1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-HIZ" "Z9" "7" "34" "2" "01DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-HIZ" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03DSP-HIZ" "Z9" "7" "38" "2" "02DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-HIZ" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
      *MESSEGE
       CALL "SD_Init" USING 
            "MESSEGE" " " "15" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG" " " "15" "0" "55" " " "MESSEGE" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG" "N" "15" "10" "38" " " "DSP-MSG" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MSG" "X" "15" "49" "17" "01DSP-MSG" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-INP" "9" "15" "64" "1" "DSP-MSG" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-INP" BY REFERENCE INP-W "1" "0" RETURNING RESU.
      *MESSEGE1
       CALL "SD_Init" USING 
            "MESSEGE1" " " "15" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG1" " " "15" "0" "40" " " "MESSEGE1" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG1" "N" "15" "10" "40" " " "DSP-MSG1"
            RETURNING RESU.
      *MESSEGE2
       CALL "SD_Init" USING 
            "MESSEGE2" " " "15" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG2" " " "15" "0" "24" " " "MESSEGE2" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG2" "N" "15" "10" "24" " " "DSP-MSG2"
            RETURNING RESU.
      *MESSEGE3
       CALL "SD_Init" USING 
            "MESSEGE3" " " "15" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG3" " " "15" "0" "36" " " "MESSEGE3" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG3" "N" "15" "10" "36" " " "DSP-MSG3"
            RETURNING RESU.
      *MESSEGE4
       CALL "SD_Init" USING 
            "MESSEGE4" " " "15" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG4" " " "15" "0" "10" " " "MESSEGE4" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG4" "N" "15" "10" "10" " " "DSP-MSG4"
            RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "X" "24" "51" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-SYSIN
       CALL "SD_Init" USING 
            "DSP-SYSIN" " " "1" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SYSIN" "N" "1" "1" "10" " " "DSP-SYSIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-SYSIN" BY REFERENCE SYS-NAME "10" "0" RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS     =  PF9
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       MAINLINE-END.
           CALL "DB_Close".
           STOP  RUN.
      *
       PROC-RTN.
           PERFORM  JOLSR-RTN THRU  JOLSR-RTN-EXIT.
           PERFORM  JOJF-RTN  THRU  JOJF-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *
      *================================================================*
      *    èâä˙ê›íËèàóù                                                *
      *================================================================*
       INIT-RTN.
           ACCEPT  W-JS FROM ARGUMENT-VALUE.
           MOVE  SPACE     TO  KEY-WORK
                               CON-TMP   TCM-TMP  HIM-TMP  WTN-TMP
                               STR-TMP   NIF-TMP  OKJ-TMP
                               TDNW-TMP  TDNN-TMP  TDI-TMP  TDNA-TMP.
           MOVE  ZERO      TO  ERR-SW ERR-CD.
           MOVE  10        TO  ERR-LIN.
           CALL "SD_Arg_Match_Line" USING
            "ERR-LIN" "2" ERR-LIN RETURNING RESU.
           MOVE  "JOJF"    TO  FIL-DF(1)  FIL-DF(2) FIL-DF(3).
           MOVE  "JOLSF"   TO  FIL-DF(4).
           MOVE  "W"       TO  M-DF(1)  M-DF(4).
           MOVE  "R"       TO  M-DF(2).
           MOVE  "A"       TO  M-DF(3).
       INIT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÇoÇdÇmèàóù                                                *
      *================================================================*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JOLSR_PNAME1 " " BY REFERENCE JOLSR_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
       OPEN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëIëèàóùÅ@Å@Å@Å@               (SLCT-RTN)                   *
      *================================================================*
       SLCT-RTN.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-GMN" DSP-GMN "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
      *           READ  JOLSF    AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSF_PNAME1 BY REFERENCE JOLSF-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
               GO  TO  SLCT-010
           END-IF
           CALL "SD_Output" USING
            "DSP-MSG3" DSP-MSG3 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  200.
           MOVE      PF9      TO    END-STS
           GO   TO   SLCT-RTN-EXIT.
       SLCT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-NO "ACP-NO" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS NOT =  HTB AND SKP
               GO TO  SLCT-010
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           MOVE   0       TO  W-NOD.
           CALL "DB_F_Open" USING
            "INPUT" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
       SLCT-020.
      *           READ  JOJF  NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JOJF_PNAME1 BY REFERENCE JOJF-REC
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO  SLCT-RTN
           END-IF
           IF  JOJF-061    NOT =  1
               GO TO  SLCT-020
           END-IF
           ADD   1      TO  W-NOD.
           IF  W-NOD       NOT =  W-NO
               GO TO  SLCT-020
           END-IF
           MOVE  JOJF-05  TO  W-KBN.
           MOVE  JOJF-TBL TO  WJOJF-TBL.
           MOVE  JOJF-07  TO  W-SKB.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON4-KEY" BY REFERENCE JCON4-KEY.
           MOVE    4          TO  JCON4-01.
           MOVE    W-SKB      TO  JCON4-02.
      *           READ    JCON       UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE          TO JCON4-03
               MOVE  "ëqå…Ç»Çµ"   TO JCON4-03
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           MOVE    JCON4-03   TO  W-SNM.
           CALL "SD_Output" USING "ACP-SKB" ACP-SKB "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-SNM" ACP-SNM "p" RETURNING RESU.
           IF  W-SKB    NOT =  W-JS
               CALL "SD_Output" USING
                "DSP-MSG4" DSP-MSG4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO  SLCT-010
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSR_IDLST JOLSR_PNAME1.
       SLCT-030.
      *           READ  JOLSR   AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSR_PNAME1 BY REFERENCE JOLSR-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO  SLCT-RTN
           END-IF
           IF  JOLSR1-NO   <  W-NO
               GO TO  SLCT-030
           END-IF
           IF  JOLSR1-NO   >  W-NO
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO  SLCT-RTN
           END-IF
           IF  JOLSR1-01   NOT =  11  AND  12  AND  13
               GO TO  SLCT-RTN
           END-IF
           IF  JOLSR1-01   =  11
               MOVE  JOLSR11-05      TO  W-DATE
               MOVE  W-NGPS          TO  HIZ1-W
               IF  JOLSR11-02      >   099999
                   MOVE  1             TO  JS-SIGN
               ELSE
                   MOVE  0             TO  JS-SIGN
               END-IF
           END-IF
           IF  JOLSR1-01   =  12
               MOVE  JOLSR121-04     TO  HIZ1-W
               IF  JOLSR121-01      >   099999
                   MOVE  1             TO  JS-SIGN
               ELSE
                   MOVE  0             TO  JS-SIGN
               END-IF
           END-IF
           IF  JOLSR1-01   =  13
               MOVE  JOLSR13-04(1)   TO  HIZ1-W
               IF  JOLSR13-02(1)    >   099999
                   MOVE  1             TO  JS-SIGN
               ELSE
                   MOVE  0             TO  JS-SIGN
               END-IF
           END-IF
           IF  JS-SIGN  =  0
               MOVE  "Åmã≥Å@àÁÅn"  TO  SYS-NAME
           END-IF
           IF  JS-SIGN  =  1
               MOVE  "ÅmàÍÅ@î Ån"  TO  SYS-NAME
           END-IF
           CALL "SD_Output" USING
            "DSP-SYSIN" DSP-SYSIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS     =  BTB
               GO TO  SLCT-010
           END-IF
           IF  END-STS NOT =  HTB AND SKP
               GO TO  SLCT-050
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
           ACCEPT  TIM1-W  FROM  TIME.
           MOVE  0001      TO  JOJF-01.
      *----/ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇdÇ`Çc/----*
      *           READ  JOJF  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO  SLCT-100
           END-IF
      *
           IF  JOJF-061  =   9
               CALL "SD_Output" USING
                "DSP-MSG1" DSP-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE      PF9      TO    END-STS
               GO   TO   SLCT-RTN-EXIT
           END-IF
      *
           MOVE  1         TO  NN.
       SLCT-070.
           IF  JOJF-08(NN) NOT =  ZERO
               GO TO  SLCT-080
           END-IF
           ADD   1         TO  NN.
           IF  NN      NOT >   12
               GO TO  SLCT-070
           END-IF
           GO TO  SLCT-RTN-EXIT.
       SLCT-080.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
       SLCT-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-INP "ACP-INP" "9" "1"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS NOT =  HTB
               GO TO  SLCT-090
           END-IF
           IF  INP-W   NOT =  1  AND  INP-W   NOT =  9
               GO TO  SLCT-090
           END-IF
           IF  INP-W       =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  PF9  TO  END-STS
               GO TO  SLCT-RTN-EXIT
           END-IF
           MOVE  JOJF-90   TO  NXT-NO.
           MOVE  SPACE     TO  JOJF-REC.
           MOVE  0001      TO  JOJF-01.
           MOVE  NXT-NO    TO  JOJF-90.
           MOVE  9         TO  JOJF-061.
      *----/ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇdÇvÇqÇhÇsÇd/----*
      *           REWRITE  JOJF-REC  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  02  TO  ERR-CD
               MOVE  1   TO  ERR-SW
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  SLCT-RTN-EXIT.
       SLCT-100.
           INITIALIZE          JOJF-REC.
           MOVE  0001      TO  JOJF-01.
           MOVE  100       TO  JOJF-90.
           MOVE  9         TO  JOJF-061.
      *----/ÇnÅ^ÇkèÛãµÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE    JOJF-REC  INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  01  TO  ERR-CD
               MOVE  1   TO  ERR-SW
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      *================================================================*
      *====       ÇnÅ^ÇkëóêMÇeçÏê¨          (JOLSR-RTN)             ===*
      *================================================================*
       JOLSR-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSR_IDLST JOLSR_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JOLSR_PNAME1 " " BY REFERENCE JOLSR_IDLST "0".
       JOLSR-010.
      *           READ  JOLSR   AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSR_PNAME1 BY REFERENCE JOLSR-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO  JOLSR-RTN-EXIT
           END-IF
           IF  JOLSR1-NO   <  W-NO
               GO TO  JOLSR-010
           END-IF
           IF  JOLSR1-NO   >  W-NO
               GO TO  JOLSR-RTN-EXIT
           END-IF
           MOVE  JOLSR1-REC  TO  JOLSF1-REC.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF1-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF1-REC RETURNING RET.
           IF  ERR-STAT    =  "00"
               GO TO  JOLSR-010
           END-IF
           MOVE  JOLSR1-01  TO  KEY-01.
           MOVE  04         TO  ERR-CD.
           IF  JOLSR1-01   =  01
               MOVE  JOLSR1-REC   TO  CON-TMP
               MOVE  CON-021      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  02
               MOVE  JOLSR2-REC   TO  TCM-TMP
               MOVE  TCM-021      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  03
               MOVE  JOLSR3-REC   TO  HIM-TMP
               MOVE  HIM-021      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  04
               MOVE  JOLSR4-REC   TO  WTN-TMP
               MOVE  WTN-021      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  11
               MOVE  JOLSR11-REC  TO  STR-TMP
               MOVE  STR-020      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  12
               MOVE  JOLSR12-REC  TO  NIF-TMP
               MOVE  NIF-021(1)   TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  13
               MOVE  JOLSR13-REC  TO  OKJ-TMP
               MOVE  OKJ-021(1)   TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  14
               MOVE  JOLSR14-REC  TO  TDNW-TMP
               MOVE  TDNW-020     TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  15
               MOVE  JOLSR15-REC  TO  TDNN-TMP
               MOVE  TDNN-020     TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  16
               MOVE  JOLSR16-REC  TO  TDI-TMP
               MOVE  TDI-020      TO  KEY-02
           END-IF
           IF  JOLSR1-01   =  17
               MOVE  JOLSR17-REC  TO  TDNA-TMP
               MOVE  TDNA-020     TO  KEY-02
           END-IF
           PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT.
           CALL "DB_Close".
           STOP  RUN.
       JOLSR-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÅ^ÇkèÛãµÇeèàóù               (JOJF-RTN)                   *
      *================================================================*
       JOJF-RTN.
           MOVE  0001      TO  JOJF-01.
      *----/ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇdÇ`Çc/----*
      *           READ  JOJF  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  03  TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           ACCEPT  HIZ2-W  FROM  DATE.
           ACCEPT  TIM2-W  FROM  TIME.
           MOVE  MM2-W     TO  JOJF-021.
           MOVE  DD2-W     TO  JOJF-022.
           MOVE  JI1-W     TO  JOJF-031.
           MOVE  FU1-W     TO  JOJF-032.
           MOVE  JI2-W     TO  JOJF-041.
           MOVE  FU2-W     TO  JOJF-042.
           MOVE  W-KBN     TO  JOJF-05.
           MOVE  ZERO      TO  JOJF-063.
           MOVE  SPACE     TO  JOJF-062.
           MOVE  W-SKB     TO  JOJF-07.
           MOVE  WJOJF-TBL TO  JOJF-TBL.
      *----/ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇdÇvÇqÇhÇsÇd/----*
      *           REWRITE  JOJF-REC  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  01  TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       JOJF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇdÇqÇqÇnÇqèàóù                 (ESUB-RTN)                   *
      *================================================================*
       ESUB-RTN.
           MOVE  FIL-DF(ERR-CD)  TO  ERR-F.
           MOVE    M-DF(ERR-CD)  TO  ERR-M.
           IF  ERR-CD      = 4
               MOVE  KEY-WORK  TO  ERR-K
           ELSE
               MOVE  "0001"    TO  ERR-K
           END-IF
           PERFORM  ERR-RTN  THRU  ERR-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           IF  ERR-SW    =  1
               GO TO  ESUB-RTN-EXIT
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       ESUB-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇbÇkÇnÇrÇdèàóù                                              *
      *================================================================*
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSR_IDLST JOLSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      *================================================================
           COPY  LPERR.
