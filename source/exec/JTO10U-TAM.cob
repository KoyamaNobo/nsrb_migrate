       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JTO10U.
      *================================================================*
      *                  Å@    ÇnÅ^ÇkëóêMÇeê∂ê¨ÉvÉçÉOÉâÉÄ áT           *
      *                       ÇWÇXÅ^Å@ÇWÅ^ÇPÇU   ÇaÇxÅ@ÇrÅDÇdÇáÇÅÇóÇÅ  *
      *    JS-SK     : 1=ÇªÇÃëº , 2=ì°ìcÅEóºîı , 3=ì°ìcÅEóºîı(NEW)     *
      *    JS-SIGN   : 0=ã≥àÁ , 1=ÉèÅ[ÉNÅEÉJÉWÉÖÉAÉã                   *
      *================================================================*
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       INPUT-OUTPUT                   SECTION.
       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
           COPY  LWMSG.
      *
       77  JS-SIGN                    PIC  9(01).
       77  JS-SK                      PIC  9(01).
       77  W-BUMON                    PIC  9(01).
       01  WORK-AREA.
           02  II                     PIC  9(02).
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
               03  KAKU-W             PIC  X(01).
               03  STS-W              PIC  9(01).
               03  ERR-CD             PIC  9(01).
               03  ERR-SW             PIC  9(01).
               03  ERR-STAT           PIC  X(02).
               03  DEN-CP             PIC  9(06).
               03  NXT-NO             PIC  9(04).
               03  NN                 PIC  9(02).
               03  NA                 PIC  9(02).
               03  W-SKB              PIC  9(01).
               03  W-SNM              PIC  N(06).
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
               03  TDI-02A            PIC  X(07).
               03  TDI-020           PIC   X(245).
               03  TDI-021           PIC   X(01).
               03  TDI-022           PIC   X(01).
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
       01  KEN-WORK.
           02  KEN-W                  OCCURS  7.
               03  FST-WK.
                   04  FST-W1         PIC  9(06).
                   04  FST-W2         PIC  9(01).
               03  LST-WK.
                   04  LST-W1         PIC  9(06).
                   04  LST-W2         PIC  9(01).
               03  CNT-W              PIC  9(05).
       01  DEFINE-WORK.
           02  FIL-DF                 PIC  X(10)  OCCURS  4.
           02  M-DF                   PIC  X(01)  OCCURS  4.
           02  KBN-DF                 PIC  9(02)  OCCURS  7.
           02  HTAB                   PIC  X(02)  VALUE  "01".
           02  BSKIP                  PIC  X(02)  VALUE  "09".
           02  PF6                    PIC  X(02)  VALUE  "P6".
           02  PF9                    PIC  X(02)  VALUE  "P9".
       01  KEY-WORK.
           02  KEY-01                 PIC  9(02).
           02  KEY-02                 PIC  9(07).
      *
           COPY  L-JSTR.
           COPY  L-JNIF.
           COPY  L-JOJF.
           COPY  L-JOSF-TAM.
           COPY  L-JOS2.
           COPY  LOKJF.
           COPY  LITDNW.
           COPY  LITDNN.
           COPY  L-TDIF.
           COPY  LITDNA.
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
           02  FILLER  PIC  N(15)  VALUE
             "ÅñÅñÅ@ÇnÅ^ÇkëóêMÇeê∂ê¨áTÅ@ÅñÅñ".
           02  FILLER.
               03  FILLER  PIC  X(11)  VALUE
                   "ã≥Å@àÁ=0 , ".
               03  FILLER  PIC  X(14)  VALUE
                   "àÍÅ@î =1 ...  ".
           02  FILLER  PIC  X(22)  VALUE
               "èoâ◊ì˙ ÅÅ   îN  åé  ì˙".
           02  FILLER  PIC  X(14)  VALUE
               "ëóêMêÊ = ( ) :".
           02  FILLER  PIC  X(13)  VALUE
               "ämîF      ( )".
       01  GMN-SIGN.
           02  ACP-SIGN  PIC  9(01).
       01  HIZUKE.
           02  ACP-YY1   PIC  9(02).
           02  ACP-MM1   PIC  9(02).
           02  ACP-DD1   PIC  9(02).
       01  SOUSIN.
           02  ACP-SKB   PIC  9(01).
           02  ACP-SNM   PIC  N(06).
       01  SOUSIN2.
           02  DSP-HIZ.
               03        FILLER  PIC  9(02).
               03        FILLER  PIC  Z9 .
               03        FILLER  PIC  Z9 .
           02  CLR-HIZ.
               03        FILLER  PIC  X(02)  VALUE  " ".
               03        FILLER  PIC  X(02)  VALUE  " ".
               03        FILLER  PIC  X(02)  VALUE  " ".
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
               03        FILLER  PIC  N(18)  VALUE
                 "ÇiÇqÇbÇnÇcÇdÉGÉâÅ[èàóùÇíÜífÇµÇ‹Ç∑ÅB".
       01  ACP-KAKU.
           02        01ACP-KAKU  PIC  X(01).
           COPY  LSERR.
      *================================================================*
       PROCEDURE                      DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-GMN
       CALL "SD_Init" USING
           "CLR-GMN" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-GMN" "X" "1" "0" "12" " " "CLR-GMN" RETURNING RESU.
      *DSP-GMN
       CALL "SD_Init" USING 
            "DSP-GMN" " " "0" "0" "104" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GMN" "N" "1" "22" "30" " " "DSP-GMN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GMN" " " "4" "0" "25" "01DSP-GMN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-GMN" "X" "4" "19" "11" " " "02DSP-GMN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-GMN" "X" "4" "30" "14" "0102DSP-GMN" " "
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
      *GMN-SIGN
       CALL "SD_Init" USING 
            "GMN-SIGN" " " "4" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SIGN" "9" "4" "43" "1" " " "GMN-SIGN" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
      *HIZUKE
       CALL "SD_Init" USING 
            "HIZUKE" " " "7" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-YY1" "9" "7" "30" "2" " " "HIZUKE" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-YY1" BY REFERENCE YY1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-MM1" "9" "7" "34" "2" "ACP-YY1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-MM1" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DD1" "9" "7" "38" "2" "ACP-MM1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-DD1" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
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
            "SOUSIN2" " " "7" "0" "12" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "CLR-HIZ" " " "7" "0" "6" "DSP-HIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-HIZ" "X" "7" "30" "2" " " "CLR-HIZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-HIZ" "X" "7" "34" "2" "01CLR-HIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-HIZ" "X" "7" "38" "2" "02CLR-HIZ" " " RETURNING RESU.
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
            "MESSEGE4" " " "15" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG4" " " "15" "0" "36" " " "MESSEGE4" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG4" "N" "15" "10" "36" " " "DSP-MSG4"
            RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "X" "24" "51" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS     =  PF9
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT.
           PERFORM  ENDR-RTN  THRU  ENDR-RTN-EXIT.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       MAINLINE-END.
           CALL "DB_Close".
           STOP  RUN.
      *
       PROC-RTN.
           PERFORM  JSTR-RTN  THRU  JSTR-RTN-EXIT.
           PERFORM  JNIF-RTN  THRU  JNIF-RTN-EXIT.
           PERFORM  OKJF-RTN  THRU  OKJF-RTN-EXIT.
           IF  JS-SK = 3
               IF  W-SKB       =  3
                   PERFORM  TDNA-RTN  THRU  TDNA-RTN-EXIT
               END-IF
           END-IF
           IF  W-SKB       =  1
               PERFORM  TDNW-RTN  THRU  TDNW-RTN-EXIT
               PERFORM  TDNN-RTN  THRU  TDNN-RTN-EXIT
               PERFORM  TDI-RTN   THRU  TDI-RTN-EXIT
           END-IF
           PERFORM  JOJF-RTN  THRU  JOJF-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *
      *================================================================*
      *    èâä˙ê›íËèàóù                                                *
      *================================================================*
       INIT-RTN.
           ACCEPT  JS-SK FROM ARGUMENT-VALUE.
           IF  JS-SK = 3
               ACCEPT  JS-SIGN FROM ARGUMENT-VALUE
           END-IF
           MOVE  SPACE     TO  KEY-WORK
                               STR-TMP   NIF-TMP  OKJ-TMP
                               TDNW-TMP  TDNN-TMP TDI-TMP   TDNA-TMP.
           MOVE  ZERO      TO  KEN-WORK.
           MOVE  ZERO      TO  ERR-SW ERR-CD.
           MOVE  10        TO  ERR-LIN.
           CALL "SD_Arg_Match_Line" USING
            "ERR-LIN" "2" ERR-LIN RETURNING RESU.
           MOVE  "JOJF"    TO  FIL-DF(1)  FIL-DF(2) FIL-DF(3).
           IF  JS-SK = 1 OR 3
               MOVE  "JOLSF"   TO  FIL-DF(4)
           ELSE
               MOVE  "JOLSF2"  TO  FIL-DF(4)
           END-IF
           MOVE  "W"       TO  M-DF(1)  M-DF(4).
           MOVE  "R"       TO  M-DF(2).
           MOVE  "A"       TO  M-DF(3).
           MOVE  11        TO  KBN-DF(1).
           MOVE  12        TO  KBN-DF(2).
           MOVE  13        TO  KBN-DF(3).
           MOVE  14        TO  KBN-DF(4).
           MOVE  15        TO  KBN-DF(5).
           MOVE  16        TO  KBN-DF(6).
           MOVE  17        TO  KBN-DF(7).
       INIT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÇoÇdÇmèàóù                                                *
      *================================================================*
       OPEN-RTN.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JSTR_PNAME1 
            "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" JNIF_PNAME1 
           "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" OKJF_PNAME1 
            "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING "INPUT" JCON_PNAME1 
            "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON4-KEY" BY REFERENCE JCON4-KEY.
           IF  JS-SK = 1 OR 3
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
           ELSE
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF2_PNAME1 " " BY REFERENCE JOLSF2_IDLST "0"
           END-IF
           IF  W-SKB       =  1
               CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDNWF_PNAME1 
                "SHARED" BY REFERENCE TDNWF_IDLST
                "1" "TDNW1-KEY" BY REFERENCE TDNW1-KEY
               CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDNNF_PNAME1 
                "SHARED" BY REFERENCE TDNNF_IDLST
                "1" "TDNN1-KEY" BY REFERENCE TDNN1-KEY
               CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TDIF_PNAME1 
                "SHARED" BY REFERENCE TDIF_IDLST "1"
                "TDI-KEY" BY REFERENCE TDI-KEY
           ELSE
               IF  (W-SKB       =  3)  AND  (JS-SK       =  3)
                  CALL "DB_F_Open" USING
                   "INPUT SEQUENTIAL" TDNAF_PNAME1 "SHARED" BY REFERENCE
                   TDNAF_IDLST "1" "TDNA-KEY" BY REFERENCE TDNA-KEY
               END-IF
           END-IF.
       OPEN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëIëèàóùÅ@Å@Å@Å@               (SLCT-RTN)                   *
      *================================================================*
       SLCT-RTN.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-GMN" DSP-GMN "p" RETURNING RESU.
           IF  JS-SK   NOT =  3
               GO  TO  SLCT-005
           END-IF
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
               CALL "SD_Output" USING
                "ACP-SIGN" ACP-SIGN "p" RETURNING RESU
               GO  TO  SLCT-010
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "SD_Output" USING
            "DSP-MSG3" DSP-MSG3 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  200.
           MOVE      PF9      TO    END-STS
           GO   TO   SLCT-RTN-EXIT.
       SLCT-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-SIGN "ACP-SIGN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-005
           END-IF
           IF  JS-SIGN NOT =  0  AND  1
               GO TO  SLCT-005
           END-IF.
       SLCT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-YY1 "ACP-YY1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS     =  BSKIP
               IF  JS-SK   NOT =  3
                   GO TO  SLCT-005
               END-IF
           END-IF.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-MM1 "ACP-MM1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  BSKIP
               GO TO  SLCT-010
           END-IF
           IF  MM1-W       =  ZERO
               IF  YY1-W       =  ZERO
                   GO TO  SLCT-030
               END-IF
           END-IF
           IF  MM1-W  <  1  OR  MM1-W  >  12
               GO TO  SLCT-020
           END-IF.
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-DD1 "ACP-DD1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  BSKIP
               GO TO  SLCT-020
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-030
           END-IF
           IF  DD1-W       =  ZERO
               IF  MM1-W       =  ZERO
                   ACCEPT  HIZ1-W  FROM  DATE
                   GO TO  SLCT-035
               END-IF
           END-IF
           IF  DD1-W  <  1  OR  DD1-W  >  31
               GO TO  SLCT-030
           END-IF.
       SLCT-035.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
           IF  JS-SK   NOT =  3
               GO TO  SLCT-040
           END-IF
           IF  COMPLETION_CODE  =  010
               MOVE    3          TO  W-SKB
           ELSE
               IF  COMPLETION_CODE  =  000
                   MOVE    1          TO  W-SKB
               ELSE
                   CALL "SD_Output" USING
                    "DSP-MSG4" DSP-MSG4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  200
                   MOVE      PF9      TO    END-STS
                   GO   TO   SLCT-RTN-EXIT
               END-IF
           END-IF
           CALL "SD_Output" USING "ACP-SKB" ACP-SKB "p" RETURNING RESU.
           GO TO  SLCT-045.
       SLCT-040.
           CALL "SD_Accept" USING BY REFERENCE ACP-SKB "ACP-SKB" "9" "1"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  BSKIP
               GO TO  SLCT-010
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-040
           END-IF
           IF  W-SKB  =  0
               GO TO  SLCT-040
           END-IF.
       SLCT-045.
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
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               GO  TO  SLCT-040
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           MOVE    JCON4-03   TO  W-SNM.
           CALL "SD_Output" USING "ACP-SNM" ACP-SNM "p" RETURNING RESU.
           IF  JS-SK  =  2
               IF  W-SKB  NOT =  1 AND 3
                   GO TO  SLCT-040
               END-IF
           END-IF.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS     =  BSKIP
               IF  JS-SK       =  3
                   GO TO  SLCT-010
               ELSE
                   GO TO  SLCT-040
               END-IF
           END-IF
           IF  END-STS     =  PF6
               GO TO  SLCT-RTN
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-050
           END-IF.
       SLCT-060.
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
                USER_ID BY REFERENCE COMPLETION_CODE  200
               CALL "DB_F_Close" USING
                BY REFERENCE JOJF_IDLST JOJF_PNAME1
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
           IF  NN      NOT >   10
               GO TO  SLCT-070
           END-IF
           GO TO  SLCT-RTN-EXIT.
       SLCT-080.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
       SLCT-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-INP "ACP-INP" "9" "1"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-090
           END-IF
           IF  INP-W   NOT =  1  AND  INP-W   NOT =  9
               GO TO  SLCT-090
           END-IF
           IF  INP-W       =  9
               CALL "DB_F_Close" USING
                BY REFERENCE JOJF_IDLST JOJF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
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
      *    èoâ◊éwê}ÉgÉâÉìèàóù             (JSTR-RTN)                   *
      *================================================================*
       JSTR-RTN.
           MOVE  ZERO      TO  CNT-W(1).
           MOVE  KBN-DF(1) TO  STR-01.
       JSTR-010.
      *----/èoâ◊éwê}ÉgÉâÉìÅ@ÇqÇdÇ`Çc/----*
      *           READ  JSTR  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  JSTR-RTN-EXIT
           END-IF
           MOVE  JSTR-16   TO  W-BUMON.
           IF  W-BUMON      =  2
               MOVE  1         TO  W-BUMON
           END-IF
           IF  W-BUMON  NOT =  JS-SIGN
               GO  TO  JSTR-010
           END-IF
      *
           IF  W-SKB         =  1
               IF  JSTR-07   NOT =  6
                   GO  TO  JSTR-010
               END-IF
           END-IF
           IF  W-SKB         =  2
               IF  JSTR-07   NOT =  7
                   GO  TO  JSTR-010
               END-IF
           END-IF
           IF  W-SKB         =  3
               IF  JSTR-07   NOT =  4
                   GO  TO  JSTR-010
               END-IF
           END-IF
           IF  JSTR-158  NOT =  0
               GO  TO  JSTR-010
           END-IF
           IF  JSTR-03   NOT =  0 AND 7
               GO  TO  JSTR-010
           END-IF
           IF  JSTR-04S  NOT =  HIZ1-W
               GO  TO  JSTR-010
           END-IF
           IF  JSTR-05       =  ZERO
               GO  TO  JSTR-010
           END-IF
           IF  JSTR-17   NOT =  9
               GO  TO  JSTR-010
           END-IF
           MOVE  JSTR-R    TO  STR-02.
           MOVE  "1"       TO  STR-021A.
           MOVE  JSTR-158  TO  STR-022.
           MOVE  JSTR-16   TO  STR-023.
           MOVE  JSTR-17   TO  STR-024.
      *
           IF  JS-SK = 1 OR 3
               MOVE  STR-TMP   TO  JOLSF11-REC
               GO  TO  JSTR-W1
           ELSE
               PERFORM J211-SET-RTN THRU J211-SET-EX
               GO  TO  JSTR-W2
           END-IF.
      *
       JSTR-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF11-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF11-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(1)  TO  KEY-01
               MOVE  STR-020    TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO  TO  JSTR-100.
       JSTR-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF211-REC INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF211-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(1)  TO  KEY-01
               MOVE  STR-020    TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       JSTR-100.
           IF  CNT-W(1)    =  ZERO
               MOVE JSTR-01 TO FST-W1(1)
               MOVE JSTR-02 TO FST-W2(1)
           END-IF
           MOVE  JSTR-01   TO  LST-W1(1).
           MOVE  JSTR-02   TO  LST-W2(1).
           ADD   1         TO  CNT-W(1).
           GO TO  JSTR-010.
       JSTR-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÅ^ÇkëóêMÇeÇQèoâ◊éwê}ÉZÉbÉgèàóùÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@*
      *================================================================*
       J211-SET-RTN.
           MOVE SPACE         TO JOLSF211-REC.
           INITIALIZE            JOLSF211-REC.
           MOVE KBN-DF(1)     TO JOLSF211-01.
           MOVE JSTR-01       TO JOLSF211-02.
           MOVE JSTR-02       TO JOLSF211-03.
           MOVE JSTR-03       TO JOLSF211-04.
           MOVE JSTR-04       TO JOLSF211-05.
           MOVE JSTR-05       TO JOLSF211-06.
           MOVE JSTR-06       TO JOLSF211-07.
           MOVE JSTR-07       TO JOLSF211-08.
           MOVE JSTR-08       TO JOLSF211-09.
           MOVE JSTR-09       TO JOLSF211-10.
           MOVE JSTR-10       TO JOLSF211-11.
           PERFORM VARYING II FROM 1 BY 1 UNTIL II > 10
                   MOVE JSTR-1111(II) TO JOLSF211-1211(II)
                   IF  JSTR-1111(II) < ZERO
                       MOVE 1             TO JOLSF211-1211S(II)
                   END-IF
           END-PERFORM.
           MOVE JSTR-112      TO JOLSF211-122.
           IF  JSTR-112 < ZERO
               MOVE 1             TO JOLSF211-122S
           END-IF
           PERFORM VARYING II FROM 1 BY 1 UNTIL II > 10
                   MOVE JSTR-1211(II) TO JOLSF211-1311(II)
                   IF  JSTR-1211(II) < ZERO
                       MOVE 1             TO JOLSF211-1311S(II)
                   END-IF
           END-PERFORM.
           MOVE JSTR-122      TO JOLSF211-132.
           IF  JSTR-122 < ZERO
               MOVE 1             TO JOLSF211-132S
           END-IF
           MOVE JSTR-13       TO JOLSF211-14.
           MOVE JSTR-14       TO JOLSF211-15.
           MOVE JSTR-14A      TO JOLSF211-15A.
           MOVE JSTR-14B      TO JOLSF211-15B.
           MOVE JSTR-14C      TO JOLSF211-15C.
           MOVE JSTR-14D      TO JOLSF211-15D.
           MOVE JSTR-15       TO JOLSF211-16.
           MOVE JSTR-20       TO JOLSF211-20.
           MOVE JSTR-15A      TO JOLSF211-16A.
           IF  JSTR-15A < ZERO
               MOVE 1             TO JOLSF211-16AS
           END-IF
           MOVE "1"           TO JOLSF211-19.
           MOVE JSTR-158      TO JOLSF211-168.
           MOVE JSTR-16       TO JOLSF211-17.
           MOVE JSTR-17       TO JOLSF211-18.
       J211-SET-EX.
           EXIT.
      *================================================================*
      *    â◊éDÉgÉâÉìèàóù                 (JNIF-RTN)                   *
      *================================================================*
       JNIF-RTN.
           MOVE  ZERO      TO  CNT-W(2).
           MOVE  1         TO  STS-W.
       JNIF-010.
      *----/â◊éDÉgÉâÉìÅ@ÇqÇdÇ`Çc/----*
      *           READ  JNIF  UNLOCK  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  JNIF-050
           END-IF.
       JNIF-020.
           MOVE  JNIF1-01  TO  DEN-CP.
           IF  JNIF1-02    =  7
               GO TO  JNIF-010
           END-IF
           MOVE  JNIF1-13A TO  W-BUMON.
           IF  W-BUMON      =  2
               MOVE  1         TO  W-BUMON
           END-IF
           IF  W-BUMON  NOT =  JS-SIGN
               GO  TO  JNIF-010
           END-IF
      *
           IF  W-SKB         =  1
               IF  JNIF1-07  NOT =  6
                   GO  TO  JNIF-010
               END-IF
           END-IF
           IF  W-SKB         =  2
               IF  JNIF1-07  NOT =  7
                   GO  TO  JNIF-010
               END-IF
           END-IF
           IF  W-SKB         =  3
               IF  JNIF1-07  NOT =  4
                   GO  TO  JNIF-010
               END-IF
           END-IF
           IF  JNIF1-10  NOT =  0
               GO  TO  JNIF-010
           END-IF
           IF  JNIF1-11  NOT =  1
               GO  TO  JNIF-010
           END-IF
           IF  JNIF1-04  NOT =  HIZ1-W
               GO  TO  JNIF-010
           END-IF
           IF  JS-SK = 1 OR 3
               GO  TO  JNIF-030
           END-IF.
       JNIF-025.
           PERFORM J212-SET-RTN THRU J212-SET-EX.
           PERFORM J212-WRI-RTN THRU J212-WRI-EX.
           IF  JNIF1-02     = 7
               GO TO  JNIF-010
           END-IF
      *
      *           READ  JNIF  UNLOCK  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  JNIF-050
           END-IF
           IF  JNIF1-01    =  DEN-CP
               GO TO  JNIF-025
           END-IF
           GO TO  JNIF-020.
       JNIF-030.
           PERFORM  WNIF-RTN  THRU  WNIF-RTN-EXIT.
      *----/â◊éDÉgÉâÉìÅ@ÇqÇdÇ`Çc/----*
      *           READ  JNIF  UNLOCK  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JNIF_PNAME1 BY REFERENCE JNIF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  JNIF-050
           END-IF
           IF  JNIF1-01    =  DEN-CP
               GO TO  JNIF-030
           END-IF
           GO TO  JNIF-020.
       JNIF-050.
           IF  STS-W       =  1
               GO TO  JNIF-RTN-EXIT
           END-IF
      *
           MOVE  NIF-TMP   TO  JOLSF12-REC.
      *
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF12-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF12-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(2)  TO  KEY-01
               MOVE  NIF-021(1) TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       JNIF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÅ^ÇkëóêMÇeÇQâ◊éDÉZÉbÉgèàóù    Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@*
      *================================================================*
       J212-SET-RTN.
           MOVE KBN-DF(2)     TO JOLSF212-01.
           IF  JNIF1-02 = 7
               MOVE SPACE         TO JOLSF2122-A
               INITIALIZE            JOLSF2122-A
               MOVE JNIF2-01      TO JOLSF2122-01
               MOVE JNIF2-02      TO JOLSF2122-02
               MOVE JNIF2-02A     TO JOLSF2122-02A
               MOVE JNIF2-03      TO JOLSF2122-03
               MOVE JNIF2-04      TO JOLSF2122-04
               MOVE JNIF2-05      TO JOLSF2122-05
               MOVE JNIF2-06      TO JOLSF2122-06
               MOVE JNIF2-07      TO JOLSF2122-07
               IF  JNIF2-07 < ZERO
                   MOVE 1             TO JOLSF2122-07S
               END-IF
               MOVE JNIF2-07A     TO JOLSF2122-07A
               MOVE JNIF2-08      TO JOLSF2122-08
           ELSE
               MOVE SPACE         TO JOLSF2121-A
               INITIALIZE            JOLSF2121-A
               MOVE JNIF1-01      TO JOLSF2121-01
               MOVE JNIF1-02      TO JOLSF2121-02
               MOVE JNIF1-03      TO JOLSF2121-03
               MOVE JNIF1-04      TO JOLSF2121-04
               MOVE JNIF1-05      TO JOLSF2121-05
               MOVE JNIF1-06      TO JOLSF2121-06
               MOVE JNIF1-07      TO JOLSF2121-07
               MOVE JNIF1-08      TO JOLSF2121-08
               IF  JNIF1-08 < ZERO
                   MOVE 1             TO JOLSF2121-08S
               END-IF
               PERFORM VARYING II FROM 1 BY 1 UNTIL II > 27
                      MOVE JNIF1-091(II) TO JOLSF2121-091(II)
                      IF  JNIF1-091(II) < ZERO
                          MOVE 1             TO JOLSF2121-091S(II)
                      END-IF
              END-PERFORM
              MOVE JNIF1-10      TO JOLSF2121-10
              MOVE JNIF1-11      TO JOLSF2121-11
              MOVE JNIF1-12      TO JOLSF2121-12
              MOVE JNIF1-13      TO JOLSF2121-13
              IF  JNIF1-13 < ZERO
                  MOVE 1             TO JOLSF2121-13S
              END-IF
              MOVE JNIF1-13A     TO JOLSF2121-13A
              MOVE JNIF1-14      TO JOLSF2121-14
           END-IF.
       J212-SET-EX.
           EXIT.
      *
       J212-WRI-RTN.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF212-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF212-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(2)  TO  KEY-01
               MOVE  NIF-021(1) TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
      *
           ADD   1         TO  CNT-W(2).
           MOVE  JNIF1-01  TO  LST-W1(2).
           MOVE  JNIF1-02  TO  LST-W2(2).
       J212-WRI-EX.
           EXIT.
      *================================================================*
      *    ëóÇËèÛÇeèàóù                   (OKJF-RTN)                   *
      *================================================================*
       OKJF-RTN.
           MOVE  ZERO      TO  CNT-W(3).
           MOVE  1         TO  STS-W.
       OKJF-010.
      *----/ëóÇËèÛÇeÅ@ÇqÇdÇ`Çc/----*
      *           READ  OKJF UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  OKJF-020
           END-IF
           MOVE  OKJF-09   TO  W-BUMON.
           IF  W-BUMON      =  2
               MOVE  1         TO  W-BUMON
           END-IF
           IF  W-BUMON  NOT =  JS-SIGN
               GO  TO  OKJF-010
           END-IF
      *
           IF  W-SKB         =  1
               IF  OKJF-04   NOT =  6
                   GO  TO  OKJF-010
               END-IF
           END-IF
           IF  W-SKB         =  2
               IF  OKJF-04   NOT =  7
                   GO  TO  OKJF-010
               END-IF
           END-IF
           IF  W-SKB         =  3
               IF  OKJF-04   NOT =  4
                   GO  TO  OKJF-010
               END-IF
           END-IF
           IF  OKJF-08   NOT =  0
               GO  TO  OKJF-010
           END-IF
           IF  OKJF-07       =  ZERO
               GO  TO  OKJF-010
           END-IF
           IF  OKJF-10   NOT =  1
               GO  TO  OKJF-010
           END-IF
           IF  OKJF-03   NOT =  HIZ1-W
               GO  TO  OKJF-010
           END-IF
           PERFORM  WOKJ-RTN  THRU  WOKJ-RTN-EXIT.
           GO TO  OKJF-010.
       OKJF-020.
           IF  STS-W       =  1
               GO TO  OKJF-RTN-EXIT
           END-IF
      *
           IF  JS-SK = 1 OR 3
               MOVE  OKJ-TMP   TO  JOLSF13-REC
               GO TO  OKJF-W1
           ELSE
               MOVE  OKJ-TMP   TO  JOLSF213-REC
               GO TO  OKJF-W2
           END-IF.
       OKJF-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF13-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF13-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(3)  TO  KEY-01
               MOVE  OKJ-021(1) TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  OKJF-RTN-EXIT.
       OKJF-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF213-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF213-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(3)  TO  KEY-01
               MOVE  OKJ-021(1) TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       OKJF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÉèÅ[ÉNÉ}Éìì`ï[èàóù           (TDNW-RTN)
      *================================================================*
       TDNW-RTN.
           MOVE  ZERO      TO  CNT-W(4).
           MOVE  KBN-DF(4) TO  TDNW-01.
       TDNW-010.
      *           READ  TDNWF UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  TDNW-RTN-EXIT
           END-IF
           IF  TDNW1-PC  NOT =  1
               GO  TO  TDNW-010
           END-IF
           MOVE  TDNW-R1   TO  TDNW-02.
           MOVE  TDNW1-PC  TO  TDNW-022.
           IF  JS-SK = 1 OR 3
               MOVE  TDNW-TMP  TO  JOLSF14-REC
               GO  TO  TDNW-W1
           ELSE
               MOVE  TDNW-TMP  TO  JOLSF214-REC
               GO  TO  TDNW-W2
           END-IF.
       TDNW-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF14-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF14-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(4)  TO  KEY-01
               MOVE  TDNW-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO  TO  TDNW-100.
       TDNW-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF214-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF214-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(4)  TO  KEY-01
               MOVE  TDNW-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       TDNW-100.
           IF  CNT-W(4)    =  ZERO
               MOVE TDNW-020 TO FST-WK(4)
           END-IF
           MOVE  TDNW-020  TO  LST-WK(4).
           ADD   1         TO  CNT-W(4).
           GO TO  TDNW-010.
       TDNW-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÉiÉtÉRì`ï[èàóù               (TDNN-RTN)
      *================================================================*
       TDNN-RTN.
           MOVE  ZERO      TO  CNT-W(5).
           MOVE  KBN-DF(5) TO  TDNN-01.
       TDNN-010.
      *           READ  TDNNF UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  TDNN-RTN-EXIT
           END-IF
           IF  TDNN1-PC  NOT =  1
               GO  TO  TDNN-010
           END-IF
           MOVE  TDNN-R1   TO  TDNN-02.
           MOVE  TDNN1-PC  TO  TDNN-022.
           IF  JS-SK = 1 OR 3
               MOVE  TDNN-TMP  TO  JOLSF15-REC
               GO TO  TDNN-W1
           ELSE
               MOVE  TDNN-TMP  TO  JOLSF215-REC
               GO TO  TDNN-W2
           END-IF.
       TDNN-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF15-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF15-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(5)  TO  KEY-01
               MOVE  TDNN-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  TDNN-100.
       TDNN-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF215-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF215-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(5)  TO  KEY-01
               MOVE  TDNN-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       TDNN-100.
           IF  CNT-W(5)    =  ZERO
               MOVE TDNN-020 TO FST-WK(5)
           END-IF
           MOVE  TDNN-020  TO  LST-WK(5).
           ADD   1         TO  CNT-W(5).
           GO TO  TDNN-010.
       TDNN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÉgÉâÉXÉRëºì`ï[èàóù         (TDI-RTN)
      *================================================================*
       TDI-RTN.
           MOVE  ZERO      TO  CNT-W(6).
           MOVE  KBN-DF(6) TO  TDI-01.
       TDI-010.
      *           READ  TDIF  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDIF_PNAME1 BY REFERENCE TDI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  TDI-RTN-EXIT
           END-IF
           IF  TDI-UPC   NOT =  1
               GO  TO  TDI-010
           END-IF
           IF  TDI-PRC       =  9
               GO  TO  TDI-010
           END-IF
           IF  TDI-TCD       =  6010
               GO  TO  TDI-010
           END-IF
           IF  W-SKB         =  1
               IF  TDI-SOK   NOT =  6
                   GO  TO  TDI-010
               END-IF
           END-IF
           IF  W-SKB         =  2
               IF  TDI-SOK   NOT =  7
                   GO  TO  TDI-010
               END-IF
           END-IF
           IF  W-SKB         =  3
               IF  TDI-SOK   NOT =  4
                   GO  TO  TDI-010
               END-IF
           END-IF
           IF  TDI-DATE  NOT =  HIZ1-W
               GO  TO  TDI-010
           END-IF
           MOVE  TDI-R     TO  TDI-02.
           MOVE  TDI-PRC   TO  TDI-021.
           MOVE  TDI-UPC   TO  TDI-022.
           IF  JS-SK = 1 OR 3
               MOVE  TDI-TMP   TO  JOLSF16-REC
               GO TO  TDI-W1
           END-IF
           MOVE SPACE         TO JOLSF216-REC.
           INITIALIZE            JOLSF216-REC.
           MOVE KBN-DF(6)     TO JOLSF216-01.
           MOVE TDI-KEY       TO JOLSF216-KEY.
           MOVE TDI-DATE      TO JOLSF216-DATE.
           MOVE TDI-TCD       TO JOLSF216-TCD.
           MOVE TDI-CCD       TO JOLSF216-CCD.
           MOVE TDI-TPC       TO JOLSF216-TPC.
           MOVE TDI-HCD       TO JOLSF216-HCD.
           MOVE TDI-SIZ       TO JOLSF216-SIZ.
           MOVE TDI-SKB       TO JOLSF216-SKB.
           MOVE TDI-SNO       TO JOLSF216-SNO.
           MOVE TDI-SU        TO JOLSF216-SU.
           IF  TDI-SU        <  0
               MOVE 1             TO JOLSF216-SUS
           END-IF
           MOVE TDI-GT        TO JOLSF216-GT.
           MOVE TDI-UT        TO JOLSF216-UT.
           MOVE TDI-GKIN      TO JOLSF216-GKIN.
           IF  TDI-GKIN      <  0
               MOVE 1             TO JOLSF216-GKINS
           END-IF
           MOVE TDI-UKIN      TO JOLSF216-UKIN.
           IF  TDI-UKIN      <  0
               MOVE 1             TO JOLSF216-UKINS
           END-IF
           MOVE TDI-JNOD      TO JOLSF216-JNOD.
           MOVE TDI-SOK       TO JOLSF216-SOK.
           MOVE TDI-UNS       TO JOLSF216-UNS.
           MOVE TDI-ISU       TO JOLSF216-ISU.
           MOVE TDI-HNO       TO JOLSF216-HNO.
           MOVE TDI-TEKI      TO JOLSF216-TEKI.
           MOVE TDI-TRN       TO JOLSF216-TRN.
           MOVE TDI-PRC       TO JOLSF216-PRC.
           MOVE TDI-UPC       TO JOLSF216-UPC.
           GO TO  TDI-W2.
       TDI-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF16-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF16-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(6)  TO  KEY-01
               MOVE  TDI-02A    TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  TDI-100.
       TDI-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF216-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF216-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(6)  TO  KEY-01
               MOVE  TDI-02A    TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       TDI-100.
           IF  CNT-W(6)    =  ZERO
               MOVE TDI-02A  TO FST-WK(6)
           END-IF
           MOVE  TDI-02A   TO  LST-WK(6).
           ADD   1         TO  CNT-W(6).
           GO TO  TDI-010.
       TDI-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ê‘ÇøÇ·ÇÒñ{ï‹ì`ï[èàóù       (TDNA-RTN)
      *================================================================*
       TDNA-RTN.
           MOVE  ZERO      TO  CNT-W(6).
           MOVE  KBN-DF(7) TO  TDNA-01.
       TDNA-010.
      *           READ  TDNAF UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  TDNA-RTN-EXIT
           END-IF
           IF  TDNA-RC       =  1
               GO  TO  TDNA-010
           END-IF
           IF  TDNA-PC       =  9
               GO  TO  TDNA-010
           END-IF
           IF  TDNA-DNGPS NOT =  HIZ1-W
               GO  TO  TDNA-010
           END-IF
           MOVE  TDNA-R    TO  TDNA-02.
           MOVE  SPACE     TO  TDNA-022.
           MOVE  TDNA-DNGP TO  TDNA-023.
           MOVE  TDNA-NRC  TO  TDNA-024.
           MOVE  SPACE     TO  TDNA-025.
           MOVE  TDNA-PC   TO  TDNA-026.
           MOVE  TDNA-RC   TO  TDNA-027.
           IF  JS-SK = 1 OR 3
               MOVE  TDNA-TMP  TO  JOLSF17-REC
               GO TO  TDNA-W1
           END-IF
           MOVE SPACE         TO JOLSF217-REC.
           INITIALIZE            JOLSF217-REC.
           MOVE KBN-DF(7)     TO JOLSF217-01.
           MOVE TDNA-KEY      TO JOLSF217-KEY.
           MOVE TDNA-JAN      TO JOLSF217-JAN.
           MOVE TDNA-SU       TO JOLSF217-SU.
           IF  TDNA-SU       <  0
               MOVE 1             TO JOLSF217-SUS
           END-IF
           MOVE TDNA-GTN      TO JOLSF217-GTN.
           MOVE TDNA-UTN      TO JOLSF217-UTN.
           MOVE TDNA-GKIN     TO JOLSF217-GKIN.
           IF  TDNA-GKIN     <  0
               MOVE 1             TO JOLSF217-GKINS
           END-IF
           MOVE TDNA-UKIN     TO JOLSF217-UKIN.
           IF  TDNA-UKIN     <  0
               MOVE 1             TO JOLSF217-UKINS
           END-IF
           MOVE TDNA-DPM      TO JOLSF217-DPM.
           MOVE TDNA-CLS      TO JOLSF217-CLS.
           MOVE TDNA-SHM      TO JOLSF217-SHM.
           MOVE TDNA-MKH      TO JOLSF217-MKH.
           MOVE TDNA-MSB      TO JOLSF217-MSB.
           MOVE TDNA-TY       TO JOLSF217-TY.
           MOVE TDNA-HCD      TO JOLSF217-HCD.
           MOVE TDNA-COR      TO JOLSF217-COR.
           MOVE TDNA-SIZ      TO JOLSF217-SIZ.
           MOVE TDNA-NSU      TO JOLSF217-NSU.
           MOVE TDNA-TSC      TO JOLSF217-TSC.
           MOVE TDNA-CCD      TO JOLSF217-CCD.
           MOVE TDNA-TNA      TO JOLSF217-TNA.
           MOVE TDNA-HNO      TO JOLSF217-HNO.
           MOVE TDNA-HNGP     TO JOLSF217-HNGP.
           MOVE TDNA-NNGP     TO JOLSF217-NNGP.
           MOVE TDNA-THC      TO JOLSF217-THC.
           MOVE TDNA-BI       TO JOLSF217-BI.
           MOVE TDNA-SNGP     TO JOLSF217-SNGP.
           MOVE TDNA-HNA      TO JOLSF217-HNA.
           MOVE TDNA-ZON      TO JOLSF217-ZON.
           MOVE TDNA-DC       TO JOLSF217-DC.
           MOVE TDNA-DNGP     TO JOLSF217-DNGP.
           MOVE TDNA-NRC      TO JOLSF217-NRC.
           MOVE TDNA-PC       TO JOLSF217-PC.
           MOVE TDNA-RC       TO JOLSF217-RC.
           GO TO  TDNA-W2.
       TDNA-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF17-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF17-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(7)  TO  KEY-01
               MOVE  TDNA-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  TDNA-100.
       TDNA-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF217-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF217-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(7)  TO  KEY-01
               MOVE  TDNA-020   TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       TDNA-100.
           IF  CNT-W(7)    =  ZERO
               MOVE TDNA-020  TO FST-WK(7)
           END-IF
           MOVE  TDNA-020  TO  LST-WK(7).
           ADD   1         TO  CNT-W(7).
           GO TO  TDNA-010.
       TDNA-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÅ^ÇkèÛãµÇeèàóù               (JOJF-RTN)                   *
      *================================================================*
       JOJF-RTN.
           IF  KEN-WORK    =  ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               MOVE  0         TO  JOJF-061
               GO TO  JOJF-090
           END-IF
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
           IF  JS-SK       =  3
               MOVE  3         TO  JOJF-05
           ELSE
               MOVE  1         TO  JOJF-05
           END-IF
           MOVE  ZERO      TO  JOJF-063.
           MOVE  SPACE     TO  JOJF-062.
           MOVE  W-SKB     TO  JOJF-07.
           MOVE  1         TO  NN  NA.
       JOJF-010.
           IF  CNT-W(NN)   =  ZERO
               GO TO  JOJF-020
           END-IF
           MOVE  KBN-DF(NN)  TO  JOJF-08(NA).
           MOVE  CNT-W(NN)   TO  JOJF-09(NA).
           MOVE  ZERO        TO  JOJF-10(NA).
           MOVE  FST-WK(NN)  TO  JOJF-11(NA).
           MOVE  LST-WK(NN)  TO  JOJF-12(NA).
           ADD   1         TO  NA.
       JOJF-020.
           ADD   1         TO  NN.
           IF  NN      NOT >  7
               GO TO  JOJF-010
           END-IF.
       JOJF-090.
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
      *    â◊éDÉgÉâÉìÇvÇqÇhÇsÇdèàóù       (WNIF-RTN)                   *
      *================================================================*
       WNIF-RTN.
           IF  CNT-W(2)    =  ZERO
               MOVE  JNIF1-01  TO  FST-W1(2)
               MOVE  JNIF1-02  TO  FST-W2(2)
           END-IF
           MOVE  JNIF1-R   TO  NIF-02(STS-W).
           ADD    1        TO  STS-W.
           MOVE  KBN-DF(2) TO  NIF-01.
           IF  STS-W   NOT >  2
               GO TO  WNIF-010
           END-IF
      *
           MOVE  NIF-TMP   TO  JOLSF12-REC.
      *
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF12-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF12-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(2)  TO  KEY-01
               MOVE  NIF-021(1) TO  KEY-02
               MOVE  04  TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
      *
           MOVE  1         TO  STS-W.
           MOVE  SPACE     TO  NIF-TMP.
       WNIF-010.
           ADD   1         TO  CNT-W(2).
           MOVE  JNIF1-01  TO  LST-W1(2).
           MOVE  JNIF1-02  TO  LST-W2(2).
       WNIF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëóÇËèÛÇeÇvÇqÇhÇsÇdèàóù         (WOKJ-RTN)                   *
      *================================================================*
       WOKJ-RTN.
           IF  CNT-W(3)    =  ZERO
               MOVE  OKJF-01  TO  FST-W1(3)
           END-IF
           MOVE  OKJF-R    TO  OKJ-02(STS-W).
           ADD   1         TO  STS-W.
           MOVE  KBN-DF(3) TO  OKJ-01.
           IF  STS-W   NOT >  4
               GO TO  WOKJ-010
           END-IF
           IF  JS-SK = 1 OR 3
               MOVE  OKJ-TMP   TO  JOLSF13-REC
               GO TO  WOKJ-W1
           ELSE
               MOVE  OKJ-TMP   TO  JOLSF213-REC
               GO TO  WOKJ-W2
           END-IF.
       WOKJ-W1.
      *----/ÇnÅ^ÇkëóêMÇeÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF13-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSF_PNAME1 JOLSF_LNAME JOLSF13-REC RETURNING RET.
           IF  ERR-STAT    =  "34"
               MOVE  KBN-DF(3)  TO  KEY-01
               MOVE  OKJ-021(1) TO  KEY-02
               MOVE  04  TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO  WOKJ-005.
       WOKJ-W2.
      *----/ÇnÅ^ÇkëóêMÇeÇQÅ@ÇvÇqÇhÇsÇd/----*
      *           WRITE  JOLSF213-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOLSF2_PNAME1 JOLSF2_LNAME JOLSF213-REC RETURNING RET.
           IF  RET = 1
               MOVE  KBN-DF(3)  TO  KEY-01
               MOVE  OKJ-021(1) TO  KEY-02
               MOVE  04  TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WOKJ-005.
           MOVE  1         TO  STS-W.
           MOVE  SPACE     TO  OKJ-TMP.
       WOKJ-010.
           ADD   1         TO  CNT-W(3).
           MOVE  OKJF-01   TO  LST-W1(3).
       WOKJ-RTN-EXIT.
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
               CALL "DB_F_Close" USING
                BY REFERENCE JOJF_IDLST JOJF_PNAME1 
               GO TO  ESUB-RTN-EXIT
           END-IF
           IF  JS-SK = 1 OR 3
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF2_IDLST JOLSF2_PNAME1
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF2_PNAME1 " " BY REFERENCE JOLSF2_IDLST "0"
           END-IF
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.

       ESUB-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇbÇkÇnÇrÇdèàóù                                              *
      *================================================================*
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           IF  JS-SK = 1 OR 3
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF2_IDLST JOLSF2_PNAME1
           END-IF
           IF (JS-SK       =  3)  AND  (W-SKB       =  3)
               CALL "DB_F_Close" USING
                BY REFERENCE TDNAF_IDLST TDNAF_PNAME1
           END-IF
           IF  W-SKB       =  1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TDIF_IDLST TDIF_PNAME1
           END-IF.
       CLSE-RTN-EXIT.
           EXIT.
      *================================================================*
      *    èIóπèàóù                                                    *
      *================================================================*
       ENDR-RTN.
       ENDR-RTN-EXIT.
           EXIT.
      *================================================================
           COPY  LPERR.
