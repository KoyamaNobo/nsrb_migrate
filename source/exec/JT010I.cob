       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            JT010I.
      *********************************************************
      *    PROGRAM         :  éÛíçì¸óÕÅ@Å@Å@Å@Å@              *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  SJ010I                          *
      *    DATE      Å@Å@  :  03/09/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       NEAC-SYSTEM100.
       OBJECT-COMPUTER.       NEAC-SYSTEM100.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
       77  ERR-STAT             PIC  X(02).
       77  WJCON1-03            PIC  9(06).
       77  SU-SW                PIC  9(01).
       77  CHK-SW               PIC  9(01).
       77  UPD-SW               PIC  9(01).
       77  JS-SIGN              PIC 9(01).
       77  W-YDATE              PIC 9(08).
       77  W-EDATE              PIC 9(08).
       77  CHKNO                PIC 9(01).
       77  W-KBN                PIC 9(01).
       77  W-MID                PIC N(06).
       01  GYO.
           02  SIZU  OCCURS  4.
             03  SU  OCCURS  10   PIC  S9(06).
       01  W-DATE.
           02  WK-DATE.
             03  WK-YY          PIC  9(04).
             03  WK-YYL   REDEFINES WK-YY.
               04  WK-YY1       PIC  9(02).
               04  WK-YY2       PIC  9(02).
             03  WK-MM          PIC  9(02).
             03  WK-DD          PIC  9(02).
           02  WK-DATEL REDEFINES WK-DATE.
             03  F              PIC  9(02).
             03  WK-DATES       PIC  9(06).
       01  W-TEKID.
           02  W-HTS            PIC  N(09).
           02  W-HTSD   REDEFINES   W-HTS.
             03  W-TGET         PIC  N(02).
             03  W-TGP          PIC  N(01).
             03  W-TPEY         PIC  N(02).
             03  W-HTSM         PIC  N(04).
       01  W-TEKI.
           02  W-TEKI1          PIC  N(09).
           02  W-TEKI2          PIC  N(19).
           02  W-TEKI3          PIC  N(04).
       01  GMN-ACT              PIC  9(01).
       01  GMN-ANAM             PIC  N(02).
       01  GMN-AREA.
           02  GMN-NO           PIC  9(06).
           02  GMN-JNO          PIC  9(06).
           02  GMN-JNOR   REDEFINES   GMN-JNO.
             03  JNO-1          PIC  9(01).
             03  JNO-2          PIC  9(05).
           02  GMN-HEAD.
             03  GMN-OKC        PIC  9(01).
             03  GMN-SET        PIC S9(03).
             03  GMN-JYMD.
               04  GMN-JYY      PIC  9(04).
               04  GMN-JYYL  REDEFINES GMN-JYY.
                 05  GMN-JYY1   PIC  9(02).
                 05  GMN-JYY2   PIC  9(02).
               04  GMN-JMM      PIC  9(02).
               04  GMN-JDD      PIC  9(02).
             03  GMN-TCCD.
               04  GMN-TCD      PIC  9(04).
               04  GMN-CCD      PIC  9(03).
             03  GMN-TNAM       PIC  N(24).
             03  GMN-CNAM       PIC  N(24).
             03  GMN-TENC       PIC  9(04).
             03  GMN-TENM       PIC  N(11).
           02    GMN-DATA.
             03  GMN-TEKI       PIC  N(32).
             03  GMN-TEKIR  REDEFINES  GMN-TEKI.
               04  GMN-TEKI1    PIC  N(09).
               04  GMN-TEKI2    PIC  N(19).
               04  GMN-TEKI3    PIC  N(04).
             03  GMN-GKEI       PIC S9(07).
             03  GMN-GKIN       PIC S9(08).
             03  GMN-MEISAI   OCCURS  6.
               04  GMN-SIZ      PIC  9(01).
               04  GMN-HCD      PIC  9(06).
               04  GMN-HNAM.
                 05  GMN-HNAM1  PIC  N(12).
                 05  GMN-HNAM2  PIC  N(12).
               04  GMN-NYMD.
                 05  GMN-NYY    PIC  9(04).
                 05  GMN-NYYL  REDEFINES GMN-NYY.
                   06  GMN-NYY1 PIC  9(02).
                   06  GMN-NYY2 PIC  9(02).
                 05  GMN-NMM    PIC  9(02).
                 05  GMN-NDD    PIC  9(02).
               04  GMN-KEI      PIC S9(06).
               04  GMN-SIZE.
                 05  GMN-SURYO  OCCURS  10.
                   06  GMN-SU   PIC S9(06).
               04  GMN-TAN      PIC  9(05).
               04  GMN-KIN      PIC S9(08).
               04  GMN-BIKHO    PIC  X(10).
               04  GMN-BCD1     PIC  9(03).
       01  WK-AREA.
           02  SAVE-AREA.
             03  WS-SEQ         PIC  9(03).
             03  WS-JYMD.
               04  WS-JYY       PIC  9(04).
               04  WS-JYYL  REDEFINES WS-JYY.
                 05  WS-JYY1    PIC  9(02).
                 05  WS-JYY2    PIC  9(02).
               04  WS-JMM       PIC  9(02).
               04  WS-JDD       PIC  9(02).
             03  WS-GKEI        PIC S9(07).
             03  WS-GKIN        PIC S9(08).
             03  WS-SET         PIC S9(03).
             03  WS-KBN         PIC  9(01).
             03  WS-DATA        OCCURS  6.
               04  WS-SIZ       PIC  9(01).
               04  WS-HCD       PIC  9(06).
               04  WS-HNAM.
                 05  WS-HNAM1   PIC  N(12).
                 05  WS-HNAM2   PIC  N(12).
               04  WS-NYMD.
                 05  WS-NYY     PIC  9(04).
                 05  WS-NYYL  REDEFINES WS-NYY.
                   06  WS-NYY1  PIC  9(02).
                   06  WS-NYY2  PIC  9(02).
                 05  WS-NMM     PIC  9(02).
                 05  WS-NDD     PIC  9(02).
               04  WS-KEI       PIC S9(06).
               04  WS-CHECK     PIC  X(02).
               04  WS-SIZE.
                 05  WS-SURYO  OCCURS  10.
                   06  WS-SU    PIC S9(06).
                   06  WS-ZAN   PIC S9(06).
               04  WS-12       OCCURS  10.
                   06  WS-1211  PIC S9(06).
               04  WS-14       OCCURS  10.
                   06  WS-141   PIC S9(06).
               04  WS-15       OCCURS  10.
                   06  WS-151   PIC S9(06).
               04  WS-TAN       PIC  9(05).
               04  WS-KIN       PIC S9(08).
               04  WS-BIKHO     PIC  X(10).
           02  WK-USE.
             03  WK-SU.
               04  WK-SU1       PIC S9(06).
               04  WK-SU2       PIC S9(06).
               04  WK-SU4       PIC S9(06).
               04  WK-SU3       PIC S9(04).
             03  AA             PIC  9(02).
             03  BB             PIC  9(02).
             03  SOEJI-AREA.
               04  XXW          PIC  9(01).
               04  XX           PIC  9(01).
               04  YY           PIC  9(01).
               04  ZZ           PIC  9(02).
               04  WW           PIC  9(01).
               04  I            PIC  9(02).
               04  P            PIC  9(02).
             03  SW-AREA.
               04  SW-INV       PIC  X(02).
               04  SW-ZERO      PIC  X(02).
             03  WK-NAM         PIC  N(24).
             03  WK-NAM-R     REDEFINES      WK-NAM.
               04  WK-NAM1      PIC  N(12).
               04  WK-NAM2      PIC  N(12).
             03  WK-KEY         PIC  X(07).
             03  WK-HCD         PIC  9(06).
             03  WK-TCCD        PIC  9(07).
             03  WK-TENC        PIC  9(04).
             03  WK-SURYO       PIC S9(06).
             03  HINCD.
               04  HIN-01       PIC  9(01).
               04  HIN-02       PIC  9(01).
             03  WK-ANS         PIC  9(04).
             03  WK-AMARI       PIC  9(04).
             03  WK-NMD.
               04  WK-NMM       PIC  Z(02).
               04  WK-NDD       PIC  Z(02).
             03  WK-TAN         PIC  9(05).
       01  REV-AREA.
           02  REV-SW           PIC  X(02).
           02  REV-2.
               03  REV-201          PIC  X(04)  VALUE "12.5".
               03  REV-202          PIC  X(04)  VALUE "13.0".
               03  REV-203          PIC  X(04)  VALUE "13.5".
               03  REV-204          PIC  X(04)  VALUE "14.0".
               03  REV-205          PIC  X(04)  VALUE "15.0".
               03  REV-206          PIC  X(04)  VALUE "16.0".
               03  REV-207          PIC  X(04)  VALUE "17.0".
               03  REV-208          PIC  X(04)  VALUE "18.0".
               03  REV-209          PIC  X(04)  VALUE "19.0".
               03  REV-210          PIC  X(04)  VALUE "20.0".
           02  REV-3.
               03  REV-301          PIC  X(04)  VALUE "21.0".
               03  REV-302          PIC  X(04)  VALUE "21.5".
               03  REV-303          PIC  X(04)  VALUE "22.0".
               03  REV-304          PIC  X(04)  VALUE "22.5".
               03  REV-305          PIC  X(04)  VALUE "23.0".
               03  REV-306          PIC  X(04)  VALUE "23.5".
               03  REV-307          PIC  X(04)  VALUE "24.0".
               03  REV-308          PIC  X(04)  VALUE "24.5".
               03  REV-309          PIC  X(04)  VALUE "25.0".
               03  REV-310          PIC  X(04)  VALUE "    ".
           02  REV-4.
               03  REV-401          PIC  X(04)  VALUE "24.0".
               03  REV-402          PIC  X(04)  VALUE "24.5".
               03  REV-403          PIC  X(04)  VALUE "25.0".
               03  REV-404          PIC  X(04)  VALUE "25.5".
               03  REV-405          PIC  X(04)  VALUE "26.0".
               03  REV-406          PIC  X(04)  VALUE "26.5".
               03  REV-407          PIC  X(04)  VALUE "27.0".
               03  REV-408          PIC  X(04)  VALUE "27.5".
               03  REV-409          PIC  X(04)  VALUE "    ".
               03  REV-410          PIC  X(04)  VALUE "    ".
           02  REV-1.
               03  REV-101          PIC  X(04)  VALUE "    ".
               03  REV-102          PIC  X(04)  VALUE "    ".
               03  REV-103          PIC  X(04)  VALUE " SS ".
               03  REV-104          PIC  X(04)  VALUE "  S ".
               03  REV-105          PIC  X(04)  VALUE "  M ".
               03  REV-106          PIC  X(04)  VALUE "  L ".
               03  REV-107          PIC  X(04)  VALUE " LL ".
               03  REV-108          PIC  X(04)  VALUE "28.0".
               03  REV-109          PIC  X(04)  VALUE "29.0".
               03  REV-110          PIC  X(04)  VALUE "30.0".
           COPY   LWMSG.
      *
           COPY   LIBFDD.
           COPY   LITCM.
           COPY   LIHIM2.
           COPY   LJMSTD.
           COPY   LTDNKN.
           COPY   L-JCON.
           COPY   LJDCON.
           COPY   LITM.
           COPY   LIDTHT.
           COPY   LWTNAF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-MID.
           02  FILLER  PIC  N(18)  VALUE
                "ÅmÅ@Å@éÛíçÅEóaÇËÅEéÊÇËÇÊÇØì¸óÕÅ@Å@Ån".
           02  FILLER.
             03  FILLER  PIC  N(03)  VALUE "ã≥Å@àÁ".
             03  FILLER  PIC  X(05)  VALUE "=0 , ".
             03  FILLER  PIC  N(03)  VALUE "àÍÅ@î ".
             03  FILLER  PIC  X(13)  VALUE "=1 ....  ÿ¿-›".
           02  FILLER  PIC  X(16)  VALUE
                  "éÛíçì¸óÕ     = 0".
           02  FILLER  PIC  X(16)  VALUE
                  "óaÇËì¸óÕ     = 5".
           02  FILLER  PIC  X(27)  VALUE
                  "éÊÇËÇÊÇØì¸óÕ = 6 ....  ÿ¿-›".
           02  FILLER  PIC  X(25)  VALUE
                "ämîF(OK=1,NO=9)-->   ÿ¿∞›".
       01  ACT-AREA.
           02  ACT-SIGN PIC  9(01).
           02  ACT-KBN  PIC  9(01).
           02  ACT-ACT  PIC  9(01).
           02  ACT-JNO  PIC  9(06).
           02  ACT-JYMD.
             03  ACT-JYY       PIC  9(02).
             03  ACT-JMM       PIC  9(02).
             03  ACT-JDD       PIC  9(02).
           02  ACT-TCCD.
             03  ACT-TCD       PIC  9(04).
             03  ACT-CCD       PIC  9(03).
           02  ACT-SET  PIC  9(03).
           02  ACT-TENC PIC  9(04).
           02  ACT-SIZ  PIC  9(01).
           02  ACT-HCD  PIC  9(06).
           02  ACT-SU.
             03  ACT-SU1       PIC S9(04).
             03  ACT-TAN       PIC  9(05).
           02  ACT-END.
             03  ACT-NYMD.
               04  ACT-NYY       PIC  9(02).
               04  ACT-NMM       PIC  9(02).
               04  ACT-NDD       PIC  9(02).
           02  ACT-BIKHO PIC  X(10).
           02  ACT-TEKI1 PIC  N(09).
           02  ACT-TEKI2 PIC  N(19).
           02  ACT-OKC   PIC  9(01).
       01  DSP-AREA.
           02  DSP-HED.
             03  DSP-S0   PIC  N(06).
             03  DSP-S1   PIC  X(06) VALUE  "ã≥Å@àÁ".
             03  DSP-S2   PIC  X(06) VALUE  "àÍÅ@î ".
             03 DSP-ANAM  PIC  N(02).
           02  DSP-HEAD.
             03  DSP-BAR  PIC  X(01) VALUE   "-".
             03  DSP-SET  PIC  ZZZ .
             03  DSP-TNAM PIC  N(24).
             03  DSP-CNAM PIC  N(24).
           02  DSP-TEN.
             03  DSP-TENN PIC  X(04) VALUE   "ìXCD".
             03  DSP-TENC PIC  9(04).
             03  DSP-TENM PIC  N(11).
           02 DSP-HNAM PIC N(24).
           02  DSP-END.
             03 DSP-GKEI     PIC ZZZ,ZZZ- .
             03 DSP-GKIN     PIC ZZ,ZZZ,ZZZ- .
           02  DSP-NYMD.
               04  DSP-NA      PIC  X(01) VALUE "-".
               04  DSP-NB      PIC  X(01) VALUE "-".
               04  DSP-NYY     PIC  99 .
               04  DSP-NMM     PIC  ZZ .
               04  DSP-NDD     PIC  ZZ .
           02  DSP-TAN PIC ZZZZZ .
           02  DSP-KIN PIC ZZ,ZZZ,ZZZ- .
           02  DSP-SU.
             03  DSP-SU1   PIC ZZZZ- .
             03  DSP-KEI   PIC ZZZ,ZZZ- .
           02 DSP-NO   PIC 9(06).
           02 CLE-NO   PIC X(06) VALUE " ".
           02  DSP-BIKHO.
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE "[".
               04  FILLER  PIC  X(01) VALUE "]".
           02  DSP-BIKHO-CLE.
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(01) VALUE " ".
               04  FILLER  PIC  X(01) VALUE " ".
       01  CLR-HEAD1.
           02  CLR-JNO  PIC  X(06)   VALUE " ".
       01  CLR-HEAD.
           02  CLR-YY   PIC  X(02)   VALUE " ".
           02  CLR-MM   PIC  X(02)   VALUE " ".
           02  CLR-DD   PIC  X(02)   VALUE " ".
           02  CLR-TCD  PIC  X(04)   VALUE " ".
           02  CLR-CCD  PIC  X(03)   VALUE " ".
           02  CLR-TNAM PIC  X(48)   VALUE " ".
           02  CLR-SET  PIC  X(03)   VALUE " ".
           02  CLR-CNAM PIC  X(48)   VALUE " ".
           02  CLR-TEN  PIC  X(31)   VALUE " ".
       01  CLR-MEI.
           02  FILLER   PIC  X(11)   VALUE "CLEAR  DATA".
       01  CLR-AREA.
           02  CLR-HCD  PIC  X(07)   VALUE " ".
           02  CLR-SIZ  PIC X(07)    VALUE " ".
           02  CLR-01.
             03  FILLER   PIC  X(56)   VALUE " ".
           02  CLR-02.
             03  CLR-NA   PIC  X(01)   VALUE " ".
             03  CLR-NB   PIC  X(01)   VALUE " ".
             03  CLR-021  PIC  X(02)   VALUE " ".
             03  CLR-022  PIC  X(02)   VALUE " ".
             03  CLR-023  PIC  X(02)   VALUE " ".
           02  CLR-BIKHO  PIC  X(10)   VALUE " ".
           02  CLR-03.
             03  CLR-031  PIC  X(01)   VALUE " ".
             03  CLR-032  PIC  X(52)   VALUE " ".
             03  CLR-033  PIC  X(08)   VALUE " ".
             03  CLR-034  PIC  X(05)   VALUE " ".
             03  CLR-035  PIC  X(11)   VALUE " ".
           02  CLR-GKEI.
             03  FILLER   PIC X(08)    VALUE " ".
             03  FILLER   PIC X(11)    VALUE " ".
       01  CLR-END.
           02  CLR-TEKI1 PIC  N(09)   VALUE
               "Å@Å@Å@Å@Å@Å@Å@Å@Å@".
           02  CLR-TEKI2 PIC  N(23)   VALUE
               "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
           02  CLR-OKC   PIC  X(01)   VALUE " ".
      ****
       01  EMSG-AREA.
           02  EMSG-01           PIC  X(22)   VALUE
               "ÇiÇrÅ|ÇrÇhÇfÇmÅ@ÉGÉâÅ[".
           02  EMSG-04           PIC  X(30)   VALUE
               "íºëóêÊÉ}ÉXÉ^Åiìæà”êÊÅjÅ@ñ¢ìoò^".
           02  EMSG-05           PIC  X(30)   VALUE
               "íºëóêÊÉ}ÉXÉ^ÅiíºëóêÊÅjÅ@ñ¢ìoò^".
           02  EMSG-06           PIC  X(18)   VALUE
               "ïiñºÉ}ÉXÉ^Å@ñ¢ìoò^".
           02  EMSG-07           PIC  X(12)   VALUE
               "ï™óﬁÅ@ÉGÉâÅ[".
           02  EMSG-08           PIC  X(14)   VALUE
               "çsÉNÉäÉAÅ@ïsâ¬".
           02  EMSG-09           PIC  X(16)   VALUE
               "égópÉTÉCÉYÅ@ñ≥Çµ".
           02  EMSG-12           PIC  X(18)   VALUE
               "ÉZÉbÉgêîó Å@ÉGÉâÅ[".
           02  EMSG-13           PIC  X(14)   VALUE
               "éÊè¡êîÉIÅ[ÉoÅ[".
           02  EMSG-14           PIC  X(16)   VALUE
               "êîó É[ÉçÅ@ÉGÉâÅ[".
           02  EMSG-16           PIC  X(12)   VALUE
               "î[ä˙Å@ÉGÉâÅ[".
           02  EMSG-17           PIC  X(12)   VALUE
               "ãÊï™Å@ÉGÉâÅ[".
           02  EMSG-18           PIC  X(12)   VALUE
               "ìXñºÅ@ñ¢ìoò^".
           02  EMSG-19           PIC  X(12)   VALUE
               "íºëóÅ@ÉGÉâÅ[".
           02  EMSG-21           PIC  X(20)   VALUE
               "èoâ◊èàóùçœÅ@çÌèúïsâ¬".
           02  DEN-ERR           PIC  X(16)   VALUE
               "äYìñì`ï[áÇégópíÜ".
           02  MST-ERR           PIC  X(18)   VALUE
               "éÛíçÉ}ÉXÉ^Å@ñ¢ìoò^".
           02  ADV-ERR           PIC  X(14)   VALUE
               "çsÉNÉäÉAÅ@ïsâ¬".
           02  ERR-DNKN.
             03  DNK-ERR       PIC  X(18)   VALUE
                 "ì`ï[áÇåüçıÇeñ¢ìoò^".
             03  DNK-A         PIC  X(04)   VALUE  "KEY=".
             03  DNK-KEY       PIC X(15).
           COPY   LSMSG.
           02  DISP-MSG-SPACE-1  PIC  X(39)   VALUE
             "                                       ".
       01  DSP-REV-AREA.
           02  DSP-RR-ALL.
               03  DSP-RR1.
                   04 DSP-RR101   PIC  X(04).
                   04 DSP-RR102   PIC  X(04).
                   04 DSP-RR103   PIC  X(04).
                   04 DSP-RR104   PIC  X(04).
                   04 DSP-RR105   PIC  X(04).
                   04 DSP-RR106   PIC  X(04).
                   04 DSP-RR107   PIC  X(04).
                   04 DSP-RR108   PIC  X(04).
                   04 DSP-RR109   PIC  X(04).
                   04 DSP-RR110   PIC  X(04).
               03  DSP-RR2.
                   04 DSP-RR201   PIC  X(04).
                   04 DSP-RR202   PIC  X(04).
                   04 DSP-RR203   PIC  X(04).
                   04 DSP-RR204   PIC  X(04).
                   04 DSP-RR205   PIC  X(04).
                   04 DSP-RR206   PIC  X(04).
                   04 DSP-RR207   PIC  X(04).
                   04 DSP-RR208   PIC  X(04).
                   04 DSP-RR209   PIC  X(04).
                   04 DSP-RR210   PIC  X(04).
               03  DSP-RR3.
                   04 DSP-RR301   PIC  X(04).
                   04 DSP-RR302   PIC  X(04).
                   04 DSP-RR303   PIC  X(04).
                   04 DSP-RR304   PIC  X(04).
                   04 DSP-RR305   PIC  X(04).
                   04 DSP-RR306   PIC  X(04).
                   04 DSP-RR307   PIC  X(04).
                   04 DSP-RR308   PIC  X(04).
                   04 DSP-RR309   PIC  X(04).
                   04 DSP-RR310   PIC  X(04).
               03  DSP-RR4.
                   04 DSP-RR401   PIC  X(04).
                   04 DSP-RR402   PIC  X(04).
                   04 DSP-RR403   PIC  X(04).
                   04 DSP-RR404   PIC  X(04).
                   04 DSP-RR405   PIC  X(04).
                   04 DSP-RR406   PIC  X(04).
                   04 DSP-RR407   PIC  X(04).
                   04 DSP-RR408   PIC  X(04).
                   04 DSP-RR409   PIC  X(04).
                   04 DSP-RR410   PIC  X(04).
           02  DSP-RC-ALL.
               03  DSP-RC1.
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
               03  DSP-RC2.
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
               03  DSP-RC3.
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
               03  DSP-RC4.
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
                   04  FILLER  PIC  X(04).
           COPY  LIBSCR.
       PROCEDURE              DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLEAR
       CALL "SD_Init" USING
           "DSP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
          "01DSP-CLEAR" "X" "1" "0" "12" " " "DSP-CLEAR" RETURNING RESU.
      *DSP-MID
       CALL "SD_Init" USING 
            "DSP-MID" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID" "RN" "1" "21" "36" " " "DSP-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID" " " "7" "0" "30" "01DSP-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-MID" "N" "7" "21" "6" " " "02DSP-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-MID" "X" "7" "27" "5" "0102DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-MID" "N" "7" "32" "6" "0202DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-MID" "X" "7" "38" "13" "0302DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MID" "X" "11" "25" "16" "02DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-MID" "X" "13" "25" "16" "03DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-MID" "X" "15" "25" "27" "04DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-MID" "X" "24" "41" "25" "05DSP-MID" " "
            RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "118" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SIGN" "9" "7" "46" "1" " " "ACT-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-KBN" "9" "15" "47" "1" "ACT-SIGN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-ACT" "9" "1" "67" "1" "ACT-KBN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-ACT" BY REFERENCE GMN-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JNO" "9" "3" "2" "6" "ACT-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JNO" BY REFERENCE GMN-JNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JYMD" " " "3" "0" "6" "ACT-JNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JYY" "9" "3" "9" "2" " " "ACT-JYMD" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JYY" BY REFERENCE GMN-JYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JMM" "9" "3" "12" "2" "ACT-JYY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JMM" BY REFERENCE GMN-JMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JDD" "9" "3" "15" "2" "ACT-JMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JDD" BY REFERENCE GMN-JDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TCCD" " " "3" "0" "7" "ACT-JYMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TCD" "9" "3" "18" "4" " " "ACT-TCCD" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-TCD" BY REFERENCE GMN-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-CCD" "9" "3" "23" "3" "ACT-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-CCD" BY REFERENCE GMN-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SET" "9" "3" "28" "3" "ACT-TCCD" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SET" BY REFERENCE GMN-SET "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TENC" "9" "4" "6" "4" "ACT-SET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-TENC" BY REFERENCE GMN-TENC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SIZ" "9" "AA PLUS 1" "1" "1" "ACT-TENC" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-SIZ" BY REFERENCE GMN-SIZ(1) "1" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-HCD" "9" "AA" "2" "6" "ACT-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-HCD" BY REFERENCE GMN-HCD(1) "6" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SU" " " "AA PLUS 1" "0" "9" "ACT-HCD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SU1" "S9" "AA PLUS 1" "BB" "4" " " "ACT-SU"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SU1" BY REFERENCE GMN-SU(1,1) "6" "2" BY REFERENCE
            XX 155 BY REFERENCE ZZ 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TAN" "9" "AA PLUS 1" "64" "5" "ACT-SU1" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-TAN" BY REFERENCE GMN-TAN(1) "5" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-END" " " "AA" "0" "6" "ACT-SU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-NYMD" " " "AA" "46" "6" " " "ACT-END" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-NYY" "9" "AA" "59" "2" " " "ACT-NYMD" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-NYY" BY REFERENCE GMN-NYY2(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-NMM" "9" "AA" "62" "2" "ACT-NYY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-NMM" BY REFERENCE GMN-NMM(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-NDD" "9" "AA" "65" "2" "ACT-NMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-NDD" BY REFERENCE GMN-NDD(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-BIKHO" "X" "AA" "69" "10" "ACT-END" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-BIKHO" BY REFERENCE GMN-BIKHO(1) "10" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TEKI1" "N" "22" "7" "18" "ACT-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-TEKI1" BY REFERENCE GMN-TEKI1 "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TEKI2" "N" "23" "7" "38" "ACT-TEKI1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-TEKI2" BY REFERENCE GMN-TEKI2 "38" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-OKC" "9" "24" "60" "1" "ACT-TEKI2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-OKC" BY REFERENCE GMN-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "290" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED" " " "1" "0" "28" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S0" "N" "1" "2" "12" " " "DSP-HED" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-S0" BY REFERENCE W-MID "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S1" "bX" "1" "21" "6" "DSP-S0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S2" "bX" "1" "21" "6" "DSP-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ANAM" "N" "1" "69" "4" "DSP-S2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ANAM" BY REFERENCE GMN-ANAM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HEAD" " " "0" "0" "100" "DSP-HED" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BAR" "X" "3" "22" "1" " " "DSP-HEAD" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SET" "ZZZ" "3" "28" "3" "DSP-BAR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SET" BY REFERENCE GMN-SET "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TNAM" "N" "3" "33" "48" "DSP-SET" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TNAM" BY REFERENCE GMN-TNAM "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CNAM" "N" "4" "33" "48" "DSP-TNAM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-CNAM" BY REFERENCE GMN-CNAM "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TEN" " " "4" "0" "30" "DSP-HEAD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TENN" "X" "4" "1" "4" " " "DSP-TEN" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TENC" "9" "4" "6" "4" "DSP-TENN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TENC" BY REFERENCE GMN-TENC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TENM" "N" "4" "10" "22" "DSP-TENC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TENM" BY REFERENCE GMN-TENM "22" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HNAM" "N" "AA" "9" "48" "DSP-TEN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HNAM" BY REFERENCE GMN-HNAM(1) "48" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-END" " " "22" "0" "19" "DSP-HNAM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-GKEI" "ZZZ,ZZZ-" "22" "55" "8" " " "DSP-END"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-GKEI" BY REFERENCE GMN-GKEI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-GKIN" "ZZ,ZZZ,ZZZ-" "22" "70" "11" "DSP-GKEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-GKIN" BY REFERENCE GMN-GKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NYMD" " " "AA" "0" "0" "DSP-END" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NA" "X" "AA" "61" "1" " " "DSP-NYMD" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NB" "X" "AA" "64" "1" "DSP-NA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NYY" "99" "AA" "59" "2" "DSP-NB" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NYY" BY REFERENCE GMN-NYY2(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NMM" "ZZ" "AA" "62" "2" "DSP-NYY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NMM" BY REFERENCE GMN-NMM(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NDD" "ZZ" "AA" "65" "2" "DSP-NMM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NDD" BY REFERENCE GMN-NDD(1) "2" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TAN" "ZZZZZ" "AA PLUS 1" "64" "5" "DSP-NYMD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TAN" BY REFERENCE GMN-TAN(1) "5" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KIN" "ZZ,ZZZ,ZZZ-" "AA PLUS 1" "70" "11" "DSP-TAN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KIN" BY REFERENCE GMN-KIN(1) "8" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SU" " " "AA PLUS 1" "0" "13" "DSP-KIN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SU1" "ZZZZ-" "AA PLUS 1" "BB" "5" " " "DSP-SU"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SU1" BY REFERENCE GMN-SU(1,1) "6" "2"  BY REFERENCE
            XX 155 BY REFERENCE ZZ 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEI" "ZZZ,ZZZ-" "AA PLUS 1" "55" "8" "DSP-SU1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEI" BY REFERENCE GMN-KEI(1) "6" "1" BY REFERENCE
            XX 155 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NO" "9" "23" "75" "6" "DSP-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NO" BY REFERENCE GMN-NO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-NO" "X" "23" "75" "6" "DSP-NO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO" " " "0" "0" "12" "CLE-NO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO" " " "10" "0" "2" " " "DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-BIKHO" "X" "10" "68" "1" " " "01DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-BIKHO" "X" "10" "79" "1" "0101DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO" " " "12" "0" "2" "01DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-BIKHO" "X" "12" "68" "1" " " "02DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-BIKHO" "X" "12" "79" "1" "0102DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO" " " "14" "0" "2" "02DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-BIKHO" "X" "14" "68" "1" " " "03DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203DSP-BIKHO" "X" "14" "79" "1" "0103DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO" " " "16" "0" "2" "03DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-BIKHO" "X" "16" "68" "1" " " "04DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-BIKHO" "X" "16" "79" "1" "0104DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO" " " "18" "0" "2" "04DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0105DSP-BIKHO" "X" "18" "68" "1" " " "05DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205DSP-BIKHO" "X" "18" "79" "1" "0105DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO" " " "20" "0" "2" "05DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-BIKHO" "X" "20" "68" "1" " " "06DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-BIKHO" "X" "20" "79" "1" "0106DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO-CLE" " " "0" "0" "12" "DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO-CLE" " " "10" "0" "2" " " "DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-BIKHO-CLE" "X" "10" "68" "1" " " "01DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-BIKHO-CLE" "X" "10" "79" "1" "0101DSP-BIKHO-CLE"
            " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO-CLE" " " "12" "0" "2" "01DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-BIKHO-CLE" "X" "12" "68" "1" " " "02DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-BIKHO-CLE" "X" "12" "79" "1" "0102DSP-BIKHO-CLE"
            " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO-CLE" " " "14" "0" "2" "02DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-BIKHO-CLE" "X" "14" "68" "1" " " "03DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203DSP-BIKHO-CLE" "X" "14" "79" "1" "0103DSP-BIKHO-CLE"
            " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO-CLE" " " "16" "0" "2" "03DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-BIKHO-CLE" "X" "16" "68" "1" " " "04DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-BIKHO-CLE" "X" "16" "79" "1" "0104DSP-BIKHO-CLE"
            " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO-CLE" " " "18" "0" "2" "04DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0105DSP-BIKHO-CLE" "X" "18" "68" "1" " " "05DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205DSP-BIKHO-CLE" "X" "18" "79" "1" "0105DSP-BIKHO-CLE"
            " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO-CLE" " " "20" "0" "2" "05DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-BIKHO-CLE" "X" "20" "68" "1" " " "06DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-BIKHO-CLE" "X" "20" "79" "1" "0106DSP-BIKHO-CLE"
            " " RETURNING RESU.
      *CLR-HEAD1
       CALL "SD_Init" USING 
            "CLR-HEAD1" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-JNO" "X" "3" "2" "6" " " "CLR-HEAD1" RETURNING RESU.
      *CLR-HEAD
       CALL "SD_Init" USING 
            "CLR-HEAD" " " "0" "0" "143" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-YY" "X" "3" "9" "2" " " "CLR-HEAD" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-MM" "X" "3" "12" "2" "CLR-YY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-DD" "X" "3" "15" "2" "CLR-MM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TCD" "X" "3" "18" "4" "CLR-DD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-CCD" "X" "3" "23" "3" "CLR-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TNAM" "X" "3" "33" "48" "CLR-CCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-SET" "X" "3" "28" "3" "CLR-TNAM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-CNAM" "X" "4" "33" "48" "CLR-SET" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TEN" "X" "4" "1" "31" "CLR-CNAM" " " RETURNING RESU.
      *CLR-MEI
       CALL "SD_Init" USING 
            "CLR-MEI" " " "AA" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "AA" "21" "11" " " "CLR-MEI" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "184" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HCD" "X" "AA" "2" "7" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-SIZ" "X" "AA PLUS 1" "1" "7" "CLR-HCD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "56" "CLR-SIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "AA" "2" "56" " " "CLR-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-02" " " "AA" "0" "8" "CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-NA" "X" "AA" "61" "1" " " "CLR-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-NB" "X" "AA" "64" "1" "CLR-NA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-021" "X" "AA" "59" "2" "CLR-NB" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-022" "X" "AA" "62" "2" "CLR-021" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-023" "X" "AA" "65" "2" "CLR-022" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-BIKHO" "X" "AA" "69" "10" "CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-03" " " "AA PLUS 1" "0" "77" "CLR-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-031" "X" "AA PLUS 1" "1" "1" " " "CLR-03"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-032" "X" "AA PLUS 1" "3" "52" "CLR-031" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-033" "X" "AA PLUS 1" "55" "8" "CLR-032" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-034" "X" "AA PLUS 1" "64" "5" "CLR-033" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-035" "X" "AA PLUS 1" "70" "11" "CLR-034" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-GKEI" " " "22" "0" "19" "CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-GKEI" "X" "22" "55" "8" " " "CLR-GKEI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-GKEI" "X" "22" "70" "11" "01CLR-GKEI" " "
            RETURNING RESU.
      *CLR-END
       CALL "SD_Init" USING 
            "CLR-END" " " "0" "0" "65" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TEKI1" "N" "22" "7" "18" " " "CLR-END" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TEKI2" "N" "23" "7" "46" "CLR-TEKI1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-OKC" "X" "24" "60" "1" "CLR-TEKI2" " " RETURNING RESU.
      *EMSG-AREA
       CALL "SD_Init" USING 
            "EMSG-AREA" " " "24" "0" "382" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-01" "X" "24" "1" "22" " " "EMSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-04" "X" "24" "1" "30" "EMSG-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-05" "X" "24" "1" "30" "EMSG-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-06" "X" "24" "1" "18" "EMSG-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-07" "X" "24" "1" "12" "EMSG-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-08" "X" "24" "1" "14" "EMSG-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-09" "X" "24" "1" "16" "EMSG-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-12" "X" "24" "1" "18" "EMSG-09" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-13" "X" "24" "1" "14" "EMSG-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-14" "X" "24" "1" "16" "EMSG-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-16" "X" "24" "1" "12" "EMSG-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-17" "X" "24" "1" "12" "EMSG-16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-18" "X" "24" "1" "12" "EMSG-17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-19" "X" "24" "1" "12" "EMSG-18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-21" "X" "24" "1" "20" "EMSG-19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DEN-ERR" "X" "24" "1" "16" "EMSG-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "MST-ERR" "X" "24" "1" "18" "DEN-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ADV-ERR" "X" "24" "1" "14" "MST-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DNKN" " " "24" "0" "37" "ADV-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DNK-ERR" "X" "24" "1" "18" " " "ERR-DNKN" RETURNING RESU.
       CALL "SD_Init" USING 
            "DNK-A" "X" "24" "20" "4" "DNK-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DNK-KEY" "X" "24" "25" "15" "DNK-A" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DNK-KEY" BY REFERENCE DNKN-KEY "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE-1" "X" "24" "1" "39" "ERR-DNKN" " "
            RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "440" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "24" "0" "60" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "24" "1" "60" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE" " " "24" "0" "60" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "60" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACES" " " "24" "0" "40" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACES" "X" "24" "1" "40" " " "DISP-MSG-SPACES"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACES" BY REFERENCE ERR-SPACES "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACES" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-B" "X" "24" "80" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "24" "0" "22" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "24" "0" "22" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "24" "1" "22" " " "INV-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "24" "0" "22" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "24" "1" "22" " " "INV-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-01" " " "24" "0" "14" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01OK-01" "X" "24" "1" "14" " " "OK-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "CAN-01" " " "24" "0" "18" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CAN-01" "X" "24" "1" "18" " " "CAN-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-01" " " "24" "0" "18" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-01" "X" "24" "1" "18" " " "ERR-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "24" "0" "22" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "24" "1" "22" " " "ERR-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DIS" "X" "24" "2" "5" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-DIS" "X" "24" "7" "12" "01ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-DIS" "X" "24" "19" "1" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ERR-DIS" "X" "24" "20" "11" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ERR-DIS" "X" "24" "31" "2" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06ERR-DIS" "X" "24" "33" "5" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07ERR-DIS" "X" "24" "38" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08ERR-DIS" "X" "24" "43" "30" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE-1" "X" "24" "1" "39" "ERR-DIS" " "
            RETURNING RESU.
      *DSP-REV-AREA
       CALL "SD_Init" USING 
            "DSP-REV-AREA" " " "0" "0" "320" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR-ALL" " " "0" "0" "160" " " "DSP-REV-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR1" " " "6" "0" "40" " " "DSP-RR-ALL" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR101" "RX" "6" "3" "4" " " "DSP-RR1" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR101" BY REFERENCE REV-101 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR102" "RX" "6" "8" "4" "DSP-RR101" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR102" BY REFERENCE REV-102 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR103" "RX" "6" "13" "4" "DSP-RR102" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR103" BY REFERENCE REV-103 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR104" "RX" "6" "18" "4" "DSP-RR103" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR104" BY REFERENCE REV-104 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR105" "RX" "6" "23" "4" "DSP-RR104" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR105" BY REFERENCE REV-105 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR106" "RX" "6" "28" "4" "DSP-RR105" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR106" BY REFERENCE REV-106 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR107" "RX" "6" "33" "4" "DSP-RR106" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR107" BY REFERENCE REV-107 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR108" "RX" "6" "38" "4" "DSP-RR107" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR108" BY REFERENCE REV-108 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR109" "RX" "6" "43" "4" "DSP-RR108" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR109" BY REFERENCE REV-109 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR110" "RX" "6" "50" "4" "DSP-RR109" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR110" BY REFERENCE REV-110 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR2" " " "7" "0" "40" "DSP-RR1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR201" "RX" "7" "3" "4" " " "DSP-RR2" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR201" BY REFERENCE REV-201 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR202" "RX" "7" "8" "4" "DSP-RR201" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR202" BY REFERENCE REV-202 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR203" "RX" "7" "13" "4" "DSP-RR202" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR203" BY REFERENCE REV-203 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR204" "RX" "7" "18" "4" "DSP-RR203" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR204" BY REFERENCE REV-204 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR205" "RX" "7" "23" "4" "DSP-RR204" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR205" BY REFERENCE REV-205 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR206" "RX" "7" "28" "4" "DSP-RR205" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR206" BY REFERENCE REV-206 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR207" "RX" "7" "33" "4" "DSP-RR206" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR207" BY REFERENCE REV-207 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR208" "RX" "7" "38" "4" "DSP-RR207" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR208" BY REFERENCE REV-208 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR209" "RX" "7" "43" "4" "DSP-RR208" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR209" BY REFERENCE REV-209 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR210" "RX" "7" "50" "4" "DSP-RR209" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR210" BY REFERENCE REV-210 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR3" " " "8" "0" "40" "DSP-RR2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR301" "RX" "8" "3" "4" " " "DSP-RR3" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR301" BY REFERENCE REV-301 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR302" "RX" "8" "8" "4" "DSP-RR301" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR302" BY REFERENCE REV-302 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR303" "RX" "8" "13" "4" "DSP-RR302" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR303" BY REFERENCE REV-303 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR304" "RX" "8" "18" "4" "DSP-RR303" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR304" BY REFERENCE REV-304 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR305" "RX" "8" "23" "4" "DSP-RR304" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR305" BY REFERENCE REV-305 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR306" "RX" "8" "28" "4" "DSP-RR305" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR306" BY REFERENCE REV-306 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR307" "RX" "8" "33" "4" "DSP-RR306" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR307" BY REFERENCE REV-307 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR308" "RX" "8" "38" "4" "DSP-RR307" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR308" BY REFERENCE REV-308 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR309" "RX" "8" "43" "4" "DSP-RR308" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR309" BY REFERENCE REV-309 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR310" "RX" "8" "50" "4" "DSP-RR309" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR310" BY REFERENCE REV-310 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR4" " " "9" "0" "40" "DSP-RR3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR401" "RX" "9" "3" "4" " " "DSP-RR4" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR401" BY REFERENCE REV-401 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR402" "RX" "9" "8" "4" "DSP-RR401" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR402" BY REFERENCE REV-402 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR403" "RX" "9" "13" "4" "DSP-RR402" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR403" BY REFERENCE REV-403 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR404" "RX" "9" "18" "4" "DSP-RR403" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR404" BY REFERENCE REV-404 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR405" "RX" "9" "23" "4" "DSP-RR404" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR405" BY REFERENCE REV-405 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR406" "RX" "9" "28" "4" "DSP-RR405" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR406" BY REFERENCE REV-406 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR407" "RX" "9" "33" "4" "DSP-RR406" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR407" BY REFERENCE REV-407 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR408" "RX" "9" "38" "4" "DSP-RR407" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR408" BY REFERENCE REV-408 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR409" "RX" "9" "43" "4" "DSP-RR408" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR409" BY REFERENCE REV-409 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR410" "RX" "9" "50" "4" "DSP-RR409" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR410" BY REFERENCE REV-410 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC-ALL" " " "0" "0" "160" "DSP-RR-ALL" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC1" " " "6" "0" "40" " " "DSP-RC-ALL" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC1" "X" "6" "3" "4" " " "DSP-RC1" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC1" BY REFERENCE REV-101 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC1" "X" "6" "8" "4" "01DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC1" BY REFERENCE REV-102 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC1" "X" "6" "13" "4" "02DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC1" BY REFERENCE REV-103 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC1" "X" "6" "18" "4" "03DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC1" BY REFERENCE REV-104 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC1" "X" "6" "23" "4" "04DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC1" BY REFERENCE REV-105 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC1" "X" "6" "28" "4" "05DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC1" BY REFERENCE REV-106 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC1" "X" "6" "33" "4" "06DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC1" BY REFERENCE REV-107 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC1" "X" "6" "38" "4" "07DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC1" BY REFERENCE REV-108 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC1" "X" "6" "43" "4" "08DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC1" BY REFERENCE REV-109 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC1" "X" "6" "50" "4" "09DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC1" BY REFERENCE REV-110 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC2" " " "7" "0" "40" "DSP-RC1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC2" "X" "7" "3" "4" " " "DSP-RC2" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC2" BY REFERENCE REV-201 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC2" "X" "7" "8" "4" "01DSP-RC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC2" BY REFERENCE REV-202 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC2" "X" "7" "13" "4" "02DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC2" BY REFERENCE REV-203 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC2" "X" "7" "18" "4" "03DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC2" BY REFERENCE REV-204 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC2" "X" "7" "23" "4" "04DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC2" BY REFERENCE REV-205 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC2" "X" "7" "28" "4" "05DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC2" BY REFERENCE REV-206 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC2" "X" "7" "33" "4" "06DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC2" BY REFERENCE REV-207 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC2" "X" "7" "38" "4" "07DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC2" BY REFERENCE REV-208 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC2" "X" "7" "43" "4" "08DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC2" BY REFERENCE REV-209 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC2" "X" "7" "50" "4" "09DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC2" BY REFERENCE REV-210 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC3" " " "8" "0" "40" "DSP-RC2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC3" "X" "8" "3" "4" " " "DSP-RC3" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC3" BY REFERENCE REV-301 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC3" "X" "8" "8" "4" "01DSP-RC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC3" BY REFERENCE REV-302 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC3" "X" "8" "13" "4" "02DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC3" BY REFERENCE REV-303 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC3" "X" "8" "18" "4" "03DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC3" BY REFERENCE REV-304 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC3" "X" "8" "23" "4" "04DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC3" BY REFERENCE REV-305 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC3" "X" "8" "28" "4" "05DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC3" BY REFERENCE REV-306 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC3" "X" "8" "33" "4" "06DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC3" BY REFERENCE REV-307 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC3" "X" "8" "38" "4" "07DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC3" BY REFERENCE REV-308 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC3" "X" "8" "43" "4" "08DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC3" BY REFERENCE REV-309 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC3" "X" "8" "50" "4" "09DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC3" BY REFERENCE REV-310 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC4" " " "9" "0" "40" "DSP-RC3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC4" "X" "9" "3" "4" " " "DSP-RC4" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC4" BY REFERENCE REV-401 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC4" "X" "9" "8" "4" "01DSP-RC4" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC4" BY REFERENCE REV-402 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC4" "X" "9" "13" "4" "02DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC4" BY REFERENCE REV-403 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC4" "X" "9" "18" "4" "03DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC4" BY REFERENCE REV-404 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC4" "X" "9" "23" "4" "04DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC4" BY REFERENCE REV-405 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC4" "X" "9" "28" "4" "05DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC4" BY REFERENCE REV-406 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC4" "X" "9" "33" "4" "06DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC4" BY REFERENCE REV-407 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC4" "X" "9" "38" "4" "07DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC4" BY REFERENCE REV-408 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC4" "X" "9" "43" "4" "08DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC4" BY REFERENCE REV-409 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC4" "X" "9" "50" "4" "09DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC4" BY REFERENCE REV-410 "4" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *************************************************
      *    ÇlÇ`ÇhÇmÅ|ÇqÇsÇmÅ@ ÅiéÂèàóùÅj              *
      *************************************************
       MAIN-RTN.
           PERFORM  OPEN-RTN       THRU  OPEN-EX.
           PERFORM  ACCEPT-RTN     THRU  ACCEPT-EX.
           PERFORM  END-RTN        THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
           COPY   LPMSG.
      *************************************************
      *    çsÇmÇnÅEÅEÅEÅEÇwÇw                         *
      *    êîó íPà ÅEÅEÅEÇyÇy                         *
      *************************************************
      *************************************************
      *    èâä˙Å@èàóù        ÇnÇoÇdÇmÅ|ÇqÇsÇm         *
      *************************************************
       OPEN-RTN.
           CALL "SD_Output" USING
            "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-MID" DSP-MID "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
       OPEN-005.
           CALL "SD_Accept" USING BY REFERENCE ACT-SIGN "ACT-SIGN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         =  "P9"
               CALL "SD_Output" USING
                "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF
           IF  ESTAT    NOT  =  "01"  AND  "06"
               GO  TO  OPEN-005
           END-IF
           IF  JS-SIGN  NOT  =   0 AND 1
               GO  TO  OPEN-005
           END-IF.
       OPEN-010.
           CALL "SD_Accept" USING BY REFERENCE ACT-KBN "ACT-KBN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         =  "P9"
               CALL "SD_Output" USING
                "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF
           IF  ESTAT         =  "09"
               GO  TO  OPEN-005
           END-IF
           IF  ESTAT    NOT  =  "01"  AND  "06"
               GO  TO  OPEN-010
           END-IF
           IF  W-KBN    NOT  =  0  AND  5  AND  6
               GO  TO  OPEN-010
           END-IF.
       OPEN-020.
           CALL "SD_Accept" USING BY REFERENCE ACT-OKC "ACT-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         =  "09"
               GO  TO  OPEN-010
           END-IF
           IF  ESTAT    NOT  =  "01"  AND  "06"
               GO  TO  OPEN-020
           END-IF
           IF  GMN-OKC       =  9
               GO  TO  OPEN-005
           END-IF
           IF  GMN-OKC  NOT  =  1
               GO  TO  OPEN-020
           END-IF
      *
           IF  W-KBN         =  0
               MOVE  "Å@éÛíçì¸óÕÅ@"   TO  W-MID
           END-IF
           IF  W-KBN         =  5
               MOVE  "Å@óaÇËì¸óÕÅ@"   TO  W-MID
           END-IF
           IF  W-KBN         =  6
               MOVE  "éÊÇËÇÊÇØì¸óÕ"   TO  W-MID
           END-IF
           CALL "SD_Screen_Output" USING "SJ010I" RETURNING RESU.
           CALL "SD_Output" USING "DSP-S0" DSP-S0 "p" RETURNING RESU.
           IF  JS-SIGN   =  0
               CALL "SD_Output" USING
                "DSP-S1" DSP-S1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-S2" DSP-S2 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JDCON_PNAME1 "SHARED" BY REFERENCE JDCON_IDLST "1"
            "JDCON-KEY" BY REFERENCE JDCON-KEY.
           MOVE    ALL "Å@"    TO    W-TEKI.
           MOVE    ZERO            TO    W-EDATE.
           MOVE    11        TO    JCON1-KEY.
      *           READ    JCON      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      "JCON"  TO   ERR-F
               MOVE      JCON1-KEY TO ERR-K
               MOVE      "A"     TO   ERR-M
               PERFORM   ERR-RTN THRU ERR-EX
           END-IF
           INITIALIZE        W-DATE.
           ACCEPT  WK-DATES  FROM  DATE.
           COPY  LIBCPR.
           IF  WK-YY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO WK-YY
           END-IF
           IF  WK-YY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO WK-YY
           END-IF.
       OPEN-EX.
           EXIT.
      *************************************************
      *    âÊñ Å@ì¸óÕÅ@Å@Å@Å@Ç`ÇbÇbÇdÇoÇsÅ|ÇqÇsÇm     *
      *************************************************
       ACCEPT-RTN.
           MOVE    10        TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           PERFORM    CLR-RTN   THRU    CLR-EX.
           IF  GMN-OKC   NOT =  5
               GO  TO  ACC-ACT
           END-IF
           CALL "SD_Output" USING "ACT-ACT" ACT-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-ANAM" DSP-ANAM "p" RETURNING RESU.
           MOVE  ZERO        TO  GMN-OKC.
           IF  GMN-ACT    NOT =  1
               GO  TO  ACC-JNO
           END-IF
           IF  JS-SIGN        =  0
               CALL "SD_Output" USING
                "ACT-TEKI2" ACT-TEKI2 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN        =  1
               IF  W-EDATE    NOT =  ZERO
                   MOVE   W-EDATE      TO  GMN-JYMD
                   CALL "SD_Output" USING
                    "ACT-JYMD" ACT-JYMD "p" RETURNING RESU
               END-IF
           END-IF
           GO  TO  ACC-JYMD.
       ACC-ACT.
           INITIALIZE        GMN-AREA  WK-AREA.
           CALL "SD_Accept" USING BY REFERENCE ACT-ACT "ACT-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "P9"
               GO  TO  ACCEPT-EX
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACCEPT-RTN
           END-IF
           IF  GMN-ACT NOT = 1 AND 2 AND 3
               GO  TO  ACCEPT-RTN
           END-IF
           MOVE       ALL "Å@"   TO  GMN-TEKI.
           CALL "SD_Output" USING
            "ACT-TEKI1" ACT-TEKI1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TEKI2" ACT-TEKI2 "p" RETURNING RESU.
           IF  GMN-ACT   =  1
               CALL "SD_Output" USING
                "CLR-JNO" CLR-JNO "p" RETURNING RESU
               MOVE  SPACE       TO  SW-ZERO
               MOVE  "í«â¡"    TO  GMN-ANAM
           ELSE
               IF  GMN-ACT   =  2
                   MOVE  "ïœçX"    TO  GMN-ANAM
               ELSE
                   MOVE  "éÊè¡"    TO  GMN-ANAM
               END-IF
           END-IF
           CALL "SD_Output" USING
            "DSP-ANAM" DSP-ANAM "p" RETURNING RESU.
           INITIALIZE             GMN-DATA  WK-HCD.
           MOVE ZERO  TO  WS-SEQ.
           IF  GMN-ACT   =  1
               CALL "SD_Output" USING
                "CLE-NO" CLE-NO "p" RETURNING RESU
               GO  TO  ACC-JYMD
           END-IF.
       ACC-JNO.
           MOVE    ZERO      TO    GMN-NO  GMN-JNO.
           MOVE    1       TO     JCON1-01.
           MOVE    1       TO     JCON1-02.
      *           READ    JCON    UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE     JCON1-R
           END-IF
           IF  JS-SIGN     =  0
               MOVE   JCON1-03    TO     GMN-NO
           ELSE
               MOVE   JCON1-04    TO     GMN-NO
           END-IF
           CALL "SD_Output" USING "DSP-NO" DSP-NO "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-JNO" CLR-JNO "p" RETURNING RESU.
       ACC-JNOR.
           MOVE    10        TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           PERFORM    CLR-001  THRU  CLR-EX.
           INITIALIZE   GMN-HEAD GMN-DATA WK-AREA.
       ACC-JNO1.
           CALL "SD_Accept" USING BY REFERENCE ACT-JNO "ACT-JNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACCEPT-RTN
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-JNO
           END-IF
           MOVE     ZERO          TO  JMSTD-KEY1.
           MOVE     GMN-JNO       TO  JMSTD-07.
      *           START    JMSTD    KEY  NOT <     JMSTD-KEY1   INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNO
           END-IF
      *           READ     JMSTD    NEXT UNLOCK    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNOR
           END-IF
           MOVE    JMSTD-KEY1     TO  WK-KEY.
           IF  GMN-JNO   NOT  =  JMSTD-07
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNOR
           END-IF
           IF  W-KBN     NOT  =  JMSTD-01
               CALL "SD_Output" USING
                "EMSG-17" EMSG-17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNOR
           END-IF
           PERFORM  HYOJI-RTN      THRU  HYOJI-EX.
           MOVE     1             TO  JDCON-01.
           MOVE     GMN-JNO       TO  JDCON-02.
      *           WRITE    JDCON-R  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JDCON_PNAME1 JDCON_LNAME JDCON-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DEN-ERR" DEN-ERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNOR
           END-IF
      **
           IF  GMN-ACT  =  3  AND  SW-ZERO NOT = SPACE
               CALL "SD_Output" USING
                "EMSG-21" EMSG-21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JNOD
           END-IF
           IF  GMN-ACT  = 3
               GO  TO  ACC-OKC
           END-IF
           GO TO  ACC-JYMD.
       ACC-JNOD.
           PERFORM    JDCON-RTN  THRU  JDCON-EX.
           GO  TO     ACC-JNOR.
       ACC-JYMD.
           CALL "SD_Accept" USING BY REFERENCE ACT-JYY "ACT-JYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               IF  GMN-ACT   =   1
                   GO  TO  ACCEPT-RTN
               ELSE
                   GO  TO  ACC-JNOD
               END-IF
           END-IF
           IF  ESTAT NOT =  "00" AND "01" AND "06"
               GO  TO  ACC-JYMD
           END-IF.
       ACC-JMM.
           CALL "SD_Accept" USING BY REFERENCE ACT-JMM "ACT-JMM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-JYMD
           END-IF
           IF  ESTAT NOT =  "00" AND "01" AND "06"
               GO  TO  ACC-JMM
           END-IF
           IF  GMN-JMM   =  ZERO
               IF  GMN-JYY2     =   ZERO
                   GO  TO  ACC-JDD
               END-IF
           END-IF
           IF  GMN-JMM   <  1    OR       GMN-JMM   >   12
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JMM
           END-IF.
       ACC-JDD.
           CALL "SD_Accept" USING BY REFERENCE ACT-JDD "ACT-JDD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-JMM
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-JDD
           END-IF
           IF  GMN-JDD   =  ZERO
               IF  GMN-JMM      =   ZERO
                   MOVE  WK-DATE         TO  GMN-JYMD
                   CALL "SD_Output" USING
                    "ACT-JYMD" ACT-JYMD "p" RETURNING RESU
                   GO  TO  ACC-TCD
               END-IF
           END-IF
           IF  GMN-JMM   <  1    OR       GMN-JMM   >   12
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JMM
           END-IF
           IF  GMN-JDD   <  1    OR       GMN-JDD   >   31
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-JDD
           END-IF
           MOVE   ZERO    TO  GMN-JYY1.
           IF  GMN-JYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO GMN-JYY
           END-IF
           IF  GMN-JYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO GMN-JYY
           END-IF
           IF  GMN-JYMD <  WK-DATE
               CALL "SD_Output" USING
                 "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU
           END-IF.
       ACC-TCD.
           CALL "SD_Output" USING "DSP-BAR" DSP-BAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TCCD" ACT-TCCD "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACT-TCD "ACT-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-JYMD
           END-IF
           IF  ESTAT NOT =  "00" AND "01" AND "06"
               GO  TO  ACC-TCD
           END-IF.
       ACC-TNAM.
           MOVE     GMN-TCD       TO  TC-TCD.
           MOVE     001           TO  TC-CCD.
      *           READ     TC-M          UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "EMSG-04" EMSG-04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-TCD
           END-IF
           MOVE     TC-NAME       TO  GMN-TNAM.
           MOVE     GMN-TCD       TO  T-KEY.
      *           READ     T-M           UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     ZERO       TO   T-TNC
           END-IF.
       ACC-CCD.
           CALL "SD_Accept" USING BY REFERENCE ACT-CCD "ACT-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-TCD
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-CCD
           END-IF.
       ACC-CNAM.
           MOVE     GMN-TCD       TO  TC-TCD.
           MOVE     GMN-CCD       TO  TC-CCD.
      *           READ     TC-M          UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "EMSG-05" EMSG-05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-CCD
           END-IF
           MOVE     TC-NAME       TO  GMN-CNAM.
           MOVE     GMN-TCCD      TO  WK-TCCD.
           CALL "SD_Output" USING "DSP-BAR" DSP-BAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TCCD" ACT-TCCD "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-TNAM" DSP-TNAM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-CNAM" DSP-CNAM "p" RETURNING RESU.
           IF  TC-BIK          =  0
               CALL "SD_Output" USING
                "DSP-BIKHO-CLE" DSP-BIKHO-CLE "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-BIKHO" DSP-BIKHO "p" RETURNING RESU
           END-IF
           IF  GMN-TCD   NOT = 9850
               MOVE     ZERO      TO  GMN-TENC  WK-TENC
               MOVE     SPACE     TO  GMN-TENM
               CALL "SD_Output" USING
                "CLR-TEN" CLR-TEN "p" RETURNING RESU
               GO  TO  ACC-SSU
           END-IF
           CALL "SD_Output" USING
            "DSP-TENN" DSP-TENN "p" RETURNING RESU.
       ACC-TEN.
           CALL "SD_Accept" USING BY REFERENCE ACT-TENC "ACT-TENC"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-CCD
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-TEN
           END-IF
           IF  GMN-TENC  =  ZERO
               MOVE   SPACE      TO  WTNA-NAME
               GO  TO  ACC-TENA
           END-IF
           MOVE     GMN-TENC      TO  WTNA-KEY.
      *           READ     WTNAF         UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "EMSG-18" EMSG-18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-TEN
           END-IF
           IF  WTNA-OSN  =  0
               IF  GMN-CCD       =  003
                   CALL "SD_Output" USING
                    "EMSG-19" EMSG-19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO  ACC-TEN
               END-IF
           END-IF
           IF  WTNA-OSN  =  1
               IF  GMN-CCD       =  002
                   CALL "SD_Output" USING
                    "EMSG-19" EMSG-19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO  ACC-TEN
               END-IF
           END-IF.
       ACC-TENA.
           MOVE     WTNA-NAME     TO  GMN-TENM.
           MOVE     GMN-TENC      TO  WK-TENC.
           CALL "SD_Output" USING "DSP-TEN" DSP-TEN "p" RETURNING RESU.
       ACC-SSU.
           CALL "SD_Accept" USING BY REFERENCE ACT-SET "ACT-SET" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               IF  GMN-TCD   =   9850
                   GO  TO  ACC-TEN
               ELSE
                   GO  TO  ACC-TCD
               END-IF
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-SSU
           END-IF
           CALL "SD_Output" USING "DSP-SET" DSP-SET "p" RETURNING RESU.
       ACC-SET.
           MOVE     ZERO          TO  W-YDATE.
           MOVE     1             TO  CHKNO.
           MOVE     10            TO  AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE     1             TO  XX.
       ACC-HMAT.
           MOVE     GMN-HCD(XX)   TO  WK-HCD.
       ACC-HCD.
           MOVE     WK-HCD        TO  GMN-HCD(XX).
           CALL "SD_Output" USING "CLR-HCD" CLR-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "ACT-HCD" ACT-HCD "p" RETURNING RESU.
       ACC-HMAT-E.
           CALL "SD_Accept" USING BY REFERENCE ACT-HCD "ACT-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-BACK-R
           END-IF
           IF  ESTAT     =  "04"
               IF  XX    =  1
                   MOVE  WK-HCD  TO  GMN-HCD(XX)
                   CALL "SD_Output" USING
                    "EMSG-08" EMSG-08 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACT-HCD" ACT-HCD "p" RETURNING RESU
                   GO  TO  ACC-HMAT-E
               ELSE
                   MOVE    XX  TO  WW
                   GO  TO  ACC-ADV
               END-IF
           END-IF
           IF  ESTAT     =  "03"
               MOVE    XX  TO  WW
               GO  TO  ACC-ADV
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-HCD
           END-IF
           IF  JS-SIGN   =   0
               IF  ESTAT     =  "01"
                   IF  GMN-HCD(XX)     =  ZERO
                       IF  XX              >  1
                           IF  ZERO          =  GMN-SU(XX,1)
                                           AND  GMN-SU(XX,2)
                                           AND  GMN-SU(XX,3)
                                           AND  GMN-SU(XX,4)
                                           AND  GMN-SU(XX,5)
                                           AND  GMN-SU(XX,6)
                                           AND  GMN-SU(XX,7)
                                           AND  GMN-SU(XX,8)
                                           AND  GMN-SU(XX,9)
                                           AND  GMN-SU(XX,10)
                               COMPUTE  XXW     =   XX   -   1
                               MOVE   GMN-HCD(XXW)   TO  GMN-HCD(XX)
                               CALL "SD_Output" USING
                                "ACT-HCD" ACT-HCD "p" RETURNING RESU
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
       ACC-HNAM.
           IF  GMN-HCD(XX)  =    ZERO
               GO  TO  ACC-HMAT
           END-IF
           IF  GMN-HCD(XX)  >    999899
               GO  TO  ACC-HMAT
           END-IF
           MOVE     1             TO  WW.
       ACC-HNAM-R.
           IF  GMN-HCD(XX)  NOT  =   WK-HCD  AND
               WK-HCD       NOT  =   ZERO
               MOVE    GMN-HCD(XX)   TO  WK-HCD
               INITIALIZE            GMN-MEISAI(XX)
               MOVE    WK-HCD        TO  GMN-HCD(XX)
               CALL "SD_Output" USING
                "CLR-01" CLR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-02" CLR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-BIKHO" CLR-BIKHO "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-03" CLR-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACT-HCD" ACT-HCD "p" RETURNING RESU
           END-IF.
       ACC-HRED.
           MOVE     GMN-HCD(XX)   TO  HI-MHCD HI-HCD WK-HCD.
      *           READ     HI2-M         UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "EMSG-06" EMSG-06 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-HMAT
           END-IF
           MOVE     0             TO  HI-S4(10).
           MOVE     HI-NAME       TO  WK-NAM.
           MOVE     HI-BCD1       TO  GMN-BCD1(XX).
           MOVE     HI-BC3        TO  HINCD.
           MOVE     WK-NAM1       TO  GMN-HNAM1(XX).
           MOVE     WK-NAM2       TO  GMN-HNAM2(XX).
           CALL "SD_Output" USING "CLR-HCD" CLR-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "ACT-HCD" ACT-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HNAM" DSP-HNAM "p" RETURNING RESU.
           IF  JS-SIGN  = 0 AND  HIN-01  = 3
               GO  TO   ACC-SIZ
           END-IF
           IF  JS-SIGN  = 1 AND  HIN-01  NOT = 3
               GO  TO   ACC-SIZ
           END-IF
           CALL "SD_Output" USING "EMSG-07" EMSG-07 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU.
           GO  TO  ACC-HMAT.
       ACC-SIZ.
           CALL "SD_Accept" USING BY REFERENCE ACT-SIZ "ACT-SIZ" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-HMAT
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-SIZ
           END-IF
           IF  GMN-SIZ(XX) NOT = 1 AND 2 AND 3 AND 4
               GO TO  ACC-SIZ
           END-IF.
       ACC-SIZ1.
           MOVE     GMN-SIZ(XX)   TO  YY.
           MOVE    "OF"    TO     REV-SW.
           IF  GMN-SIZ(XX) = 1
               IF  HI-SS1   = ZERO
                   GO  TO  ACC-SIZ-ERR
               END-IF
           END-IF
           IF  GMN-SIZ(XX) = 2
               IF  HI-SS2   = ZERO
                   GO  TO  ACC-SIZ-ERR
               END-IF
           END-IF
           IF  GMN-SIZ(XX) = 3
               IF  HI-SS3   = ZERO
                   GO  TO  ACC-SIZ-ERR
               END-IF
           END-IF
           IF  GMN-SIZ(XX) = 4
               IF  HI-SS4   = ZERO
                   GO  TO  ACC-SIZ-ERR
               END-IF
           END-IF
           GO  TO   ACC-SIZE.
       ACC-SIZ-ERR.
           CALL "SD_Output" USING "EMSG-09" EMSG-09 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU.
           GO  TO   ACC-SIZ.
       ACC-SIZE.
           MOVE     1             TO  ZZ.
           MOVE     3             TO  BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
           IF  YY  =  1
               GO  TO  ACC-S1
           END-IF
           IF  YY  =  2
               GO  TO  ACC-S2
           END-IF
           IF  YY  =  3
               GO  TO  ACC-S3
           END-IF
           IF  YY  =  4
               GO  TO  ACC-S4
           END-IF.
       ACC-S1.
           IF  HI-S1(ZZ)     =   ZERO
               MOVE    0  TO     GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               IF  ZZ  =  10
                   GO  TO  ACC-KEI
               ELSE
                   ADD   1   TO  ZZ
                   IF  ZZ    =   10
                       ADD   7   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S1
                   ELSE
                       ADD   5   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S1
                   END-IF
               END-IF
           END-IF
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EX.
           MOVE     GMN-SU(XX ZZ)  TO  WK-SURYO.
           CALL "SD_Accept" USING BY REFERENCE ACT-SU1 "ACT-SU1"
            "S9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EX.
           IF  ESTAT     =  "09"
               GO  TO  ACC-B1
           END-IF
           IF  ESTAT     =  "04"
               MOVE  WK-SURYO    TO  GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               GO  TO  ACC-KEI-1
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-S1
           END-IF
           MOVE     GMN-SU(XX ZZ)  TO  WK-SURYO.
           CALL "SD_Output" USING "DSP-SU1" DSP-SU1 "p" RETURNING RESU.
           PERFORM  CHK-SU-RTN  THRU  CHK-SU-EX.
           IF  CHK-SW  =  1
               GO  TO  ACC-S1
           END-IF
           IF  ZZ  =     10
               GO   TO  ACC-KEI
           END-IF
           ADD      1             TO  ZZ.
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  +  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  +  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           GO   TO    ACC-S1.
       ACC-B1.
           IF  ZZ  =  1
               GO  TO  ACC-BS
           END-IF
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  -  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  -  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           COMPUTE     ZZ   =     ZZ  -  1.
       ACC-B1-R.
           IF  HI-S1(ZZ)     =   ZERO
               GO  TO  ACC-B1
           ELSE
               GO  TO  ACC-S1
           END-IF.
       ACC-S2.
           IF  HI-S2(ZZ)     =   ZERO
               MOVE    0  TO     GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               IF  ZZ  =  10
                   GO  TO  ACC-KEI
               ELSE
                   ADD   1   TO  ZZ
                   IF  ZZ    =   10
                       ADD   7   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S2
                   ELSE
                       ADD   5   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S2
                   END-IF
               END-IF
           END-IF
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EX.
           MOVE      GMN-SU(XX ZZ) TO  WK-SURYO.
           CALL "SD_Accept" USING BY REFERENCE ACT-SU1 "ACT-SU1"
            "S9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EX.
           IF  ESTAT     =  "09"
               GO  TO  ACC-B2
           END-IF
           IF  ESTAT     =  "04"
               MOVE  WK-SURYO    TO  GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               GO  TO  ACC-KEI-1
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-S2
           END-IF
           MOVE     GMN-SU(XX ZZ)  TO  WK-SURYO.
           CALL "SD_Output" USING "DSP-SU1" DSP-SU1 "p" RETURNING RESU.
           PERFORM  CHK-SU-RTN  THRU  CHK-SU-EX.
           IF  CHK-SW  =  1
               GO  TO  ACC-S2
           END-IF
           IF  ZZ  =     10
               GO  TO  ACC-KEI
           END-IF
           ADD      1             TO  ZZ.
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  +  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  +  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           GO   TO    ACC-S2.
       ACC-B2.
           IF  ZZ  =  1
               GO  TO  ACC-BS
           END-IF
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  -  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  -  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           COMPUTE     ZZ   =     ZZ  -  1.
       ACC-B2-R.
           IF  HI-S2(ZZ)     =   ZERO
               GO  TO  ACC-B2
           ELSE
               GO  TO  ACC-S2
           END-IF.
       ACC-S3.
           IF  HI-S3(ZZ)     =   ZERO
               MOVE    0  TO     GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               IF  ZZ  =  10
                   GO  TO  ACC-KEI
               ELSE
                   ADD   1   TO  ZZ
                   IF  ZZ    =   10
                       ADD   7   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S3
                   ELSE
                       ADD   5   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S3
                   END-IF
               END-IF
           END-IF
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EX.
           MOVE      GMN-SU(XX ZZ) TO  WK-SURYO.
           CALL "SD_Accept" USING BY REFERENCE ACT-SU1 "ACT-SU1"
            "S9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EX.
           IF  ESTAT     =  "09"
               GO  TO  ACC-B3
           END-IF
           IF  ESTAT     =  "04"
               MOVE  WK-SURYO    TO  GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               GO  TO  ACC-KEI-1
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-S3
           END-IF
           MOVE      GMN-SU(XX ZZ) TO  WK-SURYO.
           CALL "SD_Output" USING "DSP-SU1" DSP-SU1 "p" RETURNING RESU.
           PERFORM  CHK-SU-RTN  THRU  CHK-SU-EX.
           IF  CHK-SW  =  1
               GO  TO  ACC-S3
           END-IF
           IF  ZZ  =     10
               GO  TO  ACC-KEI
           END-IF
           ADD      1             TO  ZZ.
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  +  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  +  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           GO   TO    ACC-S3.
       ACC-B3.
           IF  ZZ  =  1
               GO  TO  ACC-BS
           END-IF
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  -  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  -  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           COMPUTE     ZZ   =     ZZ  -  1.
       ACC-B3-R.
           IF  HI-S3(ZZ)     =   ZERO
               GO  TO  ACC-B3
           ELSE
               GO  TO  ACC-S3
           END-IF.
       ACC-S4.
           IF  HI-S4(ZZ)     =   ZERO
               MOVE    0  TO     GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               IF  ZZ    =   10
                   GO  TO  ACC-KEI
               ELSE
                   ADD   1   TO  ZZ
                   IF  ZZ    =   10
                       ADD   7   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S4
                   ELSE
                       ADD   5   TO  BB
                       CALL "SD_Arg_Match_Col" USING
                        "BB" "2" BB RETURNING RESU
                       GO  TO  ACC-S4
                   END-IF
               END-IF
           END-IF
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EX.
           MOVE      GMN-SU(XX ZZ) TO  WK-SURYO.
           CALL "SD_Accept" USING BY REFERENCE ACT-SU1 "ACT-SU1"
            "S9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EX.
           IF  ESTAT     =  "09"
               GO  TO  ACC-B4
           END-IF
           IF  ESTAT     =  "04"
               MOVE  WK-SURYO    TO  GMN-SU(XX ZZ)
               CALL "SD_Output" USING
                "DSP-SU1" DSP-SU1 "p" RETURNING RESU
               GO  TO  ACC-KEI-1
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-S4
           END-IF
           MOVE      GMN-SU(XX ZZ) TO  WK-SURYO.
           CALL "SD_Output" USING "DSP-SU1" DSP-SU1 "p" RETURNING RESU.
           PERFORM  CHK-SU-RTN  THRU  CHK-SU-EX.
           IF  CHK-SW  =  1
               GO  TO  ACC-S4
           END-IF
           IF  ZZ  =  10
               GO  TO  ACC-KEI
           END-IF
           ADD      1             TO  ZZ.
           IF  ZZ  =  10
               ADD      7             TO  BB
               CALL "SD_Arg_Match_Col" USING
                "BB" "2" BB RETURNING RESU
           ELSE
               ADD      5             TO  BB
               CALL "SD_Arg_Match_Col" USING
                "BB" "2" BB RETURNING RESU
           END-IF
           GO   TO    ACC-S4.
       ACC-B4.
           IF  ZZ  =  1
               GO  TO  ACC-BS
           END-IF
           IF  ZZ  =  10
               COMPUTE    BB  =  BB  -  7
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           ELSE
               COMPUTE    BB  =  BB  -  5
               CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU
           END-IF
           COMPUTE     ZZ   =     ZZ  -  1.
       ACC-B4-R.
           IF  HI-S4(ZZ)     =   ZERO
               GO  TO  ACC-B4
           ELSE
               GO  TO  ACC-S4
           END-IF.
       ACC-BS.
           GO  TO  ACC-SIZ.
       ACC-KEI-1.
           MOVE     0    TO    CHK-SW.
           PERFORM  CHK-SU1-RTN  THRU  CHK-SU1-EX
                    VARYING   P  FROM  1  BY  1
                    UNTIL   ( P > 10 ) OR ( CHK-SW = 1 ).
           IF  CHK-SW =  1
               CALL "SD_Output" USING
                "EMSG-12" EMSG-12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    ACC-SIZE
           END-IF.
       ACC-KEI.
           MOVE     ZERO          TO  GMN-KEI(XX).
           MOVE     1             TO  ZZ.
       ACC-KEI-A.
           ADD      GMN-SU(XX  ZZ)    TO     GMN-KEI(XX).
           IF  ZZ  =     10
               GO  TO  ACC-KEI-D
           ELSE
               ADD   1   TO   ZZ
               GO  TO  ACC-KEI-A
           END-IF.
       ACC-KEI-D.
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
           IF  GMN-KEI(XX)     = ZERO
               CALL "SD_Output" USING
                "EMSG-14" EMSG-14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-SIZE
           END-IF
           CALL "SD_Output" USING "DSP-TAN" DSP-TAN "p" RETURNING RESU.
       ACC-NYMD.
           CALL "SD_Output" USING "DSP-NA" DSP-NA "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-NB" DSP-NB "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACT-NYY "ACT-NYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-SIZE-B
           END-IF
           IF  ESTAT NOT =  "01" AND "06" AND "00"
               GO  TO  ACC-NYMD
           END-IF
           MOVE  ZERO        TO  GMN-NYY1(XX).
           CALL "SD_Output" USING "DSP-NYY" DSP-NYY "p" RETURNING RESU.
       ACC-NMM.
           CALL "SD_Accept" USING BY REFERENCE ACT-NMM "ACT-NMM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-NYMD
           END-IF
           CALL "SD_Output" USING "DSP-NMM" DSP-NMM "p" RETURNING RESU.
           IF  GMN-NMM(XX)  =  00
               IF  GMN-NYY2(XX)   =  00
                   GO  TO  ACC-NDD
               END-IF
           END-IF
           IF  GMN-NMM(XX)  =  99
               IF  GMN-NYY2(XX)   =  99
                   GO  TO  ACC-NDD
               END-IF
           END-IF
           IF  GMN-NMM(XX)  <  1 OR   GMN-NMM(XX)   >   12
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-NMM
           END-IF.
       ACC-NDD.
           CALL "SD_Accept" USING BY REFERENCE ACT-NDD "ACT-NDD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-NMM
           END-IF
           CALL "SD_Output" USING "DSP-NDD" DSP-NDD "p" RETURNING RESU.
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-NDD
           END-IF
           IF  GMN-NDD(XX)  =  00
               IF  GMN-NMM(XX)  =  00
                   MOVE   ZERO    TO  GMN-NYY1(XX)
                   CALL "SD_Output" USING
                    "CLR-021" CLR-021 "p" RETURNING RESU
                   GO  TO  ACC-TANA
               END-IF
           END-IF
           IF  GMN-NDD(XX)  =  99
               IF  GMN-NMM(XX)  =  99
                   MOVE   99      TO  GMN-NYY1(XX)
                   GO  TO  ACC-TANA
               END-IF
           END-IF
           IF  GMN-NDD(XX)  <  1 OR  GMN-NDD(XX)    >   31
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACC-NDD
           END-IF
           MOVE   ZERO    TO  GMN-NYY1(XX).
           IF  GMN-NYY2(XX) >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO GMN-NYY(XX)
           END-IF
           IF  GMN-NYY2(XX) >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO GMN-NYY(XX)
           END-IF.
       ACC-TANA.
           IF  GMN-ACT         =   1
               IF  GMN-NYMD(XX)    NOT   =   ZERO  AND  "99999999"
                   IF  W-YDATE         =   ZERO
                       MOVE   GMN-NYMD(XX)    TO  W-YDATE
                       MOVE   XX              TO  CHKNO
                       MOVE   ALL "Å@"    TO  W-HTS
                       MOVE   GMN-NMM(XX)     TO  WK-NMM
                       MOVE   GMN-NDD(XX)     TO  WK-NDD
                       MOVE   WK-NMM          TO  W-TGET
                       MOVE   WK-NDD          TO  W-TPEY
                       MOVE   "Å^"          TO  W-TGP
                       MOVE   "îzíBéwíË"    TO  W-HTSM
                       MOVE   W-HTS           TO  GMN-TEKI1
                       CALL "SD_Output" USING
                        "ACT-TEKI1" ACT-TEKI1 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  GMN-NYMD(XX)    NOT   =   ZERO   AND   "99999999"
               IF  W-YDATE   NOT   =   GMN-NYMD(XX)  AND  ZERO
                   IF  CHKNO     >=   XX
                       MOVE   GMN-NYMD(XX)    TO  W-YDATE
                       MOVE   XX              TO  CHKNO
                       MOVE   ALL "Å@"    TO  W-HTS
                       MOVE   GMN-NMM(XX)     TO  WK-NMM
                       MOVE   GMN-NDD(XX)     TO  WK-NDD
                       MOVE   WK-NMM          TO  W-TGET
                       MOVE   WK-NDD          TO  W-TPEY
                       MOVE   "Å^"          TO  W-TGP
                       MOVE   "îzíBéwíË"    TO  W-HTSM
                       MOVE   W-HTS           TO  GMN-TEKI1
                       CALL "SD_Output" USING
                        "ACT-TEKI1" ACT-TEKI1 "p" RETURNING RESU
                   ELSE
                       CALL "SD_Output" USING
                        "EMSG-16" EMSG-16 "p" RETURNING RESU
                       CALL "SD_Output" USING
                       "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                       GO   TO   ACC-NYMD
                   END-IF
               END-IF
           END-IF
           IF     GMN-ACT  =  1
               IF  (GMN-NYMD(1)      =   ZERO   OR    "99999999") AND
                   (GMN-NYMD(2)      =   ZERO   OR    "99999999") AND
                   (GMN-NYMD(3)      =   ZERO   OR    "99999999") AND
                   (GMN-NYMD(4)      =   ZERO   OR    "99999999") AND
                   (GMN-NYMD(5)      =   ZERO   OR    "99999999") AND
                   (GMN-NYMD(6)      =   ZERO   OR    "99999999")
                   MOVE   ALL "Å@"    TO  W-HTS
                   MOVE   W-HTS           TO  GMN-TEKI1
                   CALL "SD_Output" USING
                   "ACT-TEKI1" ACT-TEKI1 "p" RETURNING RESU
               END-IF
           END-IF
           PERFORM  TAN-RTN    THRU  TAN-EX.
           CALL "SD_Output" USING "DSP-TAN" DSP-TAN "p" RETURNING RESU.
           IF  TC-BIK    NOT =  ZERO
               GO  TO  ACC-BIKHO
           END-IF
           MOVE  SPACE        TO  GMN-BIKHO(XX).
           CALL "SD_Output" USING
            "ACT-BIKHO" ACT-BIKHO "p" RETURNING RESU.
           GO  TO  ACC-TAN.
       ACC-BIKHO.
           CALL "SD_Accept" USING BY REFERENCE ACT-BIKHO "ACT-BIKHO"
            "X" "10" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-NYMD
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-BIKHO
           END-IF.
       ACC-TAN.
           CALL "SD_Accept" USING BY REFERENCE ACT-TAN "ACT-TAN" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               IF  TC-BIK       =  ZERO
                   GO  TO  ACC-NYMD
               ELSE
                   GO  TO  ACC-BIKHO
               END-IF
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-TAN
           END-IF
           CALL "SD_Output" USING "DSP-TAN" DSP-TAN "p" RETURNING RESU.
       ACC-TAN1.
           COMPUTE    GMN-KIN(XX) =  GMN-KEI(XX) * GMN-TAN(XX).
           CALL "SD_Output" USING "DSP-KIN" DSP-KIN "p" RETURNING RESU.
       ACC-SEL.
           IF  XX    =   6
               GO  TO  ACC-GKEI
           ELSE
               ADD   1   TO  XX
               ADD   2   TO  AA
               CALL "SD_Arg_Match_Line" USING
                "AA" "2" AA RETURNING RESU
               MOVE  GMN-HCD(XX) TO  WK-HCD
               GO  TO  ACC-HMAT
           END-IF.
       ACC-GKEI.
           MOVE     ZERO          TO  GMN-GKEI  GMN-GKIN.
           MOVE     1             TO  WW.
       ACC-GKEI-A.
           ADD      GMN-KEI(WW)   TO  GMN-GKEI.
           ADD      GMN-KIN(WW)   TO  GMN-GKIN.
           IF  WW  =     6
               GO  TO  ACC-GKEI-D
           ELSE
               ADD   1       TO  WW
               GO  TO  ACC-GKEI-A
           END-IF.
       ACC-GKEI-D.
           CALL "SD_Output" USING
            "DSP-GKEI" DSP-GKEI "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-GKIN" DSP-GKIN "p" RETURNING RESU.
       ACC-TEKI.
           CALL "SD_Accept" USING BY REFERENCE ACT-TEKI1 "ACT-TEKI1"
            "N" "18" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-BACK
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-TEKI
           END-IF
           IF  GMN-TEKI1  =  SPACE
               MOVE  ALL "Å@"     TO  GMN-TEKI1
           END-IF.
       ACC-TEKI2.
           CALL "SD_Accept" USING BY REFERENCE ACT-TEKI2 "ACT-TEKI2"
            "N" "18" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               GO  TO  ACC-TEKI
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-TEKI2
           END-IF
           IF  GMN-TEKI2  =  SPACE
               MOVE  ALL "Å@"     TO  GMN-TEKI2
           END-IF.
       ACC-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACT-OKC "ACT-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT     =  "09"
               IF  GMN-ACT NOT =  3
                   GO  TO  ACC-TEKI2
               ELSE
                   GO  TO  ACC-JNOD
               END-IF
           END-IF
           IF  ESTAT NOT =  "01" AND "06"
               GO  TO  ACC-OKC
           END-IF
           IF  GMN-OKC   NOT  =  1   AND  9
               GO  TO  ACC-OKC
           END-IF
           IF  GMN-OKC   =  9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  ACC-END
           ELSE
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
           END-IF
      *****∫≥º› ºÆÿ
           PERFORM  UPD-RTN   THRU  UPD-EX.
       ACC-END.
           PERFORM JDCON-RTN  THRU  JDCON-EX.
           IF  JS-SIGN      =  0
               IF  GMN-ACT      =  1
                   MOVE  GMN-TEKI2     TO  W-TEKI2
               END-IF
           END-IF
           IF  JS-SIGN      =  1
               IF  GMN-ACT      =  1
                   MOVE  GMN-JYMD      TO  W-EDATE
               ELSE
                   MOVE  ZERO          TO  W-EDATE
               END-IF
           END-IF
           CALL "SD_Output" USING
            "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ010I" RETURNING RESU
           CALL "SD_Output" USING "DSP-S0" DSP-S0 "p" RETURNING RESU.
           INITIALIZE        WK-AREA  GMN-AREA.
           MOVE     5             TO  GMN-OKC.
           IF  JS-SIGN   =  0
               CALL "SD_Output" USING
                "DSP-S1" DSP-S1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN   =  1
               CALL "SD_Output" USING
                "DSP-S2" DSP-S2 "p" RETURNING RESU
           END-IF
           GO   TO   ACCEPT-RTN.
       ACC-SIZE-B.
           MOVE     10            TO  ZZ.
           MOVE     50            TO  BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
       ACC-SIZE-B-R.
           IF  YY  =  1
               IF  HI-SS1   =    ZERO
                   GO  TO  ACC-SIZ
               ELSE
                   IF  HI-S1(ZZ)     NOT =  ZERO
                       GO  TO  ACC-S1
                   ELSE
                       PERFORM  SET-CL-RTN  THRU  SET-CL-EX
                       GO  TO  ACC-SIZE-B-R
                   END-IF
               END-IF
           END-IF
           IF  YY  =  2
               IF  HI-SS2   =    ZERO
                   GO  TO  ACC-SIZ
               ELSE
                   IF  HI-S2(ZZ)     NOT =  ZERO
                       GO  TO  ACC-S2
                   ELSE
                       PERFORM  SET-CL-RTN  THRU  SET-CL-EX
                       GO  TO  ACC-SIZE-B-R
                   END-IF
               END-IF
           END-IF
           IF  YY  =  3
               IF  HI-SS3   =    ZERO
                   GO  TO  ACC-SIZ
               ELSE
                   IF  HI-S3(ZZ)     NOT =  ZERO
                       GO  TO  ACC-S3
                   ELSE
                       PERFORM  SET-CL-RTN  THRU  SET-CL-EX
                       GO  TO  ACC-SIZE-B-R
                   END-IF
               END-IF
           END-IF
           IF  YY  NOT =  4
               GO  TO  ACC-SIZE-B-R
           END-IF
           IF  HI-SS4   =    ZERO
               GO  TO  ACC-SIZ
           END-IF
           IF  HI-S4(ZZ)     =  ZERO
               PERFORM  SET-CL-RTN  THRU  SET-CL-EX
               GO  TO  ACC-SIZE-B-R
           END-IF
           GO  TO  ACC-S4.
       ACC-ADV.
           MOVE 1   TO    I.
           MOVE WW  TO    XX.
       ACC-ADV1.
           IF  I   NOT = XX
               IF  GMN-HCD(I) NOT = ZERO
                   GO  TO  ACC-ADV-L
               END-IF
           END-IF
           ADD  1   TO  I.
           IF  ESTAT     = "03"
               IF   I NOT > 6
                    GO  TO  ACC-ADV1
               END-IF
           END-IF
           IF  ESTAT     = "04"
               IF   I NOT > XX
                    GO  TO  ACC-ADV1
               END-IF
           END-IF
           MOVE   WK-HCD    TO  GMN-HCD(XX).
           CALL "SD_Output" USING "ADV-ERR" ADV-ERR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-HCD" CLR-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "ACT-HCD" ACT-HCD "p" RETURNING RESU.
           GO  TO  ACC-HMAT.
       ACC-ADV-L.
           IF  SW-ZERO   =  SPACE
               GO  TO  ACC-CLR
           END-IF
           IF  WS-CHECK(XX) = "01"
               MOVE   WW    TO  XX
               MOVE   WK-HCD     TO  GMN-HCD(XX)
               CALL "SD_Output" USING
                "ADV-ERR" ADV-ERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-HCD" CLR-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACT-HCD" ACT-HCD "p" RETURNING RESU
               GO  TO  ACC-HMAT
           END-IF
           IF  ESTAT    =  "03"
               GO  TO  ACC-CLR
           END-IF
           IF  XX  =  6
               GO  TO  ACC-CLR
           END-IF
           ADD      1             TO  XX.
           GO   TO   ACC-ADV-L.
       ACC-CLR.
           MOVE     WW            TO  XX.
       ACC-CLR-R.
           INITIALIZE             GMN-MEISAI(XX).
           IF  ESTAT    =  "03"
               GO  TO  ACC-03
           END-IF
           IF  XX  =    6
               GO  TO  ACC-CLR-D
           END-IF
           ADD      1             TO  XX.
           GO  TO   ACC-CLR-R.
       ACC-CLR-D.
           MOVE WW  TO  XX.
       ACC-CLR-D1.
           COMPUTE  AA  =  XX * 2 +  8.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-BIKHO" CLR-BIKHO "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-03" CLR-03 "p" RETURNING RESU.
           ADD   1  TO  XX.
           IF  XX  NOT >   6
               GO  TO  ACC-CLR-D1
           END-IF
           CALL "SD_Output" USING
            "CLR-GKEI" CLR-GKEI "p" RETURNING RESU.
           COMPUTE  XX   =    WW  -   1.
           COMPUTE  AA   =    AA  -   2.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           GO   TO  ACC-GKEI.
       ACC-03.
           MOVE     WW  TO    XX.
           COMPUTE  AA  =  XX * 2 +  8.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-BIKHO" CLR-BIKHO "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-03" CLR-03 "p" RETURNING RESU.
           IF  WW  =   6
               GO  TO  ACC-GKEI
           END-IF
           ADD  1   TO  WW XX.
           COMPUTE  AA  =  XX * 2 +  8.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           GO   TO   ACC-HMAT.
       ACC-BACK.
           MOVE     6   TO XX.
       ACC-BACK1.
           IF  GMN-HCD(XX) NOT = ZERO
               COMPUTE AA  =  XX * 2 + 8
               CALL "SD_Arg_Match_Line" USING
                "AA" "2" AA RETURNING RESU
               GO  TO  ACC-HMAT
           END-IF
           IF  XX  =  1
               GO  TO  ACC-BACK-R
           END-IF
           COMPUTE  XX  =  XX - 1.
           GO   TO   ACC-BACK1.
       ACC-BACK-R.
           IF  XX  =  1
               IF  GMN-ACT  =  1  OR  2
                   GO  TO  ACC-SSU
               ELSE
                   IF  GMN-ACT  =  3
                       GO  TO  ACC-JNOD
                   ELSE
                       GO  TO  ACC-TCD
                   END-IF
               END-IF
           ELSE
               COMPUTE     XX  =  XX   -   1
               COMPUTE     AA  =  AA   -   2
               CALL "SD_Arg_Match_Line" USING
                "AA" "2" AA RETURNING RESU
           END-IF
           GO  TO  ACC-HMAT.
       ACCEPT-EX.
           EXIT.
      **
       SET-CL-RTN.
           IF  ZZ  =   10
               IF  YY  NOT =   4
                   COMPUTE BB  =  BB  -  7
                   CALL "SD_Arg_Match_Col" USING
                    "BB" "2" BB RETURNING RESU
               ELSE
                   COMPUTE BB  =  BB  -  5
                   CALL "SD_Arg_Match_Col" USING
                    "BB" "2" BB RETURNING RESU
               END-IF
           ELSE
               COMPUTE BB  =  BB  -  5
               CALL "SD_Arg_Match_Col" USING
                "BB" "2" BB RETURNING RESU
           END-IF
           COMPUTE   ZZ  =   ZZ  -   1.
       SET-CL-EX.
           EXIT.
      *************************
      *    âÊñ ÉNÉäÉAÅ[       *
      *************************
       CLR-RTN.
           CALL "SD_Output" USING "CLR-JNO" CLR-JNO "p" RETURNING RESU.
       CLR-001.
           CALL "SD_Output" USING
            "CLR-HEAD" CLR-HEAD "p" RETURNING RESU.
       CLR-005.
           PERFORM    REV-CLE-RTN  THRU  REV-CLE-EX.
           MOVE       10  TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE       1   TO    XX.
       CLR-010.
           COMPUTE    AA  =   XX * 2 + 8.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-BIKHO" CLR-BIKHO "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-03" CLR-03 "p" RETURNING RESU.
           ADD        1   TO    XX.
           IF  XX  NOT > 6
               GO  TO  CLR-010
           END-IF
           CALL "SD_Output" USING
            "CLR-GKEI" CLR-GKEI "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-END" CLR-END "p" RETURNING RESU.
       CLR-EX.
           EXIT.
      *************************************************
      *    êîó É`ÉFÉbÉNÅ@Å@Å@ÇbÇgÇjÅ|ÇrÇtÅ|ÇqÇsÇm     *
      *************************************************
       CHK-SU-RTN.
           MOVE     0         TO    CHK-SW.
           IF  GMN-SET  =    ZERO
               GO  TO  CHK-SU-100
           END-IF
           IF  GMN-ACT  =    1  OR  2
               CONTINUE
           ELSE
               GO  TO  CHK-SU-100
           END-IF
           IF  GMN-ACT  =    1
               DIVIDE   GMN-SET INTO  GMN-SU(XX ZZ)
                        GIVING  WK-ANS    REMAINDER  WK-AMARI
           END-IF
           IF  GMN-ACT  =    2
               DIVIDE   GMN-SET INTO  WS-SU(XX ZZ)
                        GIVING  WK-ANS    REMAINDER  WK-AMARI
           END-IF
           IF  WK-AMARI NOT =  0
               CALL "SD_Output" USING
                "EMSG-12" EMSG-12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               MOVE   1  TO    CHK-SW
           END-IF
           GO  TO   CHK-SU-EX.
       CHK-SU-100.
           IF  GMN-ACT  NOT =  3
               GO  TO  CHK-SU-EX
           END-IF
           IF  GMN-SU(XX ZZ) > WS-ZAN(XX ZZ)
               CALL "SD_Output" USING
                "EMSG-13" EMSG-13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               MOVE   1  TO    CHK-SW
           END-IF.
       CHK-SU-EX.
           EXIT.
      **
       CHK-SU1-RTN.
           IF  GMN-SET  =    ZERO
               GO  TO  CHK-SU1-EX
           END-IF
           IF  GMN-ACT  =    1  OR  2
               CONTINUE
           ELSE
               GO  TO  CHK-SU1-EX
           END-IF
           IF  GMN-ACT  =    1
               DIVIDE   GMN-SET INTO  GMN-SU(XX P)
                        GIVING  WK-ANS    REMAINDER  WK-AMARI
           END-IF
           IF  GMN-ACT  =    2
               DIVIDE   GMN-SET INTO  WS-SU(XX P)
                        GIVING  WK-ANS    REMAINDER  WK-AMARI
           END-IF
           IF  WK-AMARI NOT =  0
               MOVE   1  TO    CHK-SW
           END-IF.
       CHK-SU1-EX.
           EXIT.
      *************************************************
      *    âÊñ Å@ï\é¶        ÇgÇxÇnÇiÇhÅ|ÇqÇsÇm       *
      *************************************************
       HYOJI-RTN.
           IF  WW    =   7
               MOVE  10          TO  AA
               CALL "SD_Arg_Match_Line" USING
                "AA" "2" AA RETURNING RESU
               GO  TO  HYO-DSP
           END-IF
           INITIALIZE             GMN-HEAD  GMN-DATA  SAVE-AREA.
           MOVE     JMSTD-02      TO  GMN-JYMD WS-JYMD.
           MOVE     JMSTD-04      TO  GMN-TCD.
           MOVE     JMSTD-10      TO  GMN-CCD.
           MOVE     JMSTD-23      TO  GMN-TENC.
           MOVE     JMSTD-20      TO  WS-SEQ.
           MOVE     GMN-TCD       TO  TC-TCD.
           MOVE     001           TO  TC-CCD.
      *           READ     TC-M          UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  TC-NAME
           END-IF
           MOVE     TC-NAME       TO  GMN-TNAM.
           MOVE     GMN-TCD       TO  TC-TCD.
           MOVE     GMN-CCD       TO  TC-CCD.
      *           READ     TC-M          UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  TC-NAME
           END-IF
           MOVE     TC-NAME       TO  GMN-CNAM.
           IF  GMN-TCD     NOT =  9850
               GO  TO  HYO-MID
           END-IF
           MOVE     GMN-TENC      TO  WTNA-KEY.
      *           READ     WTNAF         UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  WTNA-NAME
           END-IF
           MOVE     WTNA-NAME     TO  GMN-TENM.
       HYO-MID.
           CALL "SD_Output" USING
            "ACT-JYMD" ACT-JYMD "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-BAR" DSP-BAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TCCD" ACT-TCCD "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-TNAM" DSP-TNAM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-CNAM" DSP-CNAM "p" RETURNING RESU.
           IF  GMN-TCD         =  9850
               CALL "SD_Output" USING
                "DSP-TEN" DSP-TEN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "CLR-TEN" CLR-TEN "p" RETURNING RESU
           END-IF
           IF  TC-BIK          =  0
               CALL "SD_Output" USING
                "DSP-BIKHO-CLE" DSP-BIKHO-CLE "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-BIKHO" DSP-BIKHO "p" RETURNING RESU
           END-IF
           MOVE     WK-KEY        TO  JMSTD-KEY1.
      *           START    JMSTD    KEY  NOT <     JMSTD-KEY1   INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  HYO-READ
           END-IF.
           MOVE     ZERO          TO  XX  WK-HCD.
           MOVE     ZERO          TO  WK-SU.
       HYO-READ.
      *           READ     JMSTD    NEXT UNLOCK    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  HYO-END
           END-IF
           IF  GMN-JNO   NOT  =  JMSTD-07
               GO  TO  HYO-END
           END-IF
           MOVE     JMSTD-08      TO  XX.
           MOVE     JMSTD-09      TO  YY  GMN-SIZ(XX)  WS-SIZ(XX).
           MOVE     JMSTD-05      TO  WS-HCD(XX)  GMN-HCD(XX)
                                      WK-HCD  HI-MHCD  HI-HCD.
      *           READ     HI2-M         UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  HI-NAME
           END-IF
           MOVE     HI-NAME       TO  WK-NAM.
           MOVE     WK-NAM1       TO  WS-HNAM1(XX)  GMN-HNAM1(XX).
           MOVE     WK-NAM2       TO  WS-HNAM2(XX)  GMN-HNAM2(XX).
           MOVE     JMSTD-06      TO  WS-NYMD(XX)   GMN-NYMD(XX).
           MOVE     JMSTD-17      TO  WS-TAN(XX)    GMN-TAN(XX).
           MOVE     JMSTD-22      TO  WS-BIKHO(XX)  GMN-BIKHO(XX).
           MOVE     JMSTD-16      TO  WS-SET        GMN-SET.
           MOVE     JMSTD-13      TO  GMN-TEKI.
           MOVE     1             TO  ZZ.
           MOVE     0             TO  WS-KEI(XX).
       HYO-LOOP.
           IF ( JMSTD-1211(ZZ)  NOT  =    0     )
             OR ( JMSTD-141 (ZZ)  NOT  =    0     )
               OR ( JMSTD-151 (ZZ)  NOT  =    0     )
               MOVE    "01"    TO   WS-CHECK(XX)
           END-IF
           MOVE     JMSTD-1111(ZZ)    TO  WS-SU(XX ZZ).
           MOVE     JMSTD-1211(ZZ)    TO  WS-1211(XX ZZ).
           MOVE     JMSTD-141 (ZZ)    TO  WS-141 (XX ZZ).
           MOVE     JMSTD-151 (ZZ)    TO  WS-151 (XX ZZ).
           COMPUTE  GMN-SU(XX ZZ) =  JMSTD-1111(ZZ) - JMSTD-151(ZZ)
                                  -  JMSTD-1211(ZZ) - JMSTD-141(ZZ).
           ADD      GMN-SU(XX ZZ)     TO  WS-KEI(XX)  GMN-KEI(XX)
                                          WS-GKEI     GMN-GKEI.
           MOVE     GMN-SU(XX ZZ)     TO  WS-ZAN(XX ZZ).
           ADD      JMSTD-1111(ZZ)    TO  WK-SU1.
           ADD      JMSTD-151(ZZ)     TO  WK-SU2.
           IF  ZZ  =  10
               COMPUTE   GMN-KIN(XX) =   GMN-KEI(XX) * GMN-TAN(XX)
               ADD       GMN-KIN(XX) TO  GMN-GKIN
               GO  TO    HYO-READ
           END-IF
           ADD      1             TO  ZZ.
           GO   TO   HYO-LOOP.
       HYO-END.
           COMPUTE   WK-SU4  =  WK-SU1  -  WK-SU2.
           IF  GMN-SET =  ZERO
               MOVE    ZERO  TO   WK-SU1
           ELSE
               COMPUTE   WK-SU1  =  WK-SU1  / GMN-SET
           END-IF
           IF  WK-SU1  =  0
               MOVE    0  TO   WK-SU3
           ELSE
               COMPUTE WK-SU3  =  WK-SU4 / WK-SU1
           END-IF
           MOVE     10            TO  AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE     1             TO  XX.
           CALL "SD_Output" USING
            "DSP-HEAD" DSP-HEAD "p" RETURNING RESU.
       HYO-DSP.
           IF  XX  >   6
               GO  TO  HYO-GOKEI
           END-IF
           IF  GMN-HCD(XX)   =   ZERO
               CALL "SD_Output" USING
                "CLR-01" CLR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-02" CLR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-BIKHO" CLR-BIKHO "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-03" CLR-03 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "ACT-HCD" ACT-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-HNAM" DSP-HNAM "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACT-SIZ" ACT-SIZ "p" RETURNING RESU
           END-IF.
       HYO-SIZE.
       HYO-S-R.
           MOVE     1             TO  ZZ.
           MOVE     3             TO  BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
       HYO-SURYO.
           CALL "SD_Output" USING "DSP-SU1" DSP-SU1 "p" RETURNING RESU.
           IF  ZZ  NOT =  10
               GO  TO  HYO-SURYO1
           END-IF
           IF  GMN-HCD(XX) NOT = ZERO
               IF  GMN-NYMD(XX) NOT = ZERO
                   CALL "SD_Output" USING
                    "DSP-NYMD" DSP-NYMD "p" RETURNING RESU
               END-IF
           ELSE
               CALL "SD_Output" USING
                "DSP-NA" DSP-NA "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-NB" DSP-NB "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACT-NYMD" ACT-NYMD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "ACT-BIKHO" ACT-BIKHO "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-TAN" DSP-TAN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-KIN" DSP-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
           ADD      2      TO  AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           ADD      1      TO  XX.
           GO  TO  HYO-DSP.
       HYO-SURYO1.
           ADD      1             TO  ZZ.
           IF  ZZ  =  10
               ADD    7      TO  BB
               CALL "SD_Arg_Match_Col" USING
                "BB" "2" BB RETURNING RESU
           ELSE
               ADD    5      TO  BB
               CALL "SD_Arg_Match_Col" USING
                "BB" "2" BB RETURNING RESU
           END-IF
           GO   TO   HYO-SURYO.
       HYO-GOKEI.
           IF  WS-CHECK(01)  =   SPACE    AND
               WS-CHECK(02)  =   SPACE    AND
               WS-CHECK(03)  =   SPACE    AND
               WS-CHECK(04)  =   SPACE    AND
               WS-CHECK(05)  =   SPACE    AND
               WS-CHECK(06)  =   SPACE
               MOVE    SPACE     TO       SW-ZERO
           ELSE
               MOVE    "01"      TO       SW-ZERO
           END-IF
           CALL "SD_Output" USING "DSP-END" DSP-END "p" RETURNING RESU.
           MOVE     1             TO  XX.
           MOVE     10            TO  AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TEKI1" ACT-TEKI1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-TEKI2" ACT-TEKI2 "p" RETURNING RESU.
       HYOJI-EX.
           EXIT.
      ************************************************
      *    îrëºêßå‰ÇeÅ@çÌèú  ÇiÇcÇbÇnÇmÅ|ÇqÇsÇm      *
      ************************************************
       JDCON-RTN.
           IF  GMN-ACT   =  1
               GO  TO  JDCON-EX
           END-IF
           MOVE     1             TO  JDCON-01.
           MOVE     GMN-JNO       TO  JDCON-02.
      *           READ     JDCON         INVALID
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID" JDCON_PNAME1 BY REFERENCE JDCON-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  JDCON-EX
           END-IF
      *           DELETE   JDCON                   INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JDCON_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"         TO  ERR-M
               MOVE  "JDCON"     TO  ERR-F
               MOVE  JDCON-KEY   TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       JDCON-EX.
           EXIT.
      *************************************************
      *    êîó ÅiÉTÉCÉYï ÅjÇyÇdÇqÇnÅ@É`ÉFÉbÉN         *
      *************************************************
       SUCHK-RTN.
           MOVE  0         TO  SU-SW.
       SUCHK-010.
           IF  GMN-SU (XX 1) = ZERO AND GMN-SU (XX 2) = ZERO AND
               GMN-SU (XX 3) = ZERO AND GMN-SU (XX 4) = ZERO AND
               GMN-SU (XX 5) = ZERO AND GMN-SU (XX 6) = ZERO AND
               GMN-SU (XX 7) = ZERO AND GMN-SU (XX 8) = ZERO AND
               GMN-SU (XX 9) = ZERO AND GMN-SU (XX 10) = ZERO
               GO  TO  SUCHK-EX
           END-IF
           MOVE  1         TO  SU-SW.
       SUCHK-EX.
           EXIT.
      *************************************************
      *    èIóπÅ@èàóùÅ@Å@Å@Å@ÇdÇmÇcÅ|ÇqÇsÇm           *
      *************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JDCON_IDLST JDCON_PNAME1.
       END-EX.
           EXIT.
      **   ÿ ﬁ∞Ω ÀÆ≥ºﬁ **
       REV-DSP-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
           IF  YY  =  1
               IF  ZZ  =  1
                   CALL "SD_Output" USING
                    "DSP-RR101" DSP-RR101 "p" RETURNING RESU
               ELSE
                   IF  ZZ  =  2
                       CALL "SD_Output" USING
                        "DSP-RR102" DSP-RR102 "p" RETURNING RESU
                   ELSE
                       IF  ZZ  =  3
                           CALL "SD_Output" USING
                            "DSP-RR103" DSP-RR103 "p" RETURNING RESU
                       ELSE
                           IF  ZZ  =  4
                               CALL "SD_Output" USING
                                "DSP-RR104" DSP-RR104 "p" RETURNING RESU
                           ELSE
                               IF  ZZ  =  5
                               CALL "SD_Output" USING
                                "DSP-RR105" DSP-RR105 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  6
                               CALL "SD_Output" USING
                                "DSP-RR106" DSP-RR106 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  7
                               CALL "SD_Output" USING
                                "DSP-RR107" DSP-RR107 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  8
                               CALL "SD_Output" USING
                                "DSP-RR108" DSP-RR108 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  9
                               CALL "SD_Output" USING
                                "DSP-RR109" DSP-RR109 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  10
                               CALL "SD_Output" USING
                                "DSP-RR110" DSP-RR110 "p" RETURNING RESU
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
           IF  YY  =  2
               IF  ZZ  =  1
                   CALL "SD_Output" USING
                    "DSP-RR201" DSP-RR201 "p" RETURNING RESU
               ELSE
                   IF  ZZ  =  2
                       CALL "SD_Output" USING
                        "DSP-RR202" DSP-RR202 "p" RETURNING RESU
                   ELSE
                       IF  ZZ  =  3
                           CALL "SD_Output" USING
                            "DSP-RR203" DSP-RR203 "p" RETURNING RESU
                       ELSE
                           IF  ZZ  =  4
                               CALL "SD_Output" USING
                                "DSP-RR204" DSP-RR204 "p" RETURNING RESU
                           ELSE
                               IF  ZZ  =  5
                               CALL "SD_Output" USING
                                "DSP-RR205" DSP-RR205 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  6
                               CALL "SD_Output" USING
                                "DSP-RR206" DSP-RR206 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  7
                               CALL "SD_Output" USING
                                "DSP-RR207" DSP-RR207 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  8
                               CALL "SD_Output" USING
                                "DSP-RR208" DSP-RR208 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  9
                               CALL "SD_Output" USING
                                "DSP-RR209" DSP-RR209 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  10
                               CALL "SD_Output" USING
                                "DSP-RR210" DSP-RR210 "p" RETURNING RESU
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
           IF  YY  =  3
               IF  ZZ  =  1
                   CALL "SD_Output" USING
                    "DSP-RR301" DSP-RR301 "p" RETURNING RESU
               ELSE
                   IF  ZZ  =  2
                       CALL "SD_Output" USING
                        "DSP-RR302" DSP-RR302 "p" RETURNING RESU
                   ELSE
                       IF  ZZ  =  3
                           CALL "SD_Output" USING
                            "DSP-RR303" DSP-RR303 "p" RETURNING RESU
                       ELSE
                           IF  ZZ  =  4
                               CALL "SD_Output" USING
                                "DSP-RR304" DSP-RR304 "p" RETURNING RESU
                           ELSE
                               IF  ZZ  =  5
                               CALL "SD_Output" USING
                                "DSP-RR305" DSP-RR305 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  6
                               CALL "SD_Output" USING
                                "DSP-RR306" DSP-RR306 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  7
                               CALL "SD_Output" USING
                                "DSP-RR307" DSP-RR307 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  8
                               CALL "SD_Output" USING
                                "DSP-RR308" DSP-RR308 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  9
                               CALL "SD_Output" USING
                                "DSP-RR309" DSP-RR309 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  10
                               CALL "SD_Output" USING
                                "DSP-RR310" DSP-RR310 "p" RETURNING RESU
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
           IF  YY  =  4
               IF  ZZ  =  1
                   CALL "SD_Output" USING
                    "DSP-RR401" DSP-RR401 "p" RETURNING RESU
               ELSE
                   IF  ZZ  =  2
                       CALL "SD_Output" USING
                        "DSP-RR402" DSP-RR402 "p" RETURNING RESU
                   ELSE
                       IF  ZZ  =  3
                           CALL "SD_Output" USING
                            "DSP-RR403" DSP-RR403 "p" RETURNING RESU
                       ELSE
                           IF  ZZ  =  4
                               CALL "SD_Output" USING
                                "DSP-RR404" DSP-RR404 "p" RETURNING RESU
                           ELSE
                               IF  ZZ  =  5
                               CALL "SD_Output" USING
                                "DSP-RR405" DSP-RR405 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  6
                               CALL "SD_Output" USING
                                "DSP-RR406" DSP-RR406 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  7
                               CALL "SD_Output" USING
                                "DSP-RR407" DSP-RR407 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  8
                               CALL "SD_Output" USING
                                "DSP-RR408" DSP-RR408 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  9
                               CALL "SD_Output" USING
                                "DSP-RR409" DSP-RR409 "p" RETURNING RESU
                               ELSE
                               IF  ZZ  =  10 
                               CALL "SD_Output" USING
                                "DSP-RR410" DSP-RR410 "p" RETURNING RESU
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
           END-IF.
       REV-DSP-EX.
           EXIT.
       REV-CLE-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
       REV-CLE-EX.
           EXIT.
      *****çXêVÅ@èàóù
       UPD-RTN.
           IF  GMN-ACT  NOT = 1
               ADD   1  TO    WS-SEQ
           END-IF
           PERFORM  UP1-RTN  THRU  UP1-EX.
           PERFORM  UP2-RTN  THRU  UP2-EX.
           PERFORM  UP6-RTN  THRU  UP6-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       UPD-EX.
           EXIT.
      ******************************
      *    ÉRÉìÉgÉçÅ[ÉãÇeÅ@çXêV    *
      ******************************
       UP1-RTN.
      *****  éÛíçáÇ  *****
           MOVE    SPACE   TO     SW-INV.
           IF  GMN-ACT NOT =  1
               GO  TO  UP1-EX
           END-IF
           MOVE    1       TO     JCON1-01.
           MOVE    1       TO     JCON1-02.
      *           READ    JCON    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               INITIALIZE     JCON1-R
               MOVE    1  TO  JCON1-01
               MOVE    1  TO  JCON1-02
               MOVE  "01" TO  SW-INV
           END-IF.
       UP1-010.
           IF      JS-SIGN NOT =  0
                   GO  TO  UP1-020
           END-IF
           ADD     1       TO     JCON1-03.
           IF  JCON1-03    =  200000
               MOVE  100001   TO  JCON1-03
           END-IF
           IF  SW-INV    =   "01"
               MOVE  300001   TO  JCON1-04
           END-IF
           MOVE    ZERO           TO  JMSTD-KEY1.
           MOVE    JCON1-03       TO  JMSTD-07 GMN-JNO.
           GO  TO  UP1-030.
       UP1-020.
           ADD     1       TO     JCON1-04.
           IF  JCON1-04    =  400000
               MOVE  300001   TO  JCON1-04
           END-IF
           IF  SW-INV    =   "01"
               MOVE  100001   TO  JCON1-03
           END-IF
           MOVE    ZERO           TO  JMSTD-KEY1.
           MOVE    JCON1-04       TO  JMSTD-07 GMN-JNO.
       UP1-030.
      *           START   JMSTD  KEY NOT <   JMSTD-KEY1   INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-040
           END-IF
      *           READ    JMSTD  NEXT UNLOCK AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-040
           END-IF
           IF  JS-SIGN   =    0
               IF  JCON1-03  =    JMSTD-07
                   GO  TO  UP1-010
               END-IF
           END-IF
           IF  JS-SIGN   =    1
               IF  JCON1-04  =    JMSTD-07
                   GO  TO  UP1-020
               END-IF
           END-IF.
       UP1-040.
           IF  JS-SIGN   =    0
               MOVE  JCON1-03    TO   WJCON1-03  GMN-JNO
           ELSE
               MOVE  JCON1-04    TO   WJCON1-03  GMN-JNO
           END-IF
           IF  SW-INV    =   "01"
               MOVE  SPACE       TO   SW-INV
               GO  TO  UP1-060
           END-IF.
       UP1-050.
           PERFORM REW-CON-RTN  THRU  REW-CON-EX.
           GO  TO  UP1-EX.
       UP1-060.
           PERFORM WRI-CON-RTN  THRU  WRI-CON-EX.
       UP1-EX.
           EXIT.
      ******************************
      *    ì`ï[áÇåüçıÇeÅ@çXêV      *
      ******************************
       UP2-RTN.
           IF  GMN-ACT =  2 OR 3
               CONTINUE
           ELSE
               GO  TO  UP2-050
           END-IF
           PERFORM  SUB-2D-RTN  THRU  SUB-2D-EX
                    VARYING  I  FROM  1  BY  1
                    UNTIL    I  >  6.
       UP2-050.
           IF  GMN-ACT =  1 OR 2
               CONTINUE
           ELSE
               GO  TO  UP2-EX
           END-IF
           MOVE     1   TO  UPD-SW.
           PERFORM  SUB-2W-RTN  THRU  SUB-2W-EX
                    VARYING  I  FROM  1  BY  1
                    UNTIL    I  >  6.
       UP2-EX.
           EXIT.
      **
       SUB-2D-RTN.
           IF  WS-HCD(I) =  ZERO
               GO  TO  SUB-2D-EX
           END-IF
           MOVE     1            TO   DNKN-01.
           MOVE     WS-HCD(I)    TO   DNKN-02.
           MOVE     2            TO   DNKN-03.
           MOVE     GMN-JNO      TO   DNKN-041.
           MOVE     I            TO   DNKN-042.
      *           READ     JT-DNKN      INVALID
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-DNKN" ERR-DNKN "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p"
                 RETURNING RESU
               GO  TO   SUB-2D-EX
           END-IF
           PERFORM  DEL-DNKN-RTN THRU DEL-DNKN-EX.
       SUB-2D-EX.
           EXIT.
      **
       SUB-2W-RTN.
           IF  GMN-HCD(I) = ZERO
               GO  TO  SUB-2W-EX
           END-IF
           MOVE     SPACE        TO   DNKN-R.
           MOVE     1            TO   DNKN-01.
           MOVE     GMN-HCD(I)   TO   DNKN-02.
           MOVE     2            TO   DNKN-03.
           MOVE     GMN-JNO      TO   DNKN-041.
           MOVE     I            TO   DNKN-042.
      **
           PERFORM  WRI-DNKN-RTN THRU WRI-DNKN-EX.
       SUB-2W-EX.
           EXIT.
      ******************************
      *    éÛíçÉ}ÉXÉ^Å@çXêV        *
      ******************************
       UP6-RTN.
           IF  GMN-ACT =  2 OR 3
               CONTINUE
           ELSE
               GO  TO  UP6-050
           END-IF
      **
           MOVE     ZERO    TO  JMSTD-KEY1.
           MOVE     GMN-JNO TO  JMSTD-07.
      *           START    JMSTD   KEY NOT <  JMSTD-KEY1 INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP6-050
           END-IF.
       UP6-010.
      *           READ     JMSTD   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP6-050
           END-IF
           IF  JMSTD-07 NOT =  GMN-JNO
               GO  TO  UP6-050
           END-IF
           PERFORM  DEL-JMSTD-RTN THRU  DEL-JMSTD-EX.
           GO  TO   UP6-010.
       UP6-050.
           IF  GMN-ACT =  1 OR 2
               CONTINUE
           ELSE
               GO  TO  UP6-EX
           END-IF
      **
           PERFORM  SUB-6W-RTN  THRU  SUB-6W-EX
                    VARYING  I  FROM  1  BY  1
                    UNTIL I  >  6.
       UP6-EX.
           EXIT.
      **
       SUB-6W-RTN.
           IF  GMN-HCD(I)  =  ZERO
               GO  TO  SUB-6W-EX
           END-IF
           PERFORM  SUB-MOV-RTN THRU SUB-MOV-EX.
      **
           PERFORM  WRI-JMSTD-RTN  THRU  WRI-JMSTD-EX.
       SUB-6W-EX.
           EXIT.
      **
       SUB-MOV-RTN.
           MOVE     SPACE       TO   JMSTD-R.
           INITIALIZE                JMSTD-R.
           MOVE     W-KBN       TO   JMSTD-01.
           MOVE     GMN-JYMD    TO   JMSTD-02.
           MOVE     GMN-HCD(I)  TO   JMSTD-03 JMSTD-05.
           MOVE     GMN-TCD     TO   JMSTD-04.
           MOVE     GMN-NYMD(I) TO   JMSTD-06.
           MOVE     GMN-JNO     TO   JMSTD-07.
           MOVE     I           TO   JMSTD-08.
           MOVE     GMN-SIZ(I)  TO   JMSTD-09.
           MOVE     GMN-CCD     TO   JMSTD-10.
           MOVE     GMN-TENC    TO   JMSTD-23.
           MOVE     1           TO   P.
       SUB-MOV-010.
           COMPUTE  JMSTD-1111(P)  = GMN-SU(I P) + WS-1211(I P) +
                                     WS-141(I P) + WS-151(I P).
           IF  GMN-ACT =   1
               MOVE   0    TO   JMSTD-1211(P) JMSTD-141(P)
               MOVE   0    TO   JMSTD-151(P)
           ELSE
               MOVE   WS-1211(I P) TO JMSTD-1211(P)
               MOVE   WS-141 (I P) TO JMSTD-141 (P)
               MOVE   WS-151 (I P) TO JMSTD-151 (P)
           END-IF
           ADD      1      TO   P.
           IF  P NOT  >    10
               GO  TO  SUB-MOV-010
           END-IF
           MOVE     GMN-SET     TO   JMSTD-16.
           MOVE     GMN-TAN(I)  TO   JMSTD-17.
           MOVE     GMN-TEKI    TO   JMSTD-13.
           MOVE     WS-SEQ      TO   JMSTD-20.
           MOVE     GMN-BIKHO(I) TO   JMSTD-22.
           MOVE     GMN-BCD1(I) TO   JMSTD-51.
           MOVE     WK-DATE     TO   JMSTD-891.
           MOVE     GMN-ACT     TO   JMSTD-892.
           MOVE     JS-SIGN     TO   JMSTD-90.
           MOVE     T-TNC       TO   JMSTD-91.
       SUB-MOV-EX.
           EXIT.
      *************************
      *    ÇiÇbÇnÇmÅ@çXêV     *
      *************************
       REW-CON-RTN.
      *           REWRITE  JCON1-R               INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "JCON"      TO  ERR-F
               MOVE  JCON1-KEY   TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       REW-CON-EX.
           EXIT.
       WRI-CON-RTN.
      *           WRITE    JCON1-R               INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE  "W"         TO  ERR-M
               MOVE  "JCON"      TO  ERR-F
               MOVE  JCON1-KEY   TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       WRI-CON-EX.
           EXIT.
      *****************************
      *    ì`ï[áÇåüçıÇeÅ@çXêV     *
      *****************************
       DEL-DNKN-RTN.
      *           DELETE   JT-DNKN               INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JT-DNKN_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"         TO  ERR-M
               MOVE  "JT-DNKN"   TO  ERR-F
               MOVE  DNKN-KEY    TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       DEL-DNKN-EX.
           EXIT.
       WRI-DNKN-RTN.
      *           WRITE    DNKN-R                INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               MOVE  "W"         TO  ERR-M
               MOVE  "JT-DNKN"   TO  ERR-F
               MOVE  DNKN-KEY    TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       WRI-DNKN-EX.
           EXIT.
      ***************************
      *    éÛíçÉ}ÉXÉ^Å@çXêV     *
      ***************************
       WRI-JMSTD-RTN.
      *           WRITE    JMSTD-R               INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE  "W"         TO  ERR-M
               MOVE  "JMSTD"     TO  ERR-F
               MOVE  JMSTD-KEY1  TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       WRI-JMSTD-EX.
           EXIT.
       REW-JMSTD-RTN.
      *           REWRITE  JMSTD-R               INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "JMSTD"     TO  ERR-F
               MOVE  JMSTD-KEY1  TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       REW-JMSTD-EX.
           EXIT.
       DEL-JMSTD-RTN.
      *           DELETE   JMSTD                 INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JMSTD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"         TO  ERR-M
               MOVE  "JMSTD"     TO  ERR-F
               MOVE  JMSTD-KEY1  TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       DEL-JMSTD-EX.
           EXIT.
       TAN-RTN.
           MOVE 0 TO WK-TAN.
           MOVE GMN-TCD TO THT-TCD.
           MOVE GMN-HCD(XX) TO THT-HCD.
           MOVE GMN-SIZ(XX) TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           MOVE GMN-TCD TO THT-TCD.
           MOVE GMN-HCD(XX) TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
       TAN-090.
           IF  WK-TAN NOT = ZERO
               MOVE WK-TAN TO GMN-TAN(XX)
               GO TO TAN-EX
           END-IF.
       TAN-EX.
           EXIT.
