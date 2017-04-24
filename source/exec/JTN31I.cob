       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JT030I.
       AUTHOR.               H-NODAKE.
      **************************************************
      *    PROGRAM      :    出荷指図入力              *
      *    DATA WRITTEN :    87/ 8/22                  *
      *    SCREEN  USED :    SJ030I                    *
      *    COMPILE TYPE :    COBOL                     *
      *    変更 :  88/03/03    K.TANAKA                *
      *    変更 :  91/10/15    I.N                     *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT               PIC  X(02).
       77  DIS-SW                 PIC  9(01).
       77  JS-SIGN                PIC  9(01).
       77  INP-SW                 PIC  9(01).
       77  FU-SW                  PIC  X(02).
       77  2K-SW                  PIC  X(03)  VALUE  "OFF".
       77  WRI-SW                 PIC  9(01).
       77  W-FILE                 PIC  X(13).
       01  N-24                   PIC  N(24)  VALUE  ALL "　".
       01  Z-SW                   PIC  9(02).
      ***************************************
      *    STN№ ﾄﾘｺﾐ(ﾜｰｸ)                  *
      ***************************************
       01  STN-NO.
           02  STN-NO-01          PIC  X(03).
           02  STN-NO-02          PIC  X(03).
       01  W-TIMEA.
           02  W-TIMED            PIC  9(08).
           02  W-TIMEW   REDEFINES  W-TIMED.
             03  W-TIME           PIC  9(06).
             03  F                PIC  9(02).
       01  W-88D.
           02  W-8AD              PIC  N(09)   VALUE  SPACE.
           02  W-8D               PIC  N(23)   VALUE  SPACE.
       01  W-AREA1.
           02  WYMD.
             03  WYM.
               04  WYY            PIC  9(04).
               04  WYYL    REDEFINES WYY.
                 05  WYY1         PIC  9(02).
                 05  WYY2         PIC  9(02).
               04  WMM            PIC  9(02).
             03  WDD              PIC  9(02).
           02  WYMDL   REDEFINES WYMD.
             03  F                PIC  9(02).
             03  WYMDS            PIC  9(06).
           02  WNGP.
             03  WNEN             PIC  9(04).
             03  WGET             PIC  9(02).
             03  WPEY             PIC  9(02).
           02  W-NGP.
             03  W-SNGP.
               04  W-SNEN         PIC  9(04).
               04  W-SGET         PIC  9(02).
               04  W-SPEY         PIC  9(02).
             03  W-ENGP.
               04  W-ENEN         PIC  9(04).
               04  W-EGET         PIC  9(02).
               04  W-EPEY         PIC  9(02).
           02  K-1                PIC  X(05)   VALUE  X"1A24212078".
           02  K-2                PIC  X(05)   VALUE  X"1A24212474".
           02  W-DNO              PIC  9(06)   VALUE  ZERO.
           02  W-82D              PIC  N(19).
           02  W-82DD  REDEFINES  W-82D.
               03  W-821          PIC  N(12).
               03  W-822          PIC  N(07).
       01  W-AREA2.
           02  O                  PIC  9(01)   VALUE  ZERO.
           02  M                  PIC  9(02)   VALUE  ZERO.
           02  A                  PIC  9(01)   VALUE  ZERO.
           02  B                  PIC  9(02)   VALUE  ZERO.
           02  C                  PIC  9(01)   VALUE  ZERO.
           02  D                  PIC  9(01)   VALUE  ZERO.
           02  I                  PIC  9(02)   VALUE  ZERO.
           02  LIN1               PIC  9(02)   VALUE  ZERO.
           02  LIN2               PIC  9(02)   VALUE  ZERO.
           02  COL1               PIC  9(02)   VALUE  ZERO.
           02  CHK-SW             PIC  9(01)   VALUE  ZERO.
           02  T-SW               PIC  9(01)   VALUE  ZERO.
           02  WJM-03.
             03  WJM-031          PIC  9(04).
             03  WJM-032          PIC  9(02).
           02  W-SET              PIC  9(03).
           02  W-JYUSU            PIC S9(07).
           02  W-COMSU.
             03  W-CSU            PIC S9(04)   OCCURS  10.
           02  W-ZSUD.
             03  W-ZSU            PIC S9(04)   OCCURS  10.
       01  ACT-WORK1.
           02  W-ACT              PIC  9(01)   VALUE  ZERO.
           02  W-NAM              PIC  N(02)   VALUE  SPACE.
       01  ACT-WORK2.
           02  WK-TCCD            PIC  9(07).
           02  WK-TCCD-R    REDEFINES    WK-TCCD.
             03  WK-TCD           PIC  9(04).
             03  WK-CCD           PIC  9(03).
           02  W-1                PIC  9(06)   VALUE  ZERO.
           02  W-1R    REDEFINES   W-1.
             03  W1-1             PIC  9(01).
             03  W1-2             PIC  9(05).
           02  W-2.
             03  W-21             PIC  9(01)   VALUE  ZERO.
             03  W-22             PIC  N(02)   VALUE  SPACE.
           02  W-3.
             03  W-31             PIC  9(04)   VALUE  ZERO.
             03  W-31L  REDEFINES W-31.
               04  W-311          PIC  9(02).
               04  W-312          PIC  9(02).
             03  W-32             PIC  9(02)   VALUE  ZERO.
             03  W-33             PIC  9(02)   VALUE  ZERO.
           02  W-3L    REDEFINES W-3.
             03  F                PIC  9(02).
             03  W-3S             PIC  9(06).
           02  W-4.
             03  W-4A.
               04  W-41           PIC  9(04)   VALUE  ZERO.
               04  W-42           PIC  9(03)   VALUE  ZERO.
             03  W-43             PIC  N(24)   VALUE  SPACE.
             03  W-44             PIC  N(24)   VALUE  SPACE.
             03  W-45             PIC  N(04)   VALUE  SPACE.
           02  W-5.
             03  W-51             PIC  9(01)   VALUE  ZERO.
             03  W-52             PIC  N(06)   VALUE  SPACE.
           02  W-5A               PIC  9(03)   VALUE  ZERO.
           02  W-6.
             03  W-6A    OCCURS   6.
               04  W-61.
                 05  W-611        PIC  9(06).
                 05  W-612        PIC  9(01).
               04  W-62.
                 05  W-621        PIC  9(06).
                 05  W-621R  REDEFINES  W-621.
                   06  W-621A     PIC  9(04).
                   06  W-621B     PIC  9(02).
                 05  W-622        PIC  N(24).
               04  W-63           PIC  9(01).
               04  W-64A.
                 05  W-64         PIC  S9(04)  OCCURS  10.
               04  W-65           PIC  S9(05).
               04  W-66           PIC  9(01).
               04  W-67           PIC  X(10).
           02  W-7.
               04  W-71           PIC  9(01).
               04  W-72           PIC  N(06).
           02  W-88.
             03  W-8A             PIC  N(09)   VALUE  SPACE.
             03  W-8.
                 05  W-81         PIC  N(04)   VALUE  SPACE.
                 05  W-82         PIC  N(19)   VALUE  SPACE.
           02  W-9.
               04  W-91           PIC  9(01).
               04  W-92           PIC  9(06).
               04  W-93           PIC  9(02).
               04  W-94           PIC  N(03).
           02  W-KSU              PIC  9(03).
           02  W-KIN              PIC  9(06).
           02  W-KIND             PIC  9(06).
           02  W-OKC              PIC  9(01)   VALUE  ZERO.
           02  W-4012             PIC  9(01).
           02  W-KEI              PIC S9(07)   VALUE  ZERO.
           02  W-OLD.
             03  CNT              PIC  9(01)   VALUE  ZERO.
             03  O-SCD            PIC  9(01)   VALUE  ZERO.
             03  OLD-SURYO   OCCURS   6.
               04  O-JNO.
                 05  O-JNO1       PIC  9(06).
                 05  O-JNO2       PIC  9(01).
               04  O-HCD          PIC  9(06).
               04  O-SIZ          PIC  9(01).
               04  O-SUA.
                 05  O-SU         PIC  S9(04)  OCCURS  10.
           02  W-KATON.
             03  W-KA             PIC  9(03)   OCCURS  6.
           02  W-611A.
             03  W6-01            PIC  9(01).
             03  W6-02            PIC  9(05).
           02  W-BUN.
             03  W-B1             PIC  9(01).
             03  W-B2             PIC  9(01).
           02  W-SETC.
             03  W-KSD            PIC  9(003).
             03  W-AMD            PIC  9(003).
       01  W-9D.
           02  W-92D              PIC  9(06).
           02  W-92SW             PIC  9(01).
       01  SAV-AREA.
           02  SAV-3              PIC  9(08).
           02  SAV-88.
             03  SAV-8A           PIC  N(09).
             03  SAV-8.
                 05  SAV-82       PIC  N(04).
                 05  SAV-83       PIC  N(19).
           02  S-64A.
             03  S-64             PIC  S9(04)  OCCURS  10.
           02  S-65               PIC  S9(05).
           02  S-AZS              PIC  S9(05).
       01  NEX-AREA.
           02  NEX-2.
             03  NEX-21           PIC  9(01)   VALUE  ZERO.
             03  NEX-22           PIC  N(02)   VALUE  SPACE.
           02  NEX-3.
             03  NEX-31           PIC  9(04)   VALUE  ZERO.
             03  NEX-32           PIC  9(02)   VALUE  ZERO.
             03  NEX-33           PIC  9(02)   VALUE  ZERO.
           02  NEX-4.
             03  NEX-4A.
               04  NEX-41         PIC  9(04)   VALUE  ZERO.
               04  NEX-42         PIC  9(03)   VALUE  ZERO.
             03  NEX-43           PIC  N(24)   VALUE  SPACE.
             03  NEX-44           PIC  N(24)   VALUE  SPACE.
             03  NEX-45           PIC  N(04)   VALUE  SPACE.
           02  NEX-5.
             03  NEX-51           PIC  9(01)   VALUE  ZERO.
             03  NEX-52           PIC  N(06)   VALUE  SPACE.
           02  NEX-7.
               04  NEX-71         PIC  9(01)   VALUE  ZERO.
               04  NEX-72         PIC  N(06)   VALUE  SPACE.
           02  NEX-88.
             03  NEX-8A           PIC  N(09)   VALUE  SPACE.
             03  NEX-8.
                 05  NEX-81       PIC  N(04)   VALUE  SPACE.
                 05  NEX-82       PIC  N(19)   VALUE  SPACE.
           02  NEX-9.
               04  NEX-91         PIC  9(01)   VALUE  ZERO.
               04  NEX-92         PIC  9(06)   VALUE  ZERO.
               04  NEX-93         PIC  9(02)   VALUE  ZERO.
               04  NEX-94         PIC  N(03)   VALUE  SPACE.
       01  REV-AREA.
           02  REV-1.
             03  REV-101          PIC  X(06)   VALUE  "      ".
             03  REV-102          PIC  X(06)   VALUE  "      ".
             03  REV-103          PIC  X(06)   VALUE  "  SS  ".
             03  REV-104          PIC  X(06)   VALUE  "   S  ".
             03  REV-105          PIC  X(06)   VALUE  "   M  ".
             03  REV-106          PIC  X(06)   VALUE  "   L  ".
             03  REV-107          PIC  X(06)   VALUE  "  LL  ".
             03  REV-108          PIC  X(06)   VALUE  " 28.0 ".
             03  REV-109          PIC  X(06)   VALUE  " 29.0 ".
             03  REV-110          PIC  X(06)   VALUE  " 30.0 ".
           02  REV-2.
             03  REV-201          PIC  X(06)   VALUE  " 12.5 ".
             03  REV-202          PIC  X(06)   VALUE  " 13.0 ".
             03  REV-203          PIC  X(06)   VALUE  " 13.5 ".
             03  REV-204          PIC  X(06)   VALUE  " 14.0 ".
             03  REV-205          PIC  X(06)   VALUE  " 15.0 ".
             03  REV-206          PIC  X(06)   VALUE  " 16.0 ".
             03  REV-207          PIC  X(06)   VALUE  " 17.0 ".
             03  REV-208          PIC  X(06)   VALUE  " 18.0 ".
             03  REV-209          PIC  X(06)   VALUE  " 19.0 ".
             03  REV-210          PIC  X(06)   VALUE  " 20.0 ".
           02  REV-3.
             03  REV-301          PIC  X(06)   VALUE  " 21.0 ".
             03  REV-302          PIC  X(06)   VALUE  " 21.5 ".
             03  REV-303          PIC  X(06)   VALUE  " 22.0 ".
             03  REV-304          PIC  X(06)   VALUE  " 22.5 ".
             03  REV-305          PIC  X(06)   VALUE  " 23.0 ".
             03  REV-306          PIC  X(06)   VALUE  " 23.5 ".
             03  REV-307          PIC  X(06)   VALUE  " 24.0 ".
             03  REV-308          PIC  X(06)   VALUE  " 24.5 ".
             03  REV-309          PIC  X(06)   VALUE  " 25.0 ".
             03  REV-310          PIC  X(06)   VALUE  "      ".
           02  REV-4.
             03  REV-401          PIC  X(06)   VALUE  " 24.0 ".
             03  REV-402          PIC  X(06)   VALUE  " 24.5 ".
             03  REV-403          PIC  X(06)   VALUE  " 25.0 ".
             03  REV-404          PIC  X(06)   VALUE  " 25.5 ".
             03  REV-405          PIC  X(06)   VALUE  " 26.0 ".
             03  REV-406          PIC  X(06)   VALUE  " 26.5 ".
             03  REV-407          PIC  X(06)   VALUE  " 27.0 ".
             03  REV-408          PIC  X(06)   VALUE  " 27.5 ".
             03  REV-409          PIC  X(06)   VALUE  "      ".
             03  REV-410          PIC  X(06)   VALUE  "      ".
      *
           COPY     LWMSG.
      *
           COPY     LIBFDD.
           COPY     L-JSTR.
           COPY     LIHIM2.
           COPY     LITCM.
           COPY     LJMSTD.
           COPY     L-JCON.
           COPY     LOKJF.
           COPY     LTRUIJ.
           COPY     LTDNKN.
           COPY     LNJZAI.
           COPY     LITM.
      *FD  JAZF
       01  JAZF_JTN31I.
           02  JAZF_PNAME1        PIC  X(004) VALUE "JAZF".
           02  F                  PIC  X(001).
           02  JAZF_LNAME         PIC  X(011) VALUE "JAZF_JTN31I".
           02  F                  PIC  X(001).
           02  JAZF_KEY1          PIC  X(100) VALUE SPACE.
           02  JAZF_SORT          PIC  X(100) VALUE SPACE.
           02  JAZF_IDLST         PIC  X(100) VALUE SPACE.
           02  JAZF_RES           USAGE  POINTER.
       01  JAZ-R.
           02  JAZ-KEY.
             03  JAZ-TCD          PIC 9(04).
             03  JAZ-HCD          PIC 9(06).
           02  JAZ-DATE           PIC 9(06).
           02  F                  PIC X(05).
       77  F                      PIC X(01).
      *FD  J-DATE
       01  J-DATE_JTN31I.
           02  J-DATE_PNAME1      PIC  X(006) VALUE "J-DATE".
           02  F                  PIC  X(001).
           02  J-DATE_LNAME       PIC  X(013) VALUE "J-DATE_JTN31I".
           02  F                  PIC  X(001).
           02  J-DATE_KEY1        PIC  X(100) VALUE SPACE.
           02  J-DATE_SORT        PIC  X(100) VALUE SPACE.
           02  J-DATE_IDLST       PIC  X(100) VALUE SPACE.
           02  J-DATE_RES         USAGE  POINTER.
       01  JDT-R.
           02  JDT-NGP            PIC 9(06).
           02  JDT-TIME           PIC 9(06).
           02  JDT-STN            PIC 9(03).
           02  JDT-DNO            PIC 9(06).
           02  JDT-ACT            PIC 9(01).
           02  JDT-NC             PIC 9(01).
           02  F                  PIC X(02).
       77  F                      PIC X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER  PIC  X(24)
                   VALUE "【　出荷指図書　入力　】".
           02  FILLER  PIC  X(14)
                   VALUE "教　育     = 0".
           02  FILLER  PIC  X(20)
                   VALUE "一　般     = 1 ...  ".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-SIGN    PIC  9(01).
           02  ACP-ACT     PIC  9(01).
           02  DSP-NAM     PIC  N(02).
           02  ACP-1       PIC  9(06).
           02  ACP-2.
               03  A-21       PIC  9(01).
               03  D-22       PIC  N(02).
           02  ACP-3.
               03  A-31       PIC  9(02).
               03  A-32       PIC  9(02).
               03  A-33       PIC  9(02).
           02  ACP-4.
               03  A-41       PIC  9(04).
               03  A-42       PIC  9(03).
               03  D-43       PIC  N(24).
           02  D-45   PIC  N(04) .
           02  D-44   PIC  N(24) .
           02  ACP-5.
               03  A-51       PIC  9(01).
               03  D-52       PIC  N(06).
               03  A-53       PIC  9(03).
               03  D-54       PIC  ZZ9 .
           02  ACP-6A.
               03  A-611      PIC  9(06).
               03  A-612      PIC  9(01).
               03  A-613      PIC  X(01)  VALUE  "-".
               03  A-612A     PIC  X(08)  VALUE  " ".
               03  D-622      PIC  N(24).
               03  A-67       PIC  X(10).
           02  ACP-6B.
               03  A-621      PIC  9(06).
               03  A-63       PIC  9(01).
               03  A-64       PIC  S9(04).
               03  D-64       PIC  ZZZZ- .
               03  D-65       PIC ZZZ,ZZZ- .
           02  DSP-6B.
               03  DIS-1   PIC  ZZZZ- .
               03  DIS-2   PIC  ZZZZ- .
               03  DIS-3   PIC  ZZZZ- .
               03  DIS-4   PIC  ZZZZ- .
               03  DIS-5   PIC  ZZZZ- .
               03  DIS-6   PIC  ZZZZ- .
               03  DIS-7   PIC  ZZZZ- .
               03  DIS-8   PIC  ZZZZ- .
               03  DIS-9   PIC  ZZZZ- .
               03  DIS-10  PIC  ZZZZ- .
           02  DSP-KEI     PIC  ZZZ,ZZZ- .
           02  DSP-LINE    PIC  X(01) VALUE  "-".
           02  ACP-7.
               03  A-71    PIC  9(01).
               03  D-72    PIC  N(06).
           02  ACP-8.
               03  ACP-81  PIC  N(04).
               03  ACP-82  PIC  N(19).
           02  ACP-8A      PIC  N(09).
           02  ACP-9.
               03  A-91    PIC  9(01).
               03  A-92    PIC  9(06).
               03  A-93    PIC  9(02).
           02  A-KSU       PIC  9(03).
           02  D-KSU       PIC  ZZZ .
           02  A-KIN       PIC  9(06).
           02  D-KIN       PIC  ZZZZZZ .
           02  ACP-OKC     PIC  9(01).
           02  DSP-S1      PIC  X(06) VALUE
                                  "教　育".
           02  DSP-S2      PIC  X(06) VALUE
                                  "一　般".
           02  DSP-DNO     PIC  9(06).
           02  DSP-BIKHO.
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
           02  DSP-BIKHO-CLE.
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
           02  D-KSUM      PIC X(04) VALUE
                                  "個数".
           02  D-KINM      PIC X(04) VALUE
                                  "金額".
           02  D-KSU-CLE   PIC X(12) VALUE
                                    "            ".
       01  DSP-CLE.
           02  CLE-01.
               03  C-11.
                   04  FILLER  PIC X(06) VALUE "      ".
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  FILLER  PIC X(03) VALUE "   ".
                   04  C-111   PIC  N(24).
               03  C-12.
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(12) VALUE "　　　　　　".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  C-121   PIC  N(24).
               03  C-13.
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(12) VALUE "　　　　　　".
                   04  C-131.
                       05  FILLER  PIC X(01) VALUE " ".
                       05  FILLER  PIC X(06) VALUE "      ".
                       05  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(08) VALUE "        ".
                   04  FILLER  PIC X(08) VALUE "        ".
               03  C-14.
                   04  FILLER  PIC  N(09).
                   04  FILLER  PIC  N(04).
                   04  FILLER  PIC  N(19).
               03  C-15    PIC X(09) VALUE "         ".
               03  C-16    PIC X(01) VALUE " ".
           02  CLE-02.
               03  CLE-21.
                 04  FILLER  PIC X(07) VALUE "       ".
                 04  FILLER  PIC X(01) VALUE " ".
                 04  FILLER  PIC X(24) VALUE "                        ".
                 04  FILLER  PIC X(24) VALUE "                        ".
                 04  FILLER  PIC X(12) VALUE "            ".
               03  CLE-22.
                   04  FILLER  PIC X(07) VALUE "       ".
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(08) VALUE "        ".
           02  CLE-03      PIC X(06) VALUE  "      ".
           02  CLE-04.
               03  C-41    PIC X(08) VALUE  "        ".
               03  C-42    PIC X(07) VALUE  "       ".
       01  DSP-REV-AREA.
           02  DSP-RR-ALL.
               03  DSP-RC1.
                   04  DSP-RR101  PIC  X(06).
                   04  DSP-RR102  PIC  X(06).
                   04  DSP-RR103  PIC  X(06).
                   04  DSP-RR104  PIC  X(06).
                   04  DSP-RR105  PIC  X(06).
                   04  DSP-RR106  PIC  X(06).
                   04  DSP-RR107  PIC  X(06).
                   04  DSP-RR108  PIC  X(06).
                   04  DSP-RR109  PIC  X(06).
                   04  DSP-RR110  PIC  X(06).
               03  DSP-RC2.
                   04  DSP-RR201  PIC  X(06).
                   04  DSP-RR202  PIC  X(06).
                   04  DSP-RR203  PIC  X(06).
                   04  DSP-RR204  PIC  X(06).
                   04  DSP-RR205  PIC  X(06).
                   04  DSP-RR206  PIC  X(06).
                   04  DSP-RR207  PIC  X(06).
                   04  DSP-RR208  PIC  X(06).
                   04  DSP-RR209  PIC  X(06).
                   04  DSP-RR210  PIC  X(06).
               03  DSP-RC3.
                   04  DSP-RR301  PIC  X(06).
                   04  DSP-RR302  PIC  X(06).
                   04  DSP-RR303  PIC  X(06).
                   04  DSP-RR304  PIC  X(06).
                   04  DSP-RR305  PIC  X(06).
                   04  DSP-RR306  PIC  X(06).
                   04  DSP-RR307  PIC  X(06).
                   04  DSP-RR308  PIC  X(06).
                   04  DSP-RR309  PIC  X(06).
                   04  DSP-RR310  PIC  X(06).
               03  DSP-RC4.
                   04  DSP-RR401  PIC  X(06).
                   04  DSP-RR402  PIC  X(06).
                   04  DSP-RR403  PIC  X(06).
                   04  DSP-RR404  PIC  X(06).
                   04  DSP-RR405  PIC  X(06).
                   04  DSP-RR406  PIC  X(06).
                   04  DSP-RR407  PIC  X(06).
                   04  DSP-RR408  PIC  X(06).
                   04  DSP-RR409  PIC  X(06).
                   04  DSP-RR410  PIC  X(06).
           02  DSP-RC-ALL.
               03  DSP-RC1.
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
               03  DSP-RC2.
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
               03  DSP-RC3.
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
               03  DSP-RC4.
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
                   04  FILLER  PIC  X(06).
       01  DSP-ERR.
           02  INV-T01     PIC X(30)
                           VALUE "＊＊出荷指図トラン　未登録＊＊".
           02  INV-M02     PIC X(32)
                           VALUE "＊＊出荷品名マスター　未登録＊＊".
           02  INV-M03     PIC X(30)
                           VALUE "＊＊直送先マスター　未登録＊＊".
           02  INV-M05     PIC X(28)
                           VALUE "＊＊受注マスター　未登録＊＊".
           02  INV-D02     PIC X(22)
                           VALUE "＊＊倉庫名　未登録＊＊".
           02  INV-D03     PIC X(22)
                           VALUE "＊＊運送名　未登録＊＊".
           02  ERR-04      PIC X(24)
                           VALUE "＊　該当伝票№使用中　＊".
           02  ERR-05      PIC X(22)
                           VALUE "＊　指図数オーバー　＊".
           02  ERR-06      PIC X(14)
                           VALUE "＊　出荷済　＊".
           02  ERR-07      PIC X(32)
                           VALUE "＊＊　数量ＺＥＲＯ　エラー　＊＊".
           02  ERR-08      PIC X(24)
                           VALUE "＊　入数が同一でない　＊".
           02  ERR-09      PIC X(24)
                           VALUE "＊＊送り状№　未登録＊＊".
           02  ERR-10      PIC X(24)
                           VALUE "＊　送り状ファイル無　＊".
           02  ERR-11      PIC X(22)
                           VALUE "＊　送り状　発行済　＊".
           02  ERR-12      PIC X(26)
                           VALUE "＊　送り状データ作成済　＊".
           02  ERR-13      PIC X(26)
                           VALUE "＊　　　　　アンマッチ　＊".
           02  ERR-13A     PIC N(03).
           02  ERR-14      PIC X(26)
                           VALUE "＊　入数がＺＥＲＯです　＊".
           02  ERR-15      PIC X(24)
                           VALUE "＊　日付　ＣＨＥＣＫ　＊".
           02  ERR-16      PIC X(24)
                           VALUE "＊　完了済　処理不可　＊".
           02  ERR-17      PIC X(20)
                           VALUE "月次繰越をして下さい".
           02  ERR-HIM     PIC X(40)
               VALUE "品名マスター使用サイン　ＡＬＬ　ＺＥＲＯ".
           02  OK-TKM      PIC X(24)
               VALUE "＊　ＯＫ　直送先変更　＊".
           02  ERR-SYK     PIC X(18)
               VALUE "＊　出荷　不可　＊".
           02  INV-RUI     PIC X(30)
                VALUE "＊＊類似品マスター　未登録＊＊".
           02  ERR-SET     PIC X(26)
               VALUE "＊　セット数量　エラー　＊".
           02  ERR-JYU     PIC X(24)
               VALUE "＊　受注数　オーバー　＊".
           02  ERR-TM      PIC X(20)
               VALUE "＊　得意先不一致　＊".
           02  ERR-TCM     PIC X(20)
               VALUE "＊　直送先不一致　＊".
           02  ERR-TEI     PIC X(24)
               VALUE "＊　訂正数　オーバー　＊".
           02  ERR-STN     PIC X(24)
               VALUE "＊　ＳＴＮ№　エラー　＊".
           02  ERR-READ    PIC X(34)
               VALUE "＊　ＪＳＴＲ　ＲＥＡＤ　エラー　＊".
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(60).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　登録済　＊".
           02  INV-M01.
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　未登録　＊".
           02  OK-01.
               03  FILLER  PIC X(14) VALUE
               "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  FILLER  PIC X(18) VALUE
               "＊　キャンセル　＊".
           02  ERR-01.
               03  FILLER  PIC X(18) VALUE
               "＊　入力エラー　＊".
           02  ERR-02.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　なし　　＊".
           02  ERR-03.
               03  FILLER  PIC X(22) VALUE
               "＊　倉庫　チェック　＊".
           02  ERR-21.
               03  FILLER  PIC X(22) VALUE
               "＊　預り　チェック　＊".
           02  ERR-22.
               03  FILLER  PIC X(22) VALUE
               "＊（預り）　チェック＊".
           02  ERR-31.
               03  FILLER  PIC X(30) VALUE
               "Ｊ－ＤＡＴＥ　ＷＲＩＴＥエラー".
           02  ERR-DIS.
               03  FILLER  PIC X(05) VALUE
               "<<<  ".
               03  FILLER  PIC X(12).
               03  FILLER  PIC X(01).
               03  FILLER  PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  FILLER  PIC X(02).
               03  FILLER  PIC X(05) VALUE
               "  >>>".
               03  FILLER  PIC X(05) VALUE
               " KEY=".
               03  FILLER  PIC X(30).
           COPY  LSSEM.
           COPY  LIBSCR.
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "80" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-AREA" "X" "1" "28" "24" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "6" "28" "14" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "8" "28" "20" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "24" "43" "22" "03DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "597" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SIGN" "9" "8" "47" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "67" "1" "ACP-SIGN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NAM" "N" "1" "69" "4" "ACP-ACT" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NAM" BY REFERENCE W-NAM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-1" "9" "3" "2" "6" "DSP-NAM" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-1" BY REFERENCE W-1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-2" " " "3" "0" "5" "ACP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-21" "9" "3" "9" "1" " " "ACP-2"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-21" BY REFERENCE W-21 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-22" "N" "3" "10" "4" "A-21" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-22" BY REFERENCE W-22 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-3" " " "3" "0" "6" "ACP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-31" "9" "3" "15" "2" " " "ACP-3"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-31" BY REFERENCE W-312 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-32" "9" "3" "18" "2" "A-31" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-32" BY REFERENCE W-32 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-33" "9" "3" "21" "2" "A-32" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-33" BY REFERENCE W-33 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-4" " " "3" "0" "55" "ACP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-41" "9" "3" "24" "4" " " "ACP-4"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-41" BY REFERENCE W-41 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-42" "9" "3" "29" "3" "A-41" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-42" BY REFERENCE W-42 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-43" "N" "3" "33" "48" "A-42" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-43" BY REFERENCE W-43 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-45" "N" "2" "72" "8" "ACP-4" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-45" BY REFERENCE W-45 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-44" "N" "4" "33" "48" "D-45" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-44" BY REFERENCE W-44 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-5" " " "4" "0" "19" "D-44" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-51" "9" "4" "7" "1" " " "ACP-5"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-51" BY REFERENCE W-51 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-52" "N" "4" "9" "12" "A-51" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-52" BY REFERENCE W-52 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-53" "9" "4" "29" "3" "D-52" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-53" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-54" "ZZ9" "4" "29" "3" "A-53" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-54" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6A" " " "LIN1" "0" "74" "ACP-5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-611" "9" "LIN1" "2" "6" " " "ACP-6A"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-611" BY REFERENCE W-611(1) "6" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612" "9" "LIN1" "9" "1" "A-611" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-612" BY REFERENCE W-612(1) "1" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-613" "X" "LIN1" "8" "1" "A-612" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612A" "X" "LIN1" "2" "8" "A-613" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-622" "N" "LIN1" "11" "48" "A-612A" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-622" BY REFERENCE W-622(1) "48" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-67" "X" "LIN1" "70" "10" "D-622" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-67" BY REFERENCE W-67(1) "10" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6B" " " "LIN1 PLUS 1" "0" "24" "ACP-6A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-621" "9" "LIN1 PLUS 1" "2" "6" " " "ACP-6B"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-621" BY REFERENCE W-621(1) "6" "1" BY REFERENCE A 118
       CALL "SD_Init" USING 
            "A-63" "9" "LIN1 PLUS 1" "11" "1" "A-621" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-63" BY REFERENCE W-63(1) "1" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-64" "S9" "LIN1 PLUS 1" "COL1" "4" "A-63" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "A-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118
            BY REFERENCE B 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-64" "ZZZZ-" "LIN1 PLUS 1" "COL1" "5" "A-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118
            BY REFERENCE B 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-65" "ZZZ,ZZZ-" "LIN1 PLUS 1" "73" "8" "D-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-65" BY REFERENCE W-65(1) "5" "1" BY REFERENCE A 118
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-6B" " " "LIN1 PLUS 1" "0" "50" "ACP-6B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-1" "ZZZZ-" "LIN1 PLUS 1" "13" "5" " " "DSP-6B"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-1" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "1" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-2" "ZZZZ-" "LIN1 PLUS 1" "19" "5" "DIS-1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-2" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "2" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-3" "ZZZZ-" "LIN1 PLUS 1" "25" "5" "DIS-2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-3" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "3" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-4" "ZZZZ-" "LIN1 PLUS 1" "31" "5" "DIS-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-4" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A  118 
            "4" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-5" "ZZZZ-" "LIN1 PLUS 1" "37" "5" "DIS-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-5" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "5" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-6" "ZZZZ-" "LIN1 PLUS 1" "43" "5" "DIS-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-6" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "6" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-7" "ZZZZ-" "LIN1 PLUS 1" "49" "5" "DIS-6" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-7" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "7" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-8" "ZZZZ-" "LIN1 PLUS 1" "55" "5" "DIS-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-8" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "8" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-9" "ZZZZ-" "LIN1 PLUS 1" "61" "5" "DIS-8" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-9" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "9" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-10" "ZZZZ-" "LIN1 PLUS 1" "67" "5" "DIS-9" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-10" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 118 
            "10" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEI" "ZZZ,ZZZ-" "22" "73" "8" "DSP-6B" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEI" BY REFERENCE W-KEI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-LINE" "X" "3" "28" "1" "DSP-KEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-7" " " "22" "0" "13" "DSP-LINE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-71" "9" "22" "7" "1" " " "ACP-7"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-71" BY REFERENCE W-71 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-72" "N" "22" "9" "12" "A-71" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-72" BY REFERENCE W-72 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-8" " " "23" "0" "46" "ACP-7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-81" "N" "23" "31" "8" " " "ACP-8"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-81" BY REFERENCE W-81 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-82" "N" "23" "39" "38" "ACP-81" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-82" BY REFERENCE W-82 "38" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-8A" "N" "23" "7" "18" "ACP-8" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-8A" BY REFERENCE W-8A "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-9" " " "22" "0" "9" "ACP-8A" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-91" "9" "22" "31" "1" " " "ACP-9"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-91" BY REFERENCE W-91 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-92" "9" "22" "33" "6" "A-91" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-92" BY REFERENCE W-92 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-93" "9" "22" "40" "2" "A-92" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-93" BY REFERENCE W-93 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KSU" "9" "22" "52" "3" "ACP-9" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KSU" BY REFERENCE W-KSU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KSU" "ZZZ" "22" "52" "3" "A-KSU" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KSU" BY REFERENCE W-KSU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "9" "22" "49" "6" "D-KSU" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZ" "22" "49" "6" "A-KIN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "60" "1" "D-KIN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S1" "bX" "1" "21" "6" "ACP-OKC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S2" "bX" "1" "21" "6" "DSP-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNO" "9" "24" "74" "6" "DSP-S2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO" " " "0" "0" "72" "DSP-DNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO" "X" "10" "69" "12" " " "DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO" "X" "12" "69" "12" "01DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO" "X" "14" "69" "12" "02DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO" "X" "16" "69" "12" "03DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO" "X" "18" "69" "12" "04DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO" "X" "20" "69" "12" "05DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO-CLE" " " "0" "0" "72" "DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO-CLE" "X" "10" "69" "12" " " "DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO-CLE" "X" "12" "69" "12" "01DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO-CLE" "X" "14" "69" "12" "02DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO-CLE" "X" "16" "69" "12" "03DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO-CLE" "X" "18" "69" "12" "04DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO-CLE" "X" "20" "69" "12" "05DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KSUM" "X" "22" "43" "4" "DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KINM" "X" "22" "43" "4" "D-KSUM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KSU-CLE" "X" "22" "43" "12" "D-KINM" " " RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING 
            "DSP-CLE" " " "0" "0" "404" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" " " "0" "0" "249" " " "DSP-CLE"  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-11" " " "3" "0" "72" " " "CLE-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-11" "X" "3" "2" "6" " " "C-11"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-11" "X" "3" "9" "1" "01C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-11" "X" "3" "10" "4" "02C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-11" "X" "3" "15" "2" "03C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-11" "X" "3" "18" "2" "04C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-11" "X" "3" "21" "2" "05C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-11" "X" "3" "24" "4" "06C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-11" "X" "3" "29" "3" "07C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-111" "N" "3" "33" "48" "08C-11" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "C-111" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-12" " " "4" "0" "65" "C-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-12" "X" "4" "7" "1" " " "C-12"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-12" "X" "4" "9" "12" "01C-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-12" "X" "4" "29" "4" "02C-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-121" "N" "4" "33" "48" "03C-12" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "C-121" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-13" " " "22" "0" "38" "C-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-13" "X" "22" "7" "1" " " "C-13"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-13" "X" "22" "9" "12" "01C-13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-131" " " "22" "0" "9" "02C-13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-131" "X" "22" "31" "1" " " "C-131"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-131" "X" "22" "33" "6" "01C-131" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-131" "X" "22" "40" "2" "02C-131" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-13" "X" "22" "43" "8" "C-131" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-13" "X" "22" "73" "8" "04C-13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-14" " " "23" "0" "64" "C-13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-14" "N" "23" "7" "18" " " "C-14"  RETURNING RESU.
       CALL "SD_From" USING 
            "01C-14" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-14" "N" "23" "31" "8" "01C-14" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02C-14" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-14" "N" "23" "39" "38" "02C-14" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "03C-14" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-15" "X" "21" "73" "9" "C-14" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-16" "X" "24" "60" "1" "C-15" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "134" "CLE-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-21" " " "LIN2" "0" "68" " " "CLE-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-21" "X" "LIN2" "2" "7" " " "CLE-21" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-21" "X" "LIN2" "9" "1" "01CLE-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-21" "X" "LIN2" "11" "24" "02CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-21" "X" "LIN2" "35" "24" "03CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-21" "X" "LIN2" "69" "12" "04CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-22" " " "LIN2 PLUS 1" "0" "66" "CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-22" "X" "LIN2 PLUS 1" "2" "7" " " "CLE-22"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-22" "X" "LIN2 PLUS 1" "11" "1" "01CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-22" "X" "LIN2 PLUS 1" "13" "5" "02CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-22" "X" "LIN2 PLUS 1" "19" "5" "03CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-22" "X" "LIN2 PLUS 1" "25" "5" "04CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLE-22" "X" "LIN2 PLUS 1" "31" "5" "05CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLE-22" "X" "LIN2 PLUS 1" "37" "5" "06CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08CLE-22" "X" "LIN2 PLUS 1" "43" "5" "07CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09CLE-22" "X" "LIN2 PLUS 1" "49" "5" "08CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10CLE-22" "X" "LIN2 PLUS 1" "55" "5" "09CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11CLE-22" "X" "LIN2 PLUS 1" "61" "5" "10CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12CLE-22" "X" "LIN2 PLUS 1" "67" "5" "11CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13CLE-22" "X" "LIN2 PLUS 1" "73" "8" "12CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "3" "2" "6" "CLE-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" " " "0" "0" "15" "CLE-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-41" "X" "3" "24" "8" " " "CLE-04"  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-42" "X" "LIN1 PLUS 1" "2" "7" "C-41" " "  RETURNING RESU.
      *DSP-REV-AREA
       CALL "SD_Init" USING 
            "DSP-REV-AREA" " " "0" "0" "480" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR-ALL" " " "0" "0" "240" " " "DSP-REV-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC1" " " "6" "0" "60" " " "DSP-RR-ALL" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR101" "RX" "6" "12" "6" " " "DSP-RC1" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR101" BY REFERENCE REV-101 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR102" "RX" "6" "18" "6" "DSP-RR101" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR102" BY REFERENCE REV-102 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR103" "RX" "6" "24" "6" "DSP-RR102" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR103" BY REFERENCE REV-103 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR104" "RX" "6" "30" "6" "DSP-RR103" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR104" BY REFERENCE REV-104 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR105" "RX" "6" "36" "6" "DSP-RR104" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR105" BY REFERENCE REV-105 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR106" "RX" "6" "42" "6" "DSP-RR105" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR106" BY REFERENCE REV-106 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR107" "RX" "6" "48" "6" "DSP-RR106" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR107" BY REFERENCE REV-107 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR108" "RX" "6" "54" "6" "DSP-RR107" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR108" BY REFERENCE REV-108 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR109" "RX" "6" "60" "6" "DSP-RR108" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR109" BY REFERENCE REV-109 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR110" "RX" "6" "66" "6" "DSP-RR109" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR110" BY REFERENCE REV-110 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC2" " " "7" "0" "60" "DSP-RC1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR201" "RX" "7" "12" "6" " " "DSP-RC2" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR201" BY REFERENCE REV-201 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR202" "RX" "7" "18" "6" "DSP-RR201" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR202" BY REFERENCE REV-202 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR203" "RX" "7" "24" "6" "DSP-RR202" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR203" BY REFERENCE REV-203 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR204" "RX" "7" "30" "6" "DSP-RR203" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR204" BY REFERENCE REV-204 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR205" "RX" "7" "36" "6" "DSP-RR204" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR205" BY REFERENCE REV-205 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR206" "RX" "7" "42" "6" "DSP-RR205" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR206" BY REFERENCE REV-206 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR207" "RX" "7" "48" "6" "DSP-RR206" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR207" BY REFERENCE REV-207 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR208" "RX" "7" "54" "6" "DSP-RR207" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR208" BY REFERENCE REV-208 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR209" "RX" "7" "60" "6" "DSP-RR208" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR209" BY REFERENCE REV-209 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR210" "RX" "7" "66" "6" "DSP-RR209" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR210" BY REFERENCE REV-210 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC3" " " "8" "0" "60" "DSP-RC2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR301" "RX" "8" "12" "6" " " "DSP-RC3" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR301" BY REFERENCE REV-301 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR302" "RX" "8" "18" "6" "DSP-RR301" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR302" BY REFERENCE REV-302 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR303" "RX" "8" "24" "6" "DSP-RR302" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR303" BY REFERENCE REV-303 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR304" "RX" "8" "30" "6" "DSP-RR303" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR304" BY REFERENCE REV-304 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR305" "RX" "8" "36" "6" "DSP-RR304" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR305" BY REFERENCE REV-305 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR306" "RX" "8" "42" "6" "DSP-RR305" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR306" BY REFERENCE REV-306 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR307" "RX" "8" "48" "6" "DSP-RR306" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR307" BY REFERENCE REV-307 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR308" "RX" "8" "54" "6" "DSP-RR307" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR308" BY REFERENCE REV-308 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR309" "RX" "8" "60" "6" "DSP-RR308" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR309" BY REFERENCE REV-309 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR310" "RX" "8" "66" "6" "DSP-RR309" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR310" BY REFERENCE REV-310 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC4" " " "9" "0" "60" "DSP-RC3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR401" "RX" "9" "12" "6" " " "DSP-RC4" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR401" BY REFERENCE REV-401 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR402" "RX" "9" "18" "6" "DSP-RR401" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR402" BY REFERENCE REV-402 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR403" "RX" "9" "24" "6" "DSP-RR402" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR403" BY REFERENCE REV-403 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR404" "RX" "9" "30" "6" "DSP-RR403" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR404" BY REFERENCE REV-404 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR405" "RX" "9" "36" "6" "DSP-RR404" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR405" BY REFERENCE REV-405 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR406" "RX" "9" "42" "6" "DSP-RR405" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR406" BY REFERENCE REV-406 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR407" "RX" "9" "48" "6" "DSP-RR406" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR407" BY REFERENCE REV-407 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR408" "RX" "9" "54" "6" "DSP-RR407" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR408" BY REFERENCE REV-408 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR409" "RX" "9" "60" "6" "DSP-RR408" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR409" BY REFERENCE REV-409 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR410" "RX" "9" "66" "6" "DSP-RR409" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR410" BY REFERENCE REV-410 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC-ALL" " " "0" "0" "240" "DSP-RR-ALL" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC1" " " "6" "0" "60" " " "DSP-RC-ALL" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC1" "X" "6" "12" "6" " " "DSP-RC1" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC1" BY REFERENCE REV-101 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC1" "X" "6" "18" "6" "01DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC1" BY REFERENCE REV-102 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC1" "X" "6" "24" "6" "02DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC1" BY REFERENCE REV-103 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC1" "X" "6" "30" "6" "03DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC1" BY REFERENCE REV-104 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC1" "X" "6" "36" "6" "04DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC1" BY REFERENCE REV-105 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC1" "X" "6" "42" "6" "05DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC1" BY REFERENCE REV-106 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC1" "X" "6" "48" "6" "06DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC1" BY REFERENCE REV-107 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC1" "X" "6" "54" "6" "07DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC1" BY REFERENCE REV-108 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC1" "X" "6" "60" "6" "08DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC1" BY REFERENCE REV-109 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC1" "X" "6" "66" "6" "09DSP-RC1" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC1" BY REFERENCE REV-110 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC2" " " "7" "0" "60" "DSP-RC1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC2" "X" "7" "12" "6" " " "DSP-RC2" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC2" BY REFERENCE REV-201 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC2" "X" "7" "18" "6" "01DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC2" BY REFERENCE REV-202 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC2" "X" "7" "24" "6" "02DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC2" BY REFERENCE REV-203 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC2" "X" "7" "30" "6" "03DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC2" BY REFERENCE REV-204 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC2" "X" "7" "36" "6" "04DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC2" BY REFERENCE REV-205 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC2" "X" "7" "42" "6" "05DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC2" BY REFERENCE REV-206 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC2" "X" "7" "48" "6" "06DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC2" BY REFERENCE REV-207 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC2" "X" "7" "54" "6" "07DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC2" BY REFERENCE REV-208 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC2" "X" "7" "60" "6" "08DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC2" BY REFERENCE REV-209 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC2" "X" "7" "66" "6" "09DSP-RC2" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC2" BY REFERENCE REV-210 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC3" " " "8" "0" "60" "DSP-RC2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC3" "X" "8" "12" "6" " " "DSP-RC3"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC3" BY REFERENCE REV-301 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC3" "X" "8" "18" "6" "01DSP-RC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC3" BY REFERENCE REV-302 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC3" "X" "8" "24" "6" "02DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC3" BY REFERENCE REV-303 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC3" "X" "8" "30" "6" "03DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC3" BY REFERENCE REV-304 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC3" "X" "8" "36" "6" "04DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC3" BY REFERENCE REV-305 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC3" "X" "8" "42" "6" "05DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC3" BY REFERENCE REV-306 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC3" "X" "8" "48" "6" "06DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC3" BY REFERENCE REV-307 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC3" "X" "8" "54" "6" "07DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC3" BY REFERENCE REV-308 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC3" "X" "8" "60" "6" "08DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC3" BY REFERENCE REV-309 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC3" "X" "8" "66" "6" "09DSP-RC3" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC3" BY REFERENCE REV-310 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC4" " " "9" "0" "60" "DSP-RC3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-RC4" "X" "9" "12" "6" " " "DSP-RC4" RETURNING RESU.
       CALL "SD_Using" USING 
            "01DSP-RC4" BY REFERENCE REV-401 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-RC4" "X" "9" "18" "6" "01DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02DSP-RC4" BY REFERENCE REV-402 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-RC4" "X" "9" "24" "6" "02DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03DSP-RC4" BY REFERENCE REV-403 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-RC4" "X" "9" "30" "6" "03DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "04DSP-RC4" BY REFERENCE REV-404 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-RC4" "X" "9" "36" "6" "04DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "05DSP-RC4" BY REFERENCE REV-405 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-RC4" "X" "9" "42" "6" "05DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "06DSP-RC4" BY REFERENCE REV-406 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-RC4" "X" "9" "48" "6" "06DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "07DSP-RC4" BY REFERENCE REV-407 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-RC4" "X" "9" "54" "6" "07DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "08DSP-RC4" BY REFERENCE REV-408 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-RC4" "X" "9" "60" "6" "08DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "09DSP-RC4" BY REFERENCE REV-409 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-RC4" "X" "9" "66" "6" "09DSP-RC4" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "10DSP-RC4" BY REFERENCE REV-410 "6" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "786" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-T01" "X" "24" "1" "30" " " "DSP-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M02" "X" "24" "1" "32" "INV-T01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M03" "X" "24" "1" "30" "INV-M02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M05" "X" "24" "1" "28" "INV-M03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D02" "X" "24" "1" "22" "INV-M05" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D03" "X" "24" "1" "22" "INV-D02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-04" "X" "24" "1" "24" "INV-D03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-05" "X" "24" "1" "22" "ERR-04" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-06" "X" "24" "1" "14" "ERR-05" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-07" "X" "24" "1" "32" "ERR-06" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-08" "X" "24" "1" "24" "ERR-07" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-09" "X" "24" "1" "24" "ERR-08" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-10" "X" "24" "1" "24" "ERR-09" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-11" "X" "24" "1" "22" "ERR-10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-12" "X" "24" "1" "26" "ERR-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-13" "X" "24" "1" "26" "ERR-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-13A" "N" "24" "5" "6" "ERR-13" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "ERR-13A" BY REFERENCE W-94 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-14" "X" "24" "1" "26" "ERR-13A" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-15" "X" "24" "1" "24" "ERR-14" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-16" "X" "24" "1" "24" "ERR-15" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-17" "X" "24" "1" "20" "ERR-16" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HIM" "X" "24" "1" "40" "ERR-17" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-TKM" "X" "24" "1" "24" "ERR-HIM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SYK" "X" "24" "1" "18" "OK-TKM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-RUI" "X" "24" "1" "30" "ERR-SYK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SET" "X" "24" "1" "26" "INV-RUI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-JYU" "X" "24" "1" "24" "ERR-SET" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-TM" "X" "24" "1" "20" "ERR-JYU" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-TCM" "X" "24" "1" "20" "ERR-TM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-TEI" "X" "24" "1" "24" "ERR-TCM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-STN" "X" "24" "1" "24" "ERR-TEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-READ" "X" "24" "1" "34" "ERR-STN" " " RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "437" " " " " RETURNING RESU.
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
            "DISP-MSG-SPACE" " " "24" "0" "40" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "40" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
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
            "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01" RETURNING RESU.
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
            "ERR-03" " " "24" "0" "22" "ERR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-03" "X" "24" "1" "22" " " "ERR-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-21" " " "24" "0" "22" "ERR-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-21" "X" "24" "1" "22" " " "ERR-21" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-22" " " "24" "0" "22" "ERR-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-22" "X" "24" "1" "22" " " "ERR-22" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-31" " " "24" "0" "30" "ERR-22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-31" "X" "24" "15" "30" " " "ERR-31" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-31" " " RETURNING RESU.
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
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  ACT-RTN    THRU  ACT-EX.
       MR999.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************
      *    ＩＮＩ－ＲＴＮ          *
      *          ～初期処理～      *
      ******************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           MOVE     ZERO       TO    WYMD.
           ACCEPT   WYMDS      FROM  DATE.
           COPY     LIBCPR.
           IF  WYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO WYY
           END-IF
           IF  WYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO WYY
           END-IF
           MOVE     WYMD       TO    WNGP.
           ADD      1          TO    WGET.
           IF  WGET   =   13
               ADD     1      TO   WNEN
               MOVE    1      TO   WGET
           END-IF
           MOVE     WYMD       TO    W-SNGP   W-ENGP.
           SUBTRACT 1          FROM  W-SGET.
           IF  W-SGET =   ZERO
               SUBTRACT 1     FROM W-SNEN
               MOVE     12    TO   W-SGET
           END-IF
           ADD      6          TO    W-EGET.
           IF  W-EGET >   12
               ADD      1     TO   W-ENEN
               SUBTRACT 12    FROM W-EGET
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE    SPACE       TO    JCON6-KEY  ERR-K.
           MOVE    "6"         TO    JCON6-01.
           MOVE    JCON6-KEY   TO    ERR-K.
      *           READ     JCON       UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               MOVE    "G"      TO   ERR-M
               MOVE    "JCON"   TO   ERR-F
               MOVE    ERR-STAT TO   ERR-FLG
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  JCON6-03   <  WYM
               CALL "SD_Output" USING
                "ERR-17" ERR-17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       INI-001.
           CALL "SD_Accept" USING BY REFERENCE ACP-SIGN "ACP-SIGN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-001
           END-IF
           IF  JS-SIGN NOT = 0 AND 1
               GO  TO  INI-001
           END-IF.
       INI-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INI-001
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-005
           END-IF
           IF  W-OKC  NOT =  1 AND 9
               GO  TO  INI-005
           END-IF
           IF  W-OKC      =  9
               GO  TO  INI-001
           END-IF
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ030I" RETURNING RESU.
           IF  JS-SIGN  =  0
               CALL "SD_Output" USING "DSP-S1" DSP-S1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN  =  1
               CALL "SD_Output" USING "DSP-S2" DSP-S2 "p" RETURNING RESU
           END-IF.
       INI-010.
           CALL     "CBLSTNNO"     USING  STN-NO USER_ID.
      *
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JT-RUIJ_PNAME1 "SHARED" BY REFERENCE JT-RUIJ_IDLST
            "1" "RUIJ-KEY" BY REFERENCE RUIJ-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JAZF_PNAME1 "SHARED" BY REFERENCE JAZF_IDLST "1"
            "JAZ-KEY" BY REFERENCE JAZ-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" J-DATE_PNAME1 "SHARED" BY REFERENCE
            J-DATE_IDLST "0".
      *
           MOVE    "12"        TO    JCON1-KEY  ERR-K.
      *           READ     JCON       UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    "G"      TO   ERR-M
               MOVE    "JCON"   TO   ERR-F
               PERFORM  ERR-RTN THRU  ERR-EX
           END-IF
      *
           MOVE    "14"        TO    JCON1-KEY  ERR-K.
      *           READ     JCON       UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    "G"      TO   ERR-M
               MOVE    "JCON"   TO   ERR-F
               PERFORM  ERR-RTN THRU  ERR-EX
           END-IF
      *
           MOVE     SPACE      TO    ACT-WORK1  ACT-WORK2  W-AREA2.
           INITIALIZE                ACT-WORK1  ACT-WORK2  W-AREA2.
           INITIALIZE                SAV-88.
           MOVE     ZERO       TO    W-9D.
           MOVE     24         TO    ERR-LIN.
           CALL "SD_Arg_Match_Line" USING
            "ERR-LIN" "2" ERR-LIN RETURNING RESU.
       INI-EX.
           EXIT.
      ******************************
      *    ＥＮＤ－ＲＴＮ          *
      *          ～終了処理～      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE J-DATE_IDLST J-DATE_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-RUIJ_IDLST JT-RUIJ_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JAZF_IDLST JAZF_PNAME1.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    ＡＣＴ－ＲＴＮ                 *
      *          ～画面入力＊更新処理～   *
      *************************************
       ACT-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-RTN
           END-IF
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p" RETURNING RESU.
           IF  W-ACT  NOT =  1  AND  2  AND  3
               GO  TO  ACT-RTN
           END-IF
           IF  W-ACT      =  1
               MOVE     "追加"   TO    W-NAM
           END-IF
           IF  W-ACT      =  2
               MOVE     "変更"   TO    W-NAM
           END-IF
           IF  W-ACT      =  3
               MOVE     "取消"   TO    W-NAM
           END-IF
           CALL "SD_Output" USING "DSP-NAM" DSP-NAM "p" RETURNING RESU.
           MOVE    "OFF"       TO    2K-SW.
       ACT-005.
           MOVE     1          TO    A.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           PERFORM  CR1-RTN    THRU  CR1-EX.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "D-KSU-CLE" D-KSU-CLE "p" RETURNING RESU.
           INITIALIZE          ACT-WORK2.
           IF  W-DNO  NOT =  ZERO
               CALL "SD_Output" USING
                "DSP-DNO" DSP-DNO "p" RETURNING RESU
           END-IF.
       ACT-010.
           IF  W-ACT      =  1
               CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU
               MOVE     SAV-88   TO    W-88
               CALL "SD_Output" USING "ACP-8A" ACP-8A "p" RETURNING RESU
               CALL "SD_Output" USING "ACP-8" ACP-8 "p" RETURNING RESU
               GO  TO   ACT-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-1 "ACP-1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-010
           END-IF
           CALL "SD_Output" USING "ACP-1" ACP-1 "p" RETURNING RESU.
           IF  JS-SIGN  =   0
               IF  W1-1  NOT  =  0
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO     ACT-010
               END-IF
           END-IF
           IF  JS-SIGN  =   1
               IF  W1-1  NOT  =  1 AND 2
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO     ACT-010
               END-IF
           END-IF
           MOVE     0          TO    DIS-SW.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02  A  CNT.
      *           START    JSTR KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   ACT-014
           END-IF.
       ACT-012.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               ADD   1       TO  A
               GO  TO  ACT-014
           END-IF
           IF  W-1    NOT =  JSTR-01
               ADD   1       TO  A
               GO  TO  ACT-014
           END-IF
           MOVE  JSTR-02     TO  A.
           IF  DIS-SW NOT =  0
               GO  TO  ACT-013
           END-IF
           PERFORM  CR1-RTN    THRU  CR1-EX.
           MOVE     JSTR-14A   TO    W-5A.
           MOVE     JSTR-14D   TO    W-8A.
           MOVE     JSTR-15    TO    W-8.
           MOVE     JSTR-03    TO    W-21.
           MOVE     JSTR-14B   TO    W-92.
           MOVE     JSTR-14C   TO    W-93.
           IF  JSTR-14B   NOT   =    ZERO
               CALL "SD_Output" USING
                "A-92" A-92 "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-93" A-93 "p" RETURNING RESU
           END-IF
           IF  W-21       =  0
               MOVE     JSTR-04    TO    W-3
           ELSE
               MOVE     JSTR-05    TO    W-3
           END-IF
           IF  W-21       =  0
               MOVE     "出荷"   TO    W-22
           ELSE
               IF  W-21      =  3
                   MOVE     "訂正"   TO    W-22
               ELSE
                   IF  W-21      =  5
                       MOVE     "返品"   TO    W-22
                   ELSE
                       IF  W-21      =  6
                           MOVE     "不良"   TO    W-22
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE     JSTR-061   TO    W-41  TC-TCD.
           MOVE     1          TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-43.
           MOVE     JSTR-062   TO    W-42  TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-44.
           MOVE     TC-JSU     TO    W-45.
           MOVE     3          TO    JCON3-01.
           MOVE     JSTR-07    TO    W-51  JCON3-02  O-SCD.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON3-03
           END-IF
           MOVE     JCON3-03   TO    W-52.
           MOVE     2          TO    JCON2-01.
           MOVE     JSTR-14    TO    W-71  JCON2-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON2-03
           END-IF
           MOVE     JCON2-03   TO    W-72.
           IF  TC-BIK          =  ZERO
               CALL "SD_Output" USING
                "DSP-BIKHO-CLE" DSP-BIKHO-CLE "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-BIKHO" DSP-BIKHO "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "ACP-2" ACP-2 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-41" A-41 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-42" A-42 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-43" D-43 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-44" D-44 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-45" D-45 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-5" ACP-5 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-7" ACP-7 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-8" ACP-8 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-8A" ACP-8A "p" RETURNING RESU.
           CALL "SD_Output" USING "D-72" D-72 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
           IF  W-21       =  5  OR  6
               MOVE  JSTR-15A   TO  W-KSU
               CALL "SD_Output" USING "D-KSUM" D-KSUM "p" RETURNING RESU
               CALL "SD_Output" USING "D-KSU" D-KSU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-KSU-CLE" D-KSU-CLE "p" RETURNING RESU
           END-IF
           MOVE     JSTR-4012  TO    W-4012.
           MOVE  1     TO  DIS-SW.
       ACT-013.
           IF  W-21       =  0
               IF  (JSTR-1211(1) = ZERO) AND (JSTR-1211(2) = ZERO) AND
                   (JSTR-1211(3) = ZERO) AND (JSTR-1211(4) = ZERO) AND
                   (JSTR-1211(5) = ZERO) AND (JSTR-1211(6) = ZERO) AND
                   (JSTR-1211(7) = ZERO) AND (JSTR-1211(8) = ZERO) AND
                   (JSTR-1211(9) = ZERO) AND (JSTR-1211(10) = ZERO)
                   CONTINUE
               ELSE
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO   ACT-010
               END-IF
           END-IF
           IF  W-21   NOT =  0
               IF  JSTR-17   NOT =  9
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO   ACT-010
               END-IF
           END-IF
           MOVE  JSTR-111(1)  TO  W-64(A 1) O-SU(A 1).
           MOVE  JSTR-111(2)  TO  W-64(A 2) O-SU(A 2).
           MOVE  JSTR-111(3)  TO  W-64(A 3) O-SU(A 3).
           MOVE  JSTR-111(4)  TO  W-64(A 4) O-SU(A 4).
           MOVE  JSTR-111(5)  TO  W-64(A 5) O-SU(A 5).
           MOVE  JSTR-111(6)  TO  W-64(A 6) O-SU(A 6).
           MOVE  JSTR-111(7)  TO  W-64(A 7) O-SU(A 7).
           MOVE  JSTR-111(8)  TO  W-64(A 8) O-SU(A 8).
           MOVE  JSTR-111(9)  TO  W-64(A 9) O-SU(A 9).
           MOVE  JSTR-111(10) TO  W-64(A 10) O-SU(A 10).
           MOVE  JSTR-112     TO  W-65(A).
           MOVE  JSTR-08      TO  W-61(A)  O-JNO(A).
           MOVE  JSTR-09      TO  W-621(A)  HI-MHCD HI-HCD O-HCD(A).
           MOVE  JSTR-10      TO  W-63(A)  O-SIZ(A).
      *           READ     HI2-M      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    HI-NAME
           END-IF
           MOVE     HI-NAME    TO    W-622(A).
           MOVE     JSTR-13    TO    W-66(A).
           MOVE     JSTR-20    TO    W-67(A).
       ACT-013A.
           IF  CNT NOT = A
               ADD   1   TO  CNT
               ADD   2   TO  LIN1
               CALL "SD_Arg_Match_Line" USING
                "LIN1" "2" LIN1 RETURNING RESU
               GO  TO  ACT-013A
           END-IF
           IF  W-61(A)        =  ZERO
               CALL "SD_Output" USING
                "A-612A" A-612A "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "A-611" A-611 "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-612" A-612 "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-613" A-613 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-6B" DSP-6B "p" RETURNING RESU.
           CALL "SD_Output" USING "A-67" A-67 "p" RETURNING RESU.
           ADD      1          TO    CNT.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           IF  JSTR-4012       =  0
               IF  JSTR-4011   NOT =  STN-NO-02
                   CALL "SD_Output" USING
                    "ERR-STN" ERR-STN "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               END-IF
           END-IF
           GO  TO   ACT-012.
       ACT-014.
           IF  CNT        =  1
               CALL "SD_Output" USING
                "INV-T01" INV-T01"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-010
           END-IF
           MOVE     ZERO     TO        W-KEI.
           IF  W-621(1)   <  999900
               ADD   W-65(1)  TO  W-KEI
           END-IF
           IF  W-621(2)   <  999900
               ADD   W-65(2)  TO  W-KEI
           END-IF
           IF  W-621(3)   <  999900
               ADD   W-65(3)  TO  W-KEI
           END-IF
           IF  W-621(4)   <  999900
               ADD   W-65(4)  TO  W-KEI
           END-IF
           IF  W-621(5)   <  999900
               ADD   W-65(5)  TO  W-KEI
           END-IF
           IF  W-621(6)   <  999900
               ADD   W-65(6)  TO  W-KEI
           END-IF
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
      *
           IF  W-71   NOT =  6
               GO  TO  ACT-015
           END-IF
           MOVE     W-92      TO       OKJF-KEY.
      *           READ     OKJF    UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO   TO     ACT-015
           END-IF
           MOVE     OKJF-12    TO    W-KIN.
           CALL "SD_Output" USING
            "D-KSU-CLE" D-KSU-CLE "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KINM" D-KINM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       ACT-015.
           IF  W-ACT      =  2
               GO  TO  ACT-030
           END-IF
           IF  W-ACT      =  3
               GO  TO  ACT-150
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-21 "A-21" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT  =  1
                   GO  TO  ACT-RTN
               ELSE
                   GO  TO  ACT-010
               END-IF
           END-IF
           IF  ESTAT      =  "05"
               IF  W-ACT  =  1
                   MOVE   NEX-2     TO   W-2
                   MOVE   NEX-3     TO   W-3
                   MOVE   NEX-4     TO   W-4
                   MOVE   NEX-5     TO   W-5
                   MOVE   NEX-7     TO   W-7
                   MOVE   NEX-88    TO   W-88
                   MOVE   NEX-9     TO   W-9
                   CALL "SD_Output" USING
                    "ACP-2" ACP-2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-3" ACP-3 "p" RETURNING RESU
                   CALL "SD_Output" USING "A-41" A-41 "p" RETURNING RESU
                   CALL "SD_Output" USING "A-42" A-42 "p" RETURNING RESU
                   CALL "SD_Output" USING "D-43" D-43 "p" RETURNING RESU
                   CALL "SD_Output" USING "D-44" D-44 "p" RETURNING RESU
                   CALL "SD_Output" USING "D-45" D-45 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-5" ACP-5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-7" ACP-7 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-8" ACP-8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-8A" ACP-8A "p" RETURNING RESU
                   CALL "SD_Output" USING "D-72" D-72 "p" RETURNING RESU
                   CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU
                   CALL "SD_Output" USING "A-91" A-91 "p" RETURNING RESU
                   CALL "SD_Output" USING "A-92" A-92 "p" RETURNING RESU
                   CALL "SD_Output" USING "A-93" A-93 "p" RETURNING RESU
                   GO  TO  ACT-030
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-020
           END-IF
           CALL "SD_Output" USING "A-21" A-21 "p" RETURNING RESU.
           IF  W-21   NOT =  0  AND  3  AND  5  AND  6
               GO  TO  ACT-020
           END-IF
           IF  W-21       =  0
               MOVE     "出荷"   TO    W-22
           END-IF
           IF  W-21       =  3
               MOVE     "訂正"   TO    W-22
           END-IF
           IF  W-21       =  5
               MOVE     "返品"   TO    W-22
           END-IF
           IF  W-21       =  6
               MOVE     "不良"   TO    W-22
           END-IF
           CALL "SD_Output" USING "D-22" D-22 "p" RETURNING RESU.
           IF  W-21       =  5  OR  6
               CALL "SD_Output" USING "D-KSUM" D-KSUM "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-KSU-CLE" D-KSU-CLE "p" RETURNING RESU
           END-IF.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE A-31 "A-31" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT   =    2
                   GO  TO  ACT-010
               ELSE
                   GO  TO  ACT-020
               END-IF
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-030
           END-IF
           CALL "SD_Output" USING "A-31" A-31 "p" RETURNING RESU.
       ACT-031.
           CALL "SD_Accept" USING BY REFERENCE A-32 "A-32" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-030
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-031
           END-IF
           CALL "SD_Output" USING "A-32" A-32 "p" RETURNING RESU.
           IF  (W-32  = ZERO)  AND  (W-312 = ZERO)
               GO  TO  ACT-032
           END-IF
           IF  (W-32  <  1)  OR  (W-32  >  12)
               GO  TO  ACT-031
           END-IF.
       ACT-032.
           CALL "SD_Accept" USING BY REFERENCE A-33 "A-33" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-031
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-032
           END-IF
           CALL "SD_Output" USING "A-33" A-33 "p" RETURNING RESU.
           IF  (W-33  =  ZERO)  AND  (W-32  = ZERO)
               GO  TO  ACT-033
           END-IF
           IF  (W-33  <  1)  OR  (W-33  >  31)
               GO  TO  ACT-032
           END-IF
           MOVE  ZERO      TO  W-311.
           IF W-312 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-31
           END-IF
           IF W-312 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-31
           END-IF.
       ACT-032A.
           IF  W-3  <  W-SNGP  OR  >  W-ENGP
               CALL "SD_Output" USING
                "ERR-15" ERR-15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               IF  W-21       NOT =  3
                   GO  TO  ACT-030
               ELSE
                   GO  TO  ACT-040
               END-IF
           END-IF
           IF  W-ACT =  1
               IF ( W-3 < WYMD ) OR ( W-3 > WNGP )
                  CALL "SD_Output" USING
                   "ERR-15" ERR-15 "p" RETURNING RESU
                  CALL "SD_Output" USING
                   "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
                  CALL "SD_Output" USING
                   "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               END-IF
           END-IF
           GO  TO  ACT-040.
       ACT-033.
           IF  W-ACT  =  1
               IF  SAV-3  =  ZERO
                   MOVE  WYMD       TO  W-3
               ELSE
                   MOVE  SAV-3      TO  W-3
               END-IF
           ELSE
               MOVE  WYMD       TO  W-3
           END-IF
           CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU.
       ACT-040.
           CALL "SD_Accept" USING BY REFERENCE A-41 "A-41" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-030
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-040
           END-IF
           CALL "SD_Output" USING "A-41" A-41 "p" RETURNING RESU.
       ACT-042.
           MOVE     W-41       TO    TC-TCD.
           MOVE     1          TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M03" INV-M03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-040
           END-IF
           MOVE     TC-NAME    TO    W-43.
           CALL "SD_Output" USING "D-43" D-43 "p" RETURNING RESU.
       ACT-043.
           CALL "SD_Accept" USING BY REFERENCE A-42 "A-42" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-040
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-043
           END-IF.
       ACT-043-R.
           CALL "SD_Output" USING "A-42" A-42 "p" RETURNING RESU.
           MOVE     W-41       TO    TC-TCD.
           MOVE     W-42       TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M03" INV-M03"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-043
           END-IF
           MOVE     TC-NAME    TO    W-44.
           MOVE     TC-JSU     TO    W-45.
           CALL "SD_Output" USING "D-44" D-44 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-45" D-45 "p" RETURNING RESU.
           IF  TC-SSC     =  1
               GO  TO  ACT-043
           END-IF
           IF  TC-BIK          =  ZERO
               CALL "SD_Output" USING
                "DSP-BIKHO-CLE" DSP-BIKHO-CLE "p" RETURNING RESU
           ELSE
               IF  W-ACT       =  1
                   CALL "SD_Output" USING
                    "DSP-BIKHO" DSP-BIKHO "p" RETURNING RESU
               END-IF
           END-IF.
       ACT-050.
           CALL "SD_Accept" USING BY REFERENCE A-51 "A-51" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-043
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-050
           END-IF
           CALL "SD_Output" USING "A-51" A-51 "p" RETURNING RESU.
           MOVE     3          TO    JCON3-01.
           MOVE     W-51       TO    JCON3-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D02" INV-D02"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-050
           END-IF
           MOVE     JCON3-03   TO    W-52.
           CALL "SD_Output" USING "D-52" D-52 "p" RETURNING RESU.
           IF  W-51       =  9
               GO  TO  ACT-050
           END-IF
           IF  W-21       =  5  OR  6
               MOVE  ZERO       TO  W-5A
               CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU
               GO  TO  ACT-060
           END-IF.
       ACT-055.
           CALL "SD_Accept" USING BY REFERENCE A-53 "A-53" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-050
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-055
           END-IF
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
       ACT-060.
           IF  2K-SW  NOT =  "ON "
               MOVE     10         TO    LIN1
               CALL "SD_Arg_Match_Line" USING
                "LIN1" "2" LIN1 RETURNING RESU
               MOVE     1          TO    A  B  C
           END-IF
           IF  2K-SW  =  "ON "
               GO  TO  ACT-125
           END-IF.
       ACT-061.
           IF  W-21      =  5  OR  6
               MOVE  ZERO      TO  W-61(A)  W-66(A)
               CALL "SD_Output" USING "A-612A" A-612A "p" RETURNING RESU
               GO  TO  ACT-080
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-611 "A-611" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-613" A-613 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  A      =  1
                   GO  TO  ACT-055
               ELSE
                   SUBTRACT 2          FROM  LIN1
                   CALL "SD_Arg_Match_Line" USING
                    "LIN1" "2" LIN1 RETURNING RESU
                   SUBTRACT 1          FROM  A
                   GO  TO   ACT-061
               END-IF
           END-IF
           IF  (ESTAT = "04")  AND  (A  NOT  =  1)
               PERFORM  CR1-RTN    THRU  CR1-EX
               GO  TO   ACT-120
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-061
           END-IF
           CALL "SD_Output" USING "A-611" A-611 "p" RETURNING RESU.
           IF  W-611(A)   =  ZERO
               MOVE     ZERO       TO  W-61(A) W-66(A)
               CALL "SD_Output" USING "A-612A" A-612A "p" RETURNING RESU
               GO  TO   ACT-080
           END-IF
           IF  W-611(A)   =  999999
               MOVE     9          TO  W-612(A)
               MOVE     4          TO  W-66(A)
               CALL "SD_Output" USING "A-612" A-612 "p" RETURNING RESU
               GO  TO   ACT-080
           END-IF
           MOVE  W-611(A)  TO  W-611A.
           IF  JS-SIGN  =    0
               IF  W6-01   NOT   =  0 AND 1
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO  ACT-061
               END-IF
           END-IF
           IF  JS-SIGN  =    1
               IF  W6-01   NOT   =  2 AND 3 AND 4 AND 5
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO  ACT-061
               END-IF
           END-IF.
       ACT-062.
           CALL "SD_Accept" USING BY REFERENCE A-612 "A-612" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-061
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-062
           END-IF
           CALL "SD_Output" USING "A-612" A-612 "p" RETURNING RESU.
           MOVE  W-611(A)  TO  JMSTD-07.
           MOVE  W-612(A)  TO  JMSTD-08.
      *           READ  JMSTD  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M05" INV-M05"p" RETURNING RESU
               GO  TO  ACT-061
           END-IF
           IF  JMSTD-01  NOT =  0  AND  5  AND  6
               CALL "SD_Output" USING
                "INV-M05" INV-M05"p" RETURNING RESU
               GO  TO  ACT-061
           END-IF.
       ACT-070.
           IF  W-41       =  JMSTD-04
               GO  TO  ACT-077
           END-IF
      *----  教育四国（見藤）分  ---------------------------------------
           IF  JMSTD-04   =  5350
               IF  JMSTD-10   =  555
                   GO  TO  ACT-075
               END-IF
           END-IF
           IF  JMSTD-01   =  5
               GO  TO  ACT-073
           END-IF
           IF (W-41   NOT =  2650)  AND (W-41   NOT = 2652)
               GO  TO  ACT-073
           END-IF
           IF (W-41       =  2650)  AND (JMSTD-04  NOT = 2652)
               GO  TO  ACT-073
           END-IF
           IF (W-41       =  2652)  AND (JMSTD-04  NOT = 2650)
               GO  TO  ACT-073
           END-IF
           GO  TO  ACT-078.
       ACT-073.
           CALL "SD_Output" USING "ERR-TM" ERR-TM "p" RETURNING RESU.
           GO  TO  ACT-061.
       ACT-075.
           MOVE  W-41      TO  T-KEY.
      *           READ  T-M    UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO  T-TNC
           END-IF
           IF  T-TNC  NOT =  81
               CALL "SD_Output" USING "ERR-TM" ERR-TM "p" RETURNING RESU
               GO  TO  ACT-061
           END-IF
           GO  TO  ACT-078.
       ACT-077.
           IF  W-42   NOT =  JMSTD-10
               CALL "SD_Output" USING
                "ERR-TCM" ERR-TCM "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
           END-IF.
       ACT-078.
           MOVE     JMSTD-09   TO    W-63(A).
           MOVE     JMSTD-03   TO    W-621(A)  HI-MHCD HI-HCD.
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M02" INV-M02"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-060
           END-IF
           MOVE     HI-NAME    TO    W-622(A).
           MOVE     JMSTD-01   TO    W-66(A).
           COMPUTE  W-JYUSU    =     JMSTD-1111(1) + JMSTD-1111(2)
                                   + JMSTD-1111(3) + JMSTD-1111(4)
                                   + JMSTD-1111(5) + JMSTD-1111(6)
                                   + JMSTD-1111(7) + JMSTD-1111(8)
                                   + JMSTD-1111(9) + JMSTD-1111(10).
           IF  JMSTD-16   =     ZERO
               MOVE     ZERO      TO     W-SET
           ELSE
               COMPUTE  W-SET      =     W-JYUSU  /  JMSTD-16
           END-IF
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
      **   ｼﾞｭﾁｭｳｽｳ ﾋｮｳｼﾞ  **
           MOVE  ZERO     TO   W-65(A)  Z-SW  S-65.
           PERFORM  CHK2-RTN   THRU  CHK2-EX.
           MOVE  ZERO     TO    CHK-SW.
           IF  W-ACT            =  2   AND W-61(A)          =  O-JNO(A)
               GO  TO  ACT-080
           END-IF
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           IF  W-21             =  0
               CALL "SD_Output" USING "DSP-6B" DSP-6B "p" RETURNING RESU
               CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU
           END-IF
           MOVE  JMSTD-22       TO  W-67(A).
           CALL "SD_Output" USING "A-67" A-67 "p" RETURNING RESU.
           IF  T-SW         NOT =  1
               MOVE  JMSTD-13      TO  W-88D
               MOVE  W-8AD         TO  W-8A
               MOVE  W-8D          TO  W-82
               CALL "SD_Output" USING "ACP-8" ACP-8 "p" RETURNING RESU
               CALL "SD_Output" USING "ACP-8A" ACP-8A "p" RETURNING RESU
           END-IF.
       ACT-080.
           CALL "SD_Accept" USING BY REFERENCE A-621 "A-621" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-21   NOT =   5  AND  6
                   GO  TO  ACT-061
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               IF  W-21       =   5  OR   6
                   IF  A      =  1                 
                       GO  TO  ACT-050
                   ELSE
                       SUBTRACT 2          FROM  LIN1
                       CALL "SD_Arg_Match_Line" USING
                        "LIN1" "2" LIN1 RETURNING RESU
                       SUBTRACT 1          FROM  A
                       GO  TO   ACT-080
                   END-IF
               END-IF
           END-IF
           IF  (ESTAT = "04")  AND  (A  NOT  =  1)
               PERFORM  CR1-RTN    THRU  CR1-EX
               GO  TO   ACT-120
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-080
           END-IF
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M02" INV-M02"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-080
           END-IF
           IF  W-21    NOT =  5  AND  6
               IF  HI-ISU  =    ZERO
                   CALL "SD_Output" USING
                    "ERR-14" ERR-14 "p" RETURNING RESU
                   GO  TO   ACT-080
               END-IF
           END-IF
           MOVE     HI-ISU     TO    W-KA(A).
           IF  W-21    NOT =  5  AND  6
               IF  W-5A    =    ZERO
                   IF  W-KA(A)  NOT   =    W-KA(1)
                       CALL "SD_Output" USING
                        "ERR-08" ERR-08 "p" RETURNING RESU
                       GO  TO   ACT-080
                   END-IF
               END-IF
           END-IF
           MOVE     HI-NAME    TO    W-622(A).
           MOVE     HI-BC3     TO    W-BUN.
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
           IF  JS-SIGN   =   0
               IF  W-B1  NOT  =  3
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  JS-SIGN   =   1
               IF  W-B1      =  3
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  W-ACT NOT =   1
               GO  TO  ACT-084
           END-IF
           IF  W-21  NOT =   0
               GO  TO  ACT-084
           END-IF
           IF  W-611(A)  NOT =   ZERO
               GO  TO  ACT-082
           END-IF
           MOVE  SPACE     TO  JMSTD-KEY2.
           MOVE  W-41      TO  JMSTD-04.
           MOVE  W-621(A)  TO  JMSTD-05.
      *           START JMSTD  KEY  NOT <  JMSTD-KEY2  INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY2" " NOT < " JMSTD-KEY2
            RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-082
           END-IF
           MOVE  ZERO          TO  Z-SW.
           PERFORM  AZC-RTN    THRU  AZC-EX.
           IF  Z-SW          =   1
               CALL "SD_Output" USING
                "ERR-21" ERR-21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
           END-IF
           IF  Z-SW          =   2
               CALL "SD_Output" USING
                "ERR-22" ERR-22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
           END-IF.
       ACT-082.
           MOVE     SPACE      TO    NJZAI-KEY.
           MOVE     W-51       TO    NJZAI-01.
           MOVE     W-621(A)   TO    NJZAI-02.
      *           START    NJZAI   KEY  NOT <  NJZAI-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-083
           END-IF
      *           READ     NJZAI      NEXT  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-083
           END-IF
           IF  (W-51     NOT =  NJZAI-01)  OR
               (W-621(A) NOT =  NJZAI-02)
               GO  TO  ACT-083
           END-IF
           GO  TO  ACT-084.
       ACT-083.
           MOVE     SPACE      TO    NJZAI-KEY.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(A)   TO    NJZAI-02.
      *           START    NJZAI   KEY  NOT <  NJZAI-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-084
           END-IF
      *           READ     NJZAI      NEXT  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-084
           END-IF
           IF  W-621(A) NOT =  NJZAI-02
               GO  TO  ACT-084
           END-IF
           CALL "SD_Output" USING "ERR-03" ERR-03 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU.
       ACT-084.
           IF  W-611(A)  =   ZERO  OR  999999
               GO  TO  ACT-085
           END-IF
           MOVE  JMSTD-03     TO  WJM-03.
           IF  W-621A(A)      =   WJM-031
               GO  TO  ACT-090
           END-IF
           MOVE  20           TO  RUIJ-01.
           MOVE  JMSTD-03     TO  RUIJ-02.
           MOVE  W-621(A)     TO  RUIJ-03.
      *           READ  JT-RUIJ      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-RUIJ_PNAME1 BY REFERENCE RUIJ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-RUI" INV-RUI"p" RETURNING RESU
               GO  TO  ACT-080
           END-IF
           GO  TO  ACT-090.
       ACT-085.
           CALL "SD_Accept" USING BY REFERENCE A-63 "A-63" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACT-080
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-085
           END-IF
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           IF  W-63(A)  NOT  =  1  AND  2  AND  3  AND  4
               GO  TO  ACT-085
           END-IF
      *-----------------------------------------------------------------
           IF  W-ACT     NOT  =   1
               GO  TO  ACT-090
           END-IF
           IF  W-21      NOT  =   0
               GO  TO  ACT-090
           END-IF
           IF  W-621(A)      >=   999900
               GO  TO  ACT-090
           END-IF
      *
           MOVE     ZERO       TO    W-ZSUD.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(A)   TO    NJZAI-02.
           MOVE     W-63 (A)   TO    NJZAI-03.
      *           READ     NJZAI      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO ACT-090
           END-IF
           COMPUTE  W-ZSU(01)  =  NJZAI-0411(01)  -  NJZAI-0511(01)
                               +  NJZAI-0611(01)  +  NJZAI-0711(01)
                               -  NJZAI-0811(01)  -  NJZAI-0911(01)
                               +  NJZAI-1111(01).
           COMPUTE  W-ZSU(02)  =  NJZAI-0411(02)  -  NJZAI-0511(02)
                               +  NJZAI-0611(02)  +  NJZAI-0711(02)
                               -  NJZAI-0811(02)  -  NJZAI-0911(02)
                               +  NJZAI-1111(02).
           COMPUTE  W-ZSU(03)  =  NJZAI-0411(03)  -  NJZAI-0511(03)
                               +  NJZAI-0611(03)  +  NJZAI-0711(03)
                               -  NJZAI-0811(03)  -  NJZAI-0911(03)
                               +  NJZAI-1111(03).
           COMPUTE  W-ZSU(04)  =  NJZAI-0411(04)  -  NJZAI-0511(04)
                               +  NJZAI-0611(04)  +  NJZAI-0711(04)
                               -  NJZAI-0811(04)  -  NJZAI-0911(04)
                               +  NJZAI-1111(04).
           COMPUTE  W-ZSU(05)  =  NJZAI-0411(05)  -  NJZAI-0511(05)
                               +  NJZAI-0611(05)  +  NJZAI-0711(05)
                               -  NJZAI-0811(05)  -  NJZAI-0911(05)
                               +  NJZAI-1111(05).
           COMPUTE  W-ZSU(06)  =  NJZAI-0411(06)  -  NJZAI-0511(06)
                               +  NJZAI-0611(06)  +  NJZAI-0711(06)
                               -  NJZAI-0811(06)  -  NJZAI-0911(06)
                               +  NJZAI-1111(06).
           COMPUTE  W-ZSU(07)  =  NJZAI-0411(07)  -  NJZAI-0511(07)
                               +  NJZAI-0611(07)  +  NJZAI-0711(07)
                               -  NJZAI-0811(07)  -  NJZAI-0911(07)
                               +  NJZAI-1111(07).
           COMPUTE  W-ZSU(08)  =  NJZAI-0411(08)  -  NJZAI-0511(08)
                               +  NJZAI-0611(08)  +  NJZAI-0711(08)
                               -  NJZAI-0811(08)  -  NJZAI-0911(08)
                               +  NJZAI-1111(08).
           COMPUTE  W-ZSU(09)  =  NJZAI-0411(09)  -  NJZAI-0511(09)
                               +  NJZAI-0611(09)  +  NJZAI-0711(09)
                               -  NJZAI-0811(09)  -  NJZAI-0911(09)
                               +  NJZAI-1111(09).
           COMPUTE  W-ZSU(10)  =  NJZAI-0411(10)  -  NJZAI-0511(10)
                               +  NJZAI-0611(10)  +  NJZAI-0711(10)
                               -  NJZAI-0811(10)  -  NJZAI-0911(10)
                               +  NJZAI-1111(10).
           IF  ZERO              =  W-ZSU(01)  AND  W-ZSU(02)  AND
                                    W-ZSU(03)  AND  W-ZSU(04)  AND
                                    W-ZSU(05)  AND  W-ZSU(06)  AND
                                    W-ZSU(07)  AND  W-ZSU(08)  AND
                                    W-ZSU(09)  AND  W-ZSU(10)
               GO  TO  ACT-090
           END-IF
           MOVE     W-ZSUD     TO    W-64A(A).
           COMPUTE  W-65(A)      =  W-ZSU(01) + W-ZSU(02) + W-ZSU(03)
                                  + W-ZSU(04) + W-ZSU(05) + W-ZSU(06)
                      + W-ZSU(07) + W-ZSU(08) + W-ZSU(09) + W-ZSU(10).
           CALL "SD_Output" USING "DSP-6B" DSP-6B "p" RETURNING RESU.
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
      *-----------------------------------------------------------------
       ACT-090.
           MOVE     0          TO    INP-SW.
           MOVE     1          TO    B.
           MOVE     13         TO    COL1.
           CALL "SD_Arg_Match_Col" USING
            "COL1" "2" COL1 RETURNING RESU.
       ACT-091.
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  (W-63(A)    =  4)  AND  (B   =   10)
               MOVE     ZERO       TO    W-64(A , B)
               CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
               GO  TO   ACT-092
           END-IF
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EXT.
           MOVE  1     TO  INP-SW.
           CALL "SD_Accept" USING BY REFERENCE A-64 "A-64" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT.
           IF  (ESTAT  =  "09")  AND  (B  =  1)
               IF  W-611(A)  =  0  OR  999999
                   GO  TO  ACT-085
               ELSE
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               GO  TO  ACT-093
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-091
           END-IF
           CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU.
           IF  B          >  7
               IF  W-64(A,B)       >  999  OR  <  -999
                   GO  TO  ACT-091
               END-IF
           END-IF
           IF  W-5A       <  2
               GO  TO  ACT-092
           END-IF
           IF  W-64(A,B)  =  ZERO
               GO  TO  ACT-092
           END-IF
           DIVIDE W-5A INTO W-64(A,B) GIVING W-KSD REMAINDER W-AMD.
           IF  W-AMD  NOT =  ZERO
               CALL "SD_Output" USING
                "ERR-SET" ERR-SET "p" RETURNING RESU
               GO  TO  ACT-091
           END-IF.
       ACT-092.
           IF  B      NOT =  10
               ADD      1          TO    B
               ADD      6          TO    COL1
               CALL "SD_Arg_Match_Col" USING
                "COL1" "2" COL1 RETURNING RESU
               GO  TO   ACT-091
           END-IF
           GO  TO   ACT-100.
       ACT-093.
           SUBTRACT 1          FROM  B.
           SUBTRACT 6          FROM  COL1.
           CALL "SD_Arg_Match_Col" USING
            "COL1" "2" COL1 RETURNING RESU.
           IF  B          =  ZERO
               IF  W-61(A)    =  ZERO
                   GO  TO  ACT-085
               ELSE
                   GO  TO   ACT-061
               END-IF
           END-IF
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           GO  TO   ACT-091.
       ACT-100.
           COMPUTE  W-65(A) =  W-64(A , 1) + W-64(A , 2) + W-64(A , 3)
               + W-64(A , 4) + W-64(A , 5) + W-64(A , 6) + W-64(A , 7)
               + W-64(A , 8) + W-64(A , 9) + W-64(A , 10).
           IF  W-65(A) = ZERO AND  INP-SW = 0
               CALL "SD_Output" USING
                "ERR-HIM" ERR-HIM "p" RETURNING RESU
               IF  W-61(A)    = ZERO
                   GO  TO  ACT-080
               ELSE
                   GO  TO  ACT-061
               END-IF
           END-IF
           IF  W-65(A)   =   ZERO
               CALL "SD_Output" USING
                "ERR-07" ERR-07 "p" RETURNING RESU
               GO  TO  ACT-090
           END-IF
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
           IF  (W-611(A)  =   ZERO  OR  999999)  OR
               (W-SET     =   ZERO)              OR
               (W-5A      =   ZERO)
               GO  TO  ACT-105
           END-IF
           IF  W-65(A)    NOT =  (W-SET * W-5A)
               CALL "SD_Output" USING
                "ERR-SET" ERR-SET "p" RETURNING RESU
               GO  TO  ACT-090
           END-IF.
       ACT-105.
           IF   TC-BIK         =  ZERO
                MOVE  SPACE      TO  W-67(A)
                CALL "SD_Output" USING "A-67" A-67 "p" RETURNING RESU
                GO  TO  ACT-110
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-67 "A-67" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT           =  "09"
               GO  TO  ACT-093
           END-IF
           IF  ESTAT       NOT =  "01"  AND  "06"
               GO  TO  ACT-105
           END-IF.
       ACT-110.
           ADD      1          TO    A.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           IF  A      NOT =  7
               GO  TO  ACT-061
           END-IF.
       ACT-120.
           MOVE     ZERO     TO        W-KEI.
           IF  W-621(1)   <  999900
               ADD   W-65(1)  TO  W-KEI
           END-IF
           IF  W-621(2)   <  999900
               ADD   W-65(2)  TO  W-KEI
           END-IF
           IF  W-621(3)   <  999900
               ADD   W-65(3)  TO  W-KEI
           END-IF
           IF  W-621(4)   <  999900
               ADD   W-65(4)  TO  W-KEI
           END-IF
           IF  W-621(5)   <  999900
               ADD   W-65(5)  TO  W-KEI
           END-IF
           IF  W-621(6)   <  999900
               ADD   W-65(6)  TO  W-KEI
           END-IF
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
       ACT-125.
           IF  W-21      =  3
               GO  TO  ACT-138
           END-IF
           IF  W-21      =  5  OR  6
               GO  TO  ACT-137
           END-IF
           IF  TC-MZC    =  1
               MOVE     W-82             TO    W-82D
               MOVE  "発送明細書在中"  TO    W-822
               MOVE     W-82D            TO    W-82
               CALL "SD_Output" USING "ACP-82" ACP-82 "p" RETURNING RESU
           END-IF.
       ACT-130.
           CALL "SD_Accept" USING BY REFERENCE A-71 "A-71" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  2K-SW  =  "ON "
                   GO  TO  ACT-055
               ELSE
                   SUBTRACT 2          FROM  LIN1
                   CALL "SD_Arg_Match_Line" USING
                    "LIN1" "2" LIN1 RETURNING RESU
                   SUBTRACT 1          FROM  A
                   IF  W-611(A)   =  0
                       GO  TO  ACT-080
                   ELSE
                       GO  TO  ACT-061
                   END-IF
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-130
           END-IF
           CALL "SD_Output" USING "A-71" A-71 "p" RETURNING RESU.
           MOVE     2          TO    JCON2-01.
           MOVE     W-71       TO    JCON2-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D03" INV-D03"p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-130
           END-IF
           MOVE     JCON2-03   TO    W-72.
           CALL "SD_Output" USING "D-72" D-72 "p" RETURNING RESU.
       ACT-131.
           MOVE     "OF"     TO     FU-SW.
           MOVE      1       TO     O.
           MOVE      1       TO     M.
       ACT-132.
           IF  W-64(O , M)    <     0
               MOVE     "ON"     TO     FU-SW
               CALL "SD_Output" USING "C-131" C-131 "p" RETURNING RESU
               INITIALIZE        W-9
               GO   TO           ACT-138
           END-IF
           IF  W-71  =  9
               MOVE       ZERO      TO     W-9
               CALL "SD_Output" USING "C-131" C-131 "p" RETURNING RESU
               GO  TO  ACT-138
           END-IF
           ADD       1       TO     M.
           IF  M     >    10
               MOVE     1     TO     M
               ADD      1     TO     O
           END-IF
           IF  O    NOT    >     6
               GO  TO  ACT-132
           END-IF
      *
           CALL "SD_Output" USING
            "D-KSU-CLE" D-KSU-CLE "p" RETURNING RESU.
           IF  W-71        =     6
               CALL "SD_Output" USING "D-KINM" D-KINM "p" RETURNING RESU
               CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU
               MOVE  W-KIN      TO  W-KIND
           ELSE
               MOVE  ZERO       TO  W-KIN  W-KIND
           END-IF
           IF  W-ACT    NOT   =    1
               GO  TO  ACT-134
           END-IF.
       ACT-133.
           CALL "SD_Accept" USING BY REFERENCE A-91 "A-91" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-130
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-133
           END-IF
           IF  W-91   NOT =   0   AND   1
               GO  TO  ACT-133
           END-IF
           CALL "SD_Output" USING "A-91" A-91 "p" RETURNING RESU.
           IF  W-91    =    1
               GO  TO  ACT-134
           END-IF
           IF  W-92SW  =    1
               MOVE   W-92D      TO   W-92
               GO  TO  ACT-133A
           END-IF
           MOVE      1        TO       JCON1-01.
           MOVE      4        TO       JCON1-02.
      *           READ     JCON               INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-09" ERR-09 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               PERFORM     END-RTN   THRU   END-EX
               CALL "DB_Close"
               STOP        RUN
           END-IF
           IF  JS-SIGN    =    0
               IF  JCON1-03  =  099999
                   MOVE     000001    TO    W-92
               ELSE
                   COMPUTE  W-92   =   JCON1-03  +  1
               END-IF
           END-IF
           IF  JS-SIGN    =    1
               IF  JCON1-04  =  199999
                   MOVE     100000    TO    W-92
               ELSE
                   COMPUTE  W-92   =   JCON1-04  +  1
               END-IF
           END-IF
           MOVE      W-92      TO  W-92D.
           MOVE      1         TO  W-92SW.
           PERFORM   UPD1-RTN   THRU  UPD1-EX.
       ACT-133A.
           MOVE      0    TO     W-93.
           CALL "SD_Output" USING "A-92" A-92 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-93" A-93 "p" RETURNING RESU.
           IF  W-71        =     6
               GO  TO  ACT-136
           END-IF
           GO   TO   ACT-138.
       ACT-134.
           CALL "SD_Accept" USING BY REFERENCE A-92 "A-92" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT   =  2
                   GO  TO  ACT-130
               ELSE
                   GO  TO  ACT-133
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-134
           END-IF
           CALL "SD_Output" USING "A-92" A-92 "p" RETURNING RESU.
      *
           MOVE     W-92      TO       OKJF-KEY.
      *           READ     OKJF    UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-10" ERR-10 "p" RETURNING RESU
               GO   TO     ACT-134
           END-IF
           IF  W-4A      NOT    =    OKJF-05
               MOVE     "直送先"   TO    W-94
               CALL "SD_Output" USING
                "ERR-13" ERR-13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-13A" ERR-13A "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  W-3S     NOT    =    OKJF-03
               MOVE     "出荷日"   TO    W-94
               CALL "SD_Output" USING
                "ERR-13" ERR-13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-13A" ERR-13A "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  W-51     NOT    =    OKJF-04
               MOVE     "倉Ｃ　"   TO    W-94
               CALL "SD_Output" USING
                "ERR-13" ERR-13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-13A" ERR-13A "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  W-71     NOT    =    OKJF-02
               MOVE     "運送Ｃ"   TO    W-94
               CALL "SD_Output" USING
                "ERR-13" ERR-13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-13A" ERR-13A "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  OKJF-08    =    1
               CALL "SD_Output" USING
                "ERR-11" ERR-11 "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  OKJF-10    =    1
               CALL "SD_Output" USING
                "ERR-12" ERR-12 "p" RETURNING RESU
               GO   TO    ACT-134
           END-IF
           IF  W-ACT      =    1
               IF  OKJF-13     =    0
                   IF  W-5A    NOT =    ZERO
                       MOVE     "運送Ｃ"   TO    W-94
                       CALL "SD_Output" USING
                        "ERR-13" ERR-13 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "ERR-13A" ERR-13A "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                       GO   TO    ACT-134
                   END-IF
               END-IF
           END-IF
           MOVE  OKJF-12    TO  W-KIN.
           IF  W-71        =     6
               MOVE  W-KIN      TO  W-KIND
               CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU
           END-IF.
       ACT-135.
           CALL "SD_Accept" USING BY REFERENCE A-93 "A-93" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-134
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-135
           END-IF
           IF  (0 > W-93)  OR  (W-93 > 89)
               GO  TO  ACT-135
           END-IF
           CALL "SD_Output" USING "A-93" A-93 "p" RETURNING RESU.
           IF  W-21       =  5  OR  6
               GO  TO  ACT-137
           END-IF
           IF  W-71   NOT =  6
               GO  TO  ACT-138
           END-IF.
       ACT-136.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF (W-ACT  NOT =  1) OR (W-91  = 1)
                   GO  TO  ACT-135
               ELSE
                   GO  TO  ACT-133
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-136
           END-IF
           IF  W-KIN      =  ZERO
               IF  W-KIND     =  ZERO
                   GO  TO  ACT-136
               ELSE
                   MOVE  W-KIND     TO  W-KIN
               END-IF
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           GO  TO  ACT-138.
       ACT-137.
           CALL "SD_Accept" USING BY REFERENCE A-KSU "A-KSU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  A      =  1                        
                   GO  TO  ACT-050
               ELSE
                   SUBTRACT 2          FROM  LIN1
                   CALL "SD_Arg_Match_Line" USING
                    "LIN1" "2" LIN1 RETURNING RESU
                   SUBTRACT 1          FROM  A
                   GO  TO   ACT-080
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-137
           END-IF
           CALL "SD_Output" USING "D-KSU" D-KSU "p" RETURNING RESU.
           MOVE  SPACE      TO  W-8A.
           CALL "SD_Output" USING "ACP-8A" ACP-8A "p" RETURNING RESU.
           GO  TO  ACT-140.
       ACT-138.
           CALL "SD_Accept" USING BY REFERENCE ACP-8A "ACP-8A" "N" "18"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT =  "09"
               GO  TO  ACT-139
           END-IF
           IF  W-21  NOT =  0
               IF  2K-SW  =  "ON "
                   GO  TO  ACT-055
               ELSE
                   GO  TO  ACT-062
               END-IF
           END-IF
           IF   ( FU-SW   =    "ON")  OR  (W-71  =  9)
               GO  TO  ACT-130
           END-IF
           IF  W-71   =  6
               GO  TO  ACT-136
           END-IF
           IF  (W-91  =  1) OR (W-ACT = 2)
               GO  TO  ACT-135
           ELSE
               GO  TO  ACT-133
           END-IF.
       ACT-139.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-138
           END-IF
           CALL "SD_Output" USING "ACP-8A" ACP-8A "p" RETURNING RESU.
       ACT-140.
           CALL "SD_Accept" USING BY REFERENCE ACP-81 "ACP-81" "N" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-21       =  5  OR  6
                   GO  TO  ACT-137
               ELSE
                   GO  TO  ACT-138
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-140
           END-IF
           CALL "SD_Output" USING "ACP-81" ACP-81 "p" RETURNING RESU.
       ACT-141.
           CALL "SD_Accept" USING BY REFERENCE ACP-82 "ACP-82" "N" "38"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-140
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-141
           END-IF
           CALL "SD_Output" USING "ACP-82" ACP-82 "p" RETURNING RESU.
       ACT-150.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           MOVE    "OFF"      TO  2K-SW.
           IF  (ESTAT  =  "09")  AND  (W-ACT  =  3)
               GO  TO  ACT-010
           END-IF
           IF  ESTAT      =  "09"
               GO  TO  ACT-141
           END-IF
           IF  ESTAT      =  "03"   AND        W-ACT  =  1
               IF  W-21   NOT =  5  AND 6
                   MOVE  "ON "      TO  2K-SW
                   GO  TO  ACT-155
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-150
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  W-OKC  NOT =  1 AND 9
               GO  TO  ACT-150
           END-IF
           IF  W-OKC      =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO   ACT-160
           END-IF.
       ACT-155.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           IF  2K-SW  =  "ON "
               CALL "SD_Output" USING "OK-TKM" OK-TKM "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
           END-IF
           IF  W-92D  =   W-92
               MOVE   ZERO       TO  W-9D
           END-IF.
       ACT-160.
           IF  W-ACT  =   1
               MOVE   W-88  TO  SAV-88
               MOVE   W-3   TO  SAV-3
      *
               MOVE   W-2   TO  NEX-2
               MOVE   W-3   TO  NEX-3
               MOVE   W-4   TO  NEX-4
               MOVE   W-5   TO  NEX-5
               MOVE   W-7   TO  NEX-7
               MOVE   W-88  TO  NEX-88
               MOVE   W-9   TO  NEX-9
           ELSE
               MOVE   ZERO  TO  SAV-3
           END-IF
           IF  2K-SW   NOT =  "ON "
               GO  TO  ACT-005
           END-IF
           CALL "SD_Output" USING "DSP-DNO" DSP-DNO "p" RETURNING RESU.
           GO  TO   ACT-020.
       ACT-EX.
           EXIT.
      *************************************
      *    ＵＰＤ－ＲＴＮ                 *
      *          ～ファイル更新処理～     *
      *************************************
       UPD-RTN.
      *****コントロールＦ　更新
           IF  W-ACT       NOT =   1
               GO  TO  UPD-008
           END-IF
           MOVE     ZERO       TO    CHK-SW  C.
           MOVE     1          TO    JCON1-01.
           MOVE     2          TO    JCON1-02.
      *           READ     JCON       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON1-R
               INITIALIZE                JCON1-R
               MOVE     1          TO    CHK-SW JCON1-01
               MOVE     2          TO    JCON1-02
           END-IF.
       UPD-000.
           IF  JS-SIGN     NOT =   0
               GO  TO  UPD-001
           END-IF
           ADD     1           TO  JCON1-03.
           IF  JCON1-03        =   100000
               MOVE   000001       TO    JCON1-03
           END-IF
           IF  CHK-SW          =   1
               MOVE   100001       TO    JCON1-04
           END-IF
           GO  TO  UPD-003.
       UPD-001.
           ADD     1           TO  JCON1-04.
           IF  JCON1-04        =   200000
               MOVE   100001       TO    JCON1-04
           END-IF
           IF  CHK-SW          =   1
               MOVE    1           TO    JCON1-03
           END-IF.
       UPD-003.
           IF  JS-SIGN    =     0
               MOVE     JCON1-03   TO    JSTR-01
           END-IF
           IF  JS-SIGN    =     1
               MOVE     JCON1-04   TO    JSTR-01
           END-IF
           MOVE     ZERO       TO    JSTR-02.
      *           START    JSTR  KEY   NOT <  JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-004
           END-IF
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-004
           END-IF
           IF  JS-SIGN        =   0
               IF  JSTR-01     NOT =   JCON1-03
                   GO  TO  UPD-004
               ELSE
                   GO  TO   UPD-000
               END-IF
           END-IF
           IF  JS-SIGN        =   1
               IF  JSTR-01     NOT =   JCON1-04
                   GO  TO  UPD-004
               ELSE
                   GO  TO   UPD-000
               END-IF
           END-IF.
       UPD-004.
           IF  JS-SIGN     = 0
               MOVE  JCON1-03  TO  W-1
           ELSE
               MOVE  JCON1-04  TO  W-1
           END-IF
           IF  CHK-SW          =   ZERO
               GO  TO  UPD-005
           END-IF
      *           WRITE    JCON1-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"     TO    ERR-F
               MOVE     "W"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-008.
       UPD-005.
      *           REWRITE  JCON1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       UPD-008.
      *****送り状Ｆ　更新
           IF  FU-SW    =    "ON"
               GO  TO  UPD-010
           END-IF
           IF  W-92  =  ZERO
               GO  TO  UPD-010
           END-IF
           IF  W-21  NOT =  0
               GO  TO  UPD-010
           END-IF
           IF  W-71  =  9
               GO  TO  UPD-010
           END-IF
           MOVE     W-92       TO    OKJF-KEY.
      *           READ     OKJF       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-009
           END-IF
           IF  (W-71 NOT = OKJF-02)  OR  (W-3S NOT = OKJF-03)  OR
               (W-51 NOT = OKJF-04)  OR  (W-4A NOT = OKJF-05)
               MOVE     "OKJF ｱﾝﾏｯﾁ  "  TO    ERR-F
               MOVE     " "             TO    ERR-M
               PERFORM  ERR-RTN         THRU  ERR-EX
           END-IF
           IF  W-93            =     0
               MOVE     W-8A       TO    OKJF-06
           END-IF
           MOVE     ZERO       TO    OKJF-07   OKJF-08   OKJF-10.
           MOVE     JS-SIGN    TO    OKJF-09.
           IF  W-71            =     6
               MOVE     W-KIN      TO    OKJF-12
           END-IF
           IF  W-ACT    =  1
               IF  W-5A    NOT =  ZERO
                   MOVE  1        TO  OKJF-13
               ELSE
                   MOVE  0        TO  OKJF-13
               END-IF
           END-IF
      *           REWRITE  OKJF-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-010.
       UPD-009.
           INITIALIZE                OKJF-R.
           MOVE     W-92       TO    OKJF-KEY.
           MOVE     W-71       TO    OKJF-02.
           MOVE     W-3S       TO    OKJF-03.
           MOVE     W-51       TO    OKJF-04.
           MOVE     W-4A       TO    OKJF-05.
           MOVE     W-8A       TO    OKJF-06.
           MOVE     ZERO       TO    OKJF-07   OKJF-08   OKJF-10.
           MOVE     JS-SIGN    TO    OKJF-09.
           IF  W-71            =     6
               MOVE     W-KIN      TO    OKJF-12
           END-IF
           IF  W-5A    NOT =  ZERO
               MOVE  1        TO  OKJF-13
           END-IF
      *           WRITE    OKJF-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"     TO    ERR-F
               MOVE     "W"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       UPD-010.
      *****出荷指図トラン　更新
           IF  W-ACT           =     1
               GO  TO  UPD-020
           END-IF
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02.
      *           START    JSTR  KEY   NOT <  JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-020
           END-IF.
       UPD-015.
      *           READ     JSTR       NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-020
           END-IF
           IF  JSTR-01     NOT =   W-1
               GO  TO  UPD-020
           END-IF
      ***
      *           DELETE   JSTR       INVALID
      *///////////////
           CALL "DB_Delete" USING JSTR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "D"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-015.
       UPD-020.
           IF  W-ACT           =     3
               GO  TO  UPD-030
           END-IF
           MOVE     W-1        TO    W-DNO.
           MOVE     ZERO       TO    C.
       UPD-025.
           ADD      1          TO    C.
           IF  C               =     A
               GO  TO  UPD-030
           END-IF
           MOVE     SPACE      TO    JSTR-R.
           INITIALIZE                JSTR-R.
           MOVE     W-1        TO    JSTR-01.
           MOVE     C          TO    JSTR-02.
           MOVE     W-21       TO    JSTR-03.
           MOVE     W-3        TO    JSTR-04.
           MOVE     W-64(C 1) TO    JSTR-111(1).
           MOVE     W-64(C 2) TO    JSTR-111(2).
           MOVE     W-64(C 3) TO    JSTR-111(3).
           MOVE     W-64(C 4) TO    JSTR-111(4).
           MOVE     W-64(C 5) TO    JSTR-111(5).
           MOVE     W-64(C 6) TO    JSTR-111(6).
           MOVE     W-64(C 7) TO    JSTR-111(7).
           MOVE     W-64(C 8) TO    JSTR-111(8).
           MOVE     W-64(C 9) TO    JSTR-111(9).
           MOVE     W-64(C 10) TO   JSTR-111(10).
           MOVE     W-65(C)    TO    JSTR-112.
           IF  W-21       NOT  =     0
               MOVE     W-3        TO    JSTR-05
               MOVE     W-64(C 1) TO    JSTR-121(1)
               MOVE     W-64(C 2) TO    JSTR-121(2)
               MOVE     W-64(C 3) TO    JSTR-121(3)
               MOVE     W-64(C 4) TO    JSTR-121(4)
               MOVE     W-64(C 5) TO    JSTR-121(5)
               MOVE     W-64(C 6) TO    JSTR-121(6)
               MOVE     W-64(C 7) TO    JSTR-121(7)
               MOVE     W-64(C 8) TO    JSTR-121(8)
               MOVE     W-64(C 9) TO    JSTR-121(9)
               MOVE     W-64(C 10) TO   JSTR-121(10)
               MOVE     W-65(C)    TO    JSTR-122
           END-IF
           MOVE     W-4A       TO    JSTR-06.
           MOVE     W-51       TO    JSTR-07.
           MOVE     W-61(C)    TO    JSTR-08.
           MOVE     W-621(C)   TO    JSTR-09.
           MOVE     W-63(C)    TO    JSTR-10.
           MOVE     W-66(C)    TO    JSTR-13.
           MOVE     W-71       TO    JSTR-14.
           MOVE     W-5A       TO    JSTR-14A.
           MOVE     W-92       TO    JSTR-14B.
           MOVE     W-93       TO    JSTR-14C.
           MOVE     W-8A       TO    JSTR-14D.
           MOVE     W-8        TO    JSTR-15.
           MOVE     W-67(C)    TO    JSTR-20.
           IF  W-21        =  5  OR  6
               MOVE     W-KSU      TO    JSTR-15A
           ELSE
               MOVE     ZERO       TO    JSTR-15A
           END-IF
           MOVE     JS-SIGN    TO    JSTR-16.
           IF  W-ACT        = 2
               MOVE     W-4012     TO    JSTR-4012
           ELSE
               MOVE     0          TO    JSTR-4012
           END-IF
           IF  W-21             =  0
               MOVE  0           TO  JSTR-17
           ELSE
               MOVE  9           TO  JSTR-17
           END-IF
           IF  JSTR-14          =    9
               MOVE  9           TO  JSTR-17
           END-IF
           IF  (W-51   =   1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8)  AND
               (W-21   =   0 OR 5 OR 6)
               MOVE  0           TO  JSTR-158
           ELSE
               MOVE  1           TO  JSTR-158
           END-IF
           MOVE  STN-NO-02 TO  JSTR-4011.
      ***
      *           WRITE    JSTR-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "W"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
      *
           IF  C    NOT = 1
               GO  TO   UPD-025
           END-IF.
       UPD-028.
           MOVE SPACE TO JDT-R.
           ACCEPT JDT-NGP FROM DATE.
           ACCEPT W-TIMED FROM TIME.
           MOVE W-TIME TO JDT-TIME.
           MOVE STN-NO-02 TO JDT-STN.
           MOVE W-1 TO JDT-DNO.
           MOVE W-ACT TO JDT-ACT.
      *           WRITE JDT-R.
      *//////////////
           CALL "DB_Insert" USING
            J-DATE_PNAME1 J-DATE_LNAME JDT-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO  TO   UPD-025
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "ERR-31" ERR-31 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO  TO   UPD-025
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE J-DATE_IDLST J-DATE_PNAME1.
           MOVE "J-DATE       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" J-DATE_PNAME1 "SHARED" BY REFERENCE
            J-DATE_IDLST "0".
           GO  TO   UPD-028.
       UPD-030.
      *    受注マスタ　更新   *
           MOVE     ZERO       TO    C.
           IF  W-ACT            =     1
               GO  TO  UPD-036
           END-IF.
       UPD-035.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-036
           END-IF
           IF  O-JNO1(C)       =     ZERO OR 999999
               GO  TO  UPD-035
           END-IF
           MOVE     O-JNO1(C)   TO    JMSTD-07.
           MOVE     O-JNO2(C)   TO    JMSTD-08.
           MOVE     JMSTD-KEY1 TO    ERR-K.
      *           READ     JMSTD      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE    "G"         TO    ERR-M
               MOVE    "JMSTD"     TO    ERR-F
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  UPD-035
           END-IF
           SUBTRACT  O-SU(C 1)  FROM  JMSTD-151(1).
           SUBTRACT  O-SU(C 2)  FROM  JMSTD-151(2).
           SUBTRACT  O-SU(C 3)  FROM  JMSTD-151(3).
           SUBTRACT  O-SU(C 4)  FROM  JMSTD-151(4).
           SUBTRACT  O-SU(C 5)  FROM  JMSTD-151(5).
           SUBTRACT  O-SU(C 6)  FROM  JMSTD-151(6).
           SUBTRACT  O-SU(C 7)  FROM  JMSTD-151(7).
           SUBTRACT  O-SU(C 8)  FROM  JMSTD-151(8).
           SUBTRACT  O-SU(C 9)  FROM  JMSTD-151(9).
           SUBTRACT  O-SU(C 10) FROM  JMSTD-151(10).
      *
      *           REWRITE  JMSTD-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "JMSTD"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-035.
       UPD-036.
           MOVE     ZERO       TO    C.
           IF  W-ACT            =    3
               GO  TO  UPD-040
           END-IF.
       UPD-037.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-040
           END-IF
           IF  W-611(C)        =     ZERO OR 999999
               GO  TO  UPD-037
           END-IF
           MOVE     W-611(C)   TO    JMSTD-07.
           MOVE     W-612(C)   TO    JMSTD-08.
           MOVE     JMSTD-KEY1 TO    ERR-K.
      *           READ     JMSTD      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE    "G"         TO    ERR-M
               MOVE    "JMSTD"     TO    ERR-F
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  UPD-037
           END-IF
           ADD      W-64(C 1)     TO  JMSTD-151(1).
           ADD      W-64(C 2)     TO  JMSTD-151(2).
           ADD      W-64(C 3)     TO  JMSTD-151(3).
           ADD      W-64(C 4)     TO  JMSTD-151(4).
           ADD      W-64(C 5)     TO  JMSTD-151(5).
           ADD      W-64(C 6)     TO  JMSTD-151(6).
           ADD      W-64(C 7)     TO  JMSTD-151(7).
           ADD      W-64(C 8)     TO  JMSTD-151(8).
           ADD      W-64(C 9)     TO  JMSTD-151(9).
           ADD      W-64(C 10)    TO  JMSTD-151(10).
      *
      *           REWRITE  JMSTD-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "JMSTD"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-037.
       UPD-040.
      *    伝票№検索Ｆ　更新   *
           MOVE     ZERO       TO    C.
       UPD-055.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-060
           END-IF
           IF  W-ACT           =     1
               GO  TO  UPD-056
           END-IF
           MOVE     O-SCD      TO    DNKN-01.
           MOVE     O-HCD(C)   TO    DNKN-02.
           MOVE     3          TO    DNKN-03.
           MOVE     W-1        TO    DNKN-041.
           MOVE     C          TO    DNKN-042.
           MOVE     DNKN-KEY   TO    ERR-K.
      *           READ     JT-DNKN    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-055
           END-IF
      ***
      *           DELETE   JT-DNKN    INVALID
      *///////////////
           CALL "DB_Delete" USING JT-DNKN_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "D"         TO    ERR-M
               MOVE    "JT-DNKN"   TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           IF  W-ACT            =     3
               GO  TO  UPD-055
           END-IF.
       UPD-056.
           IF  W-621(C)    =     ZERO
               GO  TO  UPD-055
           END-IF
           MOVE     SPACE      TO    DNKN-R.
           INITIALIZE                DNKN-R.
           MOVE     W-51       TO    DNKN-01.
           MOVE     W-621(C)   TO    DNKN-02.
           MOVE     3          TO    DNKN-03.
           MOVE     W-1        TO    DNKN-041.
           MOVE     C          TO    DNKN-042.
           MOVE     DNKN-KEY   TO    ERR-K.
      ***
      *           WRITE    DNKN-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               MOVE    "W"         TO    ERR-M
               MOVE    "JT-DNKN"   TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-055.
       UPD-060.
      *    倉別在庫マスタ　更新   *
           MOVE     ZERO       TO    C.
           IF  W-ACT            =     1
               GO  TO  UPD-066
           END-IF.
       UPD-062.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-066
           END-IF
           IF  O-HCD(C)        =     ZERO
               GO  TO  UPD-062
           END-IF
           IF  O-HCD(C)        >     999899
               GO  TO  UPD-062
           END-IF
           MOVE     O-SCD       TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-063
           END-IF
      *
           PERFORM  OZS-RTN     THRU  OZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-064.
       UPD-063.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     O-SCD       TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  OZS-RTN     THRU  OZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-063
           END-IF.
       UPD-064.
           MOVE     9           TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-065
           END-IF
      *
           PERFORM  OZS-RTN     THRU  OZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-062.
       UPD-065.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     9           TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  OZS-RTN     THRU  OZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-065
           END-IF
           GO  TO  UPD-062.
       UPD-066.
           MOVE     ZERO       TO    C.
           IF  W-ACT            =     3
               GO  TO  UPD-EX
           END-IF.
       UPD-067.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-EX
           END-IF
           IF  W-621(C)        =     ZERO
               GO  TO  UPD-067
           END-IF
           IF  W-621(C)        >     999899
               GO  TO  UPD-067
           END-IF
           MOVE     W-51       TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-068
           END-IF
      *
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-069.
       UPD-068.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     W-51       TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-068
           END-IF.
       UPD-069.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-070
           END-IF
      *
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-067.
       UPD-070.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-070
           END-IF
           GO  TO  UPD-067.
       UPD-EX.
           EXIT.
      **********************************************
      *    在庫マスター　指図数セット  (ADD)       *
      **********************************************
       NZS-RTN.
           IF  W-21     NOT =  5  AND 6
               ADD      W-64(C 1)  TO    NJZAI-0911(1)
               ADD      W-64(C 2)  TO    NJZAI-0911(2)
               ADD      W-64(C 3)  TO    NJZAI-0911(3)
               ADD      W-64(C 4)  TO    NJZAI-0911(4)
               ADD      W-64(C 5)  TO    NJZAI-0911(5)
               ADD      W-64(C 6)  TO    NJZAI-0911(6)
               ADD      W-64(C 7)  TO    NJZAI-0911(7)
               ADD      W-64(C 8)  TO    NJZAI-0911(8)
               ADD      W-64(C 9)  TO    NJZAI-0911(9)
               ADD      W-64(C 10) TO    NJZAI-0911(10)
           ELSE
               IF  W-21         =  5
                   SUBTRACT W-64(C 1)  FROM  NJZAI-0911(1)
                   SUBTRACT W-64(C 2)  FROM  NJZAI-0911(2)
                   SUBTRACT W-64(C 3)  FROM  NJZAI-0911(3)
                   SUBTRACT W-64(C 4)  FROM  NJZAI-0911(4)
                   SUBTRACT W-64(C 5)  FROM  NJZAI-0911(5)
                   SUBTRACT W-64(C 6)  FROM  NJZAI-0911(6)
                   SUBTRACT W-64(C 7)  FROM  NJZAI-0911(7)
                   SUBTRACT W-64(C 8)  FROM  NJZAI-0911(8)
                   SUBTRACT W-64(C 9)  FROM  NJZAI-0911(9)
                   SUBTRACT W-64(C 10) FROM  NJZAI-0911(10)
               END-IF
           END-IF.
       NZS-EX.
           EXIT.
      **********************************************
      *    在庫マスター　指図数セット  (SUBTRACT)  *
      **********************************************
       OZS-RTN.
           IF  W-21     NOT =  5  AND 6
               SUBTRACT  O-SU(C 1)  FROM  NJZAI-0911(1)
               SUBTRACT  O-SU(C 2)  FROM  NJZAI-0911(2)
               SUBTRACT  O-SU(C 3)  FROM  NJZAI-0911(3)
               SUBTRACT  O-SU(C 4)  FROM  NJZAI-0911(4)
               SUBTRACT  O-SU(C 5)  FROM  NJZAI-0911(5)
               SUBTRACT  O-SU(C 6)  FROM  NJZAI-0911(6)
               SUBTRACT  O-SU(C 7)  FROM  NJZAI-0911(7)
               SUBTRACT  O-SU(C 8)  FROM  NJZAI-0911(8)
               SUBTRACT  O-SU(C 9)  FROM  NJZAI-0911(9)
               SUBTRACT  O-SU(C 10) FROM  NJZAI-0911(10)
           ELSE
               IF  W-21         =  5
                   ADD       O-SU(C 1)  TO    NJZAI-0911(1)
                   ADD       O-SU(C 2)  TO    NJZAI-0911(2)
                   ADD       O-SU(C 3)  TO    NJZAI-0911(3)
                   ADD       O-SU(C 4)  TO    NJZAI-0911(4)
                   ADD       O-SU(C 5)  TO    NJZAI-0911(5)
                   ADD       O-SU(C 6)  TO    NJZAI-0911(6)
                   ADD       O-SU(C 7)  TO    NJZAI-0911(7)
                   ADD       O-SU(C 8)  TO    NJZAI-0911(8)
                   ADD       O-SU(C 9)  TO    NJZAI-0911(9)
                   ADD       O-SU(C 10) TO    NJZAI-0911(10)
               END-IF
           END-IF.
       OZS-EX.
           EXIT.
      *******************************************
      *    コントロールＦ　送り状№　　更新     *
      *******************************************
       UPD1-RTN.
           IF  JS-SIGN    =     0
               MOVE    W-92    TO    JCON1-03
           END-IF
           IF  JS-SIGN    =     1
               MOVE    W-92    TO    JCON1-04
           END-IF
      *           REWRITE  JCON1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD1-EX.
       UPD1-010.
           INITIALIZE                JCON1-R.
           MOVE     1          TO    JCON1-01.
           MOVE     4          TO    JCON1-02.
           IF  JS-SIGN    =     0
               MOVE    W-92    TO    JCON1-03
               MOVE    100001  TO    JCON1-04
           END-IF
           IF  JS-SIGN    =     1
               MOVE    W-92    TO    JCON1-04
               MOVE    1       TO    JCON1-03
           END-IF
      *           WRITE    JCON1-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"     TO    ERR-F
               MOVE     "W"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       UPD1-EX.
           EXIT.
      *----ＣＲ１－ＲＴＮ              *
      *        ～クリア　ルーチン～----*
       CR1-RTN.
           MOVE     LIN1       TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR1-010.
           PERFORM  CR2-RTN    THRU  CR2-EX  VARYING  C  FROM  A
                    BY  1  UNTIL  C  >   6.
       CR1-EX.
           EXIT.
      *----ＣＲ２－ＲＴＮ              *
      *      ～ワーク・画面クリア～----*
       CR2-RTN.
           INITIALIZE                W-6A(C).
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           ADD      2          TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR2-EX.
           EXIT.
      *----ＣＨＫ２－ＲＴＮ                *
      *        ～同一受注№・ｻｲｽﾞ ﾁｪｯｸ～----*
       CHK2-RTN.
           PERFORM  COM-RTN    THRU  COM-EX
                    VARYING    I     FROM  1  BY  1
                    UNTIL    I  >  10.
           MOVE     ZERO       TO    CHK-SW  C  T-SW.
           IF  W-ACT            =  2  AND  W-61(A)          =  O-JNO(A)
               CONTINUE
           ELSE
               GO  TO  CHK2-010
           END-IF
           IF  W-21             =  3
               GO  TO  CHK2-010
           END-IF
           MOVE  O-SUA(A)      TO  W-COMSU.
           PERFORM  COMP-RTN   THRU  COMP-EX
                    VARYING    I     FROM  1  BY  1
                      UNTIL    I  >  10.
       CHK2-010.
           ADD      1          TO    C.
           IF  A            =  C
               GO  TO  CHK2-020
           END-IF
           IF  W-61(C)          =  ZERO
               GO  TO  CHK2-010
           END-IF
           MOVE  1             TO  T-SW.
           IF  W-61(A)      NOT =  W-61(C)
               GO  TO  CHK2-010
           END-IF
           MOVE  W-64A(C)      TO  W-COMSU.
           IF  W-21             =  0
               PERFORM  COMM-RTN   THRU  COMM-EX
                        VARYING    I     FROM  1  BY  1
                          UNTIL    I  >  10
           ELSE
               PERFORM  COMP-RTN   THRU  COMP-EX
                        VARYING    I     FROM  1  BY  1
                          UNTIL    I  >  10
           END-IF
           GO  TO  CHK2-010.
       CHK2-020.
           ADD      1          TO    C.
           IF  C            >  6
               GO  TO  CHK2-EX
           END-IF
           IF  W-61(C)          =  ZERO
               GO  TO  CHK2-020
           END-IF
           MOVE  1             TO  T-SW.
           IF  W-61(A)      NOT =  W-61(C)
               GO  TO  CHK2-020
           END-IF
           MOVE  W-64A(C)      TO  W-COMSU.
           IF  W-21             =  0
               PERFORM  COMP-RTN   THRU  COMP-EX
                        VARYING    I     FROM  1  BY  1
                          UNTIL    I  >  10
           ELSE
               PERFORM  COMM-RTN   THRU  COMM-EX
                        VARYING    I     FROM  1  BY  1
                          UNTIL    I  >  10
           END-IF
           GO  TO  CHK2-020.
       CHK2-EX.
           EXIT.
      *--- 数量計算処理 ---*
       COM-RTN.
           PERFORM   COM2-RTN   THRU  COM2-EX.
           IF  W-ACT            =  2
               GO  TO  COM-EX
           END-IF
           MOVE  S-64(I)        TO  W-64(A I).
           ADD   W-64(A I)      TO  W-65(A).
           IF  W-64(A I)  NOT >  ZERO
               ADD   1        TO  Z-SW
           END-IF.
       COM-EX.
           EXIT.
      *--- 数量計算処理(2) ---*   (受注残数　OR　出荷累計)
       COM2-RTN.
           IF  W-21       =     0
               COMPUTE   S-64(I)    =  JMSTD-1111(I) - JMSTD-1211(I)
                                     - JMSTD-141(I)  - JMSTD-151(I)
           ELSE
               COMPUTE   S-64(I)    =  JMSTD-141(I)  + JMSTD-1211(I)
           END-IF
           ADD   S-64(I)       TO  S-65.
       COM2-EX.
           EXIT.
      *--- 数量計算処理(+) ---*
       COMP-RTN.
           ADD   W-CSU(I)      TO  S-64(I)   S-65.
       COMP-EX.
           EXIT.
      *--- 数量計算処理(-) ---*
       COMM-RTN.
           SUBTRACT  W-CSU(I)      FROM  S-64(I)   S-65.
       COMM-EX.
           EXIT.
       AZC-RTN.
      *           READ  JMSTD  NEXT RECORD  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  AZC-050
           END-IF
           IF (W-41  NOT =  JMSTD-04)  OR  (W-621(A)  NOT =  JMSTD-05)
               GO  TO  AZC-050
           END-IF
           IF  JMSTD-01  NOT =   5
               GO  TO  AZC-RTN
           END-IF
           MOVE  ZERO          TO  S-AZS  I.
       AZC-010.
           ADD   1             TO  I.
           IF  I                <  11
               COMPUTE   S-AZS      =  S-AZS   +  JMSTD-1111(I)
                      -  JMSTD-1211(I)  - JMSTD-141(I)  - JMSTD-151(I)
               GO  TO  AZC-010
           END-IF
           IF  S-AZS            <=   ZERO
               GO  TO  AZC-RTN
           END-IF
           MOVE  1             TO  Z-SW.
           GO  TO  AZC-EX.
       AZC-050.
           MOVE  W-41          TO  JAZ-TCD.
           MOVE  W-621(A)      TO  JAZ-HCD.
      *           READ  JAZF   WITH  UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JAZF_PNAME1 BY REFERENCE JAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  AZC-EX
           END-IF
           MOVE  2             TO  Z-SW.
       AZC-EX.
           EXIT.
      *****  倉別在庫マスタ　ＷＲＩＴＥ　
       NJW-RTN.
           MOVE     0          TO    WRI-SW.
      *           WRITE  NJZAI-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJW-010
           END-IF
           GO  TO  NJW-EX.
       NJW-010.
           IF  ERR-STAT      =  "24"
               GO  TO  NJW-020
           END-IF
           IF  ERR-STAT  NOT =  "00"
               MOVE    "W"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           MOVE     2          TO    WRI-SW.
           GO  TO  NJW-EX.
       NJW-020.
           MOVE     1          TO    WRI-SW.
           MOVE    "W"         TO    ERR-M.
           MOVE    "NJZAI"     TO    ERR-F.
           MOVE     NJZAI-KEY  TO    ERR-K.
           MOVE     ERR-STAT   TO    ERR-FLG.
           CALL "SD_Output" USING
            "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING
            " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ,ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       NJW-EX.
           EXIT.
      **   ﾘﾊﾞｰｽ ﾋｮｳｼﾞ **
       REV-DSP-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
           IF  W-63(A)  =  1
               IF  B        =  1
                   CALL "SD_Output" USING
                    "DSP-RR101" DSP-RR101 "p" RETURNING RESU
               ELSE
                   IF  B        =  2
                       CALL "SD_Output" USING
                        "DSP-RR102" DSP-RR102 "p" RETURNING RESU
                   ELSE
                       IF  B        =  3
                           CALL "SD_Output" USING
                            "DSP-RR103" DSP-RR103 "p" RETURNING RESU
                       ELSE
                           IF  B        =  4
                               CALL "SD_Output" USING
                                "DSP-RR104" DSP-RR104 "p" RETURNING RESU
                           ELSE
                               IF  B        =  5
                               CALL "SD_Output" USING
                                "DSP-RR105" DSP-RR105 "p" RETURNING RESU
                               ELSE
                                   IF  B        =  6
                               CALL "SD_Output" USING
                                "DSP-RR106" DSP-RR106 "p" RETURNING RESU
                                   ELSE
                                       IF  B        =  7
                               CALL "SD_Output" USING
                                "DSP-RR107" DSP-RR107 "p" RETURNING RESU
                                       ELSE
                                           IF  B        =  8
                               CALL "SD_Output" USING
                                "DSP-RR108" DSP-RR108 "p" RETURNING RESU
                                           ELSE
                                               IF  B        =  9
                               CALL "SD_Output" USING
                                "DSP-RR109" DSP-RR109 "p" RETURNING RESU
                                               ELSE
                                                   IF  B        =  10
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
           IF  W-63(A)  =  2
               IF  B        =  1
                   CALL "SD_Output" USING
                    "DSP-RR201" DSP-RR201 "p" RETURNING RESU
               ELSE
                   IF  B        =  2
                       CALL "SD_Output" USING
                        "DSP-RR202" DSP-RR202 "p" RETURNING RESU
                   ELSE
                       IF  B        =  3
                           CALL "SD_Output" USING
                            "DSP-RR203" DSP-RR203 "p" RETURNING RESU
                       ELSE
                           IF  B        =  4
                               CALL "SD_Output" USING
                                "DSP-RR204" DSP-RR204 "p" RETURNING RESU
                           ELSE
                               IF  B        =  5
                               CALL "SD_Output" USING
                                "DSP-RR205" DSP-RR205 "p" RETURNING RESU
                               ELSE
                                   IF  B        =  6
                               CALL "SD_Output" USING
                                "DSP-RR206" DSP-RR206 "p" RETURNING RESU
                                   ELSE
                                       IF  B        =  7
                               CALL "SD_Output" USING
                                "DSP-RR207" DSP-RR207 "p" RETURNING RESU
                                       ELSE
                                           IF  B        =  8
                               CALL "SD_Output" USING
                                "DSP-RR208" DSP-RR208 "p" RETURNING RESU
                                           ELSE
                                               IF  B        =  9
                               CALL "SD_Output" USING
                                "DSP-RR209" DSP-RR209 "p" RETURNING RESU
                                               ELSE
                                                   IF  B        =  10
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
           IF  W-63(A)  =  3
               IF  B        =  1
                   CALL "SD_Output" USING
                    "DSP-RR301" DSP-RR301 "p" RETURNING RESU
               ELSE
                   IF  B        =  2
                       CALL "SD_Output" USING
                        "DSP-RR302" DSP-RR302 "p" RETURNING RESU
                   ELSE
                       IF  B        =  3
                           CALL "SD_Output" USING
                            "DSP-RR303" DSP-RR303 "p" RETURNING RESU
                       ELSE
                           IF  B        =  4
                               CALL "SD_Output" USING
                                "DSP-RR304" DSP-RR304 "p" RETURNING RESU
                           ELSE
                               IF  B        =  5
                               CALL "SD_Output" USING
                                "DSP-RR305" DSP-RR305 "p" RETURNING RESU
                               ELSE
                                   IF  B        =  6
                               CALL "SD_Output" USING
                                "DSP-RR306" DSP-RR306 "p" RETURNING RESU
                                   ELSE
                                       IF  B        =  7
                               CALL "SD_Output" USING
                                "DSP-RR307" DSP-RR307 "p" RETURNING RESU
                                       ELSE
                                           IF  B        =  8
                               CALL "SD_Output" USING
                                "DSP-RR308" DSP-RR308 "p" RETURNING RESU
                                           ELSE
                                               IF  B        =  9
                               CALL "SD_Output" USING
                                "DSP-RR309" DSP-RR309 "p" RETURNING RESU
                                               ELSE
                                                   IF  B        =  10
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
           IF  W-63(A)  =  4
               IF  B        =  1
                   CALL "SD_Output" USING
                    "DSP-RR401" DSP-RR401 "p" RETURNING RESU
               ELSE
                   IF  B        =  2
                       CALL "SD_Output" USING
                        "DSP-RR402" DSP-RR402 "p" RETURNING RESU
                   ELSE
                       IF  B        =  3
                           CALL "SD_Output" USING
                            "DSP-RR403" DSP-RR403 "p" RETURNING RESU
                       ELSE
                           IF  B        =  4
                               CALL "SD_Output" USING
                                "DSP-RR404" DSP-RR404 "p" RETURNING RESU
                           ELSE
                               IF  B        =  5
                               CALL "SD_Output" USING
                                "DSP-RR405" DSP-RR405 "p" RETURNING RESU
                               ELSE
                                   IF  B        =  6
                               CALL "SD_Output" USING
                                "DSP-RR406" DSP-RR406 "p" RETURNING RESU
                                   ELSE
                                       IF  B        =  7
                               CALL "SD_Output" USING
                                "DSP-RR407" DSP-RR407 "p" RETURNING RESU
                                       ELSE
                                           IF  B        =  8
                               CALL "SD_Output" USING
                                "DSP-RR408" DSP-RR408 "p" RETURNING RESU
                                           ELSE
                                               IF  B        =  9
                               CALL "SD_Output" USING
                                "DSP-RR409" DSP-RR409 "p" RETURNING RESU
                                               ELSE
                                                   IF  B        =  10
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
       REV-DSP-EXT.
           EXIT.
       REV-CLE-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
       REV-CLE-EXT.
           EXIT.
      ***
           COPY    LPMSG.
