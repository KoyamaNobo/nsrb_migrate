       01  JWTOK.                                                       ƒ∏≤ª∑Õﬁ¬º≠Ø∂‹∞∏
           02  JWTOK_PNAME1           PIC  X(009) VALUE SPACE.
           02  F                      PIC  X(001).
           02  JWTOK_LNAME            PIC  X(005) VALUE "JWTOK".
           02  F                      PIC  X(001).
           02  JWTOK_KEY1             PIC  X(100) VALUE SPACE.
           02  JWTOK_SORT             PIC  X(100) VALUE SPACE.
           02  JWTOK_IDLST            PIC  X(100) VALUE SPACE.
           02  JWTOK_RES              USAGE  POINTER.
      *
       01  JWTOK-R.
           02   JWTOK-01.                                               ¡Æ∏ø≥ CD
                03   JWTOK-011        PIC 9(4).                         ƒ∏≤∫∞ƒﬁ
                03   JWTOK-012        PIC 9(3).                         ¡Æ∏ø≥ NO
           02   JWTOK-02              PIC 9(1).                         √ﬁ›∏
           02   JWTOK-03.                                               ªºΩﬁ
                03  JWTOK-031         PIC 9(6).                         ªºΩﬁ NO
                03  JWTOK-032         PIC 9(1).                         ∑ﬁÆ≥ NO
           02   JWTOK-04              PIC 9(1).                         ±Ωﬁ∂ÿ
           02   JWTOK-05.                                               ºﬁ≠¡≠≥
                03  JWTOK-051         PIC 9(6).                         ºﬁ≠¡≠≥NO
                03  JWTOK-052         PIC 9(1).                         ∑ﬁÆ≥
           02   JWTOK-06              PIC 9(6).                         À›∫∞ƒﬁ
           02   JWTOK-07              PIC 9(1).                         ª≤Ωﬁ∏Ãﬁ›
           02   JWTOK-08.                                               º≠Ø∂∫Ω≥
                03  JWTOK-081   OCCURS  10.                             ª≤ΩﬁÕﬁ¬
                    04  JWTOK-0811     PIC S9(4).
                03  JWTOK-082         PIC S9(6).                        π≤
           02   JWTOK-09.                                               º≠Ø∂Àﬁ
                03  JWTOK-091         PIC 9(2).                         »›
                03  JWTOK-092         PIC 9(2).                         ¬∑
                03  JWTOK-093         PIC 9(2).                         À
           02   JWTOK-10              PIC N(9).                          ≤¿¬
           02   JWTOK-11              PIC N(23).                        √∑÷≥
           02   JWTOK-12              PIC 9(01).                        ëq∫∞ƒﬁ
           02   JWTOK-13              PIC 9(01).                        â^ëó∫∞ƒﬁ
           02   JWTOK-14              PIC 9(01).                        àÍî ã≥àÁ
           02   JWTOK-15              PIC 9(03).                        æØƒêî
           02   JWTOK-16              PIC S9(03).                       å¬êî
           02   JWTOK-17              PIC 9(06).                        ëóÇËèÛáÇ
           02   JWTOK-20              PIC X(10).
           02   FILLER                PIC X(84).
           02   JWTOK-JS              PIC 9(01).
       77  F                          PIC X(01).
