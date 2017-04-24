      *
      ***  éÛíçÉ}ÉXÉ^
      *
       01  JMSTD.                                                       º≠¡≠≥
           02  JMSTD_PNAME1              PIC  X(005) VALUE "JMST1".
           02  F                         PIC  X(001).
           02  JMSTD_PNAME2              PIC  X(005) VALUE "JMST2".
           02  F                         PIC  X(001).
           02  JMSTD_PNAME3              PIC  X(005) VALUE "JMST3".
           02  F                         PIC  X(001).
           02  JMSTD_LNAME               PIC  X(005) VALUE "JMSTD".
           02  F                         PIC  X(001).
           02  JMSTD_KEY1                PIC  X(100) VALUE SPACE.
           02  JMSTD_KEY2                PIC  X(100) VALUE SPACE.
           02  JMSTD_KEY3                PIC  X(100) VALUE SPACE.
           02  JMSTD_SORT                PIC  X(100) VALUE SPACE.
           02  JMSTD_IDLST               PIC  X(100) VALUE SPACE.
           02  JMSTD_RES                 USAGE  POINTER.
      *
       01  JMSTD-R.
           02   JMSTD-01                 PIC 9(1).                      ±Ωﬁ∂ÿ
           02   JMSTD-02.                                               ºﬁ≠¡≠≥Àﬁ
                03  JMSTD-021            PIC 9(4).
                03  JMSTD-021L  REDEFINES  JMSTD-021.
                    04  JMSTD-0211       PIC 9(2).
                    04  JMSTD-0212       PIC 9(2).
                03  JMSTD-022            PIC 9(2).                      ¬∑
                03  JMSTD-023            PIC 9(2).                      À
           02   JMSTD-02L   REDEFINES  JMSTD-02.
                03  F                    PIC 9(2).
                03  JMSTD-02S            PIC 9(6).
           02   JMSTD-KEY3.
                03  JMSTD-03             PIC 9(6).                      À›∫∞ƒﬁ
                03  JMSTD-KEY2.
                    04  JMSTD-04         PIC 9(4).                      ƒ∏≤∫∞ƒﬁ
                    04  JMSTD-05         PIC 9(6).                      À›∫∞ƒﬁ
                    04  JMSTD-06.                                       …≥∑
                        05  JMSTD-061    PIC 9(4).
                        05  JMSTD-062    PIC 9(2).                      ¬∑
                        05  JMSTD-063    PIC 9(2).                      À
                    04   JMSTD-06L   REDEFINES  JMSTD-06.
                        05  F            PIC 9(2).
                        05  JMSTD-06S    PIC 9(6).
                    04  JMSTD-KEY1.
                        05  JMSTD-07     PIC 9(6).                      ºﬁ≠¡≠≥NO
                        05  JMSTD-08     PIC 9(1).                      ∑ﬁÆ≥ NO
           02   JMSTD-09                 PIC 9(1).                      ª≤Ωﬁ∏Ãﬁ›
           02   JMSTD-10                 PIC 9(3).                      ¡Æ∏ø≥ NO
           02   JMSTD-11.                                               ºﬁ≠¡≠≥Ω≥
                03  JMSTD-111            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
                    04  JMSTD-1111       PIC S9(6)   COMP-3.
           02   JMSTD-12.                                               º≠Ø∫Ω≥
                03  JMSTD-121            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
                    04  JMSTD-1211       PIC S9(6)   COMP-3.
           02   JMSTD-14.                                               æﬁ›πﬁ¬œ¬
                03  JMSTD-141  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
           02   JMSTD-15.                                               º≠Ø∂ªºΩﬁ
                03  JMSTD-151  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
           02   JMSTD-16                 PIC S9(03).                    æØƒΩ≥
           02   JMSTD-23                 PIC 9(04).
           02   F                        PIC X(08).
           02   JMSTD-20                 PIC 9(03).                     Õ›∫≥NO.
           02   JMSTD-13                 PIC N(32).                     √∑÷≥
           02   JMSTD-21                 PIC 9(01).                     äÆóπãÊï™
           02   JMSTD-17                 PIC 9(05).
           02   JMSTD-22                 PIC X(10).
           02   JMSTD-51                 PIC 9(03).
           02   FILLER                   PIC X(24).
           02   JMSTD-89.
                03  JMSTD-891            PIC 9(08).
                03  JMSTD-892            PIC 9(01).
           02   JMSTD-90                 PIC 9(01).                     ã≥àÁàÍî 
           02   JMSTD-91                 PIC 9(02).                     íSìñãÊï™
       77  F                             PIC X(01).
