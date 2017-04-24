      *
      ***  éÛíçÉ}ÉXÉ^  ÅiÇjÇdÇxÇRÅj
      *
       01  JMST3.                                                       º≠¡≠≥
           02  JMST3_PNAME1               PIC  X(005) VALUE "JMST1".
           02  F                          PIC  X(001).
           02  JMST3_LNAME                PIC  X(005) VALUE "JMST3".
           02  F                          PIC  X(001).
           02  JMST3_KEY1                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY2                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY3                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY4                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY5                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY6                 PIC  X(100) VALUE SPACE.
           02  JMST3_KEY7                 PIC  X(100) VALUE SPACE.
           02  JMST3_SORT                 PIC  X(100) VALUE SPACE.
           02  JMST3_IDLST                PIC  X(100) VALUE SPACE.
           02  JMST3_RES                  USAGE  POINTER.
      *
       01  JMST3-R.
           02   JMST3-01                  PIC 9(1).                     ±Ωﬁ∂ÿ
           02   JMST3-02.                                               ºﬁ≠¡≠≥Àﬁ
                03   JMST3-021            PIC 9(4).
                03   JMST3-021L   REDEFINES  JMST3-021.
                     04   JMST3-0211      PIC 9(2).
                     04   JMST3-0212      PIC 9(2).
                03   JMST3-022            PIC 9(2).                     ¬∑
                03   JMST3-023            PIC 9(2).                     À
           02   JMST3-02L    REDEFINES  JMST3-02.
                03   F                    PIC 9(2).
                03   JMST3-02S            PIC 9(6).
           02   JMST3-KEY.
                03   JMST3-03             PIC 9(6).                     À›∫∞ƒﬁ
                03   JMST3-04             PIC 9(4).                     ƒ∏≤∫∞ƒﬁ
                03   JMST3-05             PIC 9(6).                     À›∫∞ƒﬁ
                03   JMST3-06.                                          …≥∑
                     04   JMST3-061       PIC 9(4).
                     04   JMST3-062       PIC 9(2).                     ¬∑
                     04   JMST3-063       PIC 9(2).                     À
                03   JMST3-06L    REDEFINES  JMST3-06.
                     04   F               PIC 9(2).
                     04   JMST3-06S       PIC 9(6).
                03   JMST3-07             PIC 9(6).                     ºﬁ≠¡≠≥NO
                03   JMST3-08             PIC 9(1).                     ∑ﬁÆ≥ NO
           02   JMST3-09                  PIC 9(1).                     ª≤Ωﬁ∏Ãﬁ›
           02   JMST3-10                  PIC 9(3).                     ¡Æ∏ø≥ NO
           02   JMST3-11.                                               ºﬁ≠¡≠≥Ω≥
                03  JMST3-111            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
                    04  JMST3-1111       PIC S9(6)   COMP-3.
           02   JMST3-12.                                               º≠Ø∫Ω≥
                03  JMST3-121            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
                    04  JMST3-1211       PIC S9(6)   COMP-3.
           02   JMST3-14.                                               æﬁ›πﬁ¬œ¬
                03  JMST3-141  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
           02   JMST3-15.                                               º≠Ø∂ªºΩﬁ
                03  JMST3-151  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
           02   JMST3-16                 PIC S9(03).                    æØƒΩ≥
           02   JMST3-23                 PIC 9(04).
           02   F                        PIC X(08).
           02   JMST3-20                 PIC 9(03).                     Õ›∫≥NO.
           02   JMST3-13                 PIC N(32).                     √∑÷≥
           02   JMST3-21                 PIC 9(01).                     äÆóπãÊï™
           02   JMST3-17                 PIC 9(05).
           02   JMST3-22                 PIC X(10).
           02   JMST3-51                 PIC 9(03).
           02   FILLER                   PIC X(24).
           02   JMST3-89.
                03  JMST3-891            PIC 9(08).
                03  JMST3-892            PIC 9(01).
           02   JMST3-90                 PIC 9(01).                     ã≥àÁàÍî 
           02   JMST3-91                 PIC 9(02).                     íSìñãÊï™
       77  F                             PIC X(01).
