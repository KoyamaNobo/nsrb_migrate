000010*
000020***  éÛíçÉ}ÉXÉ^
000030*
000040 FD  JMSTD                                                        º≠¡≠≥
000050     BLOCK    3     RECORDS                                       œΩ¿∞
000060     LABEL    RECORD   STANDARD
000070     VALUE    OF  IDENTIFICATION  "JMST1"
000080     ALTERNATE    IDENTIFICATION  "JMST2"
000090     ALTERNATE    IDENTIFICATION  "JMST3".
000100*
000110 01  JMSTD-R.
000120     02   JMSTD-01                 PIC 9(1).                      ±Ωﬁ∂ÿ
000130     02   JMSTD-02.                                               ºﬁ≠¡≠≥Àﬁ
000140*****     03  JMSTD-021            PIC 9(2).                      D.980515
000150          03  JMSTD-021            PIC 9(4).                      I.980515
000160          03  JMSTD-021L  REDEFINES  JMSTD-021.                   I.980525
000170              04  JMSTD-0211       PIC 9(2).                      I.980525
000180              04  JMSTD-0212       PIC 9(2).                      I.980525
000190          03  JMSTD-022            PIC 9(2).                      ¬∑
000200          03  JMSTD-023            PIC 9(2).                      À
000210     02   JMSTD-02L   REDEFINES  JMSTD-02.                        I.980525
000220          03  F                    PIC 9(2).                      I.980525
000230          03  JMSTD-02S            PIC 9(6).                      I.980525
000240     02   JMSTD-KEY3.
000250          03  JMSTD-03             PIC 9(6).                      À›∫∞ƒﬁ
000260          03  JMSTD-KEY2.
000270              04  JMSTD-04         PIC 9(4).                      ƒ∏≤∫∞ƒﬁ
000280              04  JMSTD-05         PIC 9(6).                      À›∫∞ƒﬁ
000290              04  JMSTD-06.                                       …≥∑
000300*****             05  JMSTD-061    PIC 9(2).                      D.980515
000310                  05  JMSTD-061    PIC 9(4).                      I.980515
000320                  05  JMSTD-062    PIC 9(2).                      ¬∑
000330                  05  JMSTD-063    PIC 9(2).                      À
000340              04   JMSTD-06L   REDEFINES  JMSTD-06.               I.980525
000350                  05  F            PIC 9(2).                      I.980525
000360                  05  JMSTD-06S    PIC 9(6).                      I.980525
000370              04  JMSTD-KEY1.
000380                  05  JMSTD-07     PIC 9(6).                      ºﬁ≠¡≠≥NO
000390                  05  JMSTD-08     PIC 9(1).                      ∑ﬁÆ≥ NO
000400     02   JMSTD-09                 PIC 9(1).                      ª≤Ωﬁ∏Ãﬁ›
000410     02   JMSTD-10                 PIC 9(3).                      ¡Æ∏ø≥ NO
000420     02   JMSTD-11.                                               ºﬁ≠¡≠≥Ω≥
000430          03  JMSTD-111            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
000440              04  JMSTD-1111       PIC S9(6)   COMP-3.
000450     02   JMSTD-12.                                               º≠Ø∫Ω≥
000460          03  JMSTD-121            OCCURS  10.                    ª≤ΩﬁÕﬁ¬
000470              04  JMSTD-1211       PIC S9(6)   COMP-3.
000480     02   JMSTD-14.                                               æﬁ›πﬁ¬œ¬
000490          03  JMSTD-141  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
000500     02   JMSTD-15.                                               º≠Ø∂ªºΩﬁ
000510          03  JMSTD-151  OCCURS 10 PIC S9(06)  COMP-3.            ª≤ΩﬁÕﬁ¬
000520     02   JMSTD-16                 PIC S9(03).                    æØƒΩ≥
000530     02   JMSTD-23                 PIC 9(03).                     I.080602
000540     02   F                        PIC X(09).                     I.080602
000550*****02   F                        PIC X(12).                     D.080602
000560*****02   JMSTD-17                 PIC 9(04).                     D.941223
000570*****02   F                        PIC X(04).                     D.981027
000580*****02   JMSTD-18                 PIC 9(01).                     D.981027
000590*****02   F                        PIC X(07).                     D.981027
000600*****02   JMSTD-19.                                               D.980930
000610*****    03   JMSTD-191            PIC 9(06).                     D.980930
000620*****    03   JMSTD-192            PIC 9(01).                     D.980930
000630     02   JMSTD-20                 PIC 9(03).                     Õ›∫≥NO.
000640     02   JMSTD-13                 PIC N(32).                     √∑÷≥
000650     02   JMSTD-21                 PIC 9(01).                     äÆóπãÊï™
000660     02   JMSTD-17                 PIC 9(05).                     I.941223
000670     02   JMSTD-22                 PIC X(10).                     I.980604
000680     02   JMSTD-51                 PIC 9(03).                     I.040601
000690*****02   FILLER                   PIC X(58).                     D.941005
000700*****02   FILLER                   PIC X(55).                     D.941223
000710*****02   FILLER                   PIC X(50).                     D.980515
000720*****02   FILLER                   PIC X(46).                     D.980604
000730*****02   FILLER                   PIC X(36).                     D.981125
000740*****02   FILLER                   PIC X(27).                     D.040601
000750     02   FILLER                   PIC X(24).                     I.040601
000760     02   JMSTD-89.                                               I.981125
000770          03  JMSTD-891            PIC 9(08).                     I.981125
000780          03  JMSTD-892            PIC 9(01).                     I.981125
000790     02   JMSTD-90                 PIC 9(01).                     ã≥àÁàÍî 
000800     02   JMSTD-91                 PIC 9(02).                     íSìñãÊï™
