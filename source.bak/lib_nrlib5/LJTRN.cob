000010 FD  JTRN                                                         éÛíçƒ◊›
000020*****BLOCK    4     RECORDS                                       D.980515
000030*****BLOCK    3     RECORDS                                       D.980604
000040     BLOCK    1     RECORDS                                       I.980604
000050     LABEL    RECORD   STANDARD
000060     VALUE    OF  IDENTIFICATION  "JTRN".
000070*
000080 01  JTRN-R.
000090     02   JTRN-01               PIC 9(06).                        éÛíçáÇ
000100     02   JTRN-02               PIC 9(01).                        çs
000110     02   JTRN-03               PIC 9(01).                        óaÇËãÊï™
000120     02   JTRN-04.                                                îNåéì˙
000130*****     03  JTRN-041          PIC 9(02).                        D.980515
000140          03  JTRN-041          PIC 9(04).                        I.980515
000150          03  JTRN-042          PIC 9(02).                        åé
000160          03  JTRN-043          PIC 9(02).                        ì˙
000170     02   JTRN-05.
000180          03  JTRN-051          PIC 9(04).                        ìæà”êÊCD
000190          03  JTRN-052          PIC 9(03).                        íºëóêÊNO
000200     02   JTRN-06               PIC 9(06).                        ïiñº
000210     02   JTRN-06A              PIC 9(01).                        ª≤ΩﬁãÊï™
000220     02   JTRN-07.                                                ª≤Ωﬁ
000230          03  JTRN-071.
000240              04  JTRN-0711     PIC S9(06)    OCCURS  10.         I.980604
000250*****         04  JTRN-0711  OCCURS  10  PIC S9(06) COMP-3.       D.980604
000260     02   JTRN-08.                                                î[ä˙
000270*****     03  JTRN-081          PIC 9(02).                        D.980515
000280          03  JTRN-081          PIC 9(04).                        I.980515
000290          03  JTRN-082          PIC 9(02).                        åé
000300          03  JTRN-083          PIC 9(02).                        ì˙
000310     02   JTRN-09               PIC 9(06).                        I.980604
000320*****02   JTRN-09               PIC 9(06) COMP-3.                 D.980604
000330     02   JTRN-10               PIC 9(01).                        ACTION
000340     02   JTRN-11               PIC N(32).                        ìEóv
000350     02   JTRN-12               PIC 9(03).                        SEQ-NO
000360     02   JTRN-13               PIC X(01).                        ïœçXãÊï™
000370     02   JTRN-14.                                                óaÇË
000380          03  JTRN-141          PIC 9(06).                        áÇ
000390          03  JTRN-142          PIC 9(01).                        çsáÇ
000400*****02   JTRN-15               PIC 9(04).                        D.941223
000410*****02   F                     PIC X(04).                        D.980515
000420     02   JTRN-16               PIC S9(03).                       æØƒêî
000430     02   JTRN-90               PIC 9(01).                        I.941024
000440     02   JTRN-15               PIC 9(05).                        I.941223
000450     02   JTRN-17               PIC X(10).                        I.980604
000460     02   FILLER                PIC X(57).                        I.980604
000470*****02   FILLER                PIC X(08).                        D.941223
000480*****02   FILLER                PIC X(03).                        D.980604
