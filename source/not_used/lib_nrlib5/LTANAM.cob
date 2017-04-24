000010 FD  TANAM                                                         ∑”…
000020     BLOCK    3     RECORDS                                       ¿≈µ€º
000030     LABEL    RECORD   STANDARD                                   Ãß≤Ÿ
000040     VALUE    OF  IDENTIFICATION  "TANAM".
000050*
000060 01  TANAM-R.
000070     02   TANAM-KEY.                                              KEY
000080          03   TANAM-01         PIC 9(6).                         ïiñº∫∞ƒﬁ
000090          03   TANAM-02         PIC 9(1).                         ª≤ΩﬁãÊï™
000100          03   TANAM-03         PIC 9(3).                         ª≤Ωﬁ
000110     02   TANAM-05.                                               ç›å…êî
000120          03   TANAM-051        PIC S9(6).                        í†ïÎ
000130          03   TANAM-052        PIC S9(6).                        íIâµ
000140     02   TANAM-06.                                               ëqï íIâµ
000150          03   TANAM-061        PIC S9(6).                        1 ñ{é–
000160          03   TANAM-062        PIC S9(6).                        2 êVâ™
000170          03   TANAM-063        PIC S9(6).                        3 ízç`
000180          03   TANAM-064        PIC S9(6).                        4 ìáìc
000190          03   TANAM-065        PIC S9(6).                        5 ñkí∑ê£
000200          03   TANAM-066        PIC S9(6).                        6 Å|Å@
000210          03   TANAM-067        PIC S9(6).                        7 Å|Å@
000220          03   TANAM-068        PIC S9(6).                        8 óaÇØ
000230     02   TANAM-06A   REDEFINES   TANAM-06.
000240          03   TANAM-06R        PIC S9(6)    OCCURS   8.
000250     02   TANAM-04              PIC 9(8).                         íPâø
000260     02   TANAM-04R        REDEFINES     TANAM-04.
000270          03   TANAM-041        PIC 9(4).                         êUë÷
000280          03   TANAM-042        PIC 9(4).                         åàéZ
000290     02  TANAM-07               PIC 9(1).                         ì¸å…ãÊï™
000300     02  TANAM-08               PIC 9(2).                         ï™óﬁ∫∞ƒﬁ
000310     02  F                      PIC X(4).
