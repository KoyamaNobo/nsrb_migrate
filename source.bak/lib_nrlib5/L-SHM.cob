000010 FD  SHM                                                          º≠Ø∂
000020     BLOCK    3     RECORDS                                       À›“≤
000030     LABEL    RECORD   STANDARD                                   œΩ¿∞
000040     VALUE    OF  IDENTIFICATION  "SHM".                          170/3
000050*
000060 01  SHM-R.
000070     02   SHM-KEY.                                                KEY
000080          03   SHM-01           PIC 9(6).                         À›∫∞ƒﬁ
000090     02   SHM-02                PIC N(24).                        À›“≤
000100     02   SHM-03                PIC 9(1).                         ø≥∫∏Ãﬁ›
000110     02   SHM-04.                                                 º÷≥ª≤Ωﬁ
000120          03   SHM-041.                                           ª≤Ωﬁ 1
000130               04  SHM-0411  OCCURS  10  PIC  9.
000140          03   SHM-042.                                           ª≤Ωﬁ 2
000150               04  SHM-0421  OCCURS  10  PIC  9.
000160          03   SHM-043.                                           ª≤Ωﬁ 3
000170               04  SHM-0431  OCCURS  10  PIC  9.
000180          03   SHM-044.                                           ª≤Ωﬁ 4
000190               04  SHM-0441  OCCURS  10  PIC  9.
000200     02   SHM-05                PIC 9(2).                         Ãﬁ›Ÿ≤CD
000210*****02   SHM-06                PIC N(15).                        D.960731
000220     02   SHM-06                PIC N(14).                        I.960731
000230     02   SHM-09                PIC 9(2).                         I.960731
000240     02   SHM-07                PIC 9(3).                         ì¸Å@êîÅ@
000250     02   SHM-08.                                                 àÍë´î†ÉT
000260          03   SHM-081.                                           ª≤Ωﬁ 1
000270               04  SHM-0811  OCCURS  10  PIC  9.
000280          03   SHM-082.                                           ª≤Ωﬁ 2
000290               04  SHM-0821  OCCURS  10  PIC  9.
000300          03   SHM-083.                                           ª≤Ωﬁ 3
000310               04  SHM-0831  OCCURS  10  PIC  9.
000320          03   SHM-084.                                           ª≤Ωﬁ 4
000330               04  SHM-0841  OCCURS  10  PIC  9.
