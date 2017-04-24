000010 FD  JWNOK                                                        É³·ÍÞÂ
000020     BLOCK    1     RECORDS                                       ¼­¯¶ÖÃ²
000030     LABEL    RECORD   STANDARD                                   Ü°¸
000040     VALUE    OF  IDENTIFICATION   WK0256ID.                      I.960208
000050*****VALUE    OF  IDENTIFICATION   JT-OWS256ID.                   D.960208
000060*
000070 01  JWNOK-R.
000080     02   JWNOK-KEY.                                              ¿°Ä  KEY
000090          03    JWNOK-01.                                         É³·
000100*****           04  JWNOK-011   PIC 9(2).                         D.980515
000110                04  JWNOK-011   PIC 9(4).                         I.980515
000120                04  JWNOK-012   PIC 9(2).                         Â·
000130                04  JWNOK-013   PIC 9(2).                         Ë
000140          03  JWNOK-02.                                           Á®¿³»·C
000150              04  JWNOK-021    PIC 9(4).                          Ä¸²º°ÄÞ
000160              04  JWNOK-022    PIC 9(3).                          Á®¸¿³NO
000170          03  JWNOK-03         PIC 9(6).                          ËÝº°ÄÞ
000180          03  JWNOK-04         PIC 9(6).                          ¼Þ­Á­³NO
000190          03  JWNOK-05         PIC 9(1).                          »²½Þ¸ÌÞÝ
000200     02   JWNOK-06             PIC 9(1).                          ±½Þ¶Ø¸ÌÞ
000210     02   JWNOK-07.                                               ¼Þ­Á­³
000220          03  JWNOK-071    OCCURS  10.                            »²½ÞÍÞÂ
000230              04  JWNOK-0711   PIC S9(6).
000240*****02   FILLER               PIC X(15).                         D.941019
000250     02   JWNOK-08             PIC 9(4).                          I.941019
000260     02   JWNOK-09             PIC 9(1).                          I.941019
000270     02   FILLER               PIC X(162).                        I.980515
000280*****02   FILLER               PIC X(10).                         D.980515
000290*****02   FILLER               PIC X(154).                        D.980515
