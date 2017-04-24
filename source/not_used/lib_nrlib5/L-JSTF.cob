000010 FD  JSTF                                                         ¼­¯¶
000020     BLOCK    8     RECORDS                                       ¼­³¹²
000030     LABEL    RECORD   STANDARD                                   Ì§²Ù
000040     VALUE    OF  IDENTIFICATION  "JSTF".
000050*
000060 01  JSTF-R.
000070     02   JSTF-KEY.                                               KEY
000080          03    JSTF-01   PIC 9(1).                               ¸× º°ÄÞ
000090          03    JSTF-02   PIC 9(1).                               ³Ý¿³
000100          03    JSTF-03.
000110              04  JSTF-031   PIC 9(4).                            Ä¸² º°ÄÞ
000120              04  JSTF-032   PIC 9(3).                            Á®¸¿³NO
000130     02   JSTF-04.                                                ¼­¯¶º½³
000140          03  JSTF-041      PIC S9(6).                            ÎÝ¼ÞÂ
000150          03  JSTF-042      PIC S9(7).                            Ä³¹ÞÂ
000160     02   FILLER         PIC X(10).
