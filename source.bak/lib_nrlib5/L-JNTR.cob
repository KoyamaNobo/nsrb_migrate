000010 FD  JNTR                                                         Æ­³¼­¯º
000020     BLOCK    3     RECORDS                                       Ä×Ý
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  "JNTR".
000050*
000060 01  JNTR-R.
000070     02   JNTR-01               PIC 9(1).                         ¸×º°ÄÞ
000080     02   JNTR-02               PIC 9(2).                         Æ®³¼­Âº
000090     02   JNTR-03.                                                ÈÝ¶Þ¯Ëß
000100          03  JNTR-031          PIC 9(2).                         ÈÝ
000110          03  JNTR-032          PIC 9(2).                         Â·
000120          03  JNTR-033          PIC 9(2).                         Ë
000130     02   JNTR-04               PIC 9(6).                         ÃÞÝËß®³
000140     02   JNTR-05               PIC 9(1).                         ·Þ®³
000150     02   JNTR-06               PIC 9(6).                         ËÝº°ÄÞ
000160     02   JNTR-07               PIC 9(1).                         »²½Þ¸ÌÞÝ
000170     02   JNTR-08.                                                Æ®³¼¯º
000180          03  JNTR-081     OCCURS  10.                            »²½ÞÍÞÂ
000190              04  JNTR-0811     PIC S9(4).
000200     02   JNTR-09               PIC 9(1).                         ¾²»Ý¸ÌÞÝ
000210     02   JNTR-10               PIC 9(4).                         ¼² ¶º CD
000220     02   FILLER                PIC X(17).
