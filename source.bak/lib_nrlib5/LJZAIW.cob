000010 FD  JZAIW                                                        ¸×ÍÞÂ
000020     BLOCK    1     RECORDS                                       »Þ²º
000030     LABEL    RECORD   STANDARD                                   Ï½À°
000040     VALUE    OF  IDENTIFICATION  "JZAIW".
000050*
000060 01  JZAIW-R.
000070     02  JZAIW-KEY.                                               KEY
000080          03   JZAIW-01    PIC 9(1).                              ¸× º°ÄÞ
000090          03   JZAIW-02    PIC 9(6).                              ËÝº°ÄÞ
000100          03   JZAIW-03    PIC 9(1).                              »²½Þ¸ÌÞÝ
000110     02  JZAIW-04.                                                ¾ÞÝ¸Øº¼
000120          03  LZAI-041     OCCURS  10.                            »²½ÞÍÞÂ
000130              04 JZAIW-0411   PIC S9(6)     COMP-3.
000140     02  JZAIW-05.                                                Ä³Æ­³º
000150          03 JZAIW-051     OCCURS  10.                            »²½ÞÍÞÂ
000160              04 JZAIW-0511   PIC S9(6)     COMP-3.
000170     02  JZAIW-06.                                                Ä³¼­¯º
000180          03 JZAIW-061     OCCURS  10.                            »²½ÞÍÞÂ
000190              04 JZAIW-0611   PIC S9(6)     COMP-3.
000200     02  JZAIW-07.                                                Æ­³¶»ÞÝ
000210          03 JZAIW-071     OCCURS  10.                            »²½ÞÍÞÂ
000220              04 JZAIW-0711   PIC S9(6)     COMP-3.
000230     02  JZAIW-08.                                                ¼­¯¶»ÞÝ
000240          03 JZAIW-081     OCCURS  10.                            »²½ÞÍÞÂ
000250              04 JZAIW-0811   PIC S9(6)     COMP-3.
000260     02  JZAIW-09.                                                I.940916
000270          03 JZAIW-091     OCCURS  10.                            I.940916
000280              04 JZAIW-0911   PIC S9(6)     COMP-3.               I.940916
000290*****02   FILLER            PIC X(48).                            D.940916
000300     02   FILLER            PIC X(08).                            I.940916
