000010 FD  NJZAI                                                        ¸×ÍÞÂ
000020     BLOCK    3     RECORDS                                       »Þ²º
000030     LABEL    RECORD   STANDARD                                   Ï½À°
000040     VALUE    OF  IDENTIFICATION  "NJZAI".
000050*
000060 01  NJZAI-R.
000070     02   NJZAI-KEY.                                              KEY
000080          03    NJZAI-01    PIC 9(1).                             ¸× º°ÄÞ
000090          03    NJZAI-02    PIC 9(6).                             ËÝº°ÄÞ
000100          03    NJZAI-03    PIC 9(1).                             »²½Þ¸ÌÞÝ
000110     02   NJZAI-04.                                               ¾ÞÝ¸Øº¼
000120          03  NJZAI-041     OCCURS  10.                           »²½ÞÍÞÂ
000130              04  NJZAI-0411   PIC S9(6)     COMP-3.
000140     02   NJZAI-05.                                               Ä³Æ­³º
000150          03  NJZAI-051     OCCURS  10.                           »²½ÞÍÞÂ
000160              04  NJZAI-0511   PIC S9(6)     COMP-3.
000170     02   NJZAI-06.                                               Ä³¼­¯º
000180          03  NJZAI-061     OCCURS  10.                           »²½ÞÍÞÂ
000190              04  NJZAI-0611   PIC S9(6)     COMP-3.
000200     02   NJZAI-07.
000210          03  NJZAI-071     OCCURS  10.                           »²½ÞÍÞÂ
000220              04  NJZAI-0711   PIC S9(6)     COMP-3.
000230     02   NJZAI-08.
000240          03  NJZAI-081     OCCURS  10.                           »²½ÞÍÞÂ
000250              04  NJZAI-0811   PIC S9(6)     COMP-3.
000260     02   NJZAI-09.                                               »¼½ÞÐ¶¸Ã
000270          03  NJZAI-091     OCCURS  10.                           »²½ÞÍÞÂ
000280              04  NJZAI-0911   PIC S9(6)     COMP-3.
000290     02   NJZAI-10.                                               »Þ²ºÁ®³¾
000300          03  NJZAI-101     OCCURS  10.                           »²½ÞÍÞÂ
000310              04  NJZAI-1011   PIC S9(6)     COMP-3.
000320     02   NJZAI-11.                                               ¼ÒºÞÆ­³º
000330          03  NJZAI-111     OCCURS  10.                           »²½ÞÍÞÂ
000340              04  NJZAI-1111   PIC S9(6)     COMP-3.
000350     02   FILLER            PIC X(12).
000360     02   NJZAI-99          PIC X(01).                            ºÞ³¹²C
