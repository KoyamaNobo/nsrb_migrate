000010 FD  NJZAIW                                                       ¸×ÍÞÂ
000020*****BLOCK    3     RECORDS                                       D.960206
000030     BLOCK    1     RECORDS                                       I.960206
000040     LABEL    RECORD   STANDARD                                   Ï½À°
000050*****VALUE    OF  IDENTIFICATION  "NJZAIW".                       D.960206
000060     VALUE    OF  IDENTIFICATION  WK0512ID.                       I.960206
000070*
000080 01  NJZAIW-R.
000090     02  NJZAIW-KEY.                                              KEY
000100          03  NJZAIW-01   PIC 9(1).                               ¸× º°ÄÞ
000110          03  NJZAIW-02   PIC 9(6).                               ËÝº°ÄÞ
000120          03  NJZAIW-03   PIC 9(1).                               »²½Þ¸ÌÞÝ
000130     02  NJZAIW-04.
000140          03  NJZAI-041     OCCURS  10.                           »²½ÞÍÞÂ
000150              04  NJZAIW-0411   PIC S9(6)     COMP-3.
000160     02  NJZAIW-05.
000170          03  NJZAIW-051     OCCURS  10.                          »²½ÞÍÞÂ
000180              04  NJZAIW-0511   PIC S9(6)     COMP-3.
000190     02  NJZAIW-06.
000200          03  NJZAIW-061     OCCURS  10.                          »²½ÞÍÞÂ
000210              04  NJZAIW-0611   PIC S9(6)     COMP-3.
000220     02  NJZAIW-07.
000230          03  NJZAIW-071     OCCURS  10.                          »²½ÞÍÞÂ
000240              04  NJZAIW-0711   PIC S9(6)     COMP-3.
000250     02  NJZAIW-08.
000260          03  NJZAIW-081     OCCURS  10.                          »²½ÞÍÞÂ
000270              04  NJZAIW-0811   PIC S9(6)     COMP-3.
000280     02  NJZAIW-09.
000290          03  NJZAIW-091     OCCURS  10.
000300              04  NJZAIW-0911   PIC S9(6)     COMP-3.
000310     02  NJZAIW-10.
000320          03  NJZAIW-101     OCCURS  10.
000330              04  NJZAIW-1011   PIC S9(6)     COMP-3.
000340     02  NJZAIW-11.
000350          03  NJZAIW-111     OCCURS  10.
000360              04  NJZAIW-1111   PIC S9(6)     COMP-3.
000370*****02   FILLER            PIC X(13).                            D.960206
000380     02   FILLER            PIC X(184).                           I.960206
