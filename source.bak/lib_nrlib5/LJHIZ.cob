000010 FD  JHIZ                                                         ËÝÒ²ÍÞÂ
000020     BLOCK    1     RECORDS                                       ¼Þ­Á­³
000030     LABEL    RECORD   STANDARD                                   Ì§²Ù
000040     VALUE    OF  IDENTIFICATION  "JHIZ".
000050*
000060 01  JHIZ-R.
000070     02   JHIZ-KEY.                                               KEY
000080          03   JHIZ-01          PIC 9(6).                         ËÝº°ÄÞ
000090     02   JHIZ-02.                                                ¼Þ­Á­³
000100          03   JHIZ-021    OCCURS  10.                            »²½Þ 1
000110               04  JHIZ-0211    PIC S9(5).
000120          03  JHIZ-022     OCCURS  10.                            »²½Þ 2
000130              04  JHIZ-0221     PIC S9(5).
000140          03  JHIZ-023     OCCURS  10.                            »²½Þ 3
000150              04  JHIZ-0231     PIC S9(5).
000160          03  JHIZ-024     OCCURS  10.                            »²½Þ 4
000170              04  JHIZ-0241     PIC S9(5).
000180     02   JHIZ-03.                                                ºÞ³¹²
000190          03  JHIZ-031          PIC S9(7).                        Â³¼Þ®³
000200          03  JHIZ-032          PIC S9(7).                        ±½Þ¶Ø
000210*****02   FILLER                PIC X(36).                        D.950411
000220     02   FILLER                PIC X(35).                        I.950411
000230     02   JHIZ-99               PIC X(01).                        I.950411
