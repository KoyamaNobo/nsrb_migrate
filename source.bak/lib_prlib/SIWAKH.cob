000010***  Žd–óƒqƒXƒgƒŠ     (170/3)
000020 01  SH-REC.
000030*****02  SH-KEY1.                                                 D.980223
000040*****  03  HTRDATE        PIC 9(6).                               D.971111
000050*****  03  HTRDATE        PIC 9(8).                               D.980223
000060*****  03  HJUNLNO        PIC 9(6).                               D.980223
000070*****  03  HLINENO        PIC 9(2).                               D.980223
000080*****  03  HDR-CR         PIC 9(1).                               D.980223
000090*****02  HKACD1.                                                  D.980223
000100*****  03  HACCNTCD       PIC 9(4).                               D.980223
000110*****  03  HHOACCNT       PIC 9(4).                               D.980223
000120     02  SH-KEY3.                                                 I.980223
000130       03  HKACD1.                                                I.980223
000140         04  HACCNTCD     PIC 9(4).                               I.980223
000150         04  HHOACCNT     PIC 9(4).                               I.980223
000160       03  SH-KEY1.                                               I.980223
000170         04  HTRDATE      PIC 9(8).                               I.980223
000180         04  HJUNLNO      PIC 9(6).                               I.980223
000190         04  HLINENO      PIC 9(2).                               I.980223
000200         04  HDR-CR       PIC 9(1).                               I.980223
000210     02  HSECTCD          PIC 9(4).                               ÌÞÓÝC
000220     02  HSKINCD          PIC 9(3).                               ¼·Ý¸ÞØC
000230     02  HTAXKB           PIC X(1).                               ¶¾Þ²C
000240     02  HAMOUNT          PIC S9(10).                             ·Ý¶Þ¸
000250     02  HTEG-BAN         PIC 9(2).                               Ã¶ÞÀBKC
000260*****02  HTEG-NO          PIC X(8).                               D.980223
000270     02  HKACD2.
000280       03  HOPPCD         PIC 9(4).                               ±²Ã¶Ó¸C
000290       03  HHOOPPCD       PIC 9(4).                                Î¼Þ®¶Ó¸
000300     02  HCUSTCD          PIC 9(5).                               ÄØË·»·C
000310     02  HTEKICD          PIC 9(3).                               Ã·Ö³C
000320     02  HTEKIYO          PIC N(20).                              Ã·Ö³
000330     02  HKEIHIKB         PIC 9(1).                               ¹²ËC
000340     02  HNAMEN           PIC N(10).                              I.980223
000350     02  SH-KEY2.                                                 KEY2
000360       03  HACCNTCD2      PIC 9(4).                               ¶Ó¸C
000370*****  03  HTRDATE2       PIC 9(6).                               D.971111
000380       03  HTRDATE2       PIC 9(8).                               I.971111
000390       03  HJUNLNO2       PIC 9(6).                               ÃÞÝËß®³
000400       03  HLINENO2       PIC 9(2).                                ·Þ®³
000410       03  HDR-CR2        PIC 9(1).                               À²¼¬¸C
000420*****02  SH-KEY3.                                                 D.980223
000430*****  03  HKACD3.                                                D.980223
000440*****    04  HACCNTCD3    PIC 9(4).                               D.980223
000450*****    04  HHOACCNT3    PIC 9(4).                               D.980223
000460*****  03  HTRDATE3       PIC 9(6).                               D.971111
000470*****  03  HTRDATE3       PIC 9(8).                               D.980223
000480*****  03  HJUNLNO3       PIC 9(6).                               D.980223
000490*****  03  HLINENO3       PIC 9(2).                               D.980223
000500*****  03  HDR-CR3        PIC 9(1).                               D.980223
000510*****02  FILLER           PIC X(19).                              D.970729
000520*****02  FILLER           PIC X(18).                              D.971111
000530*****02  FILLER           PIC X(12).                              D.980223
000540     02  F                PIC X(25).                              I.980223
000550     02  HETAX            PIC X(01).                              I.970729
000560     02  HCOM             PIC 9(1).                               ¾Þ²¹²»ÝC
