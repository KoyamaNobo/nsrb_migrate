000010*******************************************
000020*****                                 *****
000030*****     ¼ Ê × ² Ã ¶Þ À  Ì § ² Ù     *****
000040*****   ( MK-SHIT SEQUENTIAL FILE )   *****
000050*******************************************
000060 FD  SHIT-F
000070     BLOCK 2 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "TK128".
000100 01  SHIT-R.
000110     02  ST-KEY     PIC 9(4).                                     ³¹ÃNO
000120     02  ST-TSCD.                                                 Ã¶ÞÀ¼­Ù²
000130       03  ST-TC1   PIC 9.
000140       03  ST-TC2   PIC 9.
000150     02  ST-TSC  REDEFINES ST-TSCD  PIC 9(2).                     Ã¶ÞÀ¼­Ù²
000160     02  ST-SKC     PIC 9(2).                                     ¼®Ø¸ÌÞÝ
000170     02  ST-BCD     PIC 9(4).                                     BKº°ÄÞ
000180     02  ST-FKC     PIC 9(2).                                     Ì¹Ýº°ÄÞ
000190     02  ST-TCD     PIC 9(4).                                     ÄØË·º°ÄÞ
000200     02  ST-KIN     PIC 9(10).                                    ·Ý¶Þ¸
000210     02  ST-FDDD.                                                 ÌØÀÞ¼ËÞ
000220       03  ST-FNG.
000230         04  ST-FDN PIC 9(2).
000240         04  ST-FDG PIC 9(2).
000250       03  ST-FDP   PIC 9(2).
000260     02  ST-FDD  REDEFINES ST-FDDD  PIC 9(6).
000270     02  ST-MKDD.                                                 ÏÝ·ËÞ
000280       03  ST-MNG.
000290         04  ST-MKN PIC 9(2).
000300         04  ST-MKG PIC 9(2).
000310       03  ST-MKP   PIC 9(2).
000320     02  ST-MKD  REDEFINES ST-MKDD  PIC 9(6).
000330*    [   ±²Ã¶Ó¸  ³ÁÜ¹ ·Ý¶Þ¸   ]
000340     02  ST-UKD.
000350       03  ST-UK  OCCURS 7  PIC 9(8).
000360     02  ST-AUK  REDEFINES ST-UKD.
000370       03  ST-ZR    PIC 9(8).                                     »Þ²Ø®³
000380       03  ST-SS    PIC 9(8).                                     ¼²Ú¼®³ËÝ
000390       03  ST-SB    PIC 9(8).                                     ¾ÂËÞ
000400       03  ST-GC    PIC 9(8).                                     ¶Þ²Á­³
000410       03  ST-SZ    PIC 9(8).                                     ¾²¿Þ³¹²Ë
000420       03  ST-EG    PIC 9(8).                                     ´²·Þ®³¹²
000430       03  ST-ST    PIC 9(8).                                     ¿ÉÀ
000440     02  F          PIC X(32).
