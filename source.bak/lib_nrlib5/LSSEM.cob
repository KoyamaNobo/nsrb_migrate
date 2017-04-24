000010 01  SCR-STN-ERR-MSG   LINE 24.
000020     02  E-STAT  COLUMN  10  PIC  X(002) FROM  ERR-STAT.
000030     02  E-ME71.
000040       03  COLUMN   1  PIC  X(013) FROM  W-FILE.
000050       03  COLUMN  15  PIC  N(025) VALUE
000060           NC"オーバーフロー、領域を拡張後、「ＣＴＲＬ＋ｆ・５」".
000070     02  E-ME78  COLUMN   5  PIC  N(002) VALUE NC"連絡".
000080     02  E-ME98  COLUMN  75  PIC  X(005) VALUE ""27"J"05"".
000090     02  E-ME99  COLUMN  75  PIC  X(005) VALUE ""27"B"05"".
000100     02  E-CL.
000110       03  COLUMN   1  PIC  X(040) VALUE
000120            "                                        ".
000130       03  COLUMN  41  PIC  X(040) VALUE
000140            "                                        ".
