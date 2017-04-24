000010*    ¼Ü¹ ²ÝÌß¯Ä         *
000020 FD  SDIW
000030     BLOCK      CONTAINS     5      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "SIWAKE-IW".
000060 01  SDIW-REC.
000070     02  SDIW-KEY.
000080       03  SDIYMDW       PIC 9(6).
000090       03  SDIJNOW       PIC 9(6).
000100       03  SDILNOW       PIC 99.
000110     02  SDIKARIW.
000120       03  KRCDW.
000130         04  KRCDMW      PIC 9(4).
000140         04  KRCDSW      PIC 9(3).
000150       03  KRBNKW        PIC 9(2).
000160       03  KRSECTW       PIC 9(4).
000170       03  KRRATEW       PIC 9(3).
000180       03  F             PIC X.
000190       03  KRKINW        PIC S9(10).
000200       03  KR-TBW        PIC 9(2).
000210     02  SDIKASIW.
000220       03  KSCDW.
000230         04  KSCDMW      PIC 9(4).
000240         04  KSCDSW      PIC 9(3).
000250       03  KSBNKW        PIC 9(2).
000260       03  KSSECTW       PIC 9(4).
000270       03  KSRATEW       PIC 9(3).
000280       03  F             PIC X.
000290       03  KSKINW        PIC S9(10).
000300       03  KS-TBW        PIC 9(2).
000310     02  SDICUSTW        PIC 9(5).
000320     02  SDITEKIW        PIC X(20).
000330     02  SDISINW         PIC X.
000340     02  SDITEKICDW      PIC X(3).
000350     02  SDIUSKBW        PIC X(1).
