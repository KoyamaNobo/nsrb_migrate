000010*    ø≥∂¬ÀÆ≥Ãß≤Ÿ        *
000020 FD  NT-SOF
000030     BLOCK      CONTAINS     1      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "NT-SOF".
000060 01  SOF-R.
000070     02  SOF-KEY.                                                 KEY
000080       03  SOF-01        PIC X(02).                               ⁄∫∞ƒﬁKBN
000090     02  SOF-02.                                                  ìñä˙ññ
000100       03  SOF-021       OCCURS  3.
000110         04  SOF-0211    PIC 9(03).                               êVãKêlêî
000120         04  SOF-0212    PIC 9(03).                               ëçêlêî
000130         04  SOF-0213    PIC 9(10).                               ã‡äz
000140     02  SOF-03.                                                  ëOä˙ññ
000150         04  SOF-031     OCCURS  3.
000160           05  SOF-0311  PIC 9(03).                               ëçêlêî
000170           05  SOF-0312  PIC 9(10).                               ã‡äz
000180     02  SOF-04.                                                  ëﬁêEé“
000190         04  SOF-041     OCCURS  3.
000200           05  SOF-0411  PIC 9(03).                               ëçêlêî
000210           05  SOF-0412  PIC 9(10).                               ã‡äz
000220     02  FILLER          PIC X(128).
