000010***********************************
000020****** iWvjγvγ[N     *
000030******                ISAM        *
000040******                128/2       *
000050***********************************
000060 FD  JT-SUKW
000070     BLOCK    2     RECORDS
000080     LABEL    RECORD   STANDARD
000090     VALUE    OF  IDENTIFICATION   "JT-SUKW".
000100*
000110 01  SUKW-R.
000120     02   SUKW-KEY.                                               KEY
000130          03   SUKW-01           PIC 9(01).                       ΪΊ°ΔήKBN
000140          03   SUKW-02           PIC 9(01).                       Γή°ΐΈΜήέ
000150          03   SUKW-03           PIC 9(06).                       ϊt
000160          03   SUKW-03R  REDEFINES    SUKW-03.
000170               04   SUKW-031          PIC 9(02).                  N
000180               04   SUKW-032          PIC 9(02).                  
000190               04   SUKW-033          PIC 9(02).                  ϊ
000200          03   SUKW-04           PIC 9(04).                       ΎΣζCD
000210          03   SUKW-05           PIC 9(03).                       SEQ-NO
000220          03   SUKW-06           PIC 9(01).                       s
000230          03   SUKW-07           PIC 9(01).                       Τζͺ
000240     02   SUKW-08                PIC 9(06).                       iΌΊ°Δή
000250     02   SUKW-09                PIC 9(01).                       »²½ήζͺ
000260     02   SUKW-10.                                                oΧ
000270          03   SUKW-101  OCCURS  10  PIC S9(04)   COMP-3.
000280     02   SUKW-11                PIC S9(06)       COMP-3.         vΚ
000290*****02   SUKW-12                PIC 9(04).                       D.941226
000300     02   F                      PIC X(04).                       I.941226
000310     02   SUKW-13                PIC 9(03).                       ­Ά
000320     02   SUKW-14                PIC 9(03).                       Όζ
000330     02   SUKW-15                PIC 9(01).                       `[ζͺ
000340     02   SUKW-16                PIC 9(01).                       qΊ°Δή
000350     02   SUKW-17                PIC S9(03).                      Β
000360     02   SUKW-18                PIC 9(03).                       Όζ
000370     02   SUKW-19.                                                aθ
000380          03   SUKW-191          PIC 9(06).                       
000390          03   SUKW-192          PIC 9(01).                       s
000400     02   SUKW-20.                                                oΧw}
000410          03   SUKW-201          PIC 9(06).                       
000420          03   SUKW-202          PIC 9(01).                       s
000430     02   SUKW-21                PIC 9(06).                       θσ
000440     02   SUKW-22                PIC N(05).                       zB
000450     02   SUKW-23                PIC N(06).                       Ev
000460     02   SUKW-24                PIC X(01).                       iΌXV
000470*****02   F                      PIC X(09).                       D.941226
000480     02   SUKW-12                PIC 9(05).                       I.941226
000490     02   F                      PIC X(04).                       I.941226
