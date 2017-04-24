000010********************************************
000020*****     出荷・値引トラン　ワーク     *****
000030*****       WK0064___       64/4       *****
000040********************************************
000050 FD  SNTRF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION WK0064ID.
000090 01  SNTR-R.
000100     02  SNTR-DATE        PIC  9(008).
000110     02  SNTR-NGP   REDEFINES SNTR-DATE.
000120       03  SNTR-NG.
000130         04  SNTR-NEN     PIC  9(004).
000140         04  SNTR-NEND  REDEFINES SNTR-NEN.
000150           05  SNTR-NEN1  PIC  9(002).
000160           05  SNTR-NEN2  PIC  9(002).
000170         04  SNTR-GET     PIC  9(002).
000180       03  SNTR-PEY       PIC  9(002).
000190     02  SNTR-NGPD  REDEFINES SNTR-DATE.
000200       03  F              PIC  9(002).
000210       03  SNTR-NGPS.
000220         04  SNTR-NENS    PIC  9(002).
000230         04  SNTR-GP      PIC  9(004).
000240     02  SNTR-TCD         PIC  9(004).
000250     02  SNTR-HCD         PIC  9(006).
000260     02  SNTR-HCDD  REDEFINES SNTR-HCD.
000270       03  SNTR-HCD1      PIC  9(004).
000280       03  SNTR-HCD2      PIC  9(002).
000290     02  SNTR-SU          PIC S9(005).
000300     02  SNTR-T           PIC  9(005).
000310     02  SNTR-KIN         PIC S9(008).
000320     02  SNTR-CSC         PIC  9(001).
000330     02  SNTR-DC          PIC  9(001).
000340     02  SNTR-FT          PIC  9(005).
000350     02  SNTR-BC.
000360       03  SNTR-BC1       PIC  9(002).
000370       03  SNTR-BC2.
000380         04  SNTR-BC21    PIC  9(001).
000390         04  SNTR-BC22    PIC  9(001).
000400       03  SNTR-BC3       PIC  9(002).
000410     02  SNTR-BCD   REDEFINES SNTR-BC.                            I.060525
000420       03  SNTR-BCD1      PIC  9(003).                            I.060525
000430       03  F              PIC  9(003).                            I.060525
000440     02  SNTR-TNC.
000450       03  SNTR-TNC1      PIC  9(001).
000460       03  SNTR-TNC2      PIC  9(001).
000470     02  SNTR-FKC         PIC  9(002).
000480     02  SNTR-SZC         PIC  9(001).
000490     02  SNTR-SNC         PIC  9(001).
000500     02  SNTR-BMC         PIC  9(002).                            I.020517
000510     02  SNTR-BMNO        PIC  9(001).                            I.020517
000520     02  SNTR-HPV         PIC  9(001).                            I.060424
000530     02  SNTR-FTC         PIC  9(001).                            I.070402
000540     02  F                PIC  X(003).                            I.080804
000550     02  SNTR-SIZ         PIC  9(001).                            I.080804
000560*****02  F                PIC  X(004).                            D.080804
000570*****02  F                PIC  X(005).                            D.070402
000580*****02  F                PIC  X(006).                            D.060424
000590*****02  F                PIC  X(009).                            D.020517
