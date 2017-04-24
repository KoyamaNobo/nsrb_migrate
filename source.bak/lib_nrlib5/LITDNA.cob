000010************************************************
000020*****    統一伝票ファイル（赤ちゃん本舗）  *****
000030*****           ( TDNAF )  256/1           *****
000040************************************************
000050 FD  TDNAF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TDNAF".
000090 01  TDNA-R.
000100     02  TDNA-KEY.
000110       03  TDNA-STC       PIC  9(007).
000120       03  TDNA-STCD  REDEFINES TDNA-STC.                         I.091026
000130         04  TDNA-STC12.                                          I.091026
000140           05  TDNA-STC1  PIC  9(002).                            I.091026
000150           05  TDNA-STC2  PIC  9(003).                            I.091026
000160         04  TDNA-STC3    PIC  9(002).                            I.091026
000170*****  03  TDNA-DNO       PIC  9(006).                            D.100720
000180       03  TDNA-DNO       PIC  9(007).                            I.100720
000190       03  TDNA-DGN       PIC  9(002).
000200     02  TDNA-JAN         PIC  X(013).
000210     02  TDNA-SU          PIC  9(006).
000220     02  TDNA-GTN         PIC  9(007).
000230     02  TDNA-UTN         PIC  9(007).
000240     02  TDNA-GKIN        PIC  9(010).
000250     02  TDNA-UKIN        PIC  9(010).
000260     02  TDNA-DPM         PIC  X(002).
000270     02  TDNA-CLS         PIC  X(003).
000280     02  TDNA-SHM         PIC  X(013).
000290     02  TDNA-MKH         PIC  X(010).
000300     02  TDNA-MSB         PIC  X(010).
000310     02  TDNA-TY          PIC  X(002).
000320     02  TDNA-HCD         PIC  9(006).                            I.090908
000330     02  TDNA-COR         PIC  N(004).
000340     02  TDNA-SIZ         PIC  X(004).
000350     02  TDNA-NSU         PIC  9(006).
000360     02  TDNA-TSC         PIC  9(001).
000370*****02  F                PIC  X(018).                            D.090903
000380*****02  F                PIC  X(015).                            D.090908
000390*****02  F                PIC  X(009).                            D.100720
000400     02  F                PIC  X(008).                            I.100720
000410     02  TDNA-CCD         PIC  9(003).                            I.090903
000420     02  TDNA-TNA         PIC  N(014).
000430     02  TDNA-HNO         PIC  9(009).
000440     02  TDNA-HNGP        PIC  9(006).
000450     02  TDNA-NNGP        PIC  9(006).
000460     02  TDNA-THC         PIC  9(006).
000470     02  TDNA-BI          PIC  X(010).
000480     02  TDNA-SNGP        PIC  9(008).
000490     02  TDNA-SNGPD REDEFINES TDNA-SNGP.
000500       03  F              PIC  9(002).
000510       03  TDNA-NGPS      PIC  9(006).
000520     02  TDNA-HNA         PIC  X(006).
000530     02  TDNA-ZON         PIC  X(004).
000540     02  TDNA-DC          PIC  9(002).
000550     02  F                PIC  X(007).
000560     02  TDNA-DNGP        PIC  9(008).
000570     02  TDNA-DNGPD REDEFINES TDNA-DNGP.
000580       03  F              PIC  9(002).
000590       03  TDNA-DNGPS     PIC  9(006).
000600     02  TDNA-NRC         PIC  9(001).
000610     02  F                PIC  X(008).
000620     02  TDNA-PC          PIC  9(001).
000630     02  TDNA-RC          PIC  9(001).
