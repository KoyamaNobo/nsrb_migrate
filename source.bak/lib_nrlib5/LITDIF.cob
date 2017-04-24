000010*****************************************
000020*****     ìùàÍì`ï[ì¸óÕÉtÉ@ÉCÉãÅ@    *****
000030*****      (  TDIF  170/3  )        *****
000040*****************************************
000050 FD  TDIF
000060*****BLOCK  3 RECORDS                                             D.100906
000070     BLOCK  1 RECORDS                                             I.100906
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "TDIF".
000100 01  TDI-R.
000110     02  TDI-KEY.
000120       03  TDI-DNO      PIC  9(006).
000130       03  TDI-GNO      PIC  9(001).
000140     02  TDI-DATE       PIC  9(006).
000150     02  TDI-NGP   REDEFINES TDI-DATE.
000160       03  TDI-NEN      PIC  9(002).
000170       03  TDI-GET      PIC  9(002).
000180       03  TDI-PEY      PIC  9(002).
000190     02  TDI-TCD        PIC  9(004).
000200     02  TDI-CCD        PIC  9(003).
000210*****02  TDI-TPC        PIC  9(003).                              D.100906
000220     02  TDI-TPC        PIC  9(004).                              I.100906
000230     02  TDI-HCD        PIC  9(006).
000240     02  TDI-SIZ        PIC  X(003).
000250     02  TDI-SKB        PIC  9(001).
000260     02  TDI-SNO        PIC  9(002).
000270     02  TDI-SU         PIC S9(005).
000280     02  TDI-GT         PIC  9(007).
000290     02  TDI-UT         PIC  9(007).
000300     02  TDI-GKIN       PIC S9(008).
000310     02  TDI-UKIN       PIC S9(008).
000320     02  TDI-JNOD.
000330       03  TDI-JNO      PIC  9(006).
000340       03  TDI-JGN      PIC  9(001).
000350     02  TDI-SOK        PIC  9(001).
000360     02  TDI-UNS        PIC  9(001).
000370     02  TDI-ISU        PIC  9(003).
000380     02  TDI-HNO        PIC  X(010).
000390     02  TDI-TEKI       PIC  N(028).
000400     02  TDI-TED   REDEFINES TDI-TEKI.
000410       03  TDI-THT      PIC  N(009).
000420       03  TDI-TTE      PIC  N(019).
000430     02  TDI-TRN        PIC  X(020).
000440     02  TDI-JAN        PIC  X(013).                              I.100906
000450*****02  TDI-JAND  REDEFINES TDI-TRN.                             D.100906
000460*****  03  TDI-JAN      PIC  X(013).                              D.100906
000470*****  03  F            PIC  X(007).                              D.100906
000480*****02  F              PIC  X(072).                              D.110622
000490     02  F              PIC  X(052).                              D.110622
000500     02  TDI-NNGP       PIC  9(006).                              I.110622
000510     02  TDI-NHMS       PIC  9(006).                              I.110622
000520     02  F              PIC  X(008).                              I.110622
000530     02  TDI-PRC        PIC  9(001).
000540     02  TDI-UPC        PIC  9(001).
