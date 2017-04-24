000010***************************************************
000020*****     ê¨å^ó\íËïœä∑ÉfÅ[É^                  *****
000030*****     (  HSYHF 128/2  )                   *****
000040***************************************************
000050 FD  HSYHF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSYHF".
000090 01  HSYH-R.
000100     02  HSYH-HSC       PIC  9(001).                              ñDêªãÊï™
000110     02  HSYH-SD        PIC  9(002).                              äJénì˙
000120     02  HSYH-ED        PIC  9(002).                              èIóπì˙
000130     02  HSYH-YGP       PIC  9(004).                              èoâ◊åéì˙
000140     02  HSYH-YGPD  REDEFINES HSYH-YGP.
000150       03  HSYH-YGET    PIC  9(002).
000160       03  HSYH-YPEY    PIC  9(002).
000170     02  HSYH-HCD       PIC  9(006).                              ∫∞ƒﬁ
000180*****02  HSYH-HNA       PIC  N(018).                              D.040202
000190*****02  HSYH-COR       PIC  N(018).                              D.040202
000200     02  HSYH-SRNO.
000210       03  HSYH-GET     PIC  9(002).                              åé
000220       03  HSYH-NO      PIC  9(003).                              áÇ
000230       03  HSYH-SUB     PIC  9(001).                              é}î‘
000240     02  HSYH-ASUD.                                               ê¨å^ó\íË
000250       03  HSYH-SUD   OCCURS  25.
000260         04  HSYH-SU    PIC S9(003).
000270     02  HSYH-SUT       PIC S9(004).
000280     02  F              PIC  X(028).                              I.040202
000290*****02  F              PIC  X(084).                              D.040202
