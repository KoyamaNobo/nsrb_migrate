000010*******************************************
000020*****     � � � � � �� �  � � � �     *****
000030*******************************************
000040 FD  SHIT-F
000050*****BLOCK  3 RECORDS                                             D.970602
000060     BLOCK  2 RECORDS                                             I.970602
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION WK0128ID.                            I.970602
000090*****VALUE OF IDENTIFICATION "TK128".                             D.970210
000100*****VALUE OF IDENTIFICATION WK0170ID.                            D.970602
000110 01  SHIT-R.
000120     02  ST-KEY         PIC  9(004).                              ���NO
000130     02  ST-TSC         PIC  9(002).                              ö����ٲ
000140     02  ST-TSCD REDEFINES ST-TSC.                                ö����ٲ
000150       03  ST-TC1       PIC  9(001).
000160       03  ST-TC2       PIC  9(001).
000170     02  ST-SKC         PIC  9(002).                              ��ظ���
000180     02  ST-BCD         PIC  9(004).                              BK����
000190     02  ST-FKC         PIC  9(002).                              ̹ݺ���
000200     02  ST-TCD         PIC  9(004).                              ��˷����
000210     02  ST-KIN         PIC  9(010).                              �ݶ޸
000220     02  ST-FDD         PIC  9(006).
000230     02  ST-FDDD REDEFINES ST-FDD.                                ���޼��
000240       03  ST-FNG.
000250         04  ST-FDN     PIC  9(002).
000260         04  ST-FDG     PIC  9(002).
000270       03  ST-FDP       PIC  9(002).
000280     02  ST-MKD         PIC  9(006).
000290     02  ST-MKDD REDEFINES ST-MKD.                                �ݷ��
000300       03  ST-MNG.
000310         04  ST-MKN     PIC  9(002).
000320         04  ST-MKG     PIC  9(002).
000330       03  ST-MKP       PIC  9(002).
000340*    [   ��öӸ  ��ܹ �ݶ޸   ]
000350     02  ST-UKD.
000360       03  ST-UK    OCCURS  7  PIC  9(008).
000370     02  ST-AUK   REDEFINES ST-UKD.
000380       03  ST-ZR        PIC  9(008).                              �޲خ�
000390       03  ST-SS        PIC  9(008).                              ��ڼ����
000400       03  ST-SB        PIC  9(008).                              ����
000410       03  ST-GC        PIC  9(008).                              �޲���
000420       03  ST-SZ        PIC  9(008).                              ���޳���
000430       03  ST-EG        PIC  9(008).                              ���ޮ���
000440       03  ST-ST        PIC  9(008).                              ���
000450     02  F              PIC  X(024).                              I.970915
000460     02  ST-SNF         PIC  9(004).                              I.970915
000470     02  ST-SNM         PIC  9(004).                              I.970915
000480*****02  F              PIC  X(032).                              D.970915
000490*****02  F              PIC  X(074).                              D.970602
