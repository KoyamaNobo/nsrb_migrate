000010*******************************************
000020*****                                 *****
000030*****     � � � � � �� �  � � � �     *****
000040*****   ( MK-SHIT SEQUENTIAL FILE )   *****
000050*******************************************
000060 FD  SHIT-F
000070     BLOCK 2 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "TK128".
000100 01  SHIT-R.
000110     02  ST-KEY     PIC 9(4).                                     ���NO
000120     02  ST-TSCD.                                                 ö����ٲ
000130       03  ST-TC1   PIC 9.
000140       03  ST-TC2   PIC 9.
000150     02  ST-TSC  REDEFINES ST-TSCD  PIC 9(2).                     ö����ٲ
000160     02  ST-SKC     PIC 9(2).                                     ��ظ���
000170     02  ST-BCD     PIC 9(4).                                     BK����
000180     02  ST-FKC     PIC 9(2).                                     ̹ݺ���
000190     02  ST-TCD     PIC 9(4).                                     ��˷����
000200     02  ST-KIN     PIC 9(10).                                    �ݶ޸
000210     02  ST-FDDD.                                                 ���޼��
000220       03  ST-FNG.
000230         04  ST-FDN PIC 9(2).
000240         04  ST-FDG PIC 9(2).
000250       03  ST-FDP   PIC 9(2).
000260     02  ST-FDD  REDEFINES ST-FDDD  PIC 9(6).
000270     02  ST-MKDD.                                                 �ݷ��
000280       03  ST-MNG.
000290         04  ST-MKN PIC 9(2).
000300         04  ST-MKG PIC 9(2).
000310       03  ST-MKP   PIC 9(2).
000320     02  ST-MKD  REDEFINES ST-MKDD  PIC 9(6).
000330*    [   ��öӸ  ��ܹ �ݶ޸   ]
000340     02  ST-UKD.
000350       03  ST-UK  OCCURS 7  PIC 9(8).
000360     02  ST-AUK  REDEFINES ST-UKD.
000370       03  ST-ZR    PIC 9(8).                                     �޲خ�
000380       03  ST-SS    PIC 9(8).                                     ��ڼ����
000390       03  ST-SB    PIC 9(8).                                     ����
000400       03  ST-GC    PIC 9(8).                                     �޲���
000410       03  ST-SZ    PIC 9(8).                                     ���޳���
000420       03  ST-EG    PIC 9(8).                                     ���ޮ���
000430       03  ST-ST    PIC 9(8).                                     ���
000440     02  F          PIC X(32).
