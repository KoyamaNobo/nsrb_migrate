000010**************************************
000020*****     ���o�ɗݐσt�@�C��     *****
000030**************************************
000040 FD  JNSR
000050     BLOCK    3     RECORDS
000060     LABEL    RECORD   STANDARD
000070     VALUE     OF  IDENTIFICATION  "JNSR-RDB".
000080 01  JNSR-R.
000090     02  JNSR-KEY1.                                               KEY1
000100       03  JNSR-01              PIC  9(06)  COMP-3.               �i������
000110       03  JNSR-02              PIC  9(08)  COMP-3.               I.980515
000120       03  JNSR-03              PIC  9(02).                       ���o�͋�
000130       03  JNSR-04              PIC  9(06)  COMP-3.               �`�[��
000140       03  JNSR-05              PIC  9(01).                       �s
000150     02  JNSR-06                PIC  9(01).                       �q����
000160     02  JNSR-07                PIC  9(01).                       �T�C�Y
000170     02  JNSR-08.                                                 ���o�ɐ�
000180       03  JNSR-081             PIC S9(04)  COMP-3    OCCURS  10.
000190     02  JNSR-09                PIC  9(01).                       ���Y�敪
000200     02  JNSR-10                PIC  9(01).                       �o�ד`��
000210     02  JNSR-11.
000220       03  JNSR-111             PIC  9(04).                       ���Ӑ�C
000230       03  JNSR-112             PIC  9(03).                       ����NO
000240     02  JNSR-12                PIC  9(06)  COMP-3.               �����
000250     02  JNSR-13                PIC  9(01).                       �a��敪
000260     02  JNSR-14                PIC  9(01).                       �^��C
000270     02  JNSR-KEY2.
000280       03  JNSR-15.                                               ��
000290         04  JNSR-151           PIC  9(06)  COMP-3.               �@�󒍇�
000300         04  JNSR-152           PIC  9(01).                       �@�s��
000310       03  JNSR-16              PIC  9(08)  COMP-3.               I.980515
000320       03  JNSR-17              PIC  9(01).                       ں���KBN
000330       03  JNSR-18.                                               �`�[
000340         04  JNSR-181           PIC  9(06)  COMP-3.               �@�`�[��
000350         04  JNSR-182           PIC  9(01).                       �@�s��
000360     02  JNSR-KEY3.
000370       03  JNSR-19              PIC  9(04).                       ���Ӑ�C
000380       03  JNSR-20              PIC  9(08)  COMP-3.               I.980515
000390       03  JNSR-21              PIC  9(01).                       ���R�[��
000400       03  JNSR-22.                                               �`�[��
000410         04  JNSR-221           PIC  9(06)  COMP-3.               �o�׎w��
000420         04  JNSR-222           PIC  9(01).                       �@�s��
000430     02  JNSR-23                PIC  N(09).                       �z�B
000440     02  JNSR-24                PIC  N(23).                       �E�v
000450     02  FILLER                 PIC  X(08).                       I.980515
000460     02  JNSR-90                PIC  9(01).                       ���͕���
000470     02  JNSR-91                PIC  9(01).                       �J�z�敪
000480     02  JNSR-92                PIC  9(02).                       ������
