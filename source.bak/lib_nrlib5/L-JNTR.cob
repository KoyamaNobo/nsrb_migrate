000010 FD  JNTR                                                         ƭ�����
000020     BLOCK    3     RECORDS                                       ���
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  "JNTR".
000050*
000060 01  JNTR-R.
000070     02   JNTR-01               PIC 9(1).                         �׺���
000080     02   JNTR-02               PIC 9(2).                         Ʈ���º
000090     02   JNTR-03.                                                �ݶޯ��
000100          03  JNTR-031          PIC 9(2).                         ��
000110          03  JNTR-032          PIC 9(2).                         ·
000120          03  JNTR-033          PIC 9(2).                         �
000130     02   JNTR-04               PIC 9(6).                         ����߮�
000140     02   JNTR-05               PIC 9(1).                         �ޮ�
000150     02   JNTR-06               PIC 9(6).                         �ݺ���
000160     02   JNTR-07               PIC 9(1).                         ���޸���
000170     02   JNTR-08.                                                Ʈ����
000180          03  JNTR-081     OCCURS  10.                            �������
000190              04  JNTR-0811     PIC S9(4).
000200     02   JNTR-09               PIC 9(1).                         ���ݸ���
000210     02   JNTR-10               PIC 9(4).                         �� �� CD
000220     02   FILLER                PIC X(17).
