000010 FD  JNIF                                                         �������
000020     BLOCK    2     RECORDS
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  "JNIF".
000050*
000060 01  JNIF1-R.
000070     02   JNIF1-KEY.                                              KEY
000080          03    JNIF1-01   PIC 9(6).                              ����߮�N
000090          03    JNIF1-02   PIC 9(1).                              �ޮ�
000100     02   JNIF1-03         PIC 9(6).                              �ݺ���
000110     02   JNIF1-04.                                               ʯ����
000120          03  JNIF1-041     PIC 9(2).                             ��
000130          03  JNIF1-042     PIC 9(2).                             ·
000140          03  JNIF1-043     PIC 9(2).                             �
000150     02   JNIF1-05.                                               �������
000160          03  JNIF1-051     PIC 9(4).                             ĸ�����
000170          03  JNIF1-052     PIC 9(3).                             ��� NO
000180     02   JNIF1-06          PIC 9(1).                             �ݿ�
000190     02   JNIF1-07          PIC 9(1).                             ������
000200     02   JNIF1-08          PIC S9(3).                            ���
000210     02   JNIF1-09    OCCURS  27.                                 �����
000220          03  JNIF1-091     PIC S9(3).                            �������
000230     02   JNIF1-10          PIC 9(1).                             �ݼ޻��
000240     02   JNIF1-11          PIC 9(1).                             ƭ�خ�
000250     02   JNIF1-12          PIC 9(1).                             �������
000260     02   JNIF1-13          PIC S9(3).                            ϲ��
000270     02   JNIF1-13A         PIC 9(1).                             ��ʋ���
000280*****02   FILLER            PIC X(2).                             D.960819
000290     02   JNIF1-15          PIC 9(2).                             I.960819
000300     02   JNIF1-14          PIC 9(6).                             ��ؼޮ�
000310     02   FILLER            PIC X(1).
000320 01  JNIF2-R.
000330     02   JNIF2-KEY.                                              KEY
000340          03  JNIF2-01      PIC 9(6).                             ����߮�
000350          03  JNIF2-02      PIC 9(1).                             �ޮ�
000360     02   JNIF2-02A         PIC N(9).                             ʲ��
000370     02   JNIF2-03          PIC N(23).                            ÷ֳ
000380     02   FILLER            PIC X(41).
000390     02   JNIF2-04          PIC 9(1).                             �ݼ޻��
000400     02   JNIF2-05          PIC 9(1).                             Ʈ�خ�
000410     02   JNIF2-06          PIC 9(1).                             �������
000420     02   JNIF2-07          PIC S9(3).                            ϲ��
000430     02   JNIF2-07A         PIC 9(1).                             ��ʋ���
000440     02   FILLER            PIC X(2).
000450     02   JNIF2-08          PIC 9(6).                             ��ؼޮ�
000460     02   FILLER            PIC X(1).
