000010 FD  JWTOK                                                        ĸ������
000020     BLOCK    1     RECORDS                                       ����
000030     LABEL    RECORD   STANDARD                                   ܰ�
000040*****VALUE    OF  IDENTIFICATION   JT-OWS256ID.                   D.960207
000050     VALUE    OF  IDENTIFICATION   WK0256ID.                      I.960207
000060*
000070 01  JWTOK-R.
000080     02   JWTOK-01.                                               ����� CD
000090          03   JWTOK-011        PIC 9(4).                         ĸ�����
000100          03   JWTOK-012        PIC 9(3).                         ����� NO
000110     02   JWTOK-02              PIC 9(1).                         ��ݸ
000120     02   JWTOK-03.                                               ����
000130          03  JWTOK-031         PIC 9(6).                         ���� NO
000140          03  JWTOK-032         PIC 9(1).                         �ޮ� NO
000150     02   JWTOK-04              PIC 9(1).                         ��޶�
000160     02   JWTOK-05.                                               �ޭ���
000170          03  JWTOK-051         PIC 9(6).                         �ޭ���NO
000180          03  JWTOK-052         PIC 9(1).                         �ޮ�
000190     02   JWTOK-06              PIC 9(6).                         �ݺ���
000200     02   JWTOK-07              PIC 9(1).                         ���޸���
000210     02   JWTOK-08.                                               �������
000220          03  JWTOK-081   OCCURS  10.                             �������
000230              04  JWTOK-0811     PIC S9(4).
000240          03  JWTOK-082         PIC S9(6).                        ��
000250     02   JWTOK-09.                                               ������
000260          03  JWTOK-091         PIC 9(2).                         ��
000270          03  JWTOK-092         PIC 9(2).                         ·
000280          03  JWTOK-093         PIC 9(2).                         �
000290     02   JWTOK-10              PIC N(9).                         ʲ��
000300     02   JWTOK-11              PIC N(23).                        ÷ֳ
000310     02   JWTOK-12              PIC 9(01).                        �q����
000320     02   JWTOK-13              PIC 9(01).                        �^������
000330     02   JWTOK-14              PIC 9(01).                        ��ʋ���
000340     02   JWTOK-15              PIC 9(03).                        ��Đ�
000350     02   JWTOK-16              PIC S9(03).                       ��
000360     02   JWTOK-17              PIC 9(06).                        �����
000370     02   JWTOK-20              PIC X(10).                        I.981016
000380     02   FILLER                PIC X(84).                        I.030717
000390     02   JWTOK-JS              PIC 9(01).                        I.030717
000400*****02   FILLER                PIC X(85).                        D.030717
000410*****02   FILLER                PIC X(95).                        D.981016
