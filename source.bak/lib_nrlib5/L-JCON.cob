000010 FD  JCON                                                         ���۰�F
000020     BLOCK    8     RECORDS
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  "JCON".
000050*
000060 01  JCON1-R.
000070     02   JCON1-KEY.                                              KEY
000080          03    JCON1-01         PIC 9(1).                        ID
000090          03    JCON1-02         PIC 9(1).                        ��ݸ NO
000100     02   JCON1-03               PIC 9(6).                        �����
000110     02   JCON1-04               PIC 9(6).                        ܰ�
000120     02   JCON1-05               PIC 9(6).                        I.030702
000130     02   JCON1-06               PIC 9(6).                        I.110120
000140     02   FILLER                 PIC X(6).                        I.110120
000150*****02   FILLER                 PIC X(12).                       D.110120
000160*****02   FILLER                 PIC X(18).                       D.030702
000170*****02   JCON1-05               PIC 9(6).                        D.030701
000180*****02   JCON1-06               PIC 9(1).                        D.030701
000190*****02   JCON1-07               PIC 9(6).                        D.030701
000200*****02   JCON1-08               PIC 9(1).                        D.030701
000210*****02   FILLER                 PIC X(4).                        D.030701
000220 01  JCON2-R.
000230     02   JCON2-KEY.                                              KEY
000240          03    JCON2-01         PIC 9(1).                        ID
000250          03    JCON2-02         PIC 9(1).                        �ݿ�����
000260     02   JCON2-03               PIC N(6).                        �ݿ�Ҳ
000270     02   FILLER                 PIC X(18).
000280 01  JCON3-R.
000290     02   JCON3-KEY.                                              KEY
000300          03    JCON3-01         PIC 9(1).                        ID
000310          03    JCON3-02         PIC 9(1).                        �������
000320     02   JCON3-03               PIC N(6).                        ���Ҳ
000330     02   JCON3-04               PIC 9(1).                        ��ؿ��KB
000340     02   FILLER                 PIC X(17).
000350*                                                                 ADD:9004
000360 01  JCON4-R.
000370     02   JCON4-KEY.                                              KEY
000380          03    JCON4-01         PIC 9(1).                        ID "4"
000390          03    JCON4-02         PIC 9(1).                        ��ݿ���K
000400     02   JCON4-03               PIC N(6).                        ��ݿ���N
000410     02   JCON4-04               PIC X(13).                       TEL.NO
000420     02   FILLER                 PIC X(05).
000430*                                                                 ADD:9004
000440 01  JCON5-R.
000450     02   JCON5-KEY.                                              KEY
000460          03    JCON5-01         PIC 9(1).                        ID "5"
000470          03    JCON5-02         PIC 9(1).                        ����߮�K
000480     02   JCON5-03               PIC 9(6).                        I.100507
000490     02   FILLER                 PIC X(24).                       I.100507
000500*****02   JCON5-03               PIC 9(6).                        D.090918
000510*****02   JCON5-03.                                               D.100507
000520*****     03   JCON5-031         PIC 9(1).                        D.100507
000530*****     03   JCON5-032         PIC 9(1).                        D.100507
000540*****     03   JCON5-033         PIC 9(1).                        D.100507
000550*****     03   JCON5-034         PIC 9(1).                        D.100507
000560*****     03   JCON5-035         PIC 9(1).                        D.100507
000570*****     03   JCON5-036         PIC 9(1).                        D.100507
000580*****     03   JCON5-037         PIC 9(1).                        D.100507
000590*****     03   JCON5-038         PIC 9(1).                        D.100507
000600*****     03   JCON5-039         PIC 9(1).                        D.100507
000610*****02   FILLER                 PIC X(21).                       D.100507
000620*****02   FILLER                 PIC X(24).                       D.090918
000630*                                                                 I.941116
000640 01  JCON6-R.
000650     02   JCON6-KEY.                                              KEY
000660          03    JCON6-01         PIC 9(1).                        ID "6"
000670          03    JCON6-02         PIC X(1).                        ��
000680     02   JCON6-03               PIC 9(6).                        �����޹
000690     02   JCON6-03D  REDEFINES  JCON6-03.
000700          03   JCON6-031         PIC 9(4).                        ��
000710          03   JCON6-031D  REDEFINES  JCON6-031.
000720               04   JCON6-0311   PIC 9(2).
000730               04   JCON6-0312   PIC 9(2).
000740          03   JCON6-032         PIC 9(2).                        ·
000750*****02   FILLER                 PIC X(24).                       D.970924
000760*****02   FILLER                 PIC X(16).                       D.030723
000770*****02   FILLER                 PIC X(15).                       D.060119
000780     02   JCON6-04               PIC 9(1).                        I.060119
000790     02   JCON6-05.                                               I.100218
000800          03   JCON6-051         PIC 9(1).                        I.100218
000810          03   JCON6-052         PIC 9(1).                        I.100218
000820          03   JCON6-053         PIC 9(1).                        I.100218
000830          03   JCON6-054         PIC 9(1).                        I.100218
000840     02   JCON6-06               PIC 9(1).                        I.100218
000850*****02   JCON6-05               PIC 9(1).                        D.100218
000860*****02   FILLER                 PIC X(14).                       D.080514
000870*****02   FILLER                 PIC X(13).                       D.100218
000880     02   FILLER                 PIC X(09).                       I.100218
000890     02   JCON6-08               PIC 9(1).                        I.030723
000900     02   JCON6-09               PIC 9(8).                        Ư�߳���
000910     02   JCON6-09D  REDEFINES  JCON6-09.
000920          03   JCON6-091         PIC 9(4).                        ��
000930          03   JCON6-091D  REDEFINES  JCON6-091.
000940               04   JCON6-0911   PIC 9(2).
000950               04   JCON6-0912   PIC 9(2).
000960          03   JCON6-092         PIC 9(2).                        ·
000970          03   JCON6-093         PIC 9(2).                        �
000980 01  JCON7-R.
000990     02   JCON7-KEY.                                              KEY
001000          03    JCON7-01         PIC 9(1).                        ID
001010          03    JCON7-02         PIC 9(1).                        ��ݸ NO
001020     02   JCON7-05               PIC 9(6).                        I.030701
001030     02   JCON7-06               PIC 9(1).                        I.030701
001040     02   JCON7-07               PIC 9(6).                        I.030701
001050     02   JCON7-08               PIC 9(1).                        I.030701
001060     02   JCON7-09               PIC 9(6).                        I.030701
001070     02   JCON7-10               PIC 9(1).                        I.030701
001080     02   JCON7-11               PIC 9(6).                        *A040324
001090     02   JCON7-12               PIC 9(1).                        *A040324
001100     02   FILLER                 PIC X(02).                       *R040324
001110*                                                                 I.941116
001120 01  JCON8-R.                                                     I.100301
001130     02   JCON8-KEY.                                              KEY
001140          03    JCON8-01         PIC 9(1).                        ID "8"
001150          03    JCON8-02         PIC X(1).                        ��
001160     02   JCON8-04               PIC 9(1).
001170     02   JCON8-05.
001180          03   JCON8-051         PIC 9(1).
001190          03   JCON8-052         PIC 9(1).
001200          03   JCON8-053         PIC 9(1).
001210          03   JCON8-054         PIC 9(1).
001220     02   JCON8-06               PIC 9(1).
001230     02   FILLER                 PIC X(24).
