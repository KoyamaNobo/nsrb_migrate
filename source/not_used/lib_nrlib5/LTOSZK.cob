000010***********************************************
000020*****                                     *****
000030**   �@�@  �n�^�k�ڑ��@�t�@�C���@�@�@�@�@�@�@**
000040*****         ( JT-OSZK )  256/1          *****
000050***********************************************
000060 FD  JT-OSZK
000070     BLOCK 1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "JT-OSZK".
000100*
000110*----�R���g���[���e�p      �i�敪���O�P�j
000120 01  OSZK1-REC.
000130     02  OSZK1-01         PIC  9(02).                             �q�b�敪
000140     02  OSZK1-KEYW.
000150         03  OSZK1-02     PIC  9(01).                             ID
000160         03  OSZK1-03     PIC  9(01).                             �^&�qCD
000170     02  OSZK1-04         PIC  N(06).                             �^&�q��
000180     02  OSZK1-05         PIC  X(18).                             FILLER
000190     02  F                 PIC  X(221).
000200     02  OSZK1-99         PIC  9(01).                             ���M��
000210*
000220*----������}�X�^�p        �i�敪���O�Q�j
000230 01  OSZK2-REC.
000240     02  OSZK2-01         PIC  9(02).                             �q�b�敪
000250     02  OSZK2-KEYW.
000260         03  OSZK2-02     PIC  9(04).                             ���Ӑ�CD
000270         03  OSZK2-03     PIC  9(03).                             ������CD
000280     02  OSZK2-04         PIC  N(24).                             �����於
000290     02  OSZK2-05         PIC  N(24).                             �Z��(��)
000300     02  OSZK2-06         PIC  N(12).                             �Z��(��)
000310*****02  OSZK2-07         PIC  X(06).                             D.970130
000320*****02  OSZK2-08         PIC  X(12).                             D.970130
000330     02  OSZK2-07         PIC  X(08).                             I.970130
000340     02  OSZK2-08         PIC  X(14).                             I.970130
000350     02  OSZK2-09         PIC  9(02).                             �{������
000360     02  OSZK2-10         PIC  9(01).                             �^������
000370*****02  OSZK2-11         PIC  X(22).                             D.970130
000380     02  OSZK2-11         PIC  X(18).                             I.970130
000390     02  OSZK2-12         PIC  9(01).                             ACT
000400     02  F                 PIC  X(82).
000410     02  OSZK2-99         PIC  9(01).                             ���M��
000420*
000430*----�o�וi���t�@�C��      �i�敪���O�R�j
000440 01  OSZK3-REC.
000450     02  OSZK3-01         PIC  9(02).                             �q�b�敪
000460     02  OSZK3-KEYW.
000470         03  OSZK3-02     PIC  9(06).                               �ݺ��
000480     02  OSZK3-03         PIC  N(24).                               ��Ҳ
000490     02  OSZK3-04         PIC  9(1).                                �����
000500     02  OSZK3-05.                                                  �ֳ��
000510         03  OSZK3-051.                                             ����
000520             04  OSZK3-0511  OCCURS  10  PIC  9(01).
000530         03  OSZK3-052.                                             ����
000540             04  OSZK3-0521  OCCURS  10  PIC  9(01).
000550         03  OSZK3-053.                                             ����
000560             04  OSZK3-0531  OCCURS  10  PIC  9(01).
000570         03  OSZK3-054.                                             ����
000580             04  OSZK3-0541  OCCURS  10  PIC  9(01).
000590     02  OSZK3-06         PIC  9(02).                               ���ٲ
000600     02  OSZK3-07         PIC  N(15).                               �װҲ
000610     02  OSZK3-08         PIC  9(03).                               ���@ 
000620     02  OSZK3-09.                                                  �ꑫ 
000630         03  OSZK3-091.                                             ����
000640             04  OSZK3-0911  OCCURS  10  PIC  9(01).
000650         03  OSZK3-092.                                             ����
000660             04  OSZK3-0921  OCCURS  10  PIC  9(01).
000670         03  OSZK3-093.                                             ����
000680             04  OSZK3-0931  OCCURS  10  PIC  9(01).
000690         03  OSZK3-094.                                             ����
000700             04  OSZK3-0941  OCCURS  10  PIC  9(01).
000710     02 OSZK3-10          PIC  9(01).                               ACT
000720     02 F                  PIC  X(82).
000730     02 OSZK3-99          PIC  9(01).                               ���M��
000740*
000750*----�o�׎w���g����        �i�敪���P�P�j
000760 01  OSZK11-REC.
000770     02  OSZK11-01        PIC 9(02).                              �q�b�敪
000780     02  OSZK11-KEYW.
000790         03  OSZK11-02    PIC 9(06).                                �����
000800         03  OSZK11-03    PIC 9(01).                                �ޮ�
000810     02  OSZK11-04        PIC 9(01).                                ��ݸ
000820     02  OSZK11-05.                                                 �����
000830*****    03  OSZK11-051   PIC 9(02).                              D.980515
000840         03  OSZK11-051   PIC 9(04).                              I.980515
000850         03  OSZK11-052   PIC 9(02).                                ·
000860         03  OSZK11-053   PIC 9(02).                                �
000870     02  OSZK11-06.                                                 �����
000880*****    03  OSZK11-061   PIC 9(02).                              D.980515
000890         03  OSZK11-061   PIC 9(04).                              I.980515
000900         03  OSZK11-062   PIC 9(02).                                ·
000910         03  OSZK11-063   PIC 9(02).                                �
000920     02  OSZK11-07.                                                 �����
000930         03  OSZK11-071   PIC 9(04).                                ĸ���
000940         03  OSZK11-072   PIC 9(03).                                ��� N
000950     02  OSZK11-08        PIC 9(01).                                �� ��
000960     02  OSZK11-09.                                                 �ޭ��
000970         03  OSZK11-091   PIC 9(06).                                �ޭ��
000980         03  OSZK11-092   PIC 9(01).                                �ޮ�
000990     02  OSZK11-10        PIC 9(06).                                �ݺ��
001000     02  OSZK11-11        PIC 9(01).                                ���޸
001010     02  OSZK11-12.                                                 �����
001020         03  OSZK11-121   OCCURS  10.                               �����
001030             04  OSZK11-1211      PIC S9(04).
001040         03  OSZK11-122   PIC S9(06).                               ��
001050     02  OSZK11-13.                                                 �����
001060         03  OSZK11-131   OCCURS  10.                               �����
001070             04  OSZK11-1311      PIC S9(04).
001080         03  OSZK11-132   PIC S9(06).                               ��
001090     02  OSZK11-14        PIC 9(01).                                ��޶�
001100     02  OSZK11-15        PIC 9(01).                                �^�� 
001110     02  OSZK11-15A       PIC 9(03).                                �Z�b 
001120     02  OSZK11-15B       PIC 9(06).                                ���� 
001130     02  OSZK11-15C       PIC 9(02).                                �}��
001140     02  FILLER            PIC X(2).
001150     02  OSZK11-99        PIC 9(01).                                ���� 
001160     02  OSZK11-15D       PIC N(09).                                �z�B
001170     02  OSZK11-16        PIC N(23) .                               �E�v
001180     02  OSZK11-16A       PIC S9(03).                               ��
001190     02  OSZK11-18A.                                              �a����
001200         03  OSZK11-181   PIC 9(06).                              �@��
001210         03  OSZK11-182   PIC 9(01).                              �@�s��
001220*****02  FILLER           PIC X(26).                              D.980515
001230     02  FILLER           PIC X(22).                              I.980515
001240     02  OSZK11-19        PIC X(01).                              ��������
001250     02  OSZK11-168       PIC 9(01).                              �󎚻��
001260     02  OSZK11-17        PIC 9(01).                              ��ʋ���
001270     02  OSZK11-18        PIC 9(01).                              �X�V���
001280*
001290*----�׎D�g����            �i�敪���P�Q�j
001300 01  OSZK12-REC.
001310     02  OSZK12-01        PIC 9(02).                              �q�b�敪
001320*
001330     02  OSZK121-A.                                                �sNOT=7
001340         03  OSZK121-1KEYW.
001350             04  OSZK121-01  PIC 9(6).                              �����
001360             04  OSZK121-02  PIC 9(1).                              �ޮ�
001370         03  OSZK121-03   PIC 9(6).                                 �ݺ��
001380         03  OSZK121-04.                                            ʯ���
001390             04  OSZK121-041 PIC 9(2).                              ��
001400             04  OSZK121-042 PIC 9(2).                              ·
001410             04  OSZK121-043 PIC 9(2).                              �
001420         03  OSZK121-05.                                            �����
001430             04  OSZK121-051 PIC 9(4).                              ĸ���
001440             04  OSZK121-052 PIC 9(3).                              ��� N
001450         03  OSZK121-06   PIC 9(1).                                 �ݿ�
001460         03  OSZK121-07   PIC 9(1).                                 �����
001470         03  OSZK121-08   PIC S9(3).                                ���
001480         03  OSZK121-09  OCCURS  27.                                �����
001490             04  OSZK121-091 PIC S9(3).                             �����
001500         03  OSZK121-10   PIC 9(1).                                 �ݼ޻
001510         03  OSZK121-11   PIC 9(1).                                 ƭ�خ
001520         03  OSZK121-12   PIC 9(1).                                 �����
001530         03  OSZK121-13   PIC S9(3).                                ϲ��
001540         03  OSZK121-13A  PIC 9(01).                              ��ʋ���
001550         03  FILLER        PIC X(2).
001560         03  OSZK121-14   PIC 9(6).                                 ��ؼ�
001570*
001580     02  OSZK122-A        REDEFINES  OSZK121-A.                     �s���V
001590         03  OSZK122-1KEYW.
001600             04  OSZK122-01  PIC 9(6).                              �����
001610             04  OSZK122-02  PIC 9(1).                              �ޮ�
001620         03  OSZK122-02A  PIC N(9).                                 ʲ��
001630         03  OSZK122-03   PIC N(23).                                ÷ֳ
001640         03  FILLER        PIC X(41).
001650         03  OSZK122-04   PIC 9(1).                                 �ݼ޻
001660         03  OSZK122-05   PIC 9(1).                                 Ʈ�خ
001670         03  OSZK122-06   PIC 9(1).                                 �����
001680         03  OSZK122-07   PIC S9(3).                                ϲ��
001690         03  OSZK122-07A  PIC 9(1).                               ��ʋ���
001700         03  FILLER        PIC X(2).
001710         03  OSZK122-08   PIC 9(6).                                 ��ؼ�
001720*
001730     02  OSZK121-B.                                                �sNOT=7
001740         03  OSZK121-2KEYW.
001750             04  OSZK121-21  PIC 9(6).                              �����
001760             04  OSZK121-22  PIC 9(1).                              �ޮ�
001770         03  OSZK121-23   PIC 9(6).                                 �ݺ��
001780         03  OSZK121-24.                                            ʯ���
001790             04  OSZK121-241 PIC 9(2).                              ��
001800             04  OSZK121-242 PIC 9(2).                              ·
001810             04  OSZK121-243 PIC 9(2).                              �
001820         03  OSZK121-25.                                            �����
001830             04  OSZK121-251 PIC 9(4).                              ĸ���
001840             04  OSZK121-252 PIC 9(3).                              ��� N
001850         03  OSZK121-26   PIC 9(1).                                 �ݿ�
001860         03  OSZK121-27   PIC 9(1).                                 �����
001870         03  OSZK121-28   PIC S9(3).                                ���
001880         03  OSZK121-29  OCCURS  27.                                �����
001890             04  OSZK121-291 PIC S9(3).                             �����
001900         03  OSZK121-30   PIC 9(1).                                 �ݼ޻
001910         03  OSZK121-31   PIC 9(1).                                 ƭ�خ
001920         03  OSZK121-32   PIC 9(1).                                 �����
001930         03  OSZK121-33   PIC S9(3).                                ϲ��
001940         03  OSZK121-33A  PIC 9(1).                               ��ʋ���
001950         03  FILLER        PIC X(2).
001960         03  OSZK121-34   PIC 9(6).                                 ��ؼ�
001970*
001980     02  OSZK122-B        REDEFINES  OSZK121-B.                     �s���V
001990         03  OSZK122-2KEYW.
002000             04  OSZK122-21  PIC 9(6).                              �����
002010             04  OSZK122-22  PIC 9(1).                              �ޮ�
002020         03  OSZK122-22A  PIC N(9).                                 ʲ��
002030         03  OSZK122-23   PIC N(23).                                ÷ֳ
002040         03  FILLER        PIC X(41).
002050         03  OSZK122-24   PIC 9(1).                                 �ݼ޻
002060         03  OSZK122-25   PIC 9(1).                                 Ʈ�خ
002070         03  OSZK122-26   PIC 9(1).                                 �����
002080         03  OSZK122-27   PIC S9(3).                                ϲ��
002090         03  OSZK122-27A  PIC 9(1).                               ��ʋ���
002100         03  FILLER        PIC X(2).
002110         03  OSZK122-28   PIC 9(6).                                 ��ؼ�
002120**** 02  F                 PIC X(54).
002130*
002140*----�����t�@�C��        �i�敪���P�R�j
002150 01  OSZK13-REC.
002160     02  OSZK13-01        PIC 9(02).                              �q�b�敪
002170     02  OSZK13-TBL       OCCURS  4.
002180         03  OSZK13-KEY   .
002190           04  OSZK13-02  PIC 9(06).                                ���� 
002200         03  OSZK13-03    PIC 9(01).                                �^�� 
002210         03  OSZK13-04    PIC 9(06).                                �N�� 
002220         03  OSZK13-05    PIC 9(01).                                �q�� 
002230         03  OSZK13-06    PIC 9(07).                                ���� 
002240         03  OSZK13-07    PIC N(09).                                �z�B 
002250         03  OSZK13-08    PIC 9(03).                                ��
002260         03  OSZK13-09    PIC 9(01).                                ��
002270         03  OSZK13-10    PIC 9(01).                                �敪
002280         03  OSZK13-11    PIC 9(01).                                �X�V
002290         03  F             PIC X(06).
002300     02  F                 PIC X(50).
