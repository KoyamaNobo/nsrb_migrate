000010***********************************************
000020*****                                     *****
000030**   �@�@�n�^�k���M�@�t�@�C���@          �@**
000040*****         ( JOLSM   )  256/1          *****
000050***********************************************
000060 FD  JOLSM
000070     BLOCK 1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "JOLSM".
000100*
000110*----�R���g���[���e�p      �i�敪���O�P�j
000120 01  JOLSM1-REC.
000130     02  JOLSM1-01         PIC  9(02).                            �q�b�敪
000140     02  JOLSM1-KEYW.
000150         03  JOLSM1-02     PIC  9(01).                            ID
000160         03  JOLSM1-03     PIC  9(01).                            �^&�qCD
000170     02  JOLSM1-04         PIC  N(06).                            �^&�q��
000180     02  JOLSM1-05         PIC  X(18).                            FILLER
000190     02  F                 PIC  X(221).
000200     02  JOLSM1-99         PIC  9(01).                            ���M��
000210*
000220*----������}�X�^�p        �i�敪���O�Q�j
000230 01  JOLSM2-REC.
000240     02  JOLSM2-01         PIC  9(02).                            �q�b�敪
000250     02  JOLSM2-KEYW.
000260         03  JOLSM2-02     PIC  9(04).                            ���Ӑ�CD
000270         03  JOLSM2-03     PIC  9(03).                            ������CD
000280     02  JOLSM2-04         PIC  N(26).
000290     02  JOLSM2-05         PIC  N(20).
000300     02  JOLSM2-06         PIC  N(20).
000310     02  JOLSM2-07         PIC  X(08).
000320     02  JOLSM2-08         PIC  X(14).
000330     02  JOLSM2-09         PIC  9(02).                            �{������
000340     02  JOLSM2-10         PIC  9(01).                            �^������
000350     02  JOLSM2-11         PIC  X(28).
000360     02  JOLSM2-12         PIC  9(01).                            ACT
000370     02  F                 PIC  X(60).
000380     02  JOLSM2-99         PIC  9(01).                            ���M��
000390*
000400*----�o�וi���t�@�C��      �i�敪���O�R�j
000410 01  JOLSM3-REC.
000420     02  JOLSM3-01         PIC  9(02).                            �q�b�敪
000430     02  JOLSM3-KEYW.
000440       03  JOLSM3-MHCD     PIC  9(006).
000450       03  JOLSM3-HCD      PIC  9(006).
000460     02  JOLSM3-NAME       PIC  N(024).
000470     02  JOLSM3-BC.
000480       03  JOLSM3-BC1      PIC  9(002).
000490       03  JOLSM3-BC2      PIC  9(002).
000500       03  JOLSM3-BC3      PIC  9(002).
000510     02  JOLSM3-ASSD.
000520       03  JOLSM3-SSD   OCCURS  4.
000530         04  JOLSM3-SS     PIC  9(010).
000540     02  JOLSM3-ASKD  REDEFINES JOLSM3-ASSD.
000550       03  JOLSM3-SKD   OCCURS  4.
000560         04  JOLSM3-SK    OCCURS 10.
000570           05  JOLSM3-S    PIC  9(001).
000580     02  JOLSM3-AHSD  REDEFINES JOLSM3-ASSD.
000590       03  JOLSM3-HSD.
000600         04  JOLSM3-SS1    PIC  9(010).
000610         04  JOLSM3-SD1   REDEFINES JOLSM3-SS1.
000620           05  JOLSM3-S1    OCCURS  10  PIC  9(001).
000630         04  JOLSM3-SS2    PIC  9(010).
000640         04  JOLSM3-SD2    REDEFINES JOLSM3-SS2.
000650           05  JOLSM3-S2    OCCURS  10  PIC  9(001).
000660         04  JOLSM3-SS3    PIC  9(010).
000670         04  JOLSM3-SD3    REDEFINES JOLSM3-SS3.
000680           05  JOLSM3-S3    OCCURS  10  PIC  9(001).
000690         04  JOLSM3-SS4    PIC  9(010).
000700         04  JOLSM3-SD4    REDEFINES JOLSM3-SS4.
000710           05  JOLSM3-S4    OCCURS  10  PIC  9(001).
000720     02  JOLSM3-SB         PIC  9(005).
000730     02  JOLSM3-FT         PIC  9(005).
000740     02  JOLSM3-ZRG        PIC  9(005).
000750     02  JOLSM3-SKG        PIC  9(005).
000760     02  JOLSM3-GKG        PIC  9(005).
000770     02  JOLSM3-KNG        PIC  9(004).
000780     02  JOLSM3-KT         PIC  9(005).
000790     02  JOLSM3-TCD        PIC  9(004).
000800     02  JOLSM3-ISU        PIC  9(003).
000810     02 JOLSM3-10          PIC  9(01).                            ACT
000820     02 F                  PIC  X(105).
000830     02 JOLSM3-99          PIC  9(01).                            ���M��
000840*
000850*----�o�׎w���g����        �i�敪���P�P�j
000860 01  JOLSM11-REC.
000870     02  JOLSM11-01        PIC 9(02).                             �q�b�敪
000880     02  JOLSM11-KEYW.
000890         03  JOLSM11-02    PIC 9(06).                                �����
000900         03  JOLSM11-03    PIC 9(01).                                �ޮ�
000910     02  JOLSM11-04        PIC 9(01).                                ��ݸ
000920     02  JOLSM11-05.                                                 �����
000930         03  JOLSM11-051   PIC 9(04).
000940         03  JOLSM11-052   PIC 9(02).                                ·
000950         03  JOLSM11-053   PIC 9(02).                                �
000960     02  JOLSM11-06.                                                 �����
000970         03  JOLSM11-061   PIC 9(04).
000980         03  JOLSM11-062   PIC 9(02).                                ·
000990         03  JOLSM11-063   PIC 9(02).                                �
001000     02  JOLSM11-07.                                                 �����
001010         03  JOLSM11-071   PIC 9(04).                                ĸ���
001020         03  JOLSM11-072   PIC 9(03).                                ��� N
001030     02  JOLSM11-08        PIC 9(01).                                �� ��
001040     02  JOLSM11-09.                                                 �ޭ��
001050         03  JOLSM11-091   PIC 9(06).                                �ޭ��
001060         03  JOLSM11-092   PIC 9(01).                                �ޮ�
001070     02  JOLSM11-10        PIC 9(06).                                �ݺ��
001080     02  JOLSM11-11        PIC 9(01).                                ���޸
001090     02  JOLSM11-12.                                                 �����
001100         03  JOLSM11-121   OCCURS  10.                               �����
001110             04  JOLSM11-1211      PIC S9(04).
001120         03  JOLSM11-122   PIC S9(05).
001130     02  JOLSM11-13.                                                 �����
001140         03  JOLSM11-131   OCCURS  10.                               �����
001150             04  JOLSM11-1311      PIC S9(04).
001160         03  JOLSM11-132   PIC S9(05).
001170     02  JOLSM11-14        PIC 9(01).                                ��޶�
001180     02  JOLSM11-15        PIC 9(01).                                �^�� 
001190     02  JOLSM11-15A       PIC 9(03).                                �Z�b 
001200     02  JOLSM11-15B       PIC 9(06).                                ���� 
001210     02  JOLSM11-15C       PIC 9(02).                                �}��
001220     02  JOLSM11-15D       PIC N(09).                                �z�B
001230     02  JOLSM11-16        PIC N(23).                                �E�v
001240     02  JOLSM11-20        PIC X(10).
001250     02  JOLSM11-16A       PIC S9(03).                            ��
001260     02  FILLER            PIC X(24).
001270     02  JOLSM11-19        PIC X(01).                             ��������
001280     02  JOLSM11-168       PIC 9(01).                             �󎚻��
001290     02  JOLSM11-17        PIC 9(01).                             ��ʋ���
001300     02  JOLSM11-18        PIC 9(01).                             �X�V���
001310*
001320*----�׎D�g����            �i�敪���P�Q�j
001330 01  JOLSM12-REC.
001340     02  JOLSM12-01        PIC 9(02).                             �q�b�敪
001350*
001360     02  JOLSM121-A.                                              �sNOT=7
001370         03  JOLSM121-1KEYW.
001380             04  JOLSM121-01  PIC 9(6).                              �����
001390             04  JOLSM121-02  PIC 9(1).                              �ޮ�
001400         03  JOLSM121-03   PIC 9(6).                                 �ݺ��
001410         03  JOLSM121-04.                                            ʯ���
001420             04  JOLSM121-041 PIC 9(2).                              ��
001430             04  JOLSM121-042 PIC 9(2).                              ·
001440             04  JOLSM121-043 PIC 9(2).                              �
001450         03  JOLSM121-05.                                            �����
001460             04  JOLSM121-051 PIC 9(4).                              ĸ���
001470             04  JOLSM121-052 PIC 9(3).                              ��� N
001480         03  JOLSM121-06   PIC 9(1).                                 �ݿ�
001490         03  JOLSM121-07   PIC 9(1).                                 �����
001500         03  JOLSM121-08   PIC S9(3).                                ���
001510         03  JOLSM121-09  OCCURS  27.                                �����
001520             04  JOLSM121-091 PIC S9(3).                             �����
001530         03  JOLSM121-10   PIC 9(1).                                 �ݼ޻
001540         03  JOLSM121-11   PIC 9(1).                                 ƭ�خ
001550         03  JOLSM121-12   PIC 9(1).                                 �����
001560         03  JOLSM121-13   PIC S9(3).                                ϲ��
001570         03  JOLSM121-13A  PIC 9(01).                             ��ʋ���
001580         03  FILLER        PIC X(2).
001590         03  JOLSM121-14   PIC 9(6).                                 ��ؼ�
001600*
001610     02  JOLSM122-A        REDEFINES  JOLSM121-A.                 �s���V
001620         03  JOLSM122-1KEYW.
001630             04  JOLSM122-01  PIC 9(6).                              �����
001640             04  JOLSM122-02  PIC 9(1).                              �ޮ�
001650         03  JOLSM122-02A  PIC N(9).                                 ʲ��
001660         03  JOLSM122-03   PIC N(23).                                ÷ֳ
001670         03  FILLER        PIC X(41).
001680         03  JOLSM122-04   PIC 9(1).                                 �ݼ޻
001690         03  JOLSM122-05   PIC 9(1).                                 Ʈ�خ
001700         03  JOLSM122-06   PIC 9(1).                                 �����
001710         03  JOLSM122-07   PIC S9(3).                                ϲ��
001720         03  JOLSM122-07A  PIC 9(1).                              ��ʋ���
001730         03  FILLER        PIC X(2).
001740         03  JOLSM122-08   PIC 9(6).                                 ��ؼ�
001750*
001760     02  JOLSM121-B.                                              �sNOT=7
001770         03  JOLSM121-2KEYW.
001780             04  JOLSM121-21  PIC 9(6).                              �����
001790             04  JOLSM121-22  PIC 9(1).                              �ޮ�
001800         03  JOLSM121-23   PIC 9(6).                                 �ݺ��
001810         03  JOLSM121-24.                                            ʯ���
001820             04  JOLSM121-241 PIC 9(2).                              ��
001830             04  JOLSM121-242 PIC 9(2).                              ·
001840             04  JOLSM121-243 PIC 9(2).                              �
001850         03  JOLSM121-25.                                            �����
001860             04  JOLSM121-251 PIC 9(4).                              ĸ���
001870             04  JOLSM121-252 PIC 9(3).                              ��� N
001880         03  JOLSM121-26   PIC 9(1).                                 �ݿ�
001890         03  JOLSM121-27   PIC 9(1).                                 �����
001900         03  JOLSM121-28   PIC S9(3).                                ���
001910         03  JOLSM121-29  OCCURS  27.                                �����
001920             04  JOLSM121-291 PIC S9(3).                             �����
001930         03  JOLSM121-30   PIC 9(1).                                 �ݼ޻
001940         03  JOLSM121-31   PIC 9(1).                                 ƭ�خ
001950         03  JOLSM121-32   PIC 9(1).                                 �����
001960         03  JOLSM121-33   PIC S9(3).                                ϲ��
001970         03  JOLSM121-33A  PIC 9(1).                              ��ʋ���
001980         03  FILLER        PIC X(2).
001990         03  JOLSM121-34   PIC 9(6).                                 ��ؼ�
002000*
002010     02  JOLSM122-B        REDEFINES  JOLSM121-B.                 �s���V
002020         03  JOLSM122-2KEYW.
002030             04  JOLSM122-21  PIC 9(6).                              �����
002040             04  JOLSM122-22  PIC 9(1).                              �ޮ�
002050         03  JOLSM122-22A  PIC N(9).                                 ʲ��
002060         03  JOLSM122-23   PIC N(23).                                ÷ֳ
002070         03  FILLER        PIC X(41).
002080         03  JOLSM122-24   PIC 9(1).                                 �ݼ޻
002090         03  JOLSM122-25   PIC 9(1).                                 Ʈ�خ
002100         03  JOLSM122-26   PIC 9(1).                                 �����
002110         03  JOLSM122-27   PIC S9(3).                                ϲ��
002120         03  JOLSM122-27A  PIC 9(1).                              ��ʋ���
002130         03  FILLER        PIC X(2).
002140         03  JOLSM122-28   PIC 9(6).                                 ��ؼ�
002150*
002160*----�����t�@�C��        �i�敪���P�R�j
002170 01  JOLSM13-REC.
002180     02  JOLSM13-01        PIC 9(02).                             �q�b�敪
002190     02  JOLSM13-TBL       OCCURS  4.
002200         03  JOLSM13-KEY   .
002210           04  JOLSM13-02  PIC 9(06).                                ���� 
002220         03  JOLSM13-03    PIC 9(01).                                �^�� 
002230         03  JOLSM13-04    PIC 9(06).                                �N�� 
002240         03  JOLSM13-05    PIC 9(01).                                �q�� 
002250         03  JOLSM13-06    PIC 9(07).                                ���� 
002260         03  JOLSM13-07    PIC N(09).                                �z�B 
002270         03  JOLSM13-08    PIC 9(03).                                ��
002280         03  JOLSM13-09    PIC 9(01).                                ��
002290         03  JOLSM13-10    PIC 9(01).                                �敪
002300         03  JOLSM13-11    PIC 9(01).                                �X�V
002310         03  JOLSM13-12    PIC 9(05).
002320         03  F             PIC X(01).
002330     02  F                 PIC X(50).
