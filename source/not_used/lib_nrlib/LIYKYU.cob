000010***************************************
000020*****     �ݶ� ���� Ҳ�� ̧��     *****
000030*****      (YKYUYOF)   512/1      *****
000040***************************************
000050 FD  YKYUYOF
000060     BLOCK 1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "YKYUYOF".
000090 01  YKYUYO-R.
000100     02  YK-KEY.
000110       03  YK-SNG           PIC  9(006).
000120       03  YK-SNGD  REDEFINES YK-SNG.                             ������
000130         04  YK-SN          PIC  9(004).
000140         04  YK-SG          PIC  9(002).
000150       03  YK-SNGSD REDEFINES YK-SNG.
000160         04  F              PIC  9(002).
000170         04  YK-SNGS        PIC  9(004).
000180       03  YK-KSC           PIC  9(001).
000190       03  YK-SIC           PIC  9(004).
000200     02  YK-SCD             PIC  9(004).
000210     02  YK-SCDD  REDEFINES YK-SCD.
000220       03  YK-KCD.
000230         04  YK-BKC.
000240           05  YK-KCD1      PIC  9(001).
000250           05  YK-KCD2      PIC  9(001).
000260         04  YK-KCD3        PIC  9(001).
000270       03  YK-CCD           PIC  9(001).
000280     02  YK-SB              PIC  9(001).                          �����C
000290     02  YK-PNO             PIC  9(002).                          ��ˮ�NO
000300     02  YK-YSC             PIC  9(002).                          Ը���C
000310     02  YK-YSCD  REDEFINES YK-YSC.
000320       03  YK-YSC1          PIC  9(001).
000330       03  YK-YSC2          PIC  9(001).
000340     02  YK-SHC             PIC  9(002).                          ��ײC
000350     02  YK-SHCD  REDEFINES YK-SHC.
000360       03  YK-SHC1          PIC  9(001).
000370       03  YK-SHC2          PIC  9(001).
000380     02  YK-JSC             PIC  9(002).                          �ޭ���C
000390*    [  � � � �  � � � �  ]
000400*
000410     02  YK-SKN             PIC  9(002).                          �������
000420     02  YK-YSN             PIC  9(002).                          ճ�����
000430     02  YK-TK1             PIC  9(002).                          ĸ���1
000440     02  YK-TK2             PIC  9(002).                          ĸ���2
000450     02  YK-KSN             PIC  9(002).                          �������
000460     02  YK-SRN             PIC  9(001).                          �������
000470     02  YK-KKN             PIC  9(002).                          ������
000480     02  YK-DKN             PIC  9(001).                          �޲�����
000490*       ( ����������޲��� )
000500     02  YK-TSN             PIC  9(002).                          ��������
000510     02  YK-TSG             PIC  9(002)V9(02).                    ������H
000520*       ( �б� ���޳ )
000530     02  YK-KMN             PIC  9(002).                          �ж���
000540     02  YK-KMG             PIC  9(003)V9(02).                    �ж�H
000550*
000560     02  YK-ZGG             PIC  9(003)V9(02).                    ��ݷޮ�H
000570*       ( ������ ����� )
000580     02  YK-KJN             PIC  9(002).                          �������
000590     02  YK-KJ1             PIC  9(002)V9(02).                    �����H1
000600*
000610     02  YK-SYG             PIC  9(003)V9(02).                    ���H
000620*
000630     02  YK-KTN             PIC  9(002).                          ������
000640*
000650     02  YK-PAG             PIC  9(003)V9(02).                    �߰�H
000660*       ( ۳�޳ �޶� )
000670     02  YK-HSG             PIC  9(003)V9(02).                    ����ޮ�H
000680     02  YK-HSGD  REDEFINES YK-HSG.
000690       03  YK-HSG1          PIC  9(003).
000700       03  YK-HSG2          PIC  9(002).
000710     02  YK-HTG             PIC  9(003)V9(02).                    ò��H
000720     02  YK-HTGD  REDEFINES YK-HTG.
000730       03  YK-HTG1          PIC  9(003).
000740       03  YK-HTG2          PIC  9(002).
000750     02  YK-HKJ             PIC  9(003)V9(02).                    ��ޭ�H
000760     02  YK-HKJD  REDEFINES YK-HKJ.
000770       03  YK-HKJ1          PIC  9(003).
000780       03  YK-HKJ2          PIC  9(002).
000790*    [  � � � �  � � � �  ]
000800*       ( ������ )
000810     02  YK-KHK             PIC S9(006).                          ��ݷ��
000820     02  YK-KMK             PIC S9(006).                          ��ѷ��
000830     02  YK-JTT             PIC S9(005).                          �ޭ���
000840*
000850     02  YK-KZT             PIC S9(005).                          ��޸
000860     02  YK-YST             PIC S9(005).                          Ը���
000870     02  YK-SMT             PIC S9(005).                          ����
000880*
000890     02  YK-ZGT             PIC S9(006).                          ��ݷޮ�
000900     02  YK-KJT             PIC S9(006).                          �����
000910     02  YK-KTT             PIC S9(005).                          ����
000920     02  YK-SYT             PIC S9(005).                          ���
000930*
000940     02  YK-TKP             PIC S9(006).                          ³���߽
000950     02  YK-TKT             PIC S9(005).                          ³��
000960     02  YK-KST             PIC S9(004).                          �����
000970     02  YK-TST             PIC S9(006).                          �����
000980     02  YK-KTK             PIC S9(006).                          ��������
000990     02  YK-KKT             PIC S9(005).                          ����
001000     02  YK-STT1            PIC S9(005).                          ���ñ�1
001010     02  YK-STT2            PIC S9(005).                          ���ñ�2
001020     02  YK-STT3            PIC S9(006).
001030     02  YK-STT4            PIC S9(006).
001040*
001050     02  YK-YHS             PIC S9(007).                          I.021023
001060*****02  YK-YHS             PIC S9(006).                          D.021023
001070*
001080     02  YK-SSG             PIC S9(007).                          ������
001090*    [  � � �� �  � � � �  ]
001100     02  YK-KSH             PIC S9(004).                          ����� �
001110     02  YK-TPH             PIC S9(006).                          �߽ �
001120     02  YK-TKH             PIC S9(005).                          ³�� �
001130*    [  � � �� �  � � � �  ]
001140*       ( ����ι� )
001150     02  YK-KKH             PIC S9(005).                          �ݺ�ι�
001160     02  YK-KNH             PIC S9(005).                          �ݷ�ι�
001170     02  YK-KYH             PIC S9(004).                          �ֳι�
001180*
001190     02  YK-KTG             PIC S9(007).                          ��޲����
001200*       ( �޲�� )
001210     02  YK-STZ             PIC S9(006).                          ��ĸ�޲
001220     02  YK-JMZ             PIC S9(006).                          �ޭ��ݾ�
001230*       ( �޲�� )
001240     02  YK-ZK1             PIC S9(006).
001250     02  YK-ZK2             PIC S9(006).
001260     02  YK-ZKN             PIC S9(006).                          �޲���ݷ
001270*       ( ���޲ )
001280     02  YK-KBH1            PIC S9(005).                          �����
001290     02  YK-KBH2            PIC S9(005).                          ��
001300     02  YK-KBH3            PIC S9(004).
001310     02  YK-KBH4            PIC S9(006).
001320*
001330     02  YK-STK1            PIC S9(006).                          ���ޮ1
001340     02  YK-STK2            PIC S9(006).                          ���ޮ2
001350     02  YK-STK3            PIC S9(006).                          ���ޮ3
001360     02  YK-STK4            PIC S9(006).                          ���ޮ4
001370     02  YK-STK5            PIC S9(006).                          ���ޮ5
001380*       ( ۳�� �ݹ� )
001390     02  YK-RKH             PIC S9(004).                          ۳���
001400     02  YK-TTM             PIC S9(004).                          ĳ�з�
001410*       ( ۳�� �ݹ� )
001420     02  YK-ZRS             PIC S9(005).                          ���۳��
001430     02  YK-RKK             PIC S9(006).                          ۳���ݻ�
001440     02  YK-RMP             PIC S9(006).                          ϲ����
001450     02  YK-RST             PIC S9(006).                          ۳�ݿ��
001460*       ( ۳� ��ײ )
001470     02  YK-RSK1            PIC S9(005).                          �ݸ�
001480     02  YK-RSK2            PIC S9(005).                          I.021023
001490     02  YK-RSK3            PIC S9(006).                          I.021023
001500*****02  YK-RSK2            PIC S9(004).                          D.021023
001510*****02  YK-RSK3            PIC S9(005).                          D.021023
001520*****02  YK-RSK4            PIC S9(006).                          D.021023
001530*       ( ����� ι� )
001540     02  YK-DHR1            PIC S9(005).
001550     02  YK-DHR2            PIC S9(005).
001560     02  YK-DHR3            PIC S9(005).
001570     02  YK-DHR4            PIC S9(005).
001580     02  YK-DHR5            PIC S9(005).
001590     02  YK-DHR6            PIC S9(005).
001600*
001610     02  YK-KGG             PIC S9(006).                          ���ޮ��
001620*       ( ��ײ �ݶ޸ )
001630     02  YK-BTK             PIC S9(006).                          �ݷ�޸
001640     02  YK-FKG1            PIC S9(007).                          �غж޸1
001650     02  YK-FKG2            PIC S9(007).                          �غж޸2
001660*
001670     02  YK-SHG             PIC S9(007).                          ��˷�޸
001680*
001690     02  YK-TEN             PIC S9(006).                          ò��
001700*
001710     02  YK-TKB             PIC S9(006).                          �ö���ײ
001720*
001730     02  F                  PIC  X(022).                          I.021023
001740*****02  F                  PIC  X(019).                          D.021023
001750*
001760     02  YK-KJC             PIC  9(001).                          ������C
001770*
001780     02  YK-OK              PIC  9(004).                          ���KEY
001790*
001800*    [  ۳�޳ Ư�� ��Ӹ  ]
001810*       ( ����� Ư�� )
001820     02  YK-SND.
001830       03  YK-SN1           PIC  9(002).                          �·�
001840       03  YK-SN2           PIC  9(002).                          2-1
001850       03  YK-SN3           PIC  9(002).                          2-2
001860       03  YK-SN4           PIC  9(002).                          3-1A
001870       03  YK-SN5           PIC  9(002).                          3-2A
001880       03  YK-SN6           PIC  9(002).                          3-3A
001890       03  YK-SN7           PIC  9(002).                          3-1B
001900       03  YK-SN8           PIC  9(002).                          3-2B
001910       03  YK-SN9           PIC  9(002).                          3-3B
001920*       ( ��ޭ� Ư�� )
001930     02  YK-KND.
001940       03  YK-KN1           PIC  9(002).                          �·�
001950       03  YK-KN2           PIC  9(002).                          2-1
001960       03  YK-KN3           PIC  9(002).                          2-2
001970       03  YK-KN4           PIC  9(002).                          3-1A
001980       03  YK-KN5           PIC  9(002).                          3-2A
001990       03  YK-KN6           PIC  9(002).                          3-3A
002000       03  YK-KN7           PIC  9(002).                          3-1B
002010       03  YK-KN8           PIC  9(002).                          3-2B
002020       03  YK-KN9           PIC  9(002).                          3-3B
