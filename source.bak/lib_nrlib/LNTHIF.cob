000010*    ˿޹̧��           *
000020 FD  NT-HIF
000030     BLOCK      CONTAINS     1      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "NT-HIF".
000060 01  HIF-R.
000070     02  HIF-KEY.                                                 KEY
000080       03  HIF-01        PIC X(02).                               ں���KBN
000090     02  HIF-02.                                                  ����
000100       03  HIF-021       PIC 9(04).                               �N
000110       03  HIF-022       PIC 9(02).                               ��
000120     02  HIF-03.                                                  ��\�X�V
000130       03  HIF-031       PIC 9(04).                               �N
000140       03  HIF-032       PIC 9(02).                               ��
000150     02  HIF-04.                                                  �������
000160       03  HIF-041       OCCURS  2.
000170         04  HIF-0411    PIC 9(02).                               FROM�N
000180         04  HIF-0412    PIC 9(02).                               TO�N
000190         04  HIF-0413    PIC 9(04).                               ����
000200     02  HIF-05.                                                  �x������
000210       03  HIF-051       PIC 9(02).                               �j
000220       03  HIF-052       PIC 9(02).                               ��
000230     02  FILLER          PIC X(222).
