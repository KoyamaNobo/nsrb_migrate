000010 FD  JNSRD                                                        ƭ�����
000020     BLOCK    5     RECORDS
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  "JNSRD".
000050*
000060 01  JNSRD-R.
000070     02  JNSRD-KEY.                                               KEY
000080         03  JNSRD-01           PIC 9(06)  COMP-3.                �i������
000090         03  JNSRD-02           PIC 9(06)  COMP-3.                �N����
000100         03  JNSRD-03           PIC 9(02).                        ���o�͋�
000110         03  JNSRD-04           PIC 9(06)  COMP-3.                �`�[��
000120         03  JNSRD-06           PIC 9(01).                        �s
000130     02  JNSRD-07               PIC 9(01).                        �q����
000140     02  JNSRD-05               PIC 9(01).                        �T�C�Y
000150     02  JNSRD-08.                                                ���o�ɐ�
000160         03  JNSRD-081          PIC S9(04)  COMP-3    OCCURS  10.
000170     02  JNSRD-09               PIC 9(01).                        ���Y�敪
000180     02  JNSRD-10               PIC 9(04).                        ���Ӑ�CD
000190     02  JNSRD-11               PIC 9(03).                        ������D
000200     02  JNSRD-12               PIC N(05).                        �z�B
000210     02  JNSRD-13               PIC N(06).                        �E�v
000220     02  JNSRD-14               PIC 9(06)  COMP-3.                ����
000230     02  JNSRD-21               PIC 9(01).                        I.941121
000240     02  JNSRD-22               PIC 9(06).                        I.941121
000250     02  JNSRD-23               PIC 9(01).                        I.941121
000260*****02  FILLER                 PIC X(13).                        D.941121
000270*****02  FILLER                 PIC X(12).                        D.941122
000280*****02  FILLER                 PIC X(04).                        D.950412
000290*****02  FILLER                 PIC X(02).                        D.950508
000300     02  FILLER                 PIC X(01).                        I.950508
000310     02  JNSRD-89               PIC 9(01).                        I.950508
000320     02  JNSRD-90               PIC 9(02).                        I.950412
000330     02  JNSRD-91               PIC 9(01).                        I.941121
