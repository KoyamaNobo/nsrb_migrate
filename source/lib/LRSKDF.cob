      ****************************************
      *****     �������f�[�^�t�@�C��     *****
      *****       (  SKDF 192/4  )       *****
      ****************************************
       01  SKDF.
           02  SKDF_PNAME1    PIC  X(008) VALUE "SKDF-RDB".
           02  F              PIC  X(001).
           02  SKDF_LNAME     PIC  X(004) VALUE "SKDF".
           02  F              PIC  X(001).
           02  SKDF_KEY1      PIC  X(100) VALUE SPACE.
           02  SKDF_SORT      PIC  X(100) VALUE SPACE.
           02  SKDF_IDLST     PIC  X(100) VALUE SPACE.
           02  SKDF_RES       USAGE  POINTER.
       01  SKD-R.
           02  SKD-KEY.                                                 KEY
             03  SKD-TCD      PIC  9(004).                              ���Ӑ�C
             03  SKD-DATE     PIC  9(008).                              ���t
             03  SKD-NGP   REDEFINES SKD-DATE.
               04  SKD-NG.
                 05  SKD-NEN  PIC  9(004).
                 05  SKD-GET  PIC  9(002).
               04  SKD-PEY    PIC  9(002).
             03  SKD-NGPL  REDEFINES SKD-DATE.
               04  F          PIC  9(002).
               04  SKD-NGPS   PIC  9(006).
             03  SKD-NGPD  REDEFINES SKD-DATE.
               04  F          PIC  9(004).
               04  SKD-GP     PIC  9(004).
             03  SKD-DTC      PIC  9(001).                              �敪
             03  SKD-DNO      PIC  9(006).                              �`�[��
             03  SKD-GNO      PIC  9(001).                              �@�s��
           02  SKD-HCD        PIC  9(006).                              �i���b
           02  SKD-HCDD  REDEFINES SKD-HCD.
             03  SKD-KCD      PIC  X(005).
             03  F            PIC  X(001).
           02  SKD-SU         PIC S9(006)V9(02).                           ����
           02  SKD-T          PIC S9(006)V9(02).                           �P��
           02  SKD-KIN        PIC S9(009).                              ���z
           02  SKD-DC         PIC  9(001).                              �`��
           02  SKD-CSC        PIC  9(001).
           02  SKD-SKD        PIC  9(008).                              ������
           02  SKD-SKDD  REDEFINES SKD-SKD.
             03  SKD-SNG.
               04  SKD-SNEN   PIC  9(004).
               04  SKD-SGET   PIC  9(002).
             03  SKD-SPEY     PIC  9(002).
           02  SKD-TNC        PIC  9(002).                              �S���b
           02  SKD-BMC        PIC  9(001).                              ����C
           02  SKD-DCC        PIC  9(001).
           02  F              PIC  X(002).
           02  SKD-TCD2       PIC  9(004).
           02  SKD-CCD        PIC  9(003).                              ������
           02  SKD-BI         PIC  N(024).                              ���l
           02  SKD-HNO        PIC  9(006).
           02  F              PIC  X(030).
           02  SKD-SHZ        PIC S9(007).                              �����
           02  SKD-KSU        PIC  9(003).                              ��
           02  SKD-JCD        PIC  9(006).
           02  F              PIC  X(012).
           02  SKD-SNO        PIC  9(006).
       77  F                  PIC  X(001).
