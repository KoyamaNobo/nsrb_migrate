      ***********************************************
      *****                                     *****
      **   �@�@�}�X�^�X�V�@�g�����@�@�@�@�@�@�@�@�@**
      *****         ( MSTRN    )  21/12         *****
      ***********************************************
       01  MSTRN.
           02  MSTRN_PNAME1        PIC  X(005) VALUE "MSTRN".
           02  F                   PIC  X(001).
           02  MSTRN_LNAME         PIC  X(005) VALUE "MSTRN".
           02  F                   PIC  X(001).
           02  MSTRN_KEY1          PIC  X(100) VALUE SPACE.
           02  MSTRN_SORT          PIC  X(100) VALUE SPACE.
           02  MSTRN_IDLST         PIC  X(100) VALUE SPACE.
           02  MSTRN_RES           USAGE  POINTER.
      *
       01  MS-REC.
      *----������p
           02  MS1-REC.
               03  MS1-KEY.                                                 KEY
                   04  MS1-011         PIC X(01).                           ID
                   04  MS1-012         PIC 9(07).                           ������
                   04  MS1-012R        REDEFINES  MS1-012.
                       05  MS1-0121    PIC 9(04).                             ���Ӑ�
                       05  MS1-0122    PIC 9(03).                             ������
               03  MS1-02.                                                  �X�VBIT
                   04  MS1-021         PIC X(01).                             1:�ʓ�
                   04  MS1-022         PIC X(01).                             2:��
                   04  MS1-023         PIC X(01).                             3:����
                   04  MS1-024         PIC X(01).                             4:��
               03  F                   PIC X(02).
               03  MS1-10              PIC 9(01).                           ACT
               03  MS1-11              PIC 9(06).                           �X�V��
               03  MS1-11R             REDEFINES  MS1-11.
                   04  MS1-111         PIC 9(02).                             �N
                   04  MS1-112         PIC 9(02).                             ��
                   04  MS1-113         PIC 9(02).                             ��
      *    
      *----�o�וi���p
           02  MS2-REC    REDEFINES  MS1-REC.
               03  MS2-KEY.                                                 KEY
                   04  MS2-011         PIC X(01).                           ID
                   04  MS2-012         PIC 9(06).                           �i���b�c
                   04  F               PIC X(01).
               03  MS2-02.                                                  �X�VBIT
                   04  MS2-021         PIC X(01).                             1:�ʓ�
                   04  MS2-022         PIC X(01).                             2:��
                   04  MS2-023         PIC X(01).                             3:����
                   04  MS2-024         PIC X(01).                             4:��
               03  F                   PIC X(02).
               03  MS2-10              PIC 9(01).                           ACT
               03  MS2-11              PIC 9(06).                           �X�V��
               03  MS2-11R             REDEFINES  MS2-11.
                   04  MS2-111         PIC 9(02).                             �N
                   04  MS2-112         PIC 9(02).                             ��
                   04  MS2-113         PIC 9(02).                             ��
      *    
      *----���[�N�}���X���i�ʓ��̂݁j
           02  MS3-REC    REDEFINES  MS1-REC.
               03  MS3-KEY.                                                 KEY
                   04  MS3-011         PIC X(01).                           ID
                   04  MS3-012         PIC 9(03).                           �X��
                   04  F               PIC X(04).
               03  MS3-02.                                                  �X�VBIT
                   04  MS3-021         PIC X(01).                             1:�ʓ�
                   04  MS3-022         PIC X(01).                             2:��
                   04  MS3-023         PIC X(01).                             3:����
                   04  MS3-024         PIC X(01).                             4:��
               03  F                   PIC X(02).
               03  MS3-10              PIC 9(01).                           ACT
               03  MS3-11              PIC 9(06).                           �X�V��
               03  MS3-11R             REDEFINES  MS3-11.
                   04  MS3-111         PIC 9(02).                             �N
                   04  MS3-112         PIC 9(02).                             ��
                   04  MS3-113         PIC 9(02).                             ��
       77  F                       PIC  X(001).
