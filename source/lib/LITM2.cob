      ***********************************************
      *****     ���@�Ӂ@��   �}�@�X�@�^�@�[     *****
      *****     ( TM1 , TM2 , TMD )  512/1      *****
      ***********************************************
       01  T-M.
           02  T-M_PNAME1     PIC  X(003) VALUE "TM1".
           02  F              PIC  X(001).
           02  T-M_LNAME      PIC  X(003) VALUE "T-M".
           02  F              PIC  X(001).
           02  T-M_KEY1       PIC  X(100) VALUE SPACE.
           02  T-M_SORT       PIC  X(100) VALUE SPACE.
           02  T-M_IDLST      PIC  X(100) VALUE SPACE.
           02  T-M_RES        USAGE  POINTER.
       01  T-R.
           02  T-KEY2.
             03  T-NTCD       PIC  9(004).
             03  T-KEY.
               04  T-TCD      PIC  9(004).
           02  T-NAME         PIC  N(026).
           02  T-JSU          PIC  N(020).
           02  T-JSS          PIC  N(020).
           02  T-UNO          PIC  X(008).
           02  T-TEL          PIC  X(014).
           02  T-FAX          PIC  X(014).
           02  T-FKC          PIC  9(002).                              �s���{��
           02  T-BC           PIC  9(001).                              ���庰��
           02  T-TKC          PIC  9(002).                              �n�溰��
           02  T-TNC          PIC  9(002).                              �S����
           02  T-SS           PIC  9(002).                              ��������
           02  T-NKY          PIC  9(003).                              ���໲�
           02  T-DCC          PIC  9(001).
           02  T-TGC          PIC  9(001).                              ��`�敪
           02  T-YG           PIC  9(006).                              �^�M���x
           02  T-BIK          PIC  9(001).
           02  F              PIC  X(010).
           02  T-TNA          PIC  N(016).                              ���Ӑ於
           02  T-SNA          PIC  N(026).
           02  T-SJSU         PIC  N(020).
           02  T-SJSS         PIC  N(020).
           02  T-SUNO         PIC  X(008).
           02  T-STEL         PIC  X(014).
           02  T-SFAX         PIC  X(014).
      *    �������
           02  T-SHD          PIC  9(002).
           02  T-SSI          PIC  9(003).
           02  T-SHC1         PIC  9(001).
           02  T-SHC2         PIC  9(001).
           02  T-SGT          PIC  9(001).
           02  T-SGR          PIC  9(001)V9(01).
           02  T-STT          PIC  9(001).
           02  T-STR          PIC  9(001)V9(01).
           02  T-SKR          PIC  9(004).
      *
           02  F              PIC  X(038).
           02  T-KANA         PIC  X(036).
           02  T-DNG          PIC  9(006).
           02  T-SNG          PIC  9(004).                              �J�n�N��
           02  T-ENG          PIC  9(004).                              ��~�N��
       77  F                  PIC  X(001).
