      ********************************************
      *****     �������i�d�����̓t�@�C��     *****
      *****         (  HSSF  256/1  )        *****
      ********************************************
       01  HSS-F.
           02  HSS-F_PNAME1   PIC  X(004) VALUE "HSSF".
           02  F              PIC  X(001).
           02  HSS-F_LNAME    PIC  X(005) VALUE "HSS-F".
           02  F              PIC  X(001).
           02  HSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HSS-F_RES      USAGE  POINTER.
       01  HSS-R.
           02  HSS-KEY.
             03  HSS-DNO      PIC  9(006).                              �`�[��
             03  HSS-SNO      PIC  9(001).
           02  HSS-DATE       PIC  9(008).                              ���t
           02  HSS-NGP   REDEFINES HSS-DATE.
             03  HSS-NG.
               04  HSS-NEN    PIC  9(004).
               04  HSS-GET    PIC  9(002).
             03  HSS-PEY      PIC  9(002).
           02  HSS-NGPD  REDEFINES HSS-DATE.
             03  F            PIC  9(004).
             03  HSS-GP       PIC  9(004).
           02  HSS-NGPL  REDEFINES HSS-DATE.
             03  F            PIC  9(002).
             03  HSS-NGPS     PIC  9(006).
           02  HSS-SCD        PIC  9(004).                              �d����
           02  HSS-JCD        PIC  9(006).                              �ޗ�
           02  HSS-SUT        PIC S9(006).                              ����
           02  HSS-CD         PIC  9(006).                              �C����
           02  HSS-HCD        PIC  9(006).                              �i��
           02  HSS-ASUD.
             03  HSS-ASU   OCCURS   4.                                  ����
               04  HSS-SUD   OCCURS  10.
                 05  HSS-SU   PIC S9(004).                              ����
           02  HSS-SKC        PIC  9(001).                              �q��
           02  HSS-RNO        PIC  9(008).
           02  HSS-RNOD  REDEFINES HSS-RNO.
             03  HSS-RSN      PIC  9(002).
             03  HSS-RNG      PIC  9(004).
             03  HSS-RND      PIC  9(002).
           02  HSS-KRC        PIC  9(001).
           02  HSS-HPC        PIC  9(001).
           02  HSS-UNO        PIC  9(006).
           02  F              PIC  X(032).
           02  HSS-BHC        PIC  9(001).                              �w���ϊ�
           02  HSS-HHC        PIC  9(001).                              �̔��ϊ�
           02  HSS-HKC        PIC  9(001).                              �c�ƕϊ�
           02  HSS-PRC        PIC  9(001).                              ��
       77  F                  PIC  X(001).
