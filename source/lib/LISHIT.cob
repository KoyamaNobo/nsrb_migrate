      **************************************************
      *****     �x�@���@��@�`�@�}�@�X�@�^�@�[     *****
      *****         ( S H I T M )    128/1         *****
      **************************************************
       01  SHIT-M.
           02  SHIT-M_PNAME1  PIC  X(005) VALUE "SHITM".
           02  F              PIC  X(001).
           02  SHIT-M_LNAME   PIC  X(006) VALUE "SHIT-M".
           02  F              PIC  X(001).
           02  SHIT-M_KEY1    PIC  X(100) VALUE SPACE.
           02  SHIT-M_SORT    PIC  X(100) VALUE SPACE.
           02  SHIT-M_IDLST   PIC  X(100) VALUE SPACE.
           02  SHIT-M_RES     USAGE  POINTER.
       01  SHIT-R.
           02  ST-KEY         PIC  X(004).                              ���NO
           02  ST-TSC         PIC  9(002).                              ö����ٲ
           02  ST-TSCD REDEFINES ST-TSC.                                ö����ٲ
             03  ST-TC1       PIC  9(001).
             03  ST-TC2       PIC  9(001).
           02  ST-SKC         PIC  9(002).                              ��ظ���
           02  ST-BCD         PIC  9(004).                              BK����
           02  ST-FKC         PIC  9(002).                              ̹ݺ���
           02  ST-TCD         PIC  9(004).                              ��˷����
           02  ST-KIN         PIC  9(010).                              �ݶ޸
           02  ST-FDD         PIC  9(006).
           02  ST-FDDD REDEFINES ST-FDD.                                ���޼��
             03  ST-FNG.
               04  ST-FDN     PIC  9(002).
               04  ST-FDG     PIC  9(002).
             03  ST-FDP       PIC  9(002).
           02  ST-MKD         PIC  9(006).
           02  ST-MKDD REDEFINES ST-MKD.                                �ݷ��
             03  ST-MNG.
               04  ST-MKN     PIC  9(002).
               04  ST-MKG     PIC  9(002).
             03  ST-MKP       PIC  9(002).
      *    [   ��öӸ  ��ܹ �ݶ޸   ]
           02  ST-UKD.
             03  ST-UK    OCCURS  7  PIC  9(008).
           02  ST-AUK  REDEFINES ST-UKD.
             03  ST-ZR        PIC  9(008).                              �޲خ�
             03  ST-SS        PIC  9(008).                              ��ڼ����
             03  ST-SB        PIC  9(008).                              ����
             03  ST-GC        PIC  9(008).                              �޲���
             03  ST-SZ        PIC  9(008).                              ���޳���
             03  ST-EG        PIC  9(008).                              ���ޮ���
             03  ST-ST        PIC  9(008).                              ���
           02  F              PIC  X(024).
           02  ST-SND.                                                  ��ڷ ��
             03  ST-SNF       PIC  9(004).
             03  ST-SNM       PIC  9(004).
       77  F                  PIC  X(001).
