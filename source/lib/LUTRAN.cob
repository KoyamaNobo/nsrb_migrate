      ********************************************
      *****     �d�����g�����@�t�@�C��     *****
      *****      ( UTRAN )   128/2           *****
      ********************************************
       01  UTRAN.
           02  UTRAN_PNAME1   PIC  X(005) VALUE "UTRAN".
           02  F              PIC  X(001).
           02  UTRAN_LNAME    PIC  X(005) VALUE "UTRAN".
           02  F              PIC  X(001).
           02  UTRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRAN_SORT     PIC  X(100) VALUE SPACE.
           02  UTRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRAN_RES      USAGE  POINTER.
       01  UTRAN-R.
           02  UTRAN-NO       PIC  9(007).                              �����
           02  UTRAN-NOD   REDEFINES UTRAN-NO.
             03  UTRAN-UNO    PIC  9(006).
             03  UTRAN-GYO    PIC  9(001).
           02  UTRAN-DATE     PIC  9(008).
           02  UTRAN-NGPD  REDEFINES UTRAN-DATE.
             03  UTRAN-NG     PIC  9(006).
             03  F            PIC  9(002).
           02  UTRAN-NGP   REDEFINES UTRAN-DATE.
             03  F            PIC  9(002).
             03  UTRAN-NGPS   PIC  9(006).
           02  UTRAN-HCD      PIC  9(006).                              �i��C
           02  UTRAN-SIZ      PIC  9(001).                              ���ދ敪
           02  UTRAN-SUD.                                               ����
             03  UTRAN-SU     PIC S9(004)  OCCURS  10.
           02  UTRAN-SUT      PIC S9(005).                              ���v����
           02  UTRAN-BKIN     PIC S9(008).                              �������z
           02  UTRAN-FKIN     PIC S9(008).                              �U�֋��z
           02  UTRAN-NRC      PIC  9(001).                              ����C
           02  UTRAN-SSC      PIC  9(001).                              ���YC
           02  UTRAN-HPC      PIC  9(001).                              �ԕiC
           02  UTRAN-SKC      PIC  9(001).                              �q��C
           02  UTRAN-BC.                                                ����C
             03  UTRAN-BC1    PIC  9(002).                                   1
             03  UTRAN-BC2    PIC  9(002).                                   2
             03  UTRAN-BC3    PIC  9(002).                                   3
           02  UTRAN-BCD   REDEFINES UTRAN-BC.
             03  UTRAN-BC1D   PIC  9(003).
             03  F            PIC  9(003).
           02  UTRAN-BMC      PIC  9(002).
           02  UTRAN-BMNO     PIC  9(001).
           02  UTRAN-KBN      PIC  9(006).
           02  F              PIC  X(025).
           02  UTRAN-PRC      PIC  9(001).
       77  F                  PIC  X(001).
