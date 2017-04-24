      **************************************************
      *****     入　金　累　積　フ　ァ　イ　ル     *****
      *****            ( NYURF ) 102/5             *****
      **************************************************
       01  NYUR-F.
           02  NYUR-F_PNAME1  PIC  X(005) VALUE "NYURF".
           02  F              PIC  X(001).
           02  NYUR-F_LNAME   PIC  X(006) VALUE "NYUR-F".
           02  F              PIC  X(001).
           02  NYUR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUR-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUR-F_RES     USAGE  POINTER.
       01  NYUR-R.
           02  NUR-DATE       PIC  9(008).
           02  NUR-NGP   REDEFINES NUR-DATE.                            日付
             03  NUR-NG.
               04  NUR-NEN    PIC  9(004).
               04  NUR-NEND  REDEFINES NUR-NEN.
                 05  NUR-NEN1 PIC  9(002).
                 05  NUR-NEN2 PIC  9(002).
               04  NUR-GET    PIC  9(002).
             03  NUR-NGD   REDEFINES NUR-NG.
               04  F          PIC  9(002).
               04  NUR-NGS    PIC  9(004).
             03  NUR-PEY      PIC  9(002).
           02  NUR-NGPD  REDEFINES NUR-DATE.
             03  F            PIC  9(002).
             03  NUR-NGPS     PIC  9(006).
           02  NUR-TCD        PIC  9(004).                              得意先C
           02  NUR-KIN        PIC S9(008).                              入金金額
           02  NUR-NC.                                                  入金区分
             03  NUR-NC1      PIC  9(001).
             03  NUR-NC2      PIC  9(001).
           02  NUR-NSC        PIC  9(001).
           02  NUR-TGK        PIC  9(008).
           02  NUR-TGKD  REDEFINES NUR-TGK.
             03  NUR-TGN      PIC  9(004).
             03  NUR-TGND  REDEFINES NUR-TGN.
               04  NUR-TGN1   PIC  9(002).
               04  NUR-TGN2   PIC  9(002).
             03  NUR-TGGP     PIC  9(004).
             03  NUR-TGGPD REDEFINES NUR-TGGP.
               04  NUR-TGG    PIC  9(002).
               04  NUR-TGP    PIC  9(002).
           02  NUR-TGKL  REDEFINES NUR-TGK.
             03  F            PIC  9(002).
             03  NUR-TGKS     PIC  9(006).
           02  NUR-SS.                                                  請求締日
             03  NUR-SSN      PIC  9(004).
             03  NUR-SSND  REDEFINES NUR-SSN.
               04  NUR-SSN1   PIC  9(002).
               04  NUR-SSN2   PIC  9(002).
             03  NUR-SSG      PIC  9(002).
           02  NUR-SSD   REDEFINES NUR-SS.
             03  F            PIC  9(002).
             03  NUR-SSS      PIC  9(004).
           02  NUR-BC         PIC  9(001).                              部門C
           02  NUR-TC         PIC  9(002).                              担当C
           02  NUR-SP         PIC  9(002).
           02  NUR-SK         PIC  9(001).
           02  NUR-KEY.
             03  NUR-NO       PIC  9(006).
             03  NUR-GNO      PIC  9(001).
           02  NUR-FDNO.
             03  NUR-FNO      PIC  9(006).
             03  NUR-FGNO     PIC  9(002).
           02  NUR-SKD        PIC  9(008).
           02  F              PIC  X(019).
           02  NUR-UZ         PIC S9(009).
           02  NUR-UZZ        PIC S9(007).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
