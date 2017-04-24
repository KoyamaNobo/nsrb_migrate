      *********************************************
      *****                                   *****
      *****       èoâ◊ÇsÇqÇ`ÇmÉtÉ@ÉCÉã        *****
      *****                      128/02       *****
      *********************************************
       01  HN-STRN.
           02  HN-STRN_PNAME1     PIC  X(005) VALUE "STRAN".
           02  F                  PIC  X(001).
           02  HN-STRN_LNAME      PIC  X(007) VALUE "HN-STRN".
           02  F                  PIC  X(001).
           02  HN-STRN_KEY1       PIC  X(100) VALUE SPACE.
           02  HN-STRN_SORT       PIC  X(100) VALUE SPACE.
           02  HN-STRN_IDLST      PIC  X(100) VALUE SPACE.
           02  HN-STRN_RES        USAGE  POINTER.
      *
       01  STRN-R.
           02  STRN-20            PIC 9(06).
           02  STRN-13            PIC 9(01).
           02  STRN-01            PIC 9(08).
           02  STRN-02            PIC 9(04).                            ìæà”êÊCD
           02  STRN-03            PIC 9(06).                            ïiñºCD
           02  STRN-04            PIC 9(01).                            ÉTÉCÉYKB
           02  STRN-05.                                                 ÉTÉCÉYï 
             03  STRN-051  OCCURS 10  PIC S9(04)  COMP-3.
           02  STRN-06            PIC S9(05).                           çáåvêîó 
           02  STRN-07            PIC 9(05).
           02  STRN-08            PIC S9(08).                           ã‡äz
           02  STRN-09            PIC 9(01).                            í≤êÆãÊï™
           02  STRN-10            PIC 9(01).                            ì`ï[ãÊï™
           02  STRN-11            PIC 9(05).
           02  STRN-14A           PIC 9(03).
           02  STRN-15.                                                 ï™óﬁCD
             03  STRN-151         PIC 9(02).
             03  STRN-152         PIC 9(02).
             03  STRN-153         PIC 9(02).
           02  STRN-15D  REDEFINES STRN-15.
             03  STRN-151D        PIC 9(03).
             03  F                PIC 9(03).
           02  STRN-16            PIC 9(01).                            ëqå…ãÊï™
           02  STRN-17            PIC 9(02).                            íSìñé“CD
           02  STRN-18            PIC 9(02).                            ìsìπï{åß
           02  STRN-19            PIC 9(01).                            è¡îÔê≈ãÊ
           02  STRN-21            PIC 9(03).                            å¬êî
           02  STRN-23            PIC 9(01).
           02  STRN-24            PIC 9(04).
           02  STRN-25            PIC X(10).
           02  STRN-26            PIC 9(08).
           02  F                  PIC X(04).
           02  STRN-90            PIC 9(01).
           02  STRN-91            PIC 9(01).
       01  STRN-R1.
           02  STRN1-20           PIC 9(06).
           02  STRN1-13           PIC 9(01).
           02  STRN1-01           PIC 9(08).
           02  STRN1-02           PIC 9(04).
           02  STRN1-51           PIC N(24).
           02  STRN1-52           PIC 9(06).
           02  F                  PIC X(30).
           02  STRN1-53           PIC S9(07).
           02  STRN1-54           PIC S9(07).
           02  STRN1-55           PIC S9(09).
           02  STRN1-90           PIC 9(01).
           02  STRN1-91           PIC 9(01).
       77  F                      PIC X(01).
