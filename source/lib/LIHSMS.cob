       01  HSMSF.
           02  HSMSF_PNAME1           PIC  X(005) VALUE "HSMSF".
           02  F                      PIC  X(001).
           02  HSMSF_LNAME            PIC  X(005) VALUE "HSMSF".
           02  F                      PIC  X(001).
           02  HSMSF_KEY1             PIC  X(100) VALUE SPACE.
           02  HSMSF_KEY2             PIC  X(100) VALUE SPACE.
           02  HSMSF_SORT             PIC  X(100) VALUE SPACE.
           02  HSMSF_IDLST            PIC  X(100) VALUE SPACE.
           02  HSMSF_RES              USAGE  POINTER.
      *
       01  HSMS-R.
           02  HSMS-R1.
               03  HSMS-KEY.                                            KEY
                   04   HSMS-01           PIC 9(6).                      º≠Ø∂ºΩﬁ
                   04   HSMS-02           PIC 9(1).                      ∑ﬁÆ≥
               03  HSMS-03                PIC 9(1).                      √ﬁ›∏
               03  HSMS-05.                                              º≠Ø∂Àﬁºﬁ
                   04  HSMS-051           PIC 9(4).
                   04  HSMS-052           PIC 9(2).                      ¬∑
                   04  HSMS-053           PIC 9(2).                      À
               03  HSMS-06.                                              ¡Æ∏ø≥ CD
                   04  HSMS-061           PIC 9(4).                      ƒ∏≤∫∞ƒﬁ
                   04  HSMS-062           PIC 9(3).                      ¡Æ∏ NO
               03  HSMS-07                PIC 9(1).                      ∏◊ ∫∞ƒﬁ
               03  HSMS-09                PIC 9(6).                      À›∫∞ƒﬁ
               03  HSMS-10                PIC 9(1).                      ª≤Ωﬁ∏Ãﬁ›
               03  HSMS-12.                                             º≠Ø∂ºﬁ¬
                   04  HSMS-121    OCCURS  10.                          ª≤ΩﬁÕﬁ¬
                       05  HSMS-1211      PIC S9(4).
                   04  HSMS-122           PIC S9(6).
               03  HSMS-13                PIC 9(1).                      ±Ωﬁ∂ÿ KB
               03  HSMS-14                PIC S9(03).                    å¬êî
               03  HSMS-21                PIC 9(01).                     ≤›ºﬁ∏Ãﬁ›
               03  HSMS-20                PIC 9(02).                     ¿›ƒ≥
               03  HSMS-16                PIC 9(02).                     Ãﬁ›Ÿ≤2
               03  HSMS-17                PIC 9(05).                     ¿›∂
               03  HSMS-18                PIC 9(08).
               03  HSMS-22                PIC X(10).
               03  HSMS-23                PIC 9(01).                     Õ›∂›∏Ãﬁ›
               03  HSMS-24                PIC 9(01).
               03  FILLER                 PIC X(14).
               03  HSMS-26                PIC 9(01).
               03  HSMS-25                PIC 9(01).
               03  HSMS-19                PIC 9(01).                     º÷≥∂≤Ω≥
           02  HSMS-R2    REDEFINES  HSMS-R1.
               03  HSMS-KEYB.                                            KEY
                   04   HSMS-01B          PIC 9(6).                      º≠Ø∂ºΩﬁ
                   04   HSMS-02B          PIC 9(1).                      ∑ﬁÆ≥
               03  HSMS-03B               PIC 9(1).
               03  HSMS-05B.
                   04  HSMS-051B          PIC 9(4).
                   04  HSMS-052B          PIC 9(2).
                   04  HSMS-053B          PIC 9(2).
               03  HSMS-06B.
                   04  HSMS-061B          PIC 9(4).
                   04  HSMS-062B          PIC 9(3).
               03  HSMS-07B               PIC 9(1).
               03  HSMS-15                PIC N(24).                     ìEóv
               03  FILLER                 PIC X(37).
               03  HSMS-23B               PIC 9(01).
               03  HSMS-24B               PIC 9(01).
               03  FILLER                 PIC X(14).
               03  HSMS-26B               PIC 9(01).
               03  HSMS-25B               PIC 9(01).
               03  HSMS-19B               PIC 9(01).                     º÷≥∂≤Ω≥
       77  F                              PIC X(01).
