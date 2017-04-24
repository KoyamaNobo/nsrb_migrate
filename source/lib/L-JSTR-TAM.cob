       01  JSTR.                                                        º≠Ø∂ªºΩﬁ
           02  JSTR_PNAME1            PIC  X(008) VALUE "JSTR-TAM".
           02  F                      PIC  X(001).
           02  JSTR_LNAME             PIC  X(004) VALUE "JSTR".
           02  F                      PIC  X(001).
           02  JSTR_KEY1              PIC  X(100) VALUE SPACE.
           02  JSTR_KEY2              PIC  X(100) VALUE SPACE.
           02  JSTR_SORT              PIC  X(100) VALUE SPACE.
           02  JSTR_IDLST             PIC  X(100) VALUE SPACE.
           02  JSTR_RES               USAGE  POINTER.                                       ƒ◊›
      *
       01  JSTR-R.
           02   JSTR-KEY.                                               KEY
                03   JSTR-01          PIC 9(6).                         º≠Ø∂ºΩﬁ
                03   JSTR-02          PIC 9(1).                         ∑ﬁÆ≥
           02   JSTR-03               PIC 9(1).                         √ﬁ›∏
           02   JSTR-04.                                                º≠Ø∂Àﬁ ª
                03  JSTR-041          PIC 9(4).
                03  JSTR-041L  REDEFINES  JSTR-041.
                    04  JSTR-0411     PIC 9(2).
                    04  JSTR-0412     PIC 9(2).
                03  JSTR-042          PIC 9(2).                         ¬∑
                03  JSTR-043          PIC 9(2).                         À
           02   JSTR-04L   REDEFINES  JSTR-04.
                03  F                 PIC 9(2).
                03  JSTR-04S          PIC 9(6).
           02   JSTR-05.                                                º≠Ø∂Àﬁºﬁ
                03  JSTR-051          PIC 9(4).
                03  JSTR-051L  REDEFINES  JSTR-051.
                    04  JSTR-0511     PIC 9(2).
                    04  JSTR-0512     PIC 9(2).
                03  JSTR-052          PIC 9(2).                         ¬∑
                03  JSTR-053          PIC 9(2).                         À
           02   JSTR-05L   REDEFINES  JSTR-05.
                03  F                 PIC 9(2).
                03  JSTR-05S          PIC 9(6).
           02   JSTR-06.                                                ¡Æ∏ø≥ CD
                03  JSTR-061          PIC 9(4).                         ƒ∏≤∫∞ƒﬁ
                03  JSTR-062          PIC 9(3).                         ¡Æ∏ NO
           02   JSTR-07               PIC 9(1).                         ∏◊ ∫∞ƒﬁ
           02   JSTR-08.                                                ºﬁ≠¡≠≥
                03  JSTR-081          PIC 9(6).                         ºﬁ≠¡≠≥NO
                03  JSTR-082          PIC 9(1).                         ∑ﬁÆ≥
           02   JSTR-09               PIC 9(6).                         À›∫∞ƒﬁ
           02   JSTR-10               PIC 9(1).                         ª≤Ωﬁ∏Ãﬁ›
           02   JSTR-11.                                                º≠Ø∂ªºΩﬁ
                03  JSTR-111    OCCURS  10.                             ª≤ΩﬁÕﬁ¬
                    04  JSTR-1111     PIC S9(4).
                03  JSTR-112          PIC S9(5).
           02   JSTR-12.                                                º≠Ø∂ºﬁ¬
                03  JSTR-121    OCCURS  10.                             ª≤ΩﬁÕﬁ¬
                    04  JSTR-1211     PIC S9(4).
                03  JSTR-122          PIC S9(5).
           02  JSTR-13                PIC 9(1).                         ±Ωﬁ∂ÿ KB
           02  JSTR-14                PIC 9(1).                         â^ëóÇbÇc
           02  JSTR-14A               PIC 9(3).                         ÉZÉbÉgêî
           02  JSTR-14B               PIC 9(6).                         ëóÇËèÛáÇ
           02  JSTR-14C               PIC 9(2).                         é}î‘
           02  JSTR-14D               PIC N(9).                         îzíB
           02  JSTR-15                PIC N(23).                        ìEóv
           02  JSTR-20                PIC X(10).                        ìEóv
           02  JSTR-15A               PIC S9(03).                       å¬êî
           02  JSTR-30                PIC 9(01).
           02  JSTR-40.
               03  JSTR-401.
                   04  JSTR-4011      PIC X(03).
                   04  JSTR-4012      PIC 9(01).
               03  JSTR-402.
                   04  JSTR-4021      PIC X(03).
                   04  JSTR-4022      PIC 9(01).
                   04  JSTR-4023      PIC 9(01).
           02  FILLER                 PIC X(16).
           02  JSTR-19                PIC X(01).
           02  JSTR-158               PIC 9(01).
           02  JSTR-16                PIC 9(01).                        àÍî ã≥àÁ
           02  JSTR-17                PIC 9(01).                        çXêVª≤›
       77  F                          PIC  X(001).
