      ***********************************************
      *****                                     *****
      **             ÇnÅ^ÇkéÛêMÉgÉâÉì              **
      *****         ( JOLJT   )  256/1          *****
      ***********************************************
       01  JOLJT.
           02  JOLJT_PNAME1      PIC  X(005) VALUE "JOLJT".
           02  F                 PIC  X(001).
           02  JOLJT_LNAME       PIC  X(005) VALUE "JOLJT".
           02  F                 PIC  X(001).
           02  JOLJT_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLJT_SORT        PIC  X(100) VALUE SPACE.
           02  JOLJT_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLJT_RES         USAGE  POINTER.
      *----èoâ◊éwé¶ÉgÉâÉì        ÅiãÊï™ÅÅÇPÇPÅj
       01  JOLJT11-REC.
           02  JOLJT11-01        PIC 9(02).                             ÇqÇbãÊï™
           02  JOLJT11-KEYW.
             03  JOLJT11-02      PIC 9(06).                                º≠Ø∂º
             03  JOLJT11-03      PIC 9(01).                                ∑ﬁÆ≥
           02  JOLJT11-04        PIC 9(01).                                √ﬁ›∏
           02  JOLJT11-05.                                                 º≠Ø∂À
               03  JOLJT11-051   PIC 9(04).
               03  JOLJT11-052   PIC 9(02).                                ¬∑
               03  JOLJT11-053   PIC 9(02).                                À
           02  JOLJT11-06.                                                 º≠Ø∂À
               03  JOLJT11-061   PIC 9(04).
               03  JOLJT11-062   PIC 9(02).                                ¬∑
               03  JOLJT11-063   PIC 9(02).                                À
           02  JOLJT11-07.                                                 ¡Æ∏ø≥
               03  JOLJT11-071   PIC 9(04).                                ƒ∏≤∫∞
               03  JOLJT11-072   PIC 9(03).                                ¡Æ∏ N
           02  JOLJT11-08        PIC 9(01).                                ∏◊ ∫∞
           02  JOLJT11-09.                                                 ºﬁ≠¡≠
               03  JOLJT11-091   PIC 9(06).                                ºﬁ≠¡≠
               03  JOLJT11-092   PIC 9(01).                                ∑ﬁÆ≥
           02  JOLJT11-10        PIC 9(06).                                À›∫∞ƒ
           02  JOLJT11-11        PIC 9(01).                                ª≤Ωﬁ∏
           02  JOLJT11-12.                                                 º≠Ø∂ª
               03  JOLJT11-121   OCCURS  10.                               ª≤ΩﬁÕ
                   04  JOLJT11-1211      PIC  9(04).
                   04  JOLJT11-1211S     PIC  9(01).                    0:+,1:-
               03  JOLJT11-122   PIC  9(05).
               03  JOLJT11-122S  PIC  9(01).                            0:+,1:-
           02  JOLJT11-13.                                                 º≠Ø∂º
               03  JOLJT11-131   OCCURS  10.                               ª≤ΩﬁÕ
                   04  JOLJT11-1311      PIC  9(04).
                   04  JOLJT11-1311S     PIC  9(01).                    0:+,1:-
               03  JOLJT11-132   PIC  9(05).
               03  JOLJT11-132S  PIC  9(01).                            0:+,1:-
           02  JOLJT11-14        PIC 9(01).                                ±Ωﬁ∂ÿ
           02  JOLJT11-15        PIC 9(01).                                â^ëó 
           02  JOLJT11-15A       PIC 9(03).                                ÉZÉb 
           02  JOLJT11-15B       PIC 9(06).                                ëóÇË 
           02  JOLJT11-15C       PIC 9(02).                                é}î‘
           02  JOLJT11-15D       PIC N(09).                                îzíB
           02  JOLJT11-16        PIC N(23).                             ìEóv
           02  JOLJT11-20        PIC X(10).
           02  JOLJT11-16A       PIC  9(03).                            å¬êî
           02  JOLJT11-16AS      PIC  9(01).                            0:+,1:-
           02  FILLER            PIC X(01).
           02  JOLJT11-19        PIC X(01).                             èàóùïîèê
           02  JOLJT11-168       PIC 9(01).                                A-890
           02  JOLJT11-17        PIC 9(01).                             àÍî  
           02  JOLJT11-18        PIC 9(01).                             çXêVª
       77  F                     PIC  X(001).
