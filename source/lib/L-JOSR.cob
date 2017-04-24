      ***********************************************
      *****                                     *****
      **   Å@Å@  ÇnÅ^ÇkëóêMó›êœÉtÉ@ÉCÉãÅ@Å@Å@Å@Å@Å@**
      *****         ( JOLSR   )  341/3          *****
      ***********************************************
       01  JOLSR.
           02  JOLSR_PNAME1      PIC  X(005) VALUE "JOLSR".
           02  F                 PIC  X(001).
           02  JOLSR_LNAME       PIC  X(005) VALUE "JOLSR".
           02  F                 PIC  X(001).
           02  JOLSR_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLSR_SORT        PIC  X(100) VALUE SPACE.
           02  JOLSR_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLSR_RES         USAGE  POINTER.
      *
       01  JOLSR-REC.
      *----ÉRÉìÉgÉçÅ[ÉãÇeóp      ÅiãÊï™ÅÅÇOÇPÅj
           02  JOLSR1-REC.
               03  JOLSR1-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSR1-02         PIC  9(01).                            ID
               03  JOLSR1-03         PIC  9(01).                            â^&ëqCD
               03  JOLSR1-04         PIC  N(06).                            â^&ëqCD
               03  JOLSR1-05         PIC  X(18).                            FILLER
               03  F                 PIC  X(221).
               03  JOLSR1-99         PIC  9(01).                            ëóêMêÊ
               03  F                 PIC  X(083).
               03  JOLSR1-NO         PIC  9(02).
      *
      *----íºëóêÊÉ}ÉXÉ^óp        ÅiãÊï™ÅÅÇOÇQÅj
           02  JOLSR2-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR2-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSR2-02         PIC  9(04).                            ìæà”êÊCD
               03  JOLSR2-03         PIC  9(03).                            íºëóêÊCD
               03  JOLSR2-04         PIC  N(26).
               03  JOLSR2-05         PIC  N(20).
               03  JOLSR2-06         PIC  N(20).
               03  JOLSR2-07         PIC  X(08).
               03  JOLSR2-08         PIC  X(14).
               03  JOLSR2-09         PIC  9(02).                            ï{åß∫∞ƒﬁ
               03  JOLSR2-10         PIC  9(01).                            â^ëó∫∞ƒﬁ
               03  JOLSR2-11         PIC  X(28).
               03  JOLSR2-12         PIC  9(01).                            ACT
               03  F                 PIC  X(60).
               03  JOLSR2-99         PIC  9(01).                            ëóêMêÊ
               03  F                 PIC  X(083).
               03  JOLSR2-NO         PIC  9(02).
      *
      *----ïiñºÉtÉ@ÉCÉã      ÅiãÊï™ÅÅÇOÇRÅj
           02  JOLSR3-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR3-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSR3-KEY2.
                   04  JOLSR3-MHCD   PIC  9(006).
                   04  JOLSR3-HCD    PIC  9(006).
               03  JOLSR3-NAME       PIC  N(024).
               03  JOLSR3-BC.
                   04  JOLSR3-BC1    PIC  9(002).
                   04  JOLSR3-BC2    PIC  9(002).
                   04  JOLSR3-BC3    PIC  9(002).
               03  JOLSR3-ASSD.
                 04  JOLSR3-SSD   OCCURS  4.
                   05  JOLSR3-SS     PIC  9(010).
               03  JOLSR3-ASKD  REDEFINES JOLSR3-ASSD.
                 04  JOLSR3-SKD   OCCURS  4.
                   05  JOLSR3-SK    OCCURS 10.
                     06  JOLSR3-S    PIC  9(001).
               03  JOLSR3-AHSD  REDEFINES JOLSR3-ASSD.
                 04  JOLSR3-HSD.
                   05  JOLSR3-SS1    PIC  9(010).
                   05  JOLSR3-SD1   REDEFINES JOLSR3-SS1.
                     06  JOLSR3-S1    OCCURS  10  PIC  9(001).
                   05  JOLSR3-SS2    PIC  9(010).
                   05  JOLSR3-SD2    REDEFINES JOLSR3-SS2.
                     06  JOLSR3-S2    OCCURS  10  PIC  9(001).
                   05  JOLSR3-SS3    PIC  9(010).
                   05  JOLSR3-SD3    REDEFINES JOLSR3-SS3.
                     06  JOLSR3-S3    OCCURS  10  PIC  9(001).
                   05  JOLSR3-SS4    PIC  9(010).
                   05  JOLSR3-SD4    REDEFINES JOLSR3-SS4.
                     06  JOLSR3-S4    OCCURS  10  PIC  9(001).
               03  JOLSR3-SB         PIC  9(005).
               03  JOLSR3-FT         PIC  9(005).
               03  F                 PIC  X(019).
               03  JOLSR3-KT         PIC  9(005).
               03  JOLSR3-TCD        PIC  9(004).
               03  JOLSR3-ISU        PIC  9(003).
               03  JOLSR3-10         PIC  9(01).                            ACT
               03  JOLSR3-SCC        PIC  9(001).
               03  JOLSR3-BMC        PIC  9(002).
               03  JOLSR3-BMNO       PIC  9(001).
               03  JOLSR3-YG         PIC  9(005).
               03  JOLSR3-HKB        PIC  9(001).
               03  JOLSR3-HPV        PIC  9(001).
               03  JOLSR3-BC4        PIC  9(001).
               03  F                 PIC  X(011).
               03  JOLSR3-SMS        PIC  N(016).
               03  JOLSR3-UNG        PIC  9(006).
               03  JOLSR3-NNG        PIC  9(006).
               03  F                 PIC  X(001).
               03  JOLSR3-CS         PIC  N(010).
               03  F                 PIC  X(003).
               03  JOLSR3-DNG        PIC  9(006).
               03  JOLSR3-SNG        PIC  9(004).
               03  JOLSR3-ENG        PIC  9(004).
               03  JOLSR3-99         PIC  9(01).                            ëóêMêÊ
               03  F                 PIC  X(083).
               03  JOLSR3-NO         PIC  9(02).
      *
      *----ÉèÅ[ÉNÉ}ÉììXñºÉtÉ@ÉCÉãÅiãÊï™ÅÅÇOÇSÅj
           02  JOLSR4-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR4-01         PIC  9(02).
               03  JOLSR4-02         PIC  9(04).
               03  JOLSR4-03         PIC  N(26).
               03  JOLSR4-04         PIC  9(1).
               03  F                 PIC  X(07).
               03  F                 PIC  X(189).
               03  JOLSR4-99         PIC  9(01).
               03  F                 PIC  X(083).
               03  JOLSR4-NO         PIC  9(02).
      *
      *----èoâ◊éwé¶ÉgÉâÉì        ÅiãÊï™ÅÅÇPÇPÅj
           02  JOLSR11-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR11-01        PIC 9(02).                             ÇqÇbãÊï™
               03  JOLSR11-02        PIC 9(06).                                º≠Ø∂º
               03  JOLSR11-03        PIC 9(01).                                ∑ﬁÆ≥
               03  JOLSR11-04        PIC 9(01).                                √ﬁ›∏
               03  JOLSR11-05.                                                 º≠Ø∂À
                   04  JOLSR11-051   PIC 9(04).
                   04  JOLSR11-052   PIC 9(02).                                ¬∑
                   04  JOLSR11-053   PIC 9(02).                                À
               03  JOLSR11-06.                                                 º≠Ø∂À
                   04  JOLSR11-061   PIC 9(04).
                   04  JOLSR11-062   PIC 9(02).                                ¬∑
                   04  JOLSR11-063   PIC 9(02).                                À
               03  JOLSR11-07.                                                 ¡Æ∏ø≥
                   04  JOLSR11-071   PIC 9(04).                                ƒ∏≤∫∞
                   04  JOLSR11-072   PIC 9(03).                                ¡Æ∏ N
               03  JOLSR11-08        PIC 9(01).                                ∏◊ ∫∞
               03  JOLSR11-09.                                                 ºﬁ≠¡≠
                   04  JOLSR11-091   PIC 9(06).                                ºﬁ≠¡≠
                   04  JOLSR11-092   PIC 9(01).                                ∑ﬁÆ≥
               03  JOLSR11-10        PIC 9(06).                                À›∫∞ƒ
               03  JOLSR11-11        PIC 9(01).                                ª≤Ωﬁ∏
               03  JOLSR11-12.                                                 º≠Ø∂ª
                   04  JOLSR11-121   OCCURS  10.                               ª≤ΩﬁÕ
                       05  JOLSR11-1211      PIC S9(04).
                   04  JOLSR11-122   PIC S9(05).
               03  JOLSR11-13.                                                 º≠Ø∂º
                   04  JOLSR11-131   OCCURS  10.                               ª≤ΩﬁÕ
                       05  JOLSR11-1311      PIC S9(04).
                   04  JOLSR11-132   PIC S9(05).
               03  JOLSR11-14        PIC 9(01).                                ±Ωﬁ∂ÿ
               03  JOLSR11-15        PIC 9(01).                                â^ëó
               03  JOLSR11-15A       PIC 9(03).                                ÉZÉb
               03  JOLSR11-15B       PIC 9(06).                                ëóÇË
               03  JOLSR11-15C       PIC 9(02).                                é}î‘
               03  JOLSR11-15D       PIC N(09).                                îzíB
               03  JOLSR11-16        PIC N(23).                                ìEóv
               03  JOLSR11-20        PIC X(10).
               03  JOLSR11-16A       PIC S9(03).                            å¬êî
               03  FILLER            PIC X(24).
               03  JOLSR11-19        PIC X(01).                             èàóùïîèê
               03  JOLSR11-168       PIC 9(01).                                A-890
               03  JOLSR11-17        PIC 9(01).                                àÍî 
               03  JOLSR11-18        PIC 9(01).                                çXêVª
               03  F                 PIC  X(083).
               03  JOLSR11-NO        PIC  9(02).
      *
      *----â◊éDÉgÉâÉì            ÅiãÊï™ÅÅÇPÇQÅj
           02  JOLSR12-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR12-01        PIC 9(02).                             ÇqÇbãÊï™
      *    
               03  JOLSR121-A.                                              çsNOT=7
                   04  JOLSR121-01   PIC 9(6).                                 √ﬁ›Àﬂ
                   04  JOLSR121-02   PIC 9(1).                                 ∑ﬁÆ≥
                   04  JOLSR121-03   PIC 9(6).                                 À›∫∞ƒ
                   04  JOLSR121-04.                                             Øø≥À
                    05  JOLSR121-041 PIC 9(2).                                 »›
                    05  JOLSR121-042 PIC 9(2).                                 ¬∑
                    05  JOLSR121-043 PIC 9(2).                                 À
                   04  JOLSR121-05.                                            ¡Æ∏∫∞
                    05  JOLSR121-051 PIC 9(4).                                 ƒ∏≤∫∞
                    05  JOLSR121-052 PIC 9(3).                                 ¡Æ∏ N
                   04  JOLSR121-06   PIC 9(1).                                 ≥›ø≥
                   04  JOLSR121-07   PIC 9(1).                                 ø≥∫∞ƒ
                   04  JOLSR121-08   PIC S9(3).                                ∫Ω≥
                   04   JOLSR121-09  OCCURS  27.                               ºØ∂Ω≥
                    05  JOLSR121-091 PIC S9(3).                                ª≤ΩﬁÕ
                   04  JOLSR121-10   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSR121-11   PIC 9(1).                                 ∆≠≥ÿÆ
                   04  JOLSR121-12   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSR121-13   PIC S9(3).                                œ≤Ω≥
                   04  JOLSR121-13A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSR121-14   PIC 9(6).                                 µ∏ÿºﬁ
      *    
               03  JOLSR122-A        REDEFINES  JOLSR121-A.                 çsÅÅÇV
                   04  JOLSR122-01   PIC 9(6).                                 √ﬁ›Àﬂ
                   04  JOLSR122-02   PIC 9(1).                                 ∑ﬁÆ≥
                   04  JOLSR122-02A  PIC N(9).                                  ≤¿¬
                   04  JOLSR122-03   PIC N(23).                                √∑÷≥
                   04  FILLER        PIC X(41).
                   04  JOLSR122-04   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSR122-05   PIC 9(1).                                 ∆Æ≥ÿÆ
                   04  JOLSR122-06   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSR122-07   PIC S9(3).                                œ≤Ω≥
                   04  JOLSR122-07A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSR122-08   PIC 9(6).                                 µ∏ÿºﬁ
      *    
               03  JOLSR121-B.                                              çsNOT=7
                   04  JOLSR121-21   PIC 9(6).                                 √ﬁ›Àﬂ
                   04  JOLSR121-22   PIC 9(1).                                 ∑ﬁÆ≥
                   04  JOLSR121-23   PIC 9(6).                                 À›∫∞ƒ
                   04  JOLSR121-24.                                             Øø≥À
                    05  JOLSR121-241 PIC 9(2).                                 »›
                    05  JOLSR121-242 PIC 9(2).                                 ¬∑
                    05  JOLSR121-243 PIC 9(2).                                 À
                   04  JOLSR121-25.                                            ¡Æ∏∫∞
                    05  JOLSR121-251 PIC 9(4).                                 ƒ∏≤∫∞
                    05  JOLSR121-252 PIC 9(3).                                 ¡Æ∏ N
                   04  JOLSR121-26   PIC 9(1).                                 ≥›ø≥
                   04  JOLSR121-27   PIC 9(1).                                 ø≥∫∞ƒ
                   04  JOLSR121-28   PIC S9(3).                                ∫Ω≥
                   04   JOLSR121-29  OCCURS  27.                               ºØ∂Ω≥
                    05  JOLSR121-291 PIC S9(3).                                ª≤ΩﬁÕ
                   04  JOLSR121-30   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSR121-31   PIC 9(1).                                 ∆≠≥ÿÆ
                   04  JOLSR121-32   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSR121-33   PIC S9(3).                                œ≤Ω≥
                   04  JOLSR121-33A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSR121-34   PIC 9(6).                                 µ∏ÿºﬁ
      *    
               03  JOLSR122-B        REDEFINES  JOLSR121-B.                 çsÅÅÇV
                   04  JOLSR122-21   PIC 9(6).                                 √ﬁ›Àﬂ
                   04  JOLSR122-22   PIC 9(1).                                 ∑ﬁÆ≥
                   04  JOLSR122-22A  PIC N(9).                                  ≤¿¬
                   04  JOLSR122-23   PIC N(23).                                √∑÷≥
                   04  FILLER        PIC X(41).
                   04  JOLSR122-24   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSR122-25   PIC 9(1).                                 ∆Æ≥ÿÆ
                   04  JOLSR122-26   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSR122-27   PIC S9(3).                                œ≤Ω≥
                   04  JOLSR122-27A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSR122-28   PIC 9(6).                                 µ∏ÿºﬁ
               03  F                 PIC  X(083).
               03  JOLSR12-NO        PIC  9(02).
      *
      *----ëóÇËèÛÉtÉ@ÉCÉã        ÅiãÊï™ÅÅÇPÇRÅj
           02  JOLSR13-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR13-01        PIC 9(02).                             ÇqÇbãÊï™
               03  JOLSR13-TBL       OCCURS  4.
                   04  JOLSR13-02    PIC 9(06).                                ëóÇË
                   04  JOLSR13-03    PIC 9(01).                                â^ëó
                   04  JOLSR13-04    PIC 9(06).                                îNåé
                   04  JOLSR13-05    PIC 9(01).                                ëqå…
                   04  JOLSR13-06    PIC 9(07).                                íºëó
                   04  JOLSR13-07    PIC N(09).                                îzíB
                   04  JOLSR13-08    PIC 9(03).                                å¬êî
                   04  JOLSR13-09    PIC 9(01).                                àÛéö
                   04  JOLSR13-10    PIC 9(01).                                ãÊï™
                   04  JOLSR13-11    PIC 9(01).                                çXêV
                   04  F             PIC X(18).
               03  F                 PIC X(02).
               03  F                 PIC  X(083).
               03  JOLSR13-NO        PIC  9(02).
      *----ÉèÅ[ÉNÉ}Éìì`ï[Çe      ÅiãÊï™ÅÅÇPÇSÅj
           02  JOLSR14-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR14-01            PIC 9(02).
      *    
               03  JOLSR141-A.
                   04  JOLSR141-KEY.
                     05  JOLSR141-STC.
                       06  F             PIC  9(002).
                       06  JOLSR141-SCD  PIC  9(002).
                       06  F             PIC  9(001).
                       06  JOLSR141-TCD  PIC  9(004).
                     05  JOLSR141-DNO.
                       06  F             PIC  9(002).
                       06  JOLSR141-DNOD PIC  9(007).
                     05  JOLSR141-DGN    PIC  9(002).
                   04  JOLSR141-BC.
                     05  JOLSR141-BCD    PIC  9(002).
                     05  F               PIC  9(001).
                   04  JOLSR141-SHC      PIC  9(001).
                   04  JOLSR141-DPC      PIC  X(002).
                   04  JOLSR141-HNGP     PIC  9(006).
                   04  JOLSR141-NNGP     PIC  9(006).
                   04  JOLSR141-THC      PIC  9(006).
                   04  JOLSR141-MHC      PIC  X(001).
                   04  F                 PIC  X(001).
                   04  JOLSR141-SNA      PIC  X(020).
                   04  JOLSR141-TNA      PIC  X(020).
                   04  JOLSR141-HCC      PIC  9(001).
                   04  JOLSR141-HSP      PIC  X(001).
                   04  JOLSR141-DHC      PIC  X(001).
                   04  JOLSR141-KHC      PIC  X(001).
                   04  JOLSR141-KCC      PIC  X(001).
                   04  JOLSR141-UBC      PIC  X(001).
                   04  JOLSR141-NCC      PIC  9(001).
                   04  JOLSR141-EDI      PIC  9(001).
                   04  JOLSR141-NKC      PIC  9(001).
                   04  JOLSR141-ZAC      PIC  9(001).
                   04  F                 PIC  X(157).
                   04  JOLSR141-PC       PIC  9(001).
      *    
               03  JOLSR142-A        REDEFINES  JOLSR141-A.
                   04  JOLSR142-KEY.
                     05  JOLSR142-STC.
                       06  F             PIC  9(002).
                       06  JOLSR142-SCD  PIC  9(002).
                       06  F             PIC  9(001).
                       06  JOLSR142-TCD  PIC  9(004).
                     05  JOLSR142-DNO.
                       06  F             PIC  9(002).
                       06  JOLSR142-DNOD PIC  9(007).
                     05  JOLSR142-DGN    PIC  9(002).
                   04  JOLSR142-HCD      PIC  X(013).
                   04  JOLSR142-ISU      PIC  9(003)V9(01).
                   04  JOLSR142-KSU      PIC  9(004).
                   04  JOLSR142-HTC      PIC  X(002).
                   04  JOLSR142-SU       PIC  9(005)V9(01).
                   04  JOLSR142-GTN      PIC  9(007)V9(02).
                   04  JOLSR142-UTN      PIC  9(007).
                   04  JOLSR142-GKIN     PIC  9(010).
                   04  JOLSR142-UKIN     PIC  9(010).
                   04  JOLSR142-GCN      PIC  9(006).
                   04  JOLSR142-CCD      PIC  X(003).
                   04  JOLSR142-SHN      PIC  X(025).
                   04  JOLSR142-JAN      PIC  X(013).
                   04  F                 PIC  X(004).
                   04  JOLSR142-TSH      PIC  9(005).
                   04  JOLSR142-TKC      PIC  X(001).
                   04  F                 PIC  X(001).
                   04  F                 PIC  X(110).
                   04  JOLSR142-PC       PIC  9(001).
               03  F                 PIC  X(083).
               03  JOLSR14-NO        PIC  9(02).
      *----ÉiÉtÉRì`ï[Çe Å@Å@     ÅiãÊï™ÅÅÇPÇTÅj
           02  JOLSR15-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR15-01            PIC 9(02).
      *    
               03  JOLSR151-A.
                   04  JOLSR151-KEY.
                     05  JOLSR151-STC.
                       06  JOLSR151-SCD  PIC  9(002).
                       06  JOLSR151-TCD  PIC  9(003).
                       06  F             PIC  X(004).
                     05  JOLSR151-DNO.
                       06  F             PIC  X(002).
                       06  JOLSR151-DNOD PIC  9(007).
                     05  JOLSR151-DGN    PIC  9(002).
                   04  F                 PIC  X(002).
                   04  JOLSR151-BCD      PIC  9(002).
                   04  JOLSR151-DPC      PIC  9(002).
                   04  JOLSR151-HNGP     PIC  9(006).
                   04  JOLSR151-HNGPD REDEFINES JOLSR151-HNGP.
                     05  JOLSR151-HNEN   PIC  9(002).
                     05  JOLSR151-HGET   PIC  9(002).
                     05  JOLSR151-HPEY   PIC  9(002).
                   04  JOLSR151-NNGP     PIC  9(006).
                   04  JOLSR151-NNGPD REDEFINES JOLSR151-NNGP.
                     05  JOLSR151-NNEN   PIC  9(002).
                     05  JOLSR151-NGET   PIC  9(002).
                     05  JOLSR151-NPEY   PIC  9(002).
                   04  JOLSR151-THC      PIC  9(006).
                   04  JOLSR151-STA      PIC  X(002).
                   04  JOLSR151-SNA      PIC  X(015).
                   04  JOLSR151-TNA      PIC  X(015).
                   04  JOLSR151-TSN      PIC  X(015).
                   04  JOLSR151-TST      PIC  X(012).
                   04  JOLSR151-HCC      PIC  9(001).
                   04  F                 PIC  X(024).
                   04  JOLSR151-AR       PIC  X(007).
                   04  JOLSR151-DUR      PIC  X(026).
                   04  JOLSR151-DSHR     PIC  X(014).
                   04  JOLSR151-DSMR     PIC  X(007).
                   04  JOLSR151-ER       PIC  X(005).
                   04  JOLSR151-FSR      PIC  X(015).
                   04  JOLSR151-FUR      PIC  X(007).
                   04  JOLSR151-LCR      PIC  X(016).
                   04  JOLSR151-LUR      PIC  X(020).
                   04  JOLSR151-LSR      PIC  X(007).
                   04  F                 PIC  X(001).
                   04  JOLSR151-PC       PIC  9(001).
      *    
               03  JOLSR152-A        REDEFINES  JOLSR151-A.
                   04  JOLSR152-KEY.
                     05  JOLSR152-STC.
                       06  JOLSR152-SCD  PIC  9(002).
                       06  JOLSR152-TCD  PIC  9(003).
                       06  F             PIC  X(004).
                     05  JOLSR152-DNO.
                       06  F             PIC  X(002).
                       06  JOLSR152-DNOD PIC  9(007).
                     05  JOLSR152-DGN    PIC  9(002).
                   04  JOLSR152-JAN      PIC  X(013).
                   04  JOLSR152-GAR      PIC  X(006).
                   04  F                 PIC  X(001).
                   04  JOLSR152-TNI      PIC  X(003).
                   04  JOLSR152-SU       PIC  9(005).
                   04  F                 PIC  X(001).
                   04  JOLSR152-GTN      PIC  9(007).
                   04  F                 PIC  X(002).
                   04  JOLSR152-UTN      PIC  9(007).
                   04  JOLSR152-GKIN     PIC  9(010).
                   04  JOLSR152-UKIN     PIC  9(010).
                   04  F                 PIC  X(009).
                   04  JOLSR152-SHN      PIC  X(025).
                   04  JOLSR152-HSC      PIC  X(008).
                   04  JOLSR152-COR      PIC  X(006).
                   04  JOLSR152-SIZ      PIC  X(005).
                   04  F                 PIC  X(004).
                   04  JOLSR152-KKK      PIC  X(025).
                   04  JOLSR152-PCH      PIC  X(001).
                   04  JOLSR152-PSI      PIC  X(001).
                   04  JOLSR152-PBM      PIC  9(002).
                   04  JOLSR152-PJAN     PIC  X(013).
                   04  JOLSR152-PSHN     PIC  X(020).
                   04  JOLSR152-PKKK     PIC  X(020).
                   04  JOLSR152-PUTN     PIC  9(007).
                   04  JOLSR152-PMS      PIC  9(005).
                   04  F                 PIC  X(017).
                   04  JOLSR152-PC       PIC  9(001).
               03  F                 PIC  X(083).
               03  JOLSR15-NO        PIC  9(02).
      *----ÉgÉâÉXÉRëºìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇUÅj
           02  JOLSR16-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR16-01            PIC 9(02).
               03  JOLSR16-A.
                   04  JOLSR16-KEY.
                       05  JOLSR16-DNO   PIC  9(006).
                       05  JOLSR16-GNO   PIC  9(001).
                   04  JOLSR16-DATE      PIC  9(006).
                   04  JOLSR16-TCD       PIC  9(004).
                   04  JOLSR16-CCD       PIC  9(003).
                   04  JOLSR16-TPC       PIC  9(004).
                   04  JOLSR16-HCD       PIC  9(006).
                   04  JOLSR16-SIZ       PIC  X(003).
                   04  JOLSR16-SKB       PIC  9(001).
                   04  JOLSR16-SNO       PIC  9(002).
                   04  JOLSR16-SU        PIC S9(005).
                   04  JOLSR16-GT        PIC  9(007).
                   04  JOLSR16-UT        PIC  9(007).
                   04  JOLSR16-GKIN      PIC S9(008).
                   04  JOLSR16-UKIN      PIC S9(008).
                   04  JOLSR16-JNOD.
                       05  JOLSR16-JNO   PIC  9(006).
                       05  JOLSR16-JGN   PIC  9(001).
                   04  JOLSR16-SOK       PIC  9(001).
                   04  JOLSR16-UNS       PIC  9(001).
                   04  JOLSR16-ISU       PIC  9(003).
                   04  JOLSR16-HNO       PIC  X(010).
                   04  JOLSR16-TEKI      PIC  N(028).
                   04  JOLSR16-TED   REDEFINES JOLSR16-TEKI.
                       05  JOLSR16-THT   PIC  N(009).
                       05  JOLSR16-TTE   PIC  N(019).
                   04  JOLSR16-TRN       PIC  X(020).
                   04  JOLSR16-JAN       PIC  X(013).
                   04  F                 PIC  X(070).
                   04  JOLSR16-PRC       PIC  9(001).
                   04  JOLSR16-UPC       PIC  9(001).
               03  F                 PIC  X(083).
               03  JOLSR16-NO        PIC  9(02).
      *----ê‘ÇøÇ·ÇÒñ{ï‹ìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇVÅj
           02  JOLSR17-REC    REDEFINES  JOLSR1-REC.
               03  JOLSR17-01            PIC 9(02).
               03  JOLSR17-KEY.
                   04  JOLSR17-STC       PIC  9(007).
                   04  JOLSR17-DNO       PIC  9(007).
                   04  JOLSR17-DGN       PIC  9(002).
               03  JOLSR17-JAN           PIC  X(013).
               03  JOLSR17-SU            PIC  9(006).
               03  JOLSR17-GTN           PIC  9(007).
               03  JOLSR17-UTN           PIC  9(007).
               03  JOLSR17-GKIN          PIC  9(010).
               03  JOLSR17-UKIN          PIC  9(010).
               03  JOLSR17-DPM           PIC  X(002).
               03  JOLSR17-CLS           PIC  X(003).
               03  JOLSR17-SHM           PIC  X(013).
               03  JOLSR17-MKH           PIC  X(010).
               03  JOLSR17-MSB           PIC  X(010).
               03  JOLSR17-TY            PIC  X(002).
               03  JOLSR17-HCD           PIC  9(006).
               03  JOLSR17-COR           PIC  N(004).
               03  JOLSR17-SIZ           PIC  X(004).
               03  JOLSR17-NSU           PIC  9(006).
               03  JOLSR17-TSC           PIC  9(001).
               03  F                     PIC  X(008).
               03  JOLSR17-CCD           PIC  9(003).
               03  JOLSR17-TNA           PIC  N(014).
               03  JOLSR17-HNO           PIC  9(009).
               03  JOLSR17-HNGP          PIC  9(006).
               03  JOLSR17-NNGP          PIC  9(006).
               03  JOLSR17-THC           PIC  9(006).
               03  JOLSR17-BI            PIC  X(010).
               03  JOLSR17-SNGP          PIC  9(008).
               03  JOLSR17-HNA           PIC  X(006).
               03  JOLSR17-ZON           PIC  X(004).
               03  JOLSR17-DC            PIC  9(002).
               03  F                     PIC  X(005).
               03  JOLSR17-DNGP          PIC  9(008).
               03  JOLSR17-NRC           PIC  9(001).
               03  F                     PIC  X(008).
               03  JOLSR17-PC            PIC  9(001).
               03  JOLSR17-RC            PIC  9(001).
               03  F                     PIC  X(083).
               03  JOLSR17-NO            PIC  9(02).
       77  F                         PIC  X(001).
