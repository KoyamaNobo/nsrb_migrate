      ***********************************************
      *****                                     *****
      **   Å@Å@ÇnÅ^ÇkëóêMÅ@ÉtÉ@ÉCÉãÇQ          Å@**
      *****         ( JOLSF2  )  256/1          *****
      ***********************************************
       01  JOLSF2.
           02  JOLSF2_PNAME1      PIC  X(006) VALUE "JOLSF2".
           02  F                  PIC  X(001).
           02  JOLSF2_LNAME       PIC  X(006) VALUE "JOLSF2".
           02  F                  PIC  X(001).
           02  JOLSF2_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLSF2_SORT        PIC  X(100) VALUE SPACE.
           02  JOLSF2_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLSF2_RES         USAGE  POINTER.
      *
      *----ÉRÉìÉgÉçÅ[ÉãÇeóp      ÅiãÊï™ÅÅÇOÇPÅj
       01  JOLSF21-REC.
           02  JOLSF21-01         PIC  9(02).                           ÇqÇbãÊï™
           02  JOLSF221-KEYW.
               03  JOLSF21-02     PIC  9(01).                           ID
               03  JOLSF21-03     PIC  9(01).                           â^&ëqCD
           02  JOLSF21-04         PIC  N(06).                           â^&ëqñº
           02  JOLSF21-05         PIC  X(18).                           FILLER
           02  F                  PIC  X(221).
           02  JOLSF21-99         PIC  9(01).                           ëóêMêÊ
      *
      *----íºëóêÊÉ}ÉXÉ^óp        ÅiãÊï™ÅÅÇOÇQÅj
       01  JOLSF22-REC.
           02  JOLSF22-01         PIC  9(02).                           ÇqÇbãÊï™
           02  JOLSF22-KEYW.
               03  JOLSF22-02     PIC  9(04).                           ìæà”êÊCD
               03  JOLSF22-03     PIC  9(03).                           íºëóêÊCD
           02  JOLSF22-04         PIC  N(26).
           02  JOLSF22-05         PIC  N(20).
           02  JOLSF22-06         PIC  N(20).
           02  JOLSF22-07         PIC  X(08).
           02  JOLSF22-08         PIC  X(14).
           02  JOLSF22-09         PIC  9(02).                           ï{åß∫∞ƒﬁ
           02  JOLSF22-10         PIC  9(01).                           â^ëó∫∞ƒﬁ
           02  JOLSF22-11         PIC  X(28).
           02  JOLSF22-12         PIC  9(01).                           ACT
           02  F                  PIC  X(60).
           02  JOLSF22-99         PIC  9(01).                           ëóêMêÊ
      *
      *----ïiñºÉtÉ@ÉCÉã          ÅiãÊï™ÅÅÇOÇRÅj
       01  JOLSF23-REC.
           02  JOLSF23-01         PIC  9(02).                           ÇqÇbãÊï™
           02  JOLSF23-KEYW.
             03  JOLSF23-MHCD     PIC  9(006).
             03  JOLSF23-HCD      PIC  9(006).
           02  JOLSF23-NAME       PIC  N(024).
           02  JOLSF23-BC.
             03  JOLSF23-BC1      PIC  9(002).
             03  JOLSF23-BC2      PIC  9(002).
             03  JOLSF23-BC3      PIC  9(002).
           02  JOLSF23-ASSD.
             03  JOLSF23-SSD   OCCURS  4.
               04  JOLSF23-SS     PIC  9(010).
           02  JOLSF23-ASKD  REDEFINES JOLSF23-ASSD.
             03  JOLSF23-SKD   OCCURS  4.
               04  JOLSF23-SK    OCCURS 10.
                 05  JOLSF23-S    PIC  9(001).
           02  JOLSF23-AHSD  REDEFINES JOLSF23-ASSD.
             03  JOLSF23-HSD.
               04  JOLSF23-SS1    PIC  9(010).
               04  JOLSF23-SD1   REDEFINES JOLSF23-SS1.
                 05  JOLSF23-S1    OCCURS  10  PIC  9(001).
               04  JOLSF23-SS2    PIC  9(010).
               04  JOLSF23-SD2    REDEFINES JOLSF23-SS2.
                 05  JOLSF23-S2    OCCURS  10  PIC  9(001).
               04  JOLSF23-SS3    PIC  9(010).
               04  JOLSF23-SD3    REDEFINES JOLSF23-SS3.
                 05  JOLSF23-S3    OCCURS  10  PIC  9(001).
               04  JOLSF23-SS4    PIC  9(010).
               04  JOLSF23-SD4    REDEFINES JOLSF23-SS4.
                 05  JOLSF23-S4    OCCURS  10  PIC  9(001).
           02  JOLSF23-SB         PIC  9(005).
           02  JOLSF23-FT         PIC  9(005).
           02  F                  PIC  X(019).
           02  JOLSF23-KT         PIC  9(005).
           02  JOLSF23-TCD        PIC  9(004).
           02  JOLSF23-ISU        PIC  9(003).
           02  JOLSF23-10         PIC  9(01).                           ACT
           02  JOLSF23-SCC        PIC  9(001).
           02  JOLSF23-BMC        PIC  9(002).
           02  JOLSF23-BMNO       PIC  9(001).
           02  JOLSF23-YG         PIC  9(005).
           02  JOLSF23-HKB        PIC  9(001).
           02  JOLSF23-HPV        PIC  9(001).
           02  JOLSF23-BC4        PIC  9(001).
           02  F                  PIC  X(011).
           02  JOLSF23-SMS        PIC  N(016).
           02  JOLSF23-UNG        PIC  9(006).
           02  JOLSF23-NNG        PIC  9(006).
           02  F                  PIC  X(001).
           02  JOLSF23-CS         PIC  N(010).
           02  F                  PIC  X(003).
           02  JOLSF23-DNG        PIC  9(006).
           02  JOLSF23-SNG        PIC  9(004).
           02  JOLSF23-ENG        PIC  9(004).
           02 JOLSF23-99          PIC  9(01).                           ëóêMêÊ
      *
      *----ÉèÅ[ÉNÉ}ÉììXñº        ÅiãÊï™ÅÅÇOÇSÅj
       01  JOLSF24-REC.
           02  JOLSF24-01         PIC  9(02).
           02  JOLSF24-KEYW.
             03  JOLSF24-TNC      PIC  9(004).
           02  JOLSF24-NAME       PIC  N(026).
           02  JOLSF24-BSC        PIC  9(001).
           02  F                  PIC  X(007).
           02 JOLSF24-10          PIC  9(01).
           02  F                  PIC  X(188).
           02 JOLSF24-99          PIC  9(01).
      *
      *----èoâ◊éwé¶ÉgÉâÉì        ÅiãÊï™ÅÅÇPÇPÅj
       01  JOLSF211-REC.
           02  JOLSF211-01        PIC 9(02).                            ÇqÇbãÊï™
           02  JOLSF211-KEYW.
               03  JOLSF211-02    PIC 9(06).                               º≠Ø∂º
               03  JOLSF211-03    PIC 9(01).                               ∑ﬁÆ≥
           02  JOLSF211-04        PIC 9(01).                               √ﬁ›∏
           02  JOLSF211-05.                                                º≠Ø∂À
               03  JOLSF211-051   PIC 9(04).
               03  JOLSF211-052   PIC 9(02).                               ¬∑
               03  JOLSF211-053   PIC 9(02).                               À
           02  JOLSF211-06.                                                º≠Ø∂À
               03  JOLSF211-061   PIC 9(04).
               03  JOLSF211-062   PIC 9(02).                               ¬∑
               03  JOLSF211-063   PIC 9(02).                               À
           02  JOLSF211-07.                                                ¡Æ∏ø≥
               03  JOLSF211-071   PIC 9(04).                               ƒ∏≤∫∞
               03  JOLSF211-072   PIC 9(03).                               ¡Æ∏ N
           02  JOLSF211-08        PIC 9(01).                               ∏◊ ∫∞
           02  JOLSF211-09.                                                ºﬁ≠¡≠
               03  JOLSF211-091   PIC 9(06).                               ºﬁ≠¡≠
               03  JOLSF211-092   PIC 9(01).                               ∑ﬁÆ≥
           02  JOLSF211-10        PIC 9(06).                               À›∫∞ƒ
           02  JOLSF211-11        PIC 9(01).                               ª≤Ωﬁ∏
           02  JOLSF211-12.                                                º≠Ø∂ª
               03  JOLSF211-121   OCCURS  10.                              ª≤ΩﬁÕ
                   04  JOLSF211-1211      PIC  9(04).
                   04  JOLSF211-1211S     PIC  9(01).                   0:+,1:-
               03  JOLSF211-122   PIC  9(05).
               03  JOLSF211-122S  PIC  9(01).                           0:+,1:-
           02  JOLSF211-13.                                                º≠Ø∂º
               03  JOLSF211-131   OCCURS  10.                              ª≤ΩﬁÕ
                   04  JOLSF211-1311      PIC  9(04).
                   04  JOLSF211-1311S     PIC  9(01).                   0:+,1:-
               03  JOLSF211-132   PIC  9(05).
               03  JOLSF211-132S  PIC  9(01).                           0:+,1:-
           02  JOLSF211-14        PIC 9(01).                               ±Ωﬁ∂ÿ
           02  JOLSF211-15        PIC 9(01).                               â^ëó
           02  JOLSF211-15A       PIC 9(03).                               ÉZÉb
           02  JOLSF211-15B       PIC 9(06).                               ëóÇË
           02  JOLSF211-15C       PIC 9(02).                               é}î‘
           02  JOLSF211-15D       PIC N(09).                               îzíB
           02  JOLSF211-16        PIC N(23).                               ìEóv
           02  JOLSF211-20        PIC X(10).
           02  JOLSF211-16A       PIC  9(03).                           å¬êî
           02  JOLSF211-16AS      PIC  9(01).                           0:+,1:-
           02  FILLER             PIC X(01).
           02  JOLSF211-19        PIC X(01).                            èàóùïîèê
           02  JOLSF211-168       PIC 9(01).                            àÛéöª≤›
           02  JOLSF211-17        PIC 9(01).                            àÍî ã≥àÁ
           02  JOLSF211-18        PIC 9(01).                            çXêVª≤›
      *
      *----â◊éDÉgÉâÉì            ÅiãÊï™ÅÅÇPÇQÅj
       01  JOLSF212-REC.
           02  JOLSF212-01        PIC 9(02).                            ÇqÇbãÊï™
      *
           02  JOLSF2121-A.                                             çsNOT=7
               03  JOLSF2121-1KEYW.
                   04  JOLSF2121-01  PIC 9(6).                             √ﬁ›Àﬂ
                   04  JOLSF2121-02  PIC 9(1).                             ∑ﬁÆ≥
               03  JOLSF2121-03   PIC 9(6).                                À›∫∞ƒ
               03  JOLSF2121-04.                                            Øø≥À
                   04  JOLSF2121-041 PIC 9(2).                             »›
                   04  JOLSF2121-042 PIC 9(2).                             ¬∑
                   04  JOLSF2121-043 PIC 9(2).                             À
               03  JOLSF2121-05.                                           ¡Æ∏∫∞
                   04  JOLSF2121-051 PIC 9(4).                             ƒ∏≤∫∞
                   04  JOLSF2121-052 PIC 9(3).                             ¡Æ∏ N
               03  JOLSF2121-06   PIC 9(1).                                ≥›ø≥
               03  JOLSF2121-07   PIC 9(1).                                ø≥∫∞ƒ
               03  JOLSF2121-08   PIC 9(3).                                ∫Ω≥
               03  JOLSF2121-08S  PIC 9(1).                             0:+,1:-
               03  JOLSF2121-09  OCCURS  27.                               ºØ∂Ω≥
                   04  JOLSF2121-091  PIC  9(3).                           ª≤ΩﬁÕ
                   04  JOLSF2121-091S PIC  9(1).                        0:+,1:-
               03  JOLSF2121-10   PIC 9(1).                                ≤›ºﬁª
               03  JOLSF2121-11   PIC 9(1).                                ∆≠≥ÿÆ
               03  JOLSF2121-12   PIC 9(1).                                º≠Ø∂ª
               03  JOLSF2121-13   PIC 9(3).                                œ≤Ω≥
               03  JOLSF2121-13S  PIC 9(1).                             0:+,1:-
               03  JOLSF2121-13A  PIC 9(01).                            àÍî ã≥àÁ
               03  FILLER         PIC X(2).
               03  JOLSF2121-14   PIC 9(6).                                µ∏ÿºﬁ
               03  FILLER         PIC X(98).
      *
           02  JOLSF2122-A        REDEFINES  JOLSF2121-A.               çsÅÅÇV
               03  JOLSF2122-1KEYW.
                   04  JOLSF2122-01  PIC 9(6).                             √ﬁ›Àﬂ
                   04  JOLSF2122-02  PIC 9(1).                             ∑ﬁÆ≥
               03  JOLSF2122-02A  PIC N(9).                                 ≤¿¬
               03  JOLSF2122-03   PIC N(23).                               √∑÷≥
               03  FILLER         PIC X(69).
               03  JOLSF2122-04   PIC 9(1).                                ≤›ºﬁª
               03  JOLSF2122-05   PIC 9(1).                                ∆Æ≥ÿÆ
               03  JOLSF2122-06   PIC 9(1).                                º≠Ø∂ª
               03  JOLSF2122-07   PIC 9(3).                                œ≤Ω≥
               03  JOLSF2122-07S  PIC 9(1).                             0:+,1:-
               03  JOLSF2122-07A  PIC 9(1).                             àÍî ã≥àÁ
               03  FILLER         PIC X(2).
               03  JOLSF2122-08   PIC 9(6).                                µ∏ÿºﬁ
               03  FILLER         PIC X(98).
      *
      *----ëóÇËèÛÉtÉ@ÉCÉã        ÅiãÊï™ÅÅÇPÇRÅj
       01  JOLSF213-REC.
           02  JOLSF213-01        PIC 9(02).                            ÇqÇbãÊï™
           02  JOLSF213-TBL       OCCURS  4.
               03  JOLSF213-KEY   .
                 04  JOLSF213-02  PIC 9(06).                               ëóÇË
               03  JOLSF213-03    PIC 9(01).                               â^ëó
               03  JOLSF213-04    PIC 9(06).                               îNåé
               03  JOLSF213-05    PIC 9(01).                               ëqå…
               03  JOLSF213-06    PIC 9(07).                               íºëó
               03  JOLSF213-07    PIC N(09).                               îzíB
               03  JOLSF213-08    PIC 9(03).                               å¬êî
               03  JOLSF213-09    PIC 9(01).                               àÛéö
               03  JOLSF213-10    PIC 9(01).                               ãÊï™
               03  JOLSF213-11    PIC 9(01).                               çXêV
               03  JOLSF213-12    PIC 9(05).
               03  JOLSF213-13    PIC 9(06).
               03  F              PIC X(07).
           02  F                  PIC X(02).
      *----ÉèÅ[ÉNÉ}Éìì`ï[Çe      ÅiãÊï™ÅÅÇPÇSÅj
       01  JOLSF214-REC.
           02  JOLSF214-01            PIC 9(02).
      *
           02  JOLSF2141-A.
               03  JOLSF2141-KEY.
                 04  JOLSF2141-STC.
                   05  F              PIC  9(002).
                   05  JOLSF2141-SCD  PIC  9(002).
                   05  F              PIC  9(001).
                   05  JOLSF2141-TCD  PIC  9(004).
                 04  JOLSF2141-DNO.
                   05  F              PIC  9(002).
                   05  JOLSF2141-DNOD PIC  9(007).
                 04  JOLSF2141-DGN    PIC  9(002).
               03  JOLSF2141-BC.
                 04  JOLSF2141-BCD    PIC  9(002).
                 04  F                PIC  9(001).
               03  JOLSF2141-SHC      PIC  9(001).
               03  JOLSF2141-DPC      PIC  X(002).
               03  JOLSF2141-HNGP     PIC  9(006).
               03  JOLSF2141-NNGP     PIC  9(006).
               03  JOLSF2141-THC      PIC  9(006).
               03  JOLSF2141-MHC      PIC  X(001).
               03  F                  PIC  X(001).
               03  JOLSF2141-SNA      PIC  X(020).
               03  JOLSF2141-TNA      PIC  X(020).
               03  JOLSF2141-HCC      PIC  9(001).
               03  JOLSF2141-HSP      PIC  X(001).
               03  JOLSF2141-DHC      PIC  X(001).
               03  JOLSF2141-KHC      PIC  X(001).
               03  JOLSF2141-KCC      PIC  X(001).
               03  JOLSF2141-UBC      PIC  X(001).
               03  JOLSF2141-NCC      PIC  9(001).
               03  JOLSF2141-EDI      PIC  9(001).
               03  JOLSF2141-NKC      PIC  9(001).
               03  JOLSF2141-ZAC      PIC  9(001).
               03  F                  PIC  X(157).
               03  JOLSF2141-PC       PIC  9(001).
      *
           02  JOLSF2142-A        REDEFINES  JOLSF2141-A.
               03  JOLSF2142-KEY.
                 04  JOLSF2142-STC.
                   05  F              PIC  9(002).
                   05  JOLSF2142-SCD  PIC  9(002).
                   05  F              PIC  9(001).
                   05  JOLSF2142-TCD  PIC  9(004).
                 04  JOLSF2142-DNO.
                   05  F              PIC  9(002).
                   05  JOLSF2142-DNOD PIC  9(007).
                 04  JOLSF2142-DGN    PIC  9(002).
               03  JOLSF2142-HCD      PIC  X(013).
               03  JOLSF2142-ISU      PIC  9(003)V9(01).
               03  JOLSF2142-KSU      PIC  9(004).
               03  JOLSF2142-HTC      PIC  X(002).
               03  JOLSF2142-SU       PIC  9(005)V9(01).
               03  JOLSF2142-GTN      PIC  9(007)V9(02).
               03  JOLSF2142-UTN      PIC  9(007).
               03  JOLSF2142-GKIN     PIC  9(010).
               03  JOLSF2142-UKIN     PIC  9(010).
               03  JOLSF2142-GCN      PIC  9(006).
               03  JOLSF2142-CCD      PIC  X(003).
               03  JOLSF2142-SHN      PIC  X(025).
               03  JOLSF2142-JAN      PIC  X(013).
               03  F                  PIC  X(004).
               03  JOLSF2142-TSH      PIC  9(005).
               03  JOLSF2142-TKC      PIC  X(001).
               03  F                  PIC  X(001).
               03  F                  PIC  X(110).
               03  JOLSF2142-PC       PIC  9(001).
      *----ÉiÉtÉRì`ï[Çe Å@Å@     ÅiãÊï™ÅÅÇPÇTÅj
       01  JOLSF215-REC.
           02  JOLSF215-01            PIC 9(02).
      *
           02  JOLSF2151-A.
               03  JOLSF2151-KEY.
                 04  JOLSF2151-STC.
                   05  JOLSF2151-SCD  PIC  9(002).
                   05  JOLSF2151-TCD  PIC  9(003).
                   05  F              PIC  X(004).
                 04  JOLSF2151-DNO.
                   05  F              PIC  X(002).
                   05  JOLSF2151-DNOD PIC  9(007).
                 04  JOLSF2151-DGN    PIC  9(002).
               03  F                  PIC  X(002).
               03  JOLSF2151-BCD      PIC  9(002).
               03  JOLSF2151-DPC      PIC  9(002).
               03  JOLSF2151-HNGP     PIC  9(006).
               03  JOLSF2151-HNGPD REDEFINES JOLSF2151-HNGP.
                 04  JOLSF2151-HNEN   PIC  9(002).
                 04  JOLSF2151-HGET   PIC  9(002).
                 04  JOLSF2151-HPEY   PIC  9(002).
               03  JOLSF2151-NNGP     PIC  9(006).
               03  JOLSF2151-NNGPD REDEFINES JOLSF2151-NNGP.
                 04  JOLSF2151-NNEN   PIC  9(002).
                 04  JOLSF2151-NGET   PIC  9(002).
                 04  JOLSF2151-NPEY   PIC  9(002).
               03  JOLSF2151-THC      PIC  9(006).
               03  JOLSF2151-STA      PIC  X(002).
               03  JOLSF2151-SNA      PIC  X(015).
               03  JOLSF2151-TNA      PIC  X(015).
               03  JOLSF2151-TSN      PIC  X(015).
               03  JOLSF2151-TST      PIC  X(012).
               03  JOLSF2151-HCC      PIC  9(001).
               03  F                  PIC  X(024).
               03  JOLSF2151-AR       PIC  X(007).
               03  JOLSF2151-DUR      PIC  X(026).
               03  JOLSF2151-DSHR     PIC  X(014).
               03  JOLSF2151-DSMR     PIC  X(007).
               03  JOLSF2151-ER       PIC  X(005).
               03  JOLSF2151-FSR      PIC  X(015).
               03  JOLSF2151-FUR      PIC  X(007).
               03  JOLSF2151-LCR      PIC  X(016).
               03  JOLSF2151-LUR      PIC  X(020).
               03  JOLSF2151-LSR      PIC  X(007).
               03  F                  PIC  X(001).
               03  JOLSF2151-PC       PIC  9(001).
      *
           02  JOLSF2152-A        REDEFINES  JOLSF2151-A.
               03  JOLSF2152-KEY.
                 04  JOLSF2152-STC.
                   05  JOLSF2152-SCD  PIC  9(002).
                   05  JOLSF2152-TCD  PIC  9(003).
                   05  F              PIC  X(004).
                 04  JOLSF2152-DNO.
                   05  F              PIC  X(002).
                   05  JOLSF2152-DNOD PIC  9(007).
                 04  JOLSF2152-DGN    PIC  9(002).
               03  JOLSF2152-JAN      PIC  X(013).
               03  JOLSF2152-GAR      PIC  X(006).
               03  F                  PIC  X(001).
               03  JOLSF2152-TNI      PIC  X(003).
               03  JOLSF2152-SU       PIC  9(005).
               03  F                  PIC  X(001).
               03  JOLSF2152-GTN      PIC  9(007).
               03  F                  PIC  X(002).
               03  JOLSF2152-UTN      PIC  9(007).
               03  JOLSF2152-GKIN     PIC  9(010).
               03  JOLSF2152-UKIN     PIC  9(010).
               03  F                  PIC  X(009).
               03  JOLSF2152-SHN      PIC  X(025).
               03  JOLSF2152-HSC      PIC  X(008).
               03  JOLSF2152-COR      PIC  X(006).
               03  JOLSF2152-SIZ      PIC  X(005).
               03  F                  PIC  X(004).
               03  JOLSF2152-KKK      PIC  X(025).
               03  JOLSF2152-PCH      PIC  X(001).
               03  JOLSF2152-PSI      PIC  X(001).
               03  JOLSF2152-PBM      PIC  9(002).
               03  JOLSF2152-PJAN     PIC  X(013).
               03  JOLSF2152-PSHN     PIC  X(020).
               03  JOLSF2152-PKKK     PIC  X(020).
               03  JOLSF2152-PUTN     PIC  9(007).
               03  JOLSF2152-PMS      PIC  9(005).
               03  F                  PIC  X(017).
               03  JOLSF2152-PC       PIC  9(001).
      *----ÉgÉâÉXÉRëºìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇUÅj
       01  JOLSF216-REC.
           02  JOLSF216-01            PIC 9(02).
           02  JOLSF216-A.
               03  JOLSF216-KEY.
                   04  JOLSF216-DNO   PIC  9(006).
                   04  JOLSF216-GNO   PIC  9(001).
               03  JOLSF216-DATE      PIC  9(006).
               03  JOLSF216-TCD       PIC  9(004).
               03  JOLSF216-CCD       PIC  9(003).
               03  JOLSF216-TPC       PIC  9(004).
               03  JOLSF216-HCD       PIC  9(006).
               03  JOLSF216-SIZ       PIC  X(003).
               03  JOLSF216-SKB       PIC  9(001).
               03  JOLSF216-SNO       PIC  9(002).
               03  JOLSF216-SU        PIC  9(005).
               03  JOLSF216-SUS       PIC  9(001).
               03  JOLSF216-GT        PIC  9(007).
               03  JOLSF216-UT        PIC  9(007).
               03  JOLSF216-GKIN      PIC  9(008).
               03  JOLSF216-GKINS     PIC  9(001).
               03  JOLSF216-UKIN      PIC  9(008).
               03  JOLSF216-UKINS     PIC  9(001).
               03  JOLSF216-JNOD.
                   04  JOLSF216-JNO   PIC  9(006).
                   04  JOLSF216-JGN   PIC  9(001).
               03  JOLSF216-SOK       PIC  9(001).
               03  JOLSF216-UNS       PIC  9(001).
               03  JOLSF216-ISU       PIC  9(003).
               03  JOLSF216-HNO       PIC  X(010).
               03  JOLSF216-TEKI      PIC  N(028).
               03  JOLSF216-TED   REDEFINES JOLSF216-TEKI.
                   04  JOLSF216-THT   PIC  N(009).
                   04  JOLSF216-TTE   PIC  N(019).
               03  JOLSF216-TRN       PIC  X(020).
               03  JOLSF216-JAN       PIC  X(013).
               03  JOLSF216-PRC       PIC  9(001).
               03  JOLSF216-UPC       PIC  9(001).
               03  F                  PIC  X(067).
      *----ê‘ÇøÇ·ÇÒñ{ï‹ìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇVÅj
       01  JOLSF217-REC.
           02  JOLSF217-01           PIC 9(02).
           02  JOLSF217-KEY.
               03  JOLSF217-STC      PIC  9(007).
               03  JOLSF217-DNO      PIC  9(007).
               03  JOLSF217-DGN      PIC  9(002).
           02  JOLSF217-JAN          PIC  X(013).
           02  JOLSF217-SU           PIC  9(006).
           02  JOLSF217-SUS          PIC  9(001).
           02  JOLSF217-GTN          PIC  9(007).
           02  JOLSF217-UTN          PIC  9(007).
           02  JOLSF217-GKIN         PIC  9(010).
           02  JOLSF217-GKINS        PIC  9(001).
           02  JOLSF217-UKIN         PIC  9(010).
           02  JOLSF217-UKINS        PIC  9(001).
           02  JOLSF217-DPM          PIC  X(002).
           02  JOLSF217-CLS          PIC  X(003).
           02  JOLSF217-SHM          PIC  X(013).
           02  JOLSF217-MKH          PIC  X(010).
           02  JOLSF217-MSB          PIC  X(010).
           02  JOLSF217-TY           PIC  X(002).
           02  JOLSF217-HCD          PIC  9(006).
           02  JOLSF217-COR          PIC  N(004).
           02  JOLSF217-SIZ          PIC  X(004).
           02  JOLSF217-NSU          PIC  9(006).
           02  JOLSF217-TSC          PIC  9(001).
           02  F                     PIC  X(008).
           02  JOLSF217-CCD          PIC  9(003).
           02  JOLSF217-TNA          PIC  N(014).
           02  JOLSF217-HNO          PIC  9(009).
           02  JOLSF217-HNGP         PIC  9(006).
           02  JOLSF217-NNGP         PIC  9(006).
           02  JOLSF217-THC          PIC  9(006).
           02  JOLSF217-BI           PIC  X(010).
           02  JOLSF217-SNGP         PIC  9(008).
           02  JOLSF217-HNA          PIC  X(006).
           02  JOLSF217-ZON          PIC  X(004).
           02  JOLSF217-DC           PIC  9(002).
           02  F                     PIC  X(002).
           02  JOLSF217-DNGP         PIC  9(008).
           02  JOLSF217-NRC          PIC  9(001).
           02  F                     PIC  X(008).
           02  JOLSF217-PC           PIC  9(001).
           02  JOLSF217-RC           PIC  9(001).
       77  F                         PIC  X(001).
