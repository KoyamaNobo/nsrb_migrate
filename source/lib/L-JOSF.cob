      ***********************************************
      *****                                     *****
      **   Å@Å@ÇnÅ^ÇkëóêMÅ@ÉtÉ@ÉCÉãÅ@          Å@**
      *****         ( JOLSF   )  256/1          *****
      ***********************************************
       01  JOLSF.
           02  JOLSF_PNAME1      PIC  X(005) VALUE "JOLSF".
           02  F                 PIC  X(001).
           02  JOLSF_LNAME       PIC  X(005) VALUE "JOLSF".
           02  F                 PIC  X(001).
           02  JOLSF_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLSF_SORT        PIC  X(100) VALUE SPACE.
           02  JOLSF_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLSF_RES         USAGE  POINTER.
      *
       01  JOLSF-REC.
      *----ÉRÉìÉgÉçÅ[ÉãÇeóp      ÅiãÊï™ÅÅÇOÇPÅj
           02  JOLSF1-REC.
               03  JOLSF1-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSF1-KEYW.
                   04  JOLSF1-02     PIC  9(01).                            ID
                   04  JOLSF1-03     PIC  9(01).                            â^&ëqCD
               03  JOLSF1-04         PIC  N(06).                            â^&ëqñº
               03  JOLSF1-05         PIC  X(18).                            FILLER
               03  F                 PIC  X(221).
               03  JOLSF1-99         PIC  9(01).                            ëóêMêÊ
      *
      *----íºëóêÊÉ}ÉXÉ^óp        ÅiãÊï™ÅÅÇOÇQÅj
           02  JOLSF2-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF2-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSF2-KEYW.
                   04  JOLSF2-02     PIC  9(04).                            ìæà”êÊCD
                   04  JOLSF2-03     PIC  9(03).                            íºëóêÊCD
               03  JOLSF2-04         PIC  N(26).
               03  JOLSF2-05         PIC  N(20).
               03  JOLSF2-06         PIC  N(20).
               03  JOLSF2-07         PIC  X(08).
               03  JOLSF2-08         PIC  X(14).
               03  JOLSF2-09         PIC  9(02).                            ï{åß∫∞ƒﬁ
               03  JOLSF2-10         PIC  9(01).                            â^ëó∫∞ƒﬁ
               03  JOLSF2-11         PIC  X(28).
               03  JOLSF2-12         PIC  9(01).                            ACT
               03  F                 PIC  X(60).
               03  JOLSF2-99         PIC  9(01).                            ëóêMêÊ
      *
      *----ïiñºÉtÉ@ÉCÉã          ÅiãÊï™ÅÅÇOÇRÅj
           02  JOLSF3-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF3-01         PIC  9(02).                            ÇqÇbãÊï™
               03  JOLSF3-KEYW.
                 04  JOLSF3-MHCD     PIC  9(006).
                 04  JOLSF3-HCD      PIC  9(006).
               03  JOLSF3-NAME       PIC  N(024).
               03  JOLSF3-BC.
                 04  JOLSF3-BC1      PIC  9(002).
                 04  JOLSF3-BC2      PIC  9(002).
                 04  JOLSF3-BC3      PIC  9(002).
               03  JOLSF3-ASSD.
                 04  JOLSF3-SSD   OCCURS  4.
                   05  JOLSF3-SS     PIC  9(010).
               03  JOLSF3-ASKD  REDEFINES JOLSF3-ASSD.
                 04  JOLSF3-SKD   OCCURS  4.
                   05  JOLSF3-SK    OCCURS 10.
                     06  JOLSF3-S    PIC  9(001).
               03  JOLSF3-AHSD  REDEFINES JOLSF3-ASSD.
                 04  JOLSF3-HSD.
                   05  JOLSF3-SS1    PIC  9(010).
                   05  JOLSF3-SD1   REDEFINES JOLSF3-SS1.
                     06  JOLSF3-S1    OCCURS  10  PIC  9(001).
                   05  JOLSF3-SS2    PIC  9(010).
                   05  JOLSF3-SD2    REDEFINES JOLSF3-SS2.
                     06  JOLSF3-S2    OCCURS  10  PIC  9(001).
                   05  JOLSF3-SS3    PIC  9(010).
                   05  JOLSF3-SD3    REDEFINES JOLSF3-SS3.
                     06  JOLSF3-S3    OCCURS  10  PIC  9(001).
                   05  JOLSF3-SS4    PIC  9(010).
                   05  JOLSF3-SD4    REDEFINES JOLSF3-SS4.
                     06  JOLSF3-S4    OCCURS  10  PIC  9(001).
               03  JOLSF3-SB         PIC  9(005).
               03  JOLSF3-FT         PIC  9(005).
               03  F                 PIC  X(019).
               03  JOLSF3-KT         PIC  9(005).
               03  JOLSF3-TCD        PIC  9(004).
               03  JOLSF3-ISU        PIC  9(003).
               03  JOLSF3-10         PIC  9(01).                            ACT
               03  JOLSF3-SCC        PIC  9(001).
               03  JOLSF3-BMC        PIC  9(002).
               03  JOLSF3-BMNO       PIC  9(001).
               03  JOLSF3-YG         PIC  9(005).
               03  JOLSF3-HKB        PIC  9(001).
               03  JOLSF3-HPV        PIC  9(001).
               03  JOLSF3-BC4        PIC  9(001).
               03  F                 PIC  X(011).
               03  JOLSF3-SMS        PIC  N(016).
               03  JOLSF3-UNG        PIC  9(006).
               03  JOLSF3-NNG        PIC  9(006).
               03  F                 PIC  X(001).
               03  JOLSF3-CS         PIC  N(010).
               03  F                 PIC  X(003).
               03  JOLSF3-DNG        PIC  9(006).
               03  JOLSF3-SNG        PIC  9(004).
               03  JOLSF3-ENG        PIC  9(004).
               03 JOLSF3-99          PIC  9(01).                            ëóêMêÊ
      *
      *----ÉèÅ[ÉNÉ}ÉììXñº        ÅiãÊï™ÅÅÇOÇSÅj
           02  JOLSF4-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF4-01         PIC  9(02).
               03  JOLSF4-KEYW.
                 04  JOLSF4-TNC      PIC  9(004).
               03  JOLSF4-NAME       PIC  N(026).
               03  JOLSF4-BSC        PIC  9(001).
               03  F                 PIC  X(007).
               03 JOLSF4-10          PIC  9(01).
               03  F                 PIC  X(188).
               03 JOLSF4-99          PIC  9(01).
      *
      *----èoâ◊éwé¶ÉgÉâÉì        ÅiãÊï™ÅÅÇPÇPÅj
           02  JOLSF11-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF11-01        PIC 9(02).                             ÇqÇbãÊï™
               03  JOLSF11-KEYW.
                   04  JOLSF11-02    PIC 9(06).                                º≠Ø∂º
                   04  JOLSF11-03    PIC 9(01).                                ∑ﬁÆ≥
               03  JOLSF11-04        PIC 9(01).                                √ﬁ›∏
               03  JOLSF11-05.                                                 º≠Ø∂À
                   04  JOLSF11-051   PIC 9(04).
                   04  JOLSF11-052   PIC 9(02).                                ¬∑
                   04  JOLSF11-053   PIC 9(02).                                À
               03  JOLSF11-06.                                                 º≠Ø∂À
                   04  JOLSF11-061   PIC 9(04).
                   04  JOLSF11-062   PIC 9(02).                                ¬∑
                   04  JOLSF11-063   PIC 9(02).                                À
               03  JOLSF11-07.                                                 ¡Æ∏ø≥
                   04  JOLSF11-071   PIC 9(04).                                ƒ∏≤∫∞
                   04  JOLSF11-072   PIC 9(03).                                ¡Æ∏ N
               03  JOLSF11-08        PIC 9(01).                                ∏◊ ∫∞
               03  JOLSF11-09.                                                 ºﬁ≠¡≠
                   04  JOLSF11-091   PIC 9(06).                                ºﬁ≠¡≠
                   04  JOLSF11-092   PIC 9(01).                                ∑ﬁÆ≥
               03  JOLSF11-10        PIC 9(06).                                À›∫∞ƒ
               03  JOLSF11-11        PIC 9(01).                                ª≤Ωﬁ∏
               03  JOLSF11-12.                                                 º≠Ø∂ª
                   04  JOLSF11-121   OCCURS  10.                               ª≤ΩﬁÕ
                       05  JOLSF11-1211      PIC S9(04).
                   04  JOLSF11-122   PIC S9(05).
               03  JOLSF11-13.                                                 º≠Ø∂º
                   04  JOLSF11-131   OCCURS  10.                               ª≤ΩﬁÕ
                       05  JOLSF11-1311      PIC S9(04).
                   04  JOLSF11-132   PIC S9(05).
               03  JOLSF11-14        PIC 9(01).                                ±Ωﬁ∂ÿ
               03  JOLSF11-15        PIC 9(01).                                â^ëó
               03  JOLSF11-15A       PIC 9(03).                                ÉZÉb
               03  JOLSF11-15B       PIC 9(06).                                ëóÇË
               03  JOLSF11-15C       PIC 9(02).                                é}î‘
               03  JOLSF11-15D       PIC N(09).                                îzíB
               03  JOLSF11-16        PIC N(23).                                ìEóv
               03  JOLSF11-20        PIC X(10).
               03  JOLSF11-16A       PIC S9(03).                            å¬êî
               03  FILLER            PIC X(24).
               03  JOLSF11-19        PIC X(01).                             èàóùïîèê
               03  JOLSF11-168       PIC 9(01).                             àÛéöª≤›
               03  JOLSF11-17        PIC 9(01).                             àÍî ã≥àÁ
               03  JOLSF11-18        PIC 9(01).                             çXêVª≤›
      *
      *----â◊éDÉgÉâÉì            ÅiãÊï™ÅÅÇPÇQÅj
           02  JOLSF12-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF12-01        PIC 9(02).                             ÇqÇbãÊï™
      *
               03  JOLSF121-A.                                              çsNOT=7
                   04  JOLSF121-1KEYW.
                       05  JOLSF121-01  PIC 9(6).                              √ﬁ›Àﬂ
                       05  JOLSF121-02  PIC 9(1).                              ∑ﬁÆ≥
                   04  JOLSF121-03   PIC 9(6).                                 À›∫∞ƒ
                   04  JOLSF121-04.                                             Øø≥À
                       05  JOLSF121-041 PIC 9(2).                              »›
                       05  JOLSF121-042 PIC 9(2).                              ¬∑
                       05  JOLSF121-043 PIC 9(2).                              À
                   04  JOLSF121-05.                                            ¡Æ∏∫∞
                       05  JOLSF121-051 PIC 9(4).                              ƒ∏≤∫∞
                       05  JOLSF121-052 PIC 9(3).                              ¡Æ∏ N
                   04  JOLSF121-06   PIC 9(1).                                 ≥›ø≥
                   04  JOLSF121-07   PIC 9(1).                                 ø≥∫∞ƒ
                   04  JOLSF121-08   PIC S9(3).                                ∫Ω≥
                   04  JOLSF121-09  OCCURS  27.                                ºØ∂Ω≥
                       05  JOLSF121-091 PIC S9(3).                             ª≤ΩﬁÕ
                   04  JOLSF121-10   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSF121-11   PIC 9(1).                                 ∆≠≥ÿÆ
                   04  JOLSF121-12   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSF121-13   PIC S9(3).                                œ≤Ω≥
                   04  JOLSF121-13A  PIC 9(01).                             àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSF121-14   PIC 9(6).                                 µ∏ÿºﬁ
      *
               03  JOLSF122-A        REDEFINES  JOLSF121-A.                 çsÅÅÇV
                   04  JOLSF122-1KEYW.
                       05  JOLSF122-01  PIC 9(6).                              √ﬁ›Àﬂ
                       05  JOLSF122-02  PIC 9(1).                              ∑ﬁÆ≥
                   04  JOLSF122-02A  PIC N(9).                                  ≤¿¬
                   04  JOLSF122-03   PIC N(23).                                √∑÷≥
                   04  FILLER        PIC X(41).
                   04  JOLSF122-04   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSF122-05   PIC 9(1).                                 ∆Æ≥ÿÆ
                   04  JOLSF122-06   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSF122-07   PIC S9(3).                                œ≤Ω≥
                   04  JOLSF122-07A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSF122-08   PIC 9(6).                                 µ∏ÿºﬁ
      *
               03  JOLSF121-B.                                              çsNOT=7
                   04  JOLSF121-2KEYW.
                       05  JOLSF121-21  PIC 9(6).                              √ﬁ›Àﬂ
                       05  JOLSF121-22  PIC 9(1).                              ∑ﬁÆ≥
                   04  JOLSF121-23   PIC 9(6).                                 À›∫∞ƒ
                   04  JOLSF121-24.                                             Øø≥À
                       05  JOLSF121-241 PIC 9(2).                              »›
                       05  JOLSF121-242 PIC 9(2).                              ¬∑
                       05  JOLSF121-243 PIC 9(2).                              À
                   04  JOLSF121-25.                                            ¡Æ∏∫∞
                       05  JOLSF121-251 PIC 9(4).                              ƒ∏≤∫∞
                       05  JOLSF121-252 PIC 9(3).                              ¡Æ∏ N
                   04  JOLSF121-26   PIC 9(1).                                 ≥›ø≥
                   04  JOLSF121-27   PIC 9(1).                                 ø≥∫∞ƒ
                   04  JOLSF121-28   PIC S9(3).                                ∫Ω≥
                   04  JOLSF121-29  OCCURS  27.                                ºØ∂Ω≥
                       05  JOLSF121-291 PIC S9(3).                             ª≤ΩﬁÕ
                   04  JOLSF121-30   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSF121-31   PIC 9(1).                                 ∆≠≥ÿÆ
                   04  JOLSF121-32   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSF121-33   PIC S9(3).                                œ≤Ω≥
                   04  JOLSF121-33A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSF121-34   PIC 9(6).                                 µ∏ÿºﬁ
      *
               03  JOLSF122-B        REDEFINES  JOLSF121-B.                 çsÅÅÇV
                   04  JOLSF122-2KEYW.
                       05  JOLSF122-21  PIC 9(6).                              √ﬁ›Àﬂ
                       05  JOLSF122-22  PIC 9(1).                              ∑ﬁÆ≥
                   04  JOLSF122-22A  PIC N(9).                                  ≤¿¬
                   04  JOLSF122-23   PIC N(23).                                √∑÷≥
                   04  FILLER        PIC X(41).
                   04  JOLSF122-24   PIC 9(1).                                 ≤›ºﬁª
                   04  JOLSF122-25   PIC 9(1).                                 ∆Æ≥ÿÆ
                   04  JOLSF122-26   PIC 9(1).                                 º≠Ø∂ª
                   04  JOLSF122-27   PIC S9(3).                                œ≤Ω≥
                   04  JOLSF122-27A  PIC 9(1).                              àÍî ã≥àÁ
                   04  FILLER        PIC X(2).
                   04  JOLSF122-28   PIC 9(6).                                 µ∏ÿºﬁ
      *----ëóÇËèÛÉtÉ@ÉCÉã        ÅiãÊï™ÅÅÇPÇRÅj
           02  JOLSF13-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF13-01        PIC 9(02).                             ÇqÇbãÊï™
               03  JOLSF13-TBL       OCCURS  4.
                   04  JOLSF13-KEY   .
                     05  JOLSF13-02  PIC 9(06).                                ëóÇË
                   04  JOLSF13-03    PIC 9(01).                                â^ëó
                   04  JOLSF13-04    PIC 9(06).                                îNåé
                   04  JOLSF13-05    PIC 9(01).                                ëqå…
                   04  JOLSF13-06    PIC 9(07).                                íºëó
                   04  JOLSF13-07    PIC N(09).                                îzíB
                   04  JOLSF13-08    PIC 9(03).                                å¬êî
                   04  JOLSF13-09    PIC 9(01).                                àÛéö
                   04  JOLSF13-10    PIC 9(01).                                ãÊï™
                   04  JOLSF13-11    PIC 9(01).                                çXêV
                   04  JOLSF13-12    PIC 9(05).
                   04  JOLSF13-13    PIC 9(06).
                   04  F             PIC X(07).
               03  F                 PIC X(02).
      *----ÉèÅ[ÉNÉ}Éìì`ï[Çe      ÅiãÊï™ÅÅÇPÇSÅj
           02  JOLSF14-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF14-01            PIC 9(02).
      *
               03  JOLSF141-A.
                   04  JOLSF141-KEY.
                     05  JOLSF141-STC.
                       06  F             PIC  9(002).
                       06  JOLSF141-SCD  PIC  9(002).
                       06  F             PIC  9(001).
                       06  JOLSF141-TCD  PIC  9(004).
                     05  JOLSF141-DNO.
                       06  F             PIC  9(002).
                       06  JOLSF141-DNOD PIC  9(007).
                     05  JOLSF141-DGN    PIC  9(002).
                   04  JOLSF141-BC.
                     05  JOLSF141-BCD    PIC  9(002).
                     05  F               PIC  9(001).
                   04  JOLSF141-SHC      PIC  9(001).
                   04  JOLSF141-DPC      PIC  X(002).
                   04  JOLSF141-HNGP     PIC  9(006).
                   04  JOLSF141-NNGP     PIC  9(006).
                   04  JOLSF141-THC      PIC  9(006).
                   04  JOLSF141-MHC      PIC  X(001).
                   04  F                 PIC  X(001).
                   04  JOLSF141-SNA      PIC  X(020).
                   04  JOLSF141-TNA      PIC  X(020).
                   04  JOLSF141-HCC      PIC  9(001).
                   04  JOLSF141-HSP      PIC  X(001).
                   04  JOLSF141-DHC      PIC  X(001).
                   04  JOLSF141-KHC      PIC  X(001).
                   04  JOLSF141-KCC      PIC  X(001).
                   04  JOLSF141-UBC      PIC  X(001).
                   04  JOLSF141-NCC      PIC  9(001).
                   04  JOLSF141-EDI      PIC  9(001).
                   04  JOLSF141-NKC      PIC  9(001).
                   04  JOLSF141-ZAC      PIC  9(001).
                   04  F                 PIC  X(157).
                   04  JOLSF141-PC       PIC  9(001).
      *
               03  JOLSF142-A        REDEFINES  JOLSF141-A.
                   04  JOLSF142-KEY.
                     05  JOLSF142-STC.
                       06  F             PIC  9(002).
                       06  JOLSF142-SCD  PIC  9(002).
                       06  F             PIC  9(001).
                       06  JOLSF142-TCD  PIC  9(004).
                     05  JOLSF142-DNO.
                       06  F             PIC  9(002).
                       06  JOLSF142-DNOD PIC  9(007).
                     05  JOLSF142-DGN    PIC  9(002).
                   04  JOLSF142-HCD      PIC  X(013).
                   04  JOLSF142-ISU      PIC  9(003)V9(01).
                   04  JOLSF142-KSU      PIC  9(004).
                   04  JOLSF142-HTC      PIC  X(002).
                   04  JOLSF142-SU       PIC  9(005)V9(01).
                   04  JOLSF142-GTN      PIC  9(007)V9(02).
                   04  JOLSF142-UTN      PIC  9(007).
                   04  JOLSF142-GKIN     PIC  9(010).
                   04  JOLSF142-UKIN     PIC  9(010).
                   04  JOLSF142-GCN      PIC  9(006).
                   04  JOLSF142-CCD      PIC  X(003).
                   04  JOLSF142-SHN      PIC  X(025).
                   04  JOLSF142-JAN      PIC  X(013).
                   04  F                 PIC  X(004).
                   04  JOLSF142-TSH      PIC  9(005).
                   04  JOLSF142-TKC      PIC  X(001).
                   04  F                 PIC  X(001).
                   04  F                 PIC  X(110).
                   04  JOLSF142-PC       PIC  9(001).
      *----ÉiÉtÉRì`ï[Çe Å@Å@     ÅiãÊï™ÅÅÇPÇTÅj
           02  JOLSF15-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF15-01            PIC 9(02).
      *
               03  JOLSF151-A.
                   04  JOLSF151-KEY.
                     05  JOLSF151-STC.
                       06  JOLSF151-SCD  PIC  9(002).
                       06  JOLSF151-TCD  PIC  9(003).
                       06  F             PIC  X(004).
                     05  JOLSF151-DNO.
                       06  F             PIC  X(002).
                       06  JOLSF151-DNOD PIC  9(007).
                     05  JOLSF151-DGN    PIC  9(002).
                   04  F                 PIC  X(002).
                   04  JOLSF151-BCD      PIC  9(002).
                   04  JOLSF151-DPC      PIC  9(002).
                   04  JOLSF151-HNGP     PIC  9(006).
                   04  JOLSF151-HNGPD REDEFINES JOLSF151-HNGP.
                     05  JOLSF151-HNEN   PIC  9(002).
                     05  JOLSF151-HGET   PIC  9(002).
                     05  JOLSF151-HPEY   PIC  9(002).
                   04  JOLSF151-NNGP     PIC  9(006).
                   04  JOLSF151-NNGPD REDEFINES JOLSF151-NNGP.
                     05  JOLSF151-NNEN   PIC  9(002).
                     05  JOLSF151-NGET   PIC  9(002).
                     05  JOLSF151-NPEY   PIC  9(002).
                   04  JOLSF151-THC      PIC  9(006).
                   04  JOLSF151-STA      PIC  X(002).
                   04  JOLSF151-SNA      PIC  X(015).
                   04  JOLSF151-TNA      PIC  X(015).
                   04  JOLSF151-TSN      PIC  X(015).
                   04  JOLSF151-TST      PIC  X(012).
                   04  JOLSF151-HCC      PIC  9(001).
                   04  F                 PIC  X(024).
                   04  JOLSF151-AR       PIC  X(007).
                   04  JOLSF151-DUR      PIC  X(026).
                   04  JOLSF151-DSHR     PIC  X(014).
                   04  JOLSF151-DSMR     PIC  X(007).
                   04  JOLSF151-ER       PIC  X(005).
                   04  JOLSF151-FSR      PIC  X(015).
                   04  JOLSF151-FUR      PIC  X(007).
                   04  JOLSF151-LCR      PIC  X(016).
                   04  JOLSF151-LUR      PIC  X(020).
                   04  JOLSF151-LSR      PIC  X(007).
                   04  F                 PIC  X(001).
                   04  JOLSF151-PC       PIC  9(001).
      *
               03  JOLSF152-A        REDEFINES  JOLSF151-A.
                   04  JOLSF152-KEY.
                     05  JOLSF152-STC.
                       06  JOLSF152-SCD  PIC  9(002).
                       06  JOLSF152-TCD  PIC  9(003).
                       06  F             PIC  X(004).
                     05  JOLSF152-DNO.
                       06  F             PIC  X(002).
                       06  JOLSF152-DNOD PIC  9(007).
                     05  JOLSF152-DGN    PIC  9(002).
                   04  JOLSF152-JAN      PIC  X(013).
                   04  JOLSF152-GAR      PIC  X(006).
                   04  F                 PIC  X(001).
                   04  JOLSF152-TNI      PIC  X(003).
                   04  JOLSF152-SU       PIC  9(005).
                   04  F                 PIC  X(001).
                   04  JOLSF152-GTN      PIC  9(007).
                   04  F                 PIC  X(002).
                   04  JOLSF152-UTN      PIC  9(007).
                   04  JOLSF152-GKIN     PIC  9(010).
                   04  JOLSF152-UKIN     PIC  9(010).
                   04  F                 PIC  X(009).
                   04  JOLSF152-SHN      PIC  X(025).
                   04  JOLSF152-HSC      PIC  X(008).
                   04  JOLSF152-COR      PIC  X(006).
                   04  JOLSF152-SIZ      PIC  X(005).
                   04  F                 PIC  X(004).
                   04  JOLSF152-KKK      PIC  X(025).
                   04  JOLSF152-PCH      PIC  X(001).
                   04  JOLSF152-PSI      PIC  X(001).
                   04  JOLSF152-PBM      PIC  9(002).
                   04  JOLSF152-PJAN     PIC  X(013).
                   04  JOLSF152-PSHN     PIC  X(020).
                   04  JOLSF152-PKKK     PIC  X(020).
                   04  JOLSF152-PUTN     PIC  9(007).
                   04  JOLSF152-PMS      PIC  9(005).
                   04  F                 PIC  X(017).
                   04  JOLSF152-PC       PIC  9(001).
      *----ÉgÉâÉXÉRëºìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇUÅj
           02  JOLSF16-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF16-01            PIC 9(02).
               03  JOLSF16-A.
                   04  JOLSF16-KEY.
                       05  JOLSF16-DNO   PIC  9(006).
                       05  JOLSF16-GNO   PIC  9(001).
                   04  JOLSF16-DATE      PIC  9(006).
                   04  JOLSF16-TCD       PIC  9(004).
                   04  JOLSF16-CCD       PIC  9(003).
                   04  JOLSF16-TPC       PIC  9(004).
                   04  JOLSF16-HCD       PIC  9(006).
                   04  JOLSF16-SIZ       PIC  X(003).
                   04  JOLSF16-SKB       PIC  9(001).
                   04  JOLSF16-SNO       PIC  9(002).
                   04  JOLSF16-SU        PIC S9(005).
                   04  JOLSF16-GT        PIC  9(007).
                   04  JOLSF16-UT        PIC  9(007).
                   04  JOLSF16-GKIN      PIC S9(008).
                   04  JOLSF16-UKIN      PIC S9(008).
                   04  JOLSF16-JNOD.
                       05  JOLSF16-JNO   PIC  9(006).
                       05  JOLSF16-JGN   PIC  9(001).
                   04  JOLSF16-SOK       PIC  9(001).
                   04  JOLSF16-UNS       PIC  9(001).
                   04  JOLSF16-ISU       PIC  9(003).
                   04  JOLSF16-HNO       PIC  X(010).
                   04  JOLSF16-TEKI      PIC  N(028).
                   04  JOLSF16-TED   REDEFINES JOLSF16-TEKI.
                       05  JOLSF16-THT   PIC  N(009).
                       05  JOLSF16-TTE   PIC  N(019).
                   04  JOLSF16-TRN       PIC  X(020).
                   04  JOLSF16-JAN       PIC  X(013).
                   04  F                 PIC  X(070).
                   04  JOLSF16-PRC       PIC  9(001).
                   04  JOLSF16-UPC       PIC  9(001).
      *----ê‘ÇøÇ·ÇÒñ{ï‹ìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇVÅj
           02  JOLSF17-REC    REDEFINES  JOLSF1-REC.
               03  JOLSF17-01            PIC 9(02).
               03  JOLSF17-KEY.
                   04  JOLSF17-STC       PIC  9(007).
                   04  JOLSF17-DNO       PIC  9(007).
                   04  JOLSF17-DGN       PIC  9(002).
               03  JOLSF17-JAN           PIC  X(013).
               03  JOLSF17-SU            PIC  9(006).
               03  JOLSF17-GTN           PIC  9(007).
               03  JOLSF17-UTN           PIC  9(007).
               03  JOLSF17-GKIN          PIC  9(010).
               03  JOLSF17-UKIN          PIC  9(010).
               03  JOLSF17-DPM           PIC  X(002).
               03  JOLSF17-CLS           PIC  X(003).
               03  JOLSF17-SHM           PIC  X(013).
               03  JOLSF17-MKH           PIC  X(010).
               03  JOLSF17-MSB           PIC  X(010).
               03  JOLSF17-TY            PIC  X(002).
               03  JOLSF17-HCD           PIC  9(006).
               03  JOLSF17-COR           PIC  N(004).
               03  JOLSF17-SIZ           PIC  X(004).
               03  JOLSF17-NSU           PIC  9(006).
               03  JOLSF17-TSC           PIC  9(001).
               03  F                     PIC  X(008).
               03  JOLSF17-CCD           PIC  9(003).
               03  JOLSF17-TNA           PIC  N(014).
               03  JOLSF17-HNO           PIC  9(009).
               03  JOLSF17-HNGP          PIC  9(006).
               03  JOLSF17-NNGP          PIC  9(006).
               03  JOLSF17-THC           PIC  9(006).
               03  JOLSF17-BI            PIC  X(010).
               03  JOLSF17-SNGP          PIC  9(008).
               03  JOLSF17-HNA           PIC  X(006).
               03  JOLSF17-ZON           PIC  X(004).
               03  JOLSF17-DC            PIC  9(002).
               03  F                     PIC  X(005).
               03  JOLSF17-DNGP          PIC  9(008).
               03  JOLSF17-NRC           PIC  9(001).
               03  F                     PIC  X(008).
               03  JOLSF17-PC            PIC  9(001).
               03  JOLSF17-RC            PIC  9(001).
       77  F                         PIC  X(001).
