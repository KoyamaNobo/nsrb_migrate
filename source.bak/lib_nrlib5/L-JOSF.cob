000010***********************************************
000020*****                                     *****
000030**   Å@Å@ÇnÅ^ÇkëóêMÅ@ÉtÉ@ÉCÉãÅ@          Å@**
000040*****         ( JOLSF   )  256/1          *****
000050***********************************************
000060 FD  JOLSF
000070     BLOCK 1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "JOLSF".
000100*
000110*----ÉRÉìÉgÉçÅ[ÉãÇeóp      ÅiãÊï™ÅÅÇOÇPÅj
000120 01  JOLSF1-REC.
000130     02  JOLSF1-01         PIC  9(02).                            ÇqÇbãÊï™
000140     02  JOLSF1-KEYW.
000150         03  JOLSF1-02     PIC  9(01).                            ID
000160         03  JOLSF1-03     PIC  9(01).                            â^&ëqCD
000170     02  JOLSF1-04         PIC  N(06).                            â^&ëqñº
000180     02  JOLSF1-05         PIC  X(18).                            FILLER
000190     02  F                 PIC  X(221).
000200     02  JOLSF1-99         PIC  9(01).                            ëóêMêÊ
000210*
000220*----íºëóêÊÉ}ÉXÉ^óp        ÅiãÊï™ÅÅÇOÇQÅj
000230 01  JOLSF2-REC.
000240     02  JOLSF2-01         PIC  9(02).                            ÇqÇbãÊï™
000250     02  JOLSF2-KEYW.
000260         03  JOLSF2-02     PIC  9(04).                            ìæà”êÊCD
000270         03  JOLSF2-03     PIC  9(03).                            íºëóêÊCD
000280     02  JOLSF2-04         PIC  N(26).                            I.020418
000290     02  JOLSF2-05         PIC  N(20).                            I.020418
000300     02  JOLSF2-06         PIC  N(20).                            I.020418
000310*****02  JOLSF2-04         PIC  N(24).                            D.020418
000320*****02  JOLSF2-05         PIC  N(24).                            D.020418
000330*****02  JOLSF2-06         PIC  N(12).                            D.020418
000340*****02  JOLSF2-07         PIC  X(06).                            D.970130
000350*****02  JOLSF2-08         PIC  X(12).                            D.970130
000360     02  JOLSF2-07         PIC  X(08).                            I.970130
000370     02  JOLSF2-08         PIC  X(14).                            I.970130
000380     02  JOLSF2-09         PIC  9(02).                            ï{åß∫∞ƒﬁ
000390     02  JOLSF2-10         PIC  9(01).                            â^ëó∫∞ƒﬁ
000400*****02  JOLSF2-11         PIC  X(22).                            D.970130
000410*****02  JOLSF2-11         PIC  X(18).                            D.020418
000420     02  JOLSF2-11         PIC  X(28).                            I.020418
000430     02  JOLSF2-12         PIC  9(01).                            ACT
000440*****02  F                 PIC  X(82).                            D.020418
000450     02  F                 PIC  X(60).                            I.020418
000460     02  JOLSF2-99         PIC  9(01).                            ëóêMêÊ
000470*
000480*----ïiñºÉtÉ@ÉCÉã          ÅiãÊï™ÅÅÇOÇRÅj
000490 01  JOLSF3-REC.
000500     02  JOLSF3-01         PIC  9(02).                            ÇqÇbãÊï™
000510     02  JOLSF3-KEYW.
000520       03  JOLSF3-MHCD     PIC  9(006).                           I.010810
000530       03  JOLSF3-HCD      PIC  9(006).                           I.010810
000540     02  JOLSF3-NAME       PIC  N(024).                           I.010810
000550     02  JOLSF3-BC.                                               I.010810
000560       03  JOLSF3-BC1      PIC  9(002).                           I.010810
000570       03  JOLSF3-BC2      PIC  9(002).                           I.010810
000580       03  JOLSF3-BC3      PIC  9(002).                           I.010810
000590     02  JOLSF3-ASSD.                                             I.010810
000600       03  JOLSF3-SSD   OCCURS  4.                                I.010810
000610         04  JOLSF3-SS     PIC  9(010).                           I.010810
000620     02  JOLSF3-ASKD  REDEFINES JOLSF3-ASSD.                      I.010810
000630       03  JOLSF3-SKD   OCCURS  4.                                I.010810
000640         04  JOLSF3-SK    OCCURS 10.                              I.010810
000650           05  JOLSF3-S    PIC  9(001).                           I.010810
000660     02  JOLSF3-AHSD  REDEFINES JOLSF3-ASSD.                      I.010810
000670       03  JOLSF3-HSD.                                            I.010810
000680         04  JOLSF3-SS1    PIC  9(010).                           I.010810
000690         04  JOLSF3-SD1   REDEFINES JOLSF3-SS1.                   I.010810
000700           05  JOLSF3-S1    OCCURS  10  PIC  9(001).              I.010810
000710         04  JOLSF3-SS2    PIC  9(010).                           I.010810
000720         04  JOLSF3-SD2    REDEFINES JOLSF3-SS2.                  I.010810
000730           05  JOLSF3-S2    OCCURS  10  PIC  9(001).              I.010810
000740         04  JOLSF3-SS3    PIC  9(010).                           I.010810
000750         04  JOLSF3-SD3    REDEFINES JOLSF3-SS3.                  I.010810
000760           05  JOLSF3-S3    OCCURS  10  PIC  9(001).              I.010810
000770         04  JOLSF3-SS4    PIC  9(010).                           I.010810
000780         04  JOLSF3-SD4    REDEFINES JOLSF3-SS4.                  I.010810
000790           05  JOLSF3-S4    OCCURS  10  PIC  9(001).              I.010810
000800     02  JOLSF3-SB         PIC  9(005).                           I.010810
000810     02  JOLSF3-FT         PIC  9(005).                           I.010810
000820*****02  JOLSF3-ZRG        PIC  9(005).                           D.090126
000830*****02  JOLSF3-SKG        PIC  9(005).                           D.090126
000840*****02  JOLSF3-GKG        PIC  9(005).                           D.090126
000850*****02  JOLSF3-KNG        PIC  9(004).                           D.090126
000860     02  F                 PIC  X(019).                           I.090126
000870     02  JOLSF3-KT         PIC  9(005).                           I.010810
000880     02  JOLSF3-TCD        PIC  9(004).                           I.010810
000890     02  JOLSF3-ISU        PIC  9(003).                           I.010810
000900*****    03  JOLSF3-02     PIC  9(06).                            D.010810
000910*****02  JOLSF3-03         PIC  N(24).                            D.010810
000920*****02  JOLSF3-04         PIC  9(1).                             D.010810
000930*****02  JOLSF3-05.                                               D.010810
000940*****    03  JOLSF3-051.                                          D.010810
000950*****        04  JOLSF3-0511  OCCURS  10  PIC  9(01).             D.010810
000960*****    03  JOLSF3-052.                                          D.010810
000970*****        04  JOLSF3-0521  OCCURS  10  PIC  9(01).             D.010810
000980*****    03  JOLSF3-053.                                          D.010810
000990*****        04  JOLSF3-0531  OCCURS  10  PIC  9(01).             D.010810
001000*****    03  JOLSF3-054.                                          D.010810
001010*****        04  JOLSF3-0541  OCCURS  10  PIC  9(01).             D.010810
001020*****02  JOLSF3-06         PIC  9(02).                            D.010810
001030*****02  JOLSF3-07         PIC  N(15).                            D.010810
001040*****02  JOLSF3-08         PIC  9(03).                            D.010810
001050*****02  JOLSF3-09.                                               D.010810
001060*****    03  JOLSF3-091.                                          D.010810
001070*****        04  JOLSF3-0911  OCCURS  10  PIC  9(01).             D.010810
001080*****    03  JOLSF3-092.                                          D.010810
001090*****        04  JOLSF3-0921  OCCURS  10  PIC  9(01).             D.010810
001100*****    03  JOLSF3-093.                                          D.010810
001110*****        04  JOLSF3-0931  OCCURS  10  PIC  9(01).             D.010810
001120*****    03  JOLSF3-094.                                          D.010810
001130*****        04  JOLSF3-0941  OCCURS  10  PIC  9(01).             D.010810
001140     02  JOLSF3-10         PIC  9(01).                            ACT
001150     02  JOLSF3-SCC        PIC  9(001).                           I.090126
001160     02  JOLSF3-BMC        PIC  9(002).                           I.090126
001170     02  JOLSF3-BMNO       PIC  9(001).                           I.090126
001180     02  JOLSF3-YG         PIC  9(005).                           I.090126
001190     02  JOLSF3-HKB        PIC  9(001).                           I.090126
001200     02  JOLSF3-HPV        PIC  9(001).                           I.090126
001210     02  JOLSF3-BC4        PIC  9(001).                           I.090126
001220     02  F                 PIC  X(011).                           I.090126
001230     02  JOLSF3-SMS        PIC  N(016).                           I.090126
001240     02  JOLSF3-UNG        PIC  9(006).                           I.090126
001250     02  JOLSF3-NNG        PIC  9(006).                           I.090126
001260     02  F                 PIC  X(001).                           I.090126
001270     02  JOLSF3-CS         PIC  N(010).                           I.090126
001280     02  F                 PIC  X(003).                           I.090126
001290     02  JOLSF3-DNG        PIC  9(006).                           I.090126
001300     02  JOLSF3-SNG        PIC  9(004).                           I.090126
001310     02  JOLSF3-ENG        PIC  9(004).                           I.090126
001320*****02 F                  PIC  X(105).                           D.090126
001330*****02 F                  PIC  X(82).                            D.010810
001340     02 JOLSF3-99          PIC  9(01).                            ëóêMêÊ
001350*
001360*----ÉèÅ[ÉNÉ}ÉììXñº        ÅiãÊï™ÅÅÇOÇSÅj
001370 01  JOLSF4-REC.                                                  I.080326
001380     02  JOLSF4-01         PIC  9(02).                            I.080326
001390     02  JOLSF4-KEYW.                                             I.080326
001400       03  JOLSF4-TNC      PIC  9(004).                           I.100922
001410*****  03  JOLSF4-TNC      PIC  9(003).                           D.100922
001420     02  JOLSF4-NAME       PIC  N(026).                           I.080326
001430     02  JOLSF4-BSC        PIC  9(001).                           I.080326
001440     02  F                 PIC  X(007).                           I.100922
001450*****02  F                 PIC  X(008).                           D.100922
001460     02 JOLSF4-10          PIC  9(01).                            I.080326
001470     02  F                 PIC  X(188).                           I.080326
001480     02 JOLSF4-99          PIC  9(01).                            I.080326
001490*
001500*----èoâ◊éwé¶ÉgÉâÉì        ÅiãÊï™ÅÅÇPÇPÅj
001510 01  JOLSF11-REC.
001520     02  JOLSF11-01        PIC 9(02).                             ÇqÇbãÊï™
001530     02  JOLSF11-KEYW.
001540         03  JOLSF11-02    PIC 9(06).                                º≠Ø∂º
001550         03  JOLSF11-03    PIC 9(01).                                ∑ﬁÆ≥
001560     02  JOLSF11-04        PIC 9(01).                                √ﬁ›∏
001570     02  JOLSF11-05.                                                 º≠Ø∂À
001580*****    03  JOLSF11-051   PIC 9(02).                             D.980515
001590         03  JOLSF11-051   PIC 9(04).                             I.980515
001600         03  JOLSF11-052   PIC 9(02).                                ¬∑
001610         03  JOLSF11-053   PIC 9(02).                                À
001620     02  JOLSF11-06.                                                 º≠Ø∂À
001630*****    03  JOLSF11-061   PIC 9(02).                             D.980515
001640         03  JOLSF11-061   PIC 9(04).                             I.980515
001650         03  JOLSF11-062   PIC 9(02).                                ¬∑
001660         03  JOLSF11-063   PIC 9(02).                                À
001670     02  JOLSF11-07.                                                 ¡Æ∏ø≥
001680         03  JOLSF11-071   PIC 9(04).                                ƒ∏≤∫∞
001690         03  JOLSF11-072   PIC 9(03).                                ¡Æ∏ N
001700     02  JOLSF11-08        PIC 9(01).                                ∏◊ ∫∞
001710     02  JOLSF11-09.                                                 ºﬁ≠¡≠
001720         03  JOLSF11-091   PIC 9(06).                                ºﬁ≠¡≠
001730         03  JOLSF11-092   PIC 9(01).                                ∑ﬁÆ≥
001740     02  JOLSF11-10        PIC 9(06).                                À›∫∞ƒ
001750     02  JOLSF11-11        PIC 9(01).                                ª≤Ωﬁ∏
001760     02  JOLSF11-12.                                                 º≠Ø∂ª
001770         03  JOLSF11-121   OCCURS  10.                               ª≤ΩﬁÕ
001780             04  JOLSF11-1211      PIC S9(04).
001790         03  JOLSF11-122   PIC S9(05).                            I.981006
001800*****    03  JOLSF11-122   PIC S9(06).                            D.981006
001810     02  JOLSF11-13.                                                 º≠Ø∂º
001820         03  JOLSF11-131   OCCURS  10.                               ª≤ΩﬁÕ
001830             04  JOLSF11-1311      PIC S9(04).
001840         03  JOLSF11-132   PIC S9(05).                            I.981006
001850*****    03  JOLSF11-132   PIC S9(06).                            D.981006
001860     02  JOLSF11-14        PIC 9(01).                                ±Ωﬁ∂ÿ
001870     02  JOLSF11-15        PIC 9(01).                                â^ëó 
001880     02  JOLSF11-15A       PIC 9(03).                                ÉZÉb 
001890     02  JOLSF11-15B       PIC 9(06).                                ëóÇË 
001900     02  JOLSF11-15C       PIC 9(02).                                é}î‘
001910*****02  FILLER            PIC X(3).                              D.981006
001920*****02  FILLER            PIC X(2).                              D.980930
001930*****02  JOLSF11-99        PIC 9(01).                             D.980930
001940     02  JOLSF11-15D       PIC N(09).                                îzíB
001950     02  JOLSF11-16        PIC N(23).                                ìEóv
001960     02  JOLSF11-20        PIC X(10).                             I.981006
001970     02  JOLSF11-16A       PIC S9(03).                            å¬êî
001980*****02  JOLSF11-18A.                                             D.980930
001990*****    03  JOLSF11-181   PIC 9(06).                             D.980930
002000*****    03  JOLSF11-182   PIC 9(01).                             D.980930
002010*****02  FILLER            PIC X(26).                             D.980515
002020*****02  FILLER            PIC X(22).                             D.980930
002030*****02  FILLER            PIC X(29).                             D.981006
002040     02  FILLER            PIC X(24).                             I.981006
002050     02  JOLSF11-19        PIC X(01).                             èàóùïîèê
002060     02  JOLSF11-168       PIC 9(01).                             àÛéöª≤›
002070     02  JOLSF11-17        PIC 9(01).                             àÍî ã≥àÁ
002080     02  JOLSF11-18        PIC 9(01).                             çXêVª≤›
002090*
002100*----â◊éDÉgÉâÉì            ÅiãÊï™ÅÅÇPÇQÅj
002110 01  JOLSF12-REC.
002120     02  JOLSF12-01        PIC 9(02).                             ÇqÇbãÊï™
002130*
002140     02  JOLSF121-A.                                              çsNOT=7
002150         03  JOLSF121-1KEYW.
002160             04  JOLSF121-01  PIC 9(6).                              √ﬁ›Àﬂ
002170             04  JOLSF121-02  PIC 9(1).                              ∑ﬁÆ≥
002180         03  JOLSF121-03   PIC 9(6).                                 À›∫∞ƒ
002190         03  JOLSF121-04.                                             Øø≥À
002200             04  JOLSF121-041 PIC 9(2).                              »›
002210             04  JOLSF121-042 PIC 9(2).                              ¬∑
002220             04  JOLSF121-043 PIC 9(2).                              À
002230         03  JOLSF121-05.                                            ¡Æ∏∫∞
002240             04  JOLSF121-051 PIC 9(4).                              ƒ∏≤∫∞
002250             04  JOLSF121-052 PIC 9(3).                              ¡Æ∏ N
002260         03  JOLSF121-06   PIC 9(1).                                 ≥›ø≥
002270         03  JOLSF121-07   PIC 9(1).                                 ø≥∫∞ƒ
002280         03  JOLSF121-08   PIC S9(3).                                ∫Ω≥
002290         03  JOLSF121-09  OCCURS  27.                                ºØ∂Ω≥
002300             04  JOLSF121-091 PIC S9(3).                             ª≤ΩﬁÕ
002310         03  JOLSF121-10   PIC 9(1).                                 ≤›ºﬁª
002320         03  JOLSF121-11   PIC 9(1).                                 ∆≠≥ÿÆ
002330         03  JOLSF121-12   PIC 9(1).                                 º≠Ø∂ª
002340         03  JOLSF121-13   PIC S9(3).                                œ≤Ω≥
002350         03  JOLSF121-13A  PIC 9(01).                             àÍî ã≥àÁ
002360         03  FILLER        PIC X(2).
002370         03  JOLSF121-14   PIC 9(6).                                 µ∏ÿºﬁ
002380*
002390     02  JOLSF122-A        REDEFINES  JOLSF121-A.                 çsÅÅÇV
002400         03  JOLSF122-1KEYW.
002410             04  JOLSF122-01  PIC 9(6).                              √ﬁ›Àﬂ
002420             04  JOLSF122-02  PIC 9(1).                              ∑ﬁÆ≥
002430         03  JOLSF122-02A  PIC N(9).                                  ≤¿¬
002440         03  JOLSF122-03   PIC N(23).                                √∑÷≥
002450         03  FILLER        PIC X(41).
002460         03  JOLSF122-04   PIC 9(1).                                 ≤›ºﬁª
002470         03  JOLSF122-05   PIC 9(1).                                 ∆Æ≥ÿÆ
002480         03  JOLSF122-06   PIC 9(1).                                 º≠Ø∂ª
002490         03  JOLSF122-07   PIC S9(3).                                œ≤Ω≥
002500         03  JOLSF122-07A  PIC 9(1).                              àÍî ã≥àÁ
002510         03  FILLER        PIC X(2).
002520         03  JOLSF122-08   PIC 9(6).                                 µ∏ÿºﬁ
002530*
002540     02  JOLSF121-B.                                              çsNOT=7
002550         03  JOLSF121-2KEYW.
002560             04  JOLSF121-21  PIC 9(6).                              √ﬁ›Àﬂ
002570             04  JOLSF121-22  PIC 9(1).                              ∑ﬁÆ≥
002580         03  JOLSF121-23   PIC 9(6).                                 À›∫∞ƒ
002590         03  JOLSF121-24.                                             Øø≥À
002600             04  JOLSF121-241 PIC 9(2).                              »›
002610             04  JOLSF121-242 PIC 9(2).                              ¬∑
002620             04  JOLSF121-243 PIC 9(2).                              À
002630         03  JOLSF121-25.                                            ¡Æ∏∫∞
002640             04  JOLSF121-251 PIC 9(4).                              ƒ∏≤∫∞
002650             04  JOLSF121-252 PIC 9(3).                              ¡Æ∏ N
002660         03  JOLSF121-26   PIC 9(1).                                 ≥›ø≥
002670         03  JOLSF121-27   PIC 9(1).                                 ø≥∫∞ƒ
002680         03  JOLSF121-28   PIC S9(3).                                ∫Ω≥
002690         03  JOLSF121-29  OCCURS  27.                                ºØ∂Ω≥
002700             04  JOLSF121-291 PIC S9(3).                             ª≤ΩﬁÕ
002710         03  JOLSF121-30   PIC 9(1).                                 ≤›ºﬁª
002720         03  JOLSF121-31   PIC 9(1).                                 ∆≠≥ÿÆ
002730         03  JOLSF121-32   PIC 9(1).                                 º≠Ø∂ª
002740         03  JOLSF121-33   PIC S9(3).                                œ≤Ω≥
002750         03  JOLSF121-33A  PIC 9(1).                              àÍî ã≥àÁ
002760         03  FILLER        PIC X(2).
002770         03  JOLSF121-34   PIC 9(6).                                 µ∏ÿºﬁ
002780*
002790     02  JOLSF122-B        REDEFINES  JOLSF121-B.                 çsÅÅÇV
002800         03  JOLSF122-2KEYW.
002810             04  JOLSF122-21  PIC 9(6).                              √ﬁ›Àﬂ
002820             04  JOLSF122-22  PIC 9(1).                              ∑ﬁÆ≥
002830         03  JOLSF122-22A  PIC N(9).                                  ≤¿¬
002840         03  JOLSF122-23   PIC N(23).                                √∑÷≥
002850         03  FILLER        PIC X(41).
002860         03  JOLSF122-24   PIC 9(1).                                 ≤›ºﬁª
002870         03  JOLSF122-25   PIC 9(1).                                 ∆Æ≥ÿÆ
002880         03  JOLSF122-26   PIC 9(1).                                 º≠Ø∂ª
002890         03  JOLSF122-27   PIC S9(3).                                œ≤Ω≥
002900         03  JOLSF122-27A  PIC 9(1).                              àÍî ã≥àÁ
002910         03  FILLER        PIC X(2).
002920         03  JOLSF122-28   PIC 9(6).                                 µ∏ÿºﬁ
002930**** 02  F                 PIC X(54).
002940*
002950*----ëóÇËèÛÉtÉ@ÉCÉã        ÅiãÊï™ÅÅÇPÇRÅj
002960 01  JOLSF13-REC.
002970     02  JOLSF13-01        PIC 9(02).                             ÇqÇbãÊï™
002980     02  JOLSF13-TBL       OCCURS  4.
002990         03  JOLSF13-KEY   .
003000           04  JOLSF13-02  PIC 9(06).                                ëóÇË 
003010         03  JOLSF13-03    PIC 9(01).                                â^ëó 
003020         03  JOLSF13-04    PIC 9(06).                                îNåé 
003030         03  JOLSF13-05    PIC 9(01).                                ëqå… 
003040         03  JOLSF13-06    PIC 9(07).                                íºëó 
003050         03  JOLSF13-07    PIC N(09).                                îzíB 
003060         03  JOLSF13-08    PIC 9(03).                                å¬êî
003070         03  JOLSF13-09    PIC 9(01).                                àÛéö
003080         03  JOLSF13-10    PIC 9(01).                                ãÊï™
003090         03  JOLSF13-11    PIC 9(01).                                çXêV
003100*****    03  F             PIC X(06).                             D.960829
003110         03  JOLSF13-12    PIC 9(05).                             I.960829
003120         03  JOLSF13-13    PIC 9(06).                             I.040628
003130         03  F             PIC X(07).                             I.040628
003140     02  F                 PIC X(02).                             I.040628
003150*****    03  F             PIC X(01).                             D.040628
003160*****02  F                 PIC X(50).                             D.040628
003170*----ÉèÅ[ÉNÉ}Éìì`ï[Çe      ÅiãÊï™ÅÅÇPÇSÅj
003180 01  JOLSF14-REC.                                                 I.040922
003190     02  JOLSF14-01            PIC 9(02).                         I.040922
003200*
003210     02  JOLSF141-A.                                              I.040922
003220         03  JOLSF141-KEY.                                        I.040922
003230           04  JOLSF141-STC.                                      I.040922
003240             05  F             PIC  9(002).                       I.040922
003250             05  JOLSF141-SCD  PIC  9(002).                       I.040922
003260             05  F             PIC  9(001).                       I.100922
003270             05  JOLSF141-TCD  PIC  9(004).                       I.100922
003280*****        05  F             PIC  9(002).                       D.100922
003290*****        05  JOLSF141-TCD  PIC  9(003).                       D.100922
003300           04  JOLSF141-DNO.                                      I.040922
003310             05  F             PIC  9(002).                       I.040922
003320             05  JOLSF141-DNOD PIC  9(007).                       I.040922
003330           04  JOLSF141-DGN    PIC  9(002).                       I.040922
003340         03  JOLSF141-BC.                                         I.040922
003350           04  JOLSF141-BCD    PIC  9(002).                       I.040922
003360           04  F               PIC  9(001).                       I.040922
003370         03  JOLSF141-SHC      PIC  9(001).                       I.040922
003380         03  JOLSF141-DPC      PIC  X(002).                       I.040922
003390         03  JOLSF141-HNGP     PIC  9(006).                       I.040922
003400         03  JOLSF141-NNGP     PIC  9(006).                       I.040922
003410         03  JOLSF141-THC      PIC  9(006).                       I.040922
003420         03  JOLSF141-MHC      PIC  X(001).                       I.040922
003430         03  F                 PIC  X(001).                       I.040922
003440         03  JOLSF141-SNA      PIC  X(020).                       I.040922
003450         03  JOLSF141-TNA      PIC  X(020).                       I.040922
003460         03  JOLSF141-HCC      PIC  9(001).                       I.040922
003470         03  JOLSF141-HSP      PIC  X(001).                       I.040922
003480         03  JOLSF141-DHC      PIC  X(001).                       I.040922
003490         03  JOLSF141-KHC      PIC  X(001).                       I.040922
003500         03  JOLSF141-KCC      PIC  X(001).                       I.040922
003510         03  JOLSF141-UBC      PIC  X(001).                       I.040922
003520         03  JOLSF141-NCC      PIC  9(001).                       I.040922
003530         03  JOLSF141-EDI      PIC  9(001).                       I.040922
003540         03  JOLSF141-NKC      PIC  9(001).                       I.040922
003550         03  JOLSF141-ZAC      PIC  9(001).                       I.040922
003560         03  F                 PIC  X(157).                       I.040922
003570         03  JOLSF141-PC       PIC  9(001).                       I.040922
003580*
003590     02  JOLSF142-A        REDEFINES  JOLSF141-A.                 I.040922
003600         03  JOLSF142-KEY.                                        I.040922
003610           04  JOLSF142-STC.                                      I.040922
003620             05  F             PIC  9(002).                       I.040922
003630             05  JOLSF142-SCD  PIC  9(002).                       I.040922
003640             05  F             PIC  9(001).                       I.100922
003650             05  JOLSF142-TCD  PIC  9(004).                       I.100922
003660*****        05  F             PIC  9(002).                       D.100922
003670*****        05  JOLSF142-TCD  PIC  9(003).                       D.100922
003680           04  JOLSF142-DNO.                                      I.040922
003690             05  F             PIC  9(002).                       I.040922
003700             05  JOLSF142-DNOD PIC  9(007).                       I.040922
003710           04  JOLSF142-DGN    PIC  9(002).                       I.040922
003720         03  JOLSF142-HCD      PIC  X(013).                       I.040922
003730         03  JOLSF142-ISU      PIC  9(003)V9(01).                 I.040922
003740         03  JOLSF142-KSU      PIC  9(004).                       I.040922
003750         03  JOLSF142-HTC      PIC  X(002).                       I.040922
003760         03  JOLSF142-SU       PIC  9(005)V9(01).                 I.040922
003770         03  JOLSF142-GTN      PIC  9(007)V9(02).                 I.040922
003780         03  JOLSF142-UTN      PIC  9(007).                       I.040922
003790         03  JOLSF142-GKIN     PIC  9(010).                       I.040922
003800         03  JOLSF142-UKIN     PIC  9(010).                       I.040922
003810         03  JOLSF142-GCN      PIC  9(006).                       I.040922
003820         03  JOLSF142-CCD      PIC  X(003).                       I.040922
003830         03  JOLSF142-SHN      PIC  X(025).                       I.040922
003840         03  JOLSF142-JAN      PIC  X(013).                       I.040922
003850         03  F                 PIC  X(004).                       I.040922
003860         03  JOLSF142-TSH      PIC  9(005).                       I.040922
003870         03  JOLSF142-TKC      PIC  X(001).                       I.040922
003880         03  F                 PIC  X(001).                       I.040922
003890         03  F                 PIC  X(110).                       I.040922
003900         03  JOLSF142-PC       PIC  9(001).                       I.040922
003910*----ÉiÉtÉRì`ï[Çe Å@Å@     ÅiãÊï™ÅÅÇPÇTÅj
003920 01  JOLSF15-REC.                                                 I.050108
003930     02  JOLSF15-01            PIC 9(02).                         I.050108
003940*
003950     02  JOLSF151-A.                                              I.050108
003960         03  JOLSF151-KEY.                                        I.050108
003970           04  JOLSF151-STC.                                      I.050108
003980             05  JOLSF151-SCD  PIC  9(002).                       I.050108
003990             05  JOLSF151-TCD  PIC  9(003).                       I.050108
004000             05  F             PIC  X(004).                       I.050108
004010           04  JOLSF151-DNO.                                      I.050108
004020             05  F             PIC  X(002).                       I.050108
004030             05  JOLSF151-DNOD PIC  9(007).                       I.050108
004040           04  JOLSF151-DGN    PIC  9(002).                       I.050108
004050         03  F                 PIC  X(002).                       I.050108
004060         03  JOLSF151-BCD      PIC  9(002).                       I.050108
004070         03  JOLSF151-DPC      PIC  9(002).                       I.050108
004080         03  JOLSF151-HNGP     PIC  9(006).                       I.050108
004090         03  JOLSF151-HNGPD REDEFINES JOLSF151-HNGP.              I.050108
004100           04  JOLSF151-HNEN   PIC  9(002).                       I.050108
004110           04  JOLSF151-HGET   PIC  9(002).                       I.050108
004120           04  JOLSF151-HPEY   PIC  9(002).                       I.050108
004130         03  JOLSF151-NNGP     PIC  9(006).                       I.050108
004140         03  JOLSF151-NNGPD REDEFINES JOLSF151-NNGP.              I.050108
004150           04  JOLSF151-NNEN   PIC  9(002).                       I.050108
004160           04  JOLSF151-NGET   PIC  9(002).                       I.050108
004170           04  JOLSF151-NPEY   PIC  9(002).                       I.050108
004180         03  JOLSF151-THC      PIC  9(006).                       I.050108
004190         03  JOLSF151-STA      PIC  X(002).                       I.050108
004200         03  JOLSF151-SNA      PIC  X(015).                       I.050108
004210         03  JOLSF151-TNA      PIC  X(015).                       I.050108
004220         03  JOLSF151-TSN      PIC  X(015).                       I.050108
004230         03  JOLSF151-TST      PIC  X(012).                       I.050108
004240         03  JOLSF151-HCC      PIC  9(001).                       I.050108
004250         03  F                 PIC  X(024).                       I.050108
004260         03  JOLSF151-AR       PIC  X(007).                       I.050108
004270         03  JOLSF151-DUR      PIC  X(026).                       I.050108
004280         03  JOLSF151-DSHR     PIC  X(014).                       I.050108
004290         03  JOLSF151-DSMR     PIC  X(007).                       I.050108
004300         03  JOLSF151-ER       PIC  X(005).                       I.050108
004310         03  JOLSF151-FSR      PIC  X(015).                       I.050108
004320         03  JOLSF151-FUR      PIC  X(007).                       I.050108
004330         03  JOLSF151-LCR      PIC  X(016).                       I.050108
004340         03  JOLSF151-LUR      PIC  X(020).                       I.050108
004350         03  JOLSF151-LSR      PIC  X(007).                       I.050108
004360         03  F                 PIC  X(001).                       I.050108
004370         03  JOLSF151-PC       PIC  9(001).                       I.050108
004380*
004390     02  JOLSF152-A        REDEFINES  JOLSF151-A.                 I.050108
004400         03  JOLSF152-KEY.                                        I.050108
004410           04  JOLSF152-STC.                                      I.050108
004420             05  JOLSF152-SCD  PIC  9(002).                       I.050108
004430             05  JOLSF152-TCD  PIC  9(003).                       I.050108
004440             05  F             PIC  X(004).                       I.050108
004450           04  JOLSF152-DNO.                                      I.050108
004460             05  F             PIC  X(002).                       I.050108
004470             05  JOLSF152-DNOD PIC  9(007).                       I.050108
004480           04  JOLSF152-DGN    PIC  9(002).                       I.050108
004490         03  JOLSF152-JAN      PIC  X(013).                       I.050108
004500         03  JOLSF152-GAR      PIC  X(006).                       I.050108
004510         03  F                 PIC  X(001).                       I.050108
004520         03  JOLSF152-TNI      PIC  X(003).                       I.050108
004530         03  JOLSF152-SU       PIC  9(005).                       I.050108
004540         03  F                 PIC  X(001).                       I.050108
004550         03  JOLSF152-GTN      PIC  9(007).                       I.050108
004560         03  F                 PIC  X(002).                       I.050108
004570         03  JOLSF152-UTN      PIC  9(007).                       I.050108
004580         03  JOLSF152-GKIN     PIC  9(010).                       I.050108
004590         03  JOLSF152-UKIN     PIC  9(010).                       I.050108
004600         03  F                 PIC  X(009).                       I.050108
004610         03  JOLSF152-SHN      PIC  X(025).                       I.050108
004620         03  JOLSF152-HSC      PIC  X(008).                       I.050108
004630         03  JOLSF152-COR      PIC  X(006).                       I.050108
004640         03  JOLSF152-SIZ      PIC  X(005).                       I.050108
004650         03  F                 PIC  X(004).                       I.050108
004660         03  JOLSF152-KKK      PIC  X(025).                       I.050108
004670         03  JOLSF152-PCH      PIC  X(001).                       I.050108
004680         03  JOLSF152-PSI      PIC  X(001).                       I.050108
004690         03  JOLSF152-PBM      PIC  9(002).                       I.050108
004700         03  JOLSF152-PJAN     PIC  X(013).                       I.050108
004710         03  JOLSF152-PSHN     PIC  X(020).                       I.050108
004720         03  JOLSF152-PKKK     PIC  X(020).                       I.050108
004730         03  JOLSF152-PUTN     PIC  9(007).                       I.050108
004740         03  JOLSF152-PMS      PIC  9(005).                       I.050108
004750         03  F                 PIC  X(017).                       I.050108
004760         03  JOLSF152-PC       PIC  9(001).                       I.050108
004770*----ÉgÉâÉXÉRëºìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇUÅj
004780 01  JOLSF16-REC.                                                 I.080325
004790     02  JOLSF16-01            PIC 9(02).                         I.080325
004800     02  JOLSF16-A.                                               I.080325
004810         03  JOLSF16-KEY.                                         I.080325
004820             04  JOLSF16-DNO   PIC  9(006).                       I.080325
004830             04  JOLSF16-GNO   PIC  9(001).                       I.080325
004840         03  JOLSF16-DATE      PIC  9(006).                       I.080325
004850         03  JOLSF16-TCD       PIC  9(004).                       I.080325
004860         03  JOLSF16-CCD       PIC  9(003).                       I.080325
004870         03  JOLSF16-TPC       PIC  9(004).                       I.100922
004880*****    03  JOLSF16-TPC       PIC  9(003).                       D.100922
004890         03  JOLSF16-HCD       PIC  9(006).                       I.080325
004900         03  JOLSF16-SIZ       PIC  X(003).                       I.080325
004910         03  JOLSF16-SKB       PIC  9(001).                       I.080325
004920         03  JOLSF16-SNO       PIC  9(002).                       I.080325
004930         03  JOLSF16-SU        PIC S9(005).                       I.080325
004940         03  JOLSF16-GT        PIC  9(007).                       I.080325
004950         03  JOLSF16-UT        PIC  9(007).                       I.080325
004960         03  JOLSF16-GKIN      PIC S9(008).                       I.080325
004970         03  JOLSF16-UKIN      PIC S9(008).                       I.080325
004980         03  JOLSF16-JNOD.                                        I.080325
004990             04  JOLSF16-JNO   PIC  9(006).                       I.080325
005000             04  JOLSF16-JGN   PIC  9(001).                       I.080325
005010         03  JOLSF16-SOK       PIC  9(001).                       I.080325
005020         03  JOLSF16-UNS       PIC  9(001).                       I.080325
005030         03  JOLSF16-ISU       PIC  9(003).                       I.080325
005040         03  JOLSF16-HNO       PIC  X(010).                       I.080325
005050         03  JOLSF16-TEKI      PIC  N(028).                       I.080325
005060         03  JOLSF16-TED   REDEFINES JOLSF16-TEKI.                I.080325
005070             04  JOLSF16-THT   PIC  N(009).                       I.080325
005080             04  JOLSF16-TTE   PIC  N(019).                       I.080325
005090         03  JOLSF16-TRN       PIC  X(020).                       I.080325
005100         03  JOLSF16-JAN       PIC  X(013).                       I.100922
005110         03  F                 PIC  X(070).                       I.100922
005120         03  JOLSF16-PRC       PIC  9(001).                       I.080325
005130         03  JOLSF16-UPC       PIC  9(001).                       I.080325
005140*****02  F                     PIC  X(084).                       D.100922
005150*----ê‘ÇøÇ·ÇÒñ{ï‹ìùàÍì`ï[    ÅiãÊï™ÅÅÇPÇVÅj
005160 01  JOLSF17-REC.                                                 I.091111
005170     02  JOLSF17-01            PIC 9(02).                         I.091111
005180     02  JOLSF17-KEY.                                             I.091111
005190         03  JOLSF17-STC       PIC  9(007).                       I.091111
005200         03  JOLSF17-DNO       PIC  9(007).                       I.100901
005210*****    03  JOLSF17-DNO       PIC  9(006).                       D.100901
005220         03  JOLSF17-DGN       PIC  9(002).                       I.091111
005230     02  JOLSF17-JAN           PIC  X(013).                       I.091111
005240     02  JOLSF17-SU            PIC  9(006).                       I.091111
005250     02  JOLSF17-GTN           PIC  9(007).                       I.091111
005260     02  JOLSF17-UTN           PIC  9(007).                       I.091111
005270     02  JOLSF17-GKIN          PIC  9(010).                       I.091111
005280     02  JOLSF17-UKIN          PIC  9(010).                       I.091111
005290     02  JOLSF17-DPM           PIC  X(002).                       I.091111
005300     02  JOLSF17-CLS           PIC  X(003).                       I.091111
005310     02  JOLSF17-SHM           PIC  X(013).                       I.091111
005320     02  JOLSF17-MKH           PIC  X(010).                       I.091111
005330     02  JOLSF17-MSB           PIC  X(010).                       I.091111
005340     02  JOLSF17-TY            PIC  X(002).                       I.091111
005350     02  JOLSF17-HCD           PIC  9(006).                       I.091111
005360     02  JOLSF17-COR           PIC  N(004).                       I.091111
005370     02  JOLSF17-SIZ           PIC  X(004).                       I.091111
005380     02  JOLSF17-NSU           PIC  9(006).                       I.091111
005390     02  JOLSF17-TSC           PIC  9(001).                       I.091111
005400*****02  F                     PIC  X(009).                       D.100901
005410     02  F                     PIC  X(008).                       I.100901
005420     02  JOLSF17-CCD           PIC  9(003).                       I.091111
005430     02  JOLSF17-TNA           PIC  N(014).                       I.091111
005440     02  JOLSF17-HNO           PIC  9(009).                       I.091111
005450     02  JOLSF17-HNGP          PIC  9(006).                       I.091111
005460     02  JOLSF17-NNGP          PIC  9(006).                       I.091111
005470     02  JOLSF17-THC           PIC  9(006).                       I.091111
005480     02  JOLSF17-BI            PIC  X(010).                       I.091111
005490     02  JOLSF17-SNGP          PIC  9(008).                       I.091111
005500     02  JOLSF17-HNA           PIC  X(006).                       I.091111
005510     02  JOLSF17-ZON           PIC  X(004).                       I.091111
005520     02  JOLSF17-DC            PIC  9(002).                       I.091111
005530     02  F                     PIC  X(005).                       I.091111
005540     02  JOLSF17-DNGP          PIC  9(008).                       I.091111
005550     02  JOLSF17-NRC           PIC  9(001).                       I.091111
005560     02  F                     PIC  X(008).                       I.091111
005570     02  JOLSF17-PC            PIC  9(001).                       I.091111
005580     02  JOLSF17-RC            PIC  9(001).                       I.091111
