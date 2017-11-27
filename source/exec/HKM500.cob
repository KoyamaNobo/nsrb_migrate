       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKM500.
      *******************************************************************
      *    ìæà”êÊ (TM•TTM) É}ÉXÉ^Å[ÉÅÉìÉeÉiÉìÉX                         *
      *    SCREEN      : SCHK50                                         *
      *    JS-SIGN     : 0=ÉÅÉìÉe , 1=ñ‚çáÇπ                            *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  HEAD01.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(008) VALUE X"1A26222166222176".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                "ìæà”êÊÉ}ÉXÉ^Å[Å@ÉäÉXÉg".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  HEAD02.
           02  F              PIC  X(106) VALUE SPACE.
           02  F              PIC  N(003) VALUE "çÏê¨ì˙".
           02  F              PIC  X(002) VALUE " '".
           02  H-NGP          PIC 99/99/99.
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "ÅñÅñÅñÅ@Å@ìæà”êÊÉ}ÉXÉ^Å[Å@ÉvÉãÅ[ÉtÉäÉXÉgÅ@Å@ÅñÅñÅñ".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC  Z(002).
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(113) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@ïî".
           02  F              PIC  N(002) VALUE "Å@ï{".
           02  F              PIC  N(002) VALUE "Å@íS".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@ë‰".
           02  F              PIC  N(002) VALUE "Å@îı".
           02  F              PIC  N(002) VALUE "Å@ê∂".
           02  F              PIC  N(002) VALUE "Å@ã≥".
       01  HEAD3.
           02  F              PIC  X(005) VALUE "∫∞ƒﬁ ".
           02  F              PIC  N(010) VALUE
                "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "èZÅ@Å@Å@èäÅ@Å@Åiè„Åj".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÅßÅ@".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ÇsÇdÇkÅ@".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ÇeÇ`ÇwÅ@".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@ñÂ".
           02  F              PIC  N(002) VALUE "Å@åß".
           02  F              PIC  N(002) VALUE "Å@ìñ".
           02  F              PIC  N(002) VALUE "Å@ê≈".
           02  F              PIC  N(002) VALUE "Å@í†".
           02  F              PIC  N(002) VALUE "Å@çl".
           02  F              PIC  N(002) VALUE "Å@ã¶".
           02  F              PIC  N(002) VALUE "Å@àÁ".
       01  HEAD4.
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(013) VALUE "ƒ ∏ ≤ ª ∑ “ ≤".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(004) VALUE "Å@Åiâ∫Åj".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(002) VALUE "í˜ì˙".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "éxï•".
           02  F              PIC  N(002) VALUE "Å@éÌ".
           02  F              PIC  X(003) VALUE "1 2".
           02  F              PIC  N(002) VALUE "Å@ì¸".
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  N(002) VALUE "Å@éË".
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ëóã‡óøÅ@".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "åªã‡à¯Å@".
           02  F              PIC  N(004) VALUE "éËêîóøÅ@".
           02  F              PIC  N(004) VALUE "ó^êMå¿ìx".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "â^í¿".
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "â^í¿".
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(008) VALUE "êøãÅópìæà”êÊñºÅ@".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "èZÅ@Å@Å@èäÅ@Å@Åiè„Åj".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÅßÅ@".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ÇsÇdÇkÅ@".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ÇeÇ`ÇwÅ@".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(004) VALUE "Å@äJénì˙".
           02  F              PIC  N(004) VALUE "Å@í‚é~ì˙".
       01  HEAD6.
           02  F              PIC  X(005) VALUE "∫∞ƒﬁ)".
           02  F              PIC  N(008) VALUE "ì¸Å@ã‡Å@êÊÅ@ñºÅ@".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(004) VALUE "Å@Åiâ∫Åj".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(008) VALUE "óÃé˚èëìæà”êÊñºÅ@".
           02  F              PIC  X(048) VALUE SPACE.
       01  W-P1.
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-JSU          PIC  N(020).
           02  F              PIC  X(001).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  P-FAX          PIC  X(014).
           02  F              PIC  X(001).
           02  P-BC           PIC  9(001).
           02  F              PIC  X(001).
           02  P-FKC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-ZEI          PIC  9(001).
           02  P-D1.
             03  F            PIC  X(002).
             03  P-DCC        PIC  9(001).
             03  F            PIC  X(002).
             03  P-BIK        PIC  9(001).
             03  F            PIC  X(002).
             03  P-SSC        PIC  9(001).
             03  F            PIC  X(002).
             03  P-KSC        PIC  9(001).
             03  F            PIC  X(001).
           02  P-D2    REDEFINES P-D1.
             03  F            PIC  X(002).
             03  P-SNGD       PIC 99/99.
             03  F            PIC  X(001).
             03  P-ENGD       PIC 99/99.
       01  W-P2.
           02  F              PIC  X(005).
           02  P-KANA         PIC  X(036).
           02  F              PIC  X(004).
           02  P-JSS          PIC  N(020).
           02  F              PIC  X(001).
           02  P-SS           PIC  Z(002).
           02  F              PIC  X(002).
           02  P-SHD          PIC  Z(002).
           02  F              PIC  X(003).
           02  P-SHC1         PIC  Z(001).
           02  F              PIC  X(001).
           02  P-SHC2         PIC  Z(001).
           02  F              PIC  X(001).
           02  P-NKY          PIC  Z(003).
           02  F              PIC  X(001).
           02  P-SSI          PIC  Z(003).
           02  F              PIC  X(001).
           02  P-SKC          PIC  Z(001).
           02  F              PIC  X(001).
           02  P-SKR          PIC  Z(004).
           02  F              PIC  X(001).
           02  P-SGT          PIC  Z(001).
           02  F              PIC  X(001).
           02  P-SGR          PIC Z.Z.
           02  F              PIC  X(001).
           02  P-STT          PIC  Z(001).
           02  F              PIC  X(001).
           02  P-STR          PIC Z.Z.
           02  F              PIC  X(001).
           02  P-YG           PIC  Z(006).
           02  F              PIC  X(001).
           02  P-UTC1         PIC  Z(001).
           02  F              PIC  X(001).
           02  P-UTK1         PIC  Z(004).
           02  F              PIC  X(001).
           02  P-UTC2         PIC  Z(001).
           02  F              PIC  X(001).
           02  P-UTK2         PIC  Z(004).
       01  W-P3.
           02  P-NTCD         PIC  9(004).
           02  P-R            PIC  X(001).
           02  P-NNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-SJSS         PIC  N(020).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(016).
           02  F              PIC  X(026).
           02  P-SNG          PIC 99/99.
           02  F              PIC  X(001).
           02  P-ENG          PIC 99/99.
       01  W-R.
           02  W-KEY2.
             03  W-NTCD       PIC  9(004).
             03  W-KEY.
               04  W-TCD      PIC  9(004).
           02  W-NAME         PIC  N(026).
           02  W-JSU          PIC  N(020).
           02  W-JSS          PIC  N(020).
           02  W-UNO          PIC  X(008).
           02  W-TEL          PIC  X(014).
           02  W-FAX          PIC  X(014).
           02  W-FKC          PIC  9(002).                              ìsìπï{åß
           02  W-BC           PIC  9(001).                              ïîñÂ∫∞ƒﬁ
           02  F              PIC  9(002).
           02  W-TNC          PIC  9(002).                              íSìñé“
           02  W-SS           PIC  9(002).                              êøãÅí˜ì˙
           02  W-NKY          PIC  9(003).                              ì¸ã‡ª≤ƒ
           02  W-DCC          PIC  9(001).
           02  W-TGC          PIC  9(001).                              éËå`ãÊï™
           02  W-YG           PIC  9(006).                              ó^êMå¿ìx
           02  W-BIK          PIC  9(001).
           02  W-ZEI          PIC  9(001).
           02  F              PIC  X(009).
           02  W-TNA          PIC  N(016).                              ìæà”êÊñº
           02  W-SNA          PIC  N(026).
           02  W-SJSU         PIC  N(020).
           02  W-SJSS         PIC  N(020).
           02  W-SUNO         PIC  X(008).
           02  W-STEL         PIC  X(014).
           02  W-SFAX         PIC  X(014).
           02  W-SHD          PIC  9(002).
           02  W-SSI          PIC  9(003).
           02  W-SHC1         PIC  9(001).
           02  W-SHC2         PIC  9(001).
           02  W-SGT          PIC  9(001).
           02  W-SGR          PIC  9(001)V9(01).
           02  W-STT          PIC  9(001).
           02  W-STR          PIC  9(001)V9(01).
           02  W-SKR          PIC  9(004).
           02  W-SKC          PIC  9(001).
           02  W-KSC          PIC  9(001).
           02  W-SSC          PIC  9(001).
           02  W-UTC1         PIC  9(001).
           02  W-UTK1         PIC  9(004).
           02  W-UTC2         PIC  9(001).
           02  W-UTK2         PIC  9(004).
           02  F              PIC  X(025).
           02  W-KANA         PIC  X(036).
           02  W-DNG          PIC  9(006).
           02  W-SNG.
             03  W-SN         PIC  9(002).
             03  W-SG         PIC  9(002).
           02  W-ENG.
             03  W-EN         PIC  9(002).
             03  W-EG         PIC  9(002).
       01  W-BACK.
           02  WB-NTCD        PIC  9(004).
           02  WB-NAME        PIC  N(026).
           02  WB-JSU         PIC  N(020).
           02  WB-JSS         PIC  N(020).
           02  WB-UNO         PIC  X(008).
           02  WB-TEL         PIC  X(014).
           02  WB-FAX         PIC  X(014).
           02  WB-FKC         PIC  9(002).
           02  WB-BC          PIC  9(001).
           02  WB-TNC         PIC  9(002).
           02  WB-SS          PIC  9(002).
           02  WB-NKY         PIC  9(003).                              ì¸ã‡ª≤ƒ
           02  WB-DCC         PIC  9(001).
           02  WB-YG          PIC  9(006).                              ó^êMå¿ìx
           02  WB-BIK         PIC  9(001).
           02  WB-ZEI         PIC  9(001).
           02  WB-TNA         PIC  N(016).                              ìæà”êÊñº
           02  WB-SNA         PIC  N(026).
           02  WB-SJSU        PIC  N(020).
           02  WB-SJSS        PIC  N(020).
           02  WB-SUNO        PIC  X(008).
           02  WB-STEL        PIC  X(014).
           02  WB-SFAX        PIC  X(014).
           02  WB-SHD         PIC  9(002).
           02  WB-SSI         PIC  9(003).
           02  WB-SHC1        PIC  9(001).
           02  WB-SHC2        PIC  9(001).
           02  WB-SGT         PIC  9(001).
           02  WB-SGR         PIC  9(001)V9(01).
           02  WB-STT         PIC  9(001).
           02  WB-STR         PIC  9(001)V9(01).
           02  WB-SKR         PIC  9(004).
           02  WB-SKC         PIC  9(001).
           02  WB-KSC         PIC  9(001).
           02  WB-SSC         PIC  9(001).
           02  WB-UTC1        PIC  9(001).
           02  WB-UTK1        PIC  9(004).
           02  WB-UTC2        PIC  9(001).
           02  WB-UTK2        PIC  9(004).
           02  WB-KANA        PIC  X(036).
           02  WB-SNG         PIC  9(004).
           02  WB-ENG         PIC  9(004).
       01  W-TR.
           02  WT-KEY.                                                  ∫∞ƒﬁ
             03  WT-TCD       PIC  9(004).                                ìæà”êÊ
           02  WT-TD.
             03  WT-TZZ       PIC S9(009).                                ëOåééc
             03  WT-TZZZ      PIC S9(007).                                Å@è¡îÔ
             03  WT-TUZ       PIC S9(009).                                îÑä|éc
             03  WT-TUZZ      PIC S9(007).                                Å@è¡îÔ
             03  WT-TUA       PIC S9(009).                                îÑè„ã‡
             03  WT-TUAZ      PIC S9(007).                                Å@è¡îÔ
             03  WT-TNB       PIC S9(008).                                ílà¯ã‡
             03  WT-TNBZ      PIC S9(006).                                Å@è¡îÔ
             03  WT-TNK       PIC S9(009).                                ì¸ã‡ã‡
             03  WT-TNKZ      PIC S9(007).                                Å@è¡îÔ
             03  WT-TUG       PIC S9(009).                                îÑè„å¥
           02  F              PIC  9(002).
           02  WT-TNC         PIC  9(002).                                íSìñé“
           02  WT-FKC         PIC  9(002).                                ìsìπï{
           02  WT-BC          PIC  9(001).                                    ïî
           02  WT-DCC         PIC  9(001).
           02  F              PIC  X(029).
       01  W-DATA.
           02  W-MC           PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-SED.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004).
           02  W-RTCD         PIC  9(004).
           02  W-NTCDB        PIC  9(004).
           02  W-NBC          PIC  9(001).
           02  W-OBC          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-NGC          PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-AS           PIC  9(001).
           02  W-NGP          PIC  9(006).
           02  W-ACTD         PIC  9(001).
           02  W-TCK          PIC  9(007).
           02  W-BD.
             03  W-BTNC       PIC  9(002).
             03  W-BBC        PIC  9(001).
           02  W-CD.
             03  W-C1         PIC  9(001).
             03  W-C2         PIC  9(001).
             03  W-C3         PIC  9(001).
           02  W-OD.
             03  W-OSS        PIC  9(002).
             03  W-OTNC       PIC  9(002).
             03  W-OBMC       PIC  9(001).
             03  W-ODCC       PIC  9(001).
           02  W-ACP          PIC  9(001).
           02  W-END          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-OVC          PIC  9(001).
           02  W-MS           PIC  9(001).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LITTM.
           COPY LITCM.
           COPY LITHTM.
           COPY LIHKBM.
           COPY LITSKF.
           COPY LSPF.
       01  TZNT-M_HKM500.
           02  TZNT-M_PNAME1   PIC  X(005)  VALUE "TZNTM".
           02  F               PIC  X(001).
           02  TZNT-M_LNAME    PIC  X(013)  VALUE "TZNT-M_HKM500".
           02  F               PIC  X(001).
           02  TZNT-M_KEY1     PIC  X(100)  VALUE SPACE.
           02  TZNT-M_KEY2     PIC  X(100)  VALUE SPACE.
           02  TZNT-M_SORT     PIC  X(100)  VALUE SPACE.
           02  TZNT-M_IDLST    PIC  X(100)  VALUE SPACE.
           02  TZNT-M_RES      USAGE  POINTER.
       01  TZNT-R.
           02  TZNT-KEY.
             03  TZNT-TCD     PIC  9(004).
             03  TZNT-IKC     PIC  9(001).
           02  TZNT-AUD.
             03  TZNT-UD  OCCURS  24  PIC S9(009).
           02  F              PIC  X(020).
           02  TZNT-AAD.
             03  TZNT-AD  OCCURS  24  PIC S9(009).
           02  F              PIC  X(055).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT     PIC  9(001).
           02  A-MC      PIC  9(001).
           02  A-CHK     PIC  9(001).
           02  FILLER.
             03  A-KEY     PIC  9(004).
             03  A-NAME    PIC  N(026).
           02  FILLER.
             03  A-KANA    PIC  X(036).
             03  A-BC      PIC  9(001).
           02  FILLER.
             03  A-FKC     PIC  9(002).
             03  A-UNO     PIC  X(008).
             03  A-TEL     PIC  X(014).
           02  FILLER.
             03  A-JSU     PIC  N(020).
             03  A-FAX     PIC  X(014).
           02  FILLER.
             03  A-JSS     PIC  N(020).
             03  A-TNC     PIC  9(002).
           02  FILLER.
             03  A-ZEI     PIC  9(001).
             03  A-DCC     PIC  9(001).
             03  A-BIK     PIC  9(001).
             03  A-SSC     PIC  9(001).
             03  A-KSC     PIC  9(001).
             03  A-SNG     PIC  9(004).
             03  A-ENG     PIC  9(004).
           02  FILLER.
             03  A-SS      PIC  9(002).
             03  A-SHD     PIC  9(002).
             03  A-SHC1    PIC  9(001).
             03  A-SHC2    PIC  9(001).
             03  A-NKY     PIC  9(003).
             03  A-SSI     PIC  9(003).
             03  A-SKC     PIC  9(001).
             03  A-SKR     PIC  9(004).
           02  FILLER.
             03  A-SGT     PIC  9(001).
             03  A-SGR     PIC  9(001)V9(01).
             03  A-STT     PIC  9(001).
             03  A-STR     PIC  9(001)V9(01).
             03  A-YG      PIC  9(006).
             03  A-UTC1    PIC  9(001).
             03  A-UTK1    PIC  9(004).
             03  A-UTC2    PIC  9(001).
             03  A-UTK2    PIC  9(004).
           02  A-SNA       PIC  N(026).
           02  FILLER.
             03  A-SJSU    PIC  N(020).
           02  FILLER.
             03  A-SJSS    PIC  N(020).
           02  FILLER.
             03  A-SUNO    PIC  X(008).
             03  A-STEL    PIC  X(014).
             03  A-SFAX    PIC  X(014).
           02  A-TNA       PIC  N(016).
           02  A-NTCD      PIC  9(004).
           02  A-SEN      PIC  9(001).
           02  FILLER.
             03  A-STCD    PIC  9(004).
             03  A-ETCD    PIC  9(004).
           02  A-DMM      PIC  9(001).
       01  C-DSP.
           02  D-BCN     PIC  N(006).
           02  FILLER.
             03  D-FKC     PIC  9(002).
             03  D-FKN     PIC  N(004).
           02  FILLER.
             03  D-TNC     PIC  9(002).
             03  D-TNN     PIC  N(011).
           02  FILLER.
             03  D-DCC     PIC  9(001).
             03  D-BIK     PIC  Z(001).
             03  D-SSC     PIC  Z(001).
             03  D-KSC     PIC  Z(001).
           02  FILLER.
             03  D-SS      PIC  Z(002).
             03  D-SHD     PIC  Z(002).
             03  D-SHC1    PIC  Z(001).
             03  D-SHC2    PIC  Z(001).
             03  D-NKY     PIC  Z(003).
             03  D-SSI     PIC  Z(003).
             03  D-SKC     PIC  Z(001).
             03  D-SKR     PIC  Z(004).
           02  FILLER.
             03  D-SGT     PIC  Z(001).
             03  D-SGR     PIC Z.Z.
             03  D-STT     PIC  Z(001).
             03  D-STR     PIC Z.Z.
             03  D-YG      PIC  Z(006).
             03  D-UTC1    PIC  Z(001).
             03  D-UTK1    PIC  Z(004).
             03  D-UTC2    PIC  Z(001).
             03  D-UTK2    PIC  Z(004).
           02  D-NNA       PIC  N(026).
           02  D-PM.
             03  FILLER  PIC  N(024) VALUE
                  "ÅñÅñÅñÅ@Å@ìæà”êÊÉ}ÉXÉ^Å[Å@ÉÅÉìÉeÉiÉìÉXÅ@Å@ÅñÅñÅñ".
             03  FILLER  PIC  X(037) VALUE
                  "ìoò^=1 èCê≥=2 çÌèú=3 çÏï\=4 ñ‚çáÇπ=5 ".
             03  FILLER  PIC  X(014) VALUE
                  " èIóπ=9   ÿ¿∞›".
             03  FILLER  PIC  X(033) VALUE
                  "ëÂå©èoÇµ PRINT  ΩŸ=5 º≈≤=1   ÿ¿∞›".
             03  FILLER  PIC  X(037) VALUE
                  "<  ëSïî=0 óöï®=1 çHïi=2 ..... ÿ¿∞›  >".
             03  FILLER  PIC  X(029) VALUE
                  "í‚é~ï™àÛéö  º≈≤=0 ΩŸ=1   ÿ¿∞›".
             03  FILLER  PIC  X(036) VALUE
                  "ìæà”êÊ∫∞ƒﬁ 0000 ÇÊÇË 9999 Ç‹Ç≈ë≈èoÇµ".
             03  FILLER    PIC  X(022) VALUE
                  "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  C-SPC.
           02  FILLER.
             03  S-SNG     PIC  X(004) VALUE "    ".
             03  S-ENG     PIC  X(004) VALUE "    ".
           02  D-NTCD.
             03    PIC  X(004) VALUE "    ".
             03    PIC  X(052) VALUE
                "                                                    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(015) VALUE
                  "***  TM ≈º  ***".
             03  E-ME2     PIC  X(016) VALUE
                  "***  TTM ≈º  ***".
             03  E-ME4     PIC  X(021) VALUE
                  "***  TM ƒ≥€∏ Ωﬁ–  ***".
             03  E-ME5     PIC  X(022) VALUE
                  "***  TTM ƒ≥€∏ Ωﬁ–  ***".
             03  E-ME7     PIC  N(013) VALUE
                  "ÅñÅñÅñÅ@ÉLÉÉÉìÉZÉãÅ@ÅñÅñÅñ".
             03  E-ME8     PIC  N(014) VALUE
                  "ÅñÅñÅñÅ@ïîñÂïœçXïsâ¬Å@ÅñÅñÅñ".
             03  E-ME9.
               04  FILLER  PIC  X(029) VALUE
                    "***  ∆≠≥∑›∫∞ƒﬁ ±ÿ (    )  ***".
               04  FILLER  PIC  9(004).
             03  E-ME10    PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME11    PIC  X(025) VALUE
                  "***  TTM WRITE ¥◊∞ 1  ***".
             03  E-ME12    PIC  X(025) VALUE
                  "***  TTM WRITE ¥◊∞ 2  ***".
             03  E-ME13    PIC  X(025) VALUE
                  "***  TTM REWRITE ¥◊∞  ***".
             03  E-ME14    PIC  X(024) VALUE
                  "***  TTM DELETE ¥◊∞  ***".
             03  E-ME25    PIC  X(025) VALUE
                  "***  TCM WRITE ¥◊∞ 1  ***".
             03  E-ME26    PIC  X(025) VALUE
                  "***  TCM WRITE ¥◊∞ 2  ***".
             03  E-ME27    PIC  X(025) VALUE
                  "***  TCM REWRITE ¥◊∞  ***".
             03  E-ME29    PIC  X(024) VALUE
                  "***  TCM DELETE ¥◊∞  ***".
             03  E-ME32    PIC  X(022) VALUE
                  "***  TM WRITE ¥◊∞  ***".
             03  E-ME33    PIC  X(024) VALUE
                  "***  TM REWRITE ¥◊∞  ***".
             03  E-ME34    PIC  X(023) VALUE
                  "***  TM DELETE ¥◊∞  ***".
             03  E-ME37    PIC  X(026) VALUE
                  "***  TZNTM DELETE ¥◊∞  ***".
             03  E-ME45.
               04  FILLER  PIC  X(039) VALUE
                    "***  THTM DELETE ¥◊∞ (           )  ***".
               04  FILLER  PIC  X(011).
             03  E-ME51    PIC  X(018) VALUE
                  "***  DATEM ≈º  ***".
             03  E-ME52    PIC  X(027) VALUE
                  "***  DATEM REWRITE ¥◊∞  ***".
             03  E-ME55    PIC  X(026) VALUE
                  "***  TSKF REWRITE ¥◊∞  ***".
             03  E-ME56    PIC  X(025) VALUE
                  "***  TSKF DELETE ¥◊∞  ***".
             03  E-ME59    PIC  X(035) VALUE
                  "***  ƒ∏≤ª∑æ≤∑≠≥Ãß≤Ÿ(TSKF) ¡™Ø∏  ***".
             03  E-ME81    PIC  X(028) VALUE
                  "***  DATA ∂ﬁ ZERO √ﬁ ≈≤  ***".
             03  E-ME82    PIC  X(021) VALUE
                  "***  √∂ﬁ¿ √ﬁ º÷≥  ***".
             03  E-ME83    PIC  X(032) VALUE
                  "***  »›∂› ≥ÿ±πﬁ ¿≤À √ﬁ∞¿ ±ÿ  ***".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "482" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "61" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MC" "9" "6" "43" "1" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MC" BY REFERENCE W-MC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "8" "44" "1" "A-MC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "5" "0" "56" "A-CHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "5" "7" "4" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "5" "21" "52" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "6" "0" "37" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KANA" "X" "6" "21" "36" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KANA" BY REFERENCE W-KANA "36" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BC" "9" "6" "64" "1" "A-KANA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BC" BY REFERENCE W-BC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "7" "0" "24" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKC" "9" "7" "11" "2" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKC" BY REFERENCE W-FKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNO" "X" "7" "26" "8" "A-FKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNO" BY REFERENCE W-UNO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEL" "X" "7" "57" "14" "A-UNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEL" BY REFERENCE W-TEL "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "8" "0" "54" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSU" "N" "8" "7" "40" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSU" BY REFERENCE W-JSU "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FAX" "X" "8" "57" "14" "A-JSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FAX" BY REFERENCE W-FAX "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "9" "0" "42" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSS" "N" "9" "7" "40" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSS" BY REFERENCE W-JSS "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNC" "9" "9" "55" "2" "A-JSS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNC" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-ACP" " " "10" "0" "13" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZEI" "9" "10" "5" "1" " " "09C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZEI" BY REFERENCE W-ZEI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DCC" "9" "10" "12" "1" "A-ZEI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DCC" BY REFERENCE W-DCC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BIK" "9" "10" "19" "1" "A-DCC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BIK" BY REFERENCE W-BIK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSC" "9" "10" "26" "1" "A-BIK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSC" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KSC" "9" "10" "37" "1" "A-SSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KSC" BY REFERENCE W-KSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNG" "9" "10" "62" "4" "A-KSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNG" BY REFERENCE W-SNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENG" "9" "10" "76" "4" "A-SNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENG" BY REFERENCE W-ENG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-ACP" " " "12" "0" "17" "09C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SS" "9" "12" "11" "2" " " "10C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SS" BY REFERENCE W-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHD" "9" "12" "21" "2" "A-SS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHD" BY REFERENCE W-SHD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHC1" "9" "12" "35" "1" "A-SHD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHC1" BY REFERENCE W-SHC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHC2" "9" "12" "40" "1" "A-SHC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHC2" BY REFERENCE W-SHC2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NKY" "9" "12" "50" "3" "A-SHC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NKY" BY REFERENCE W-NKY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSI" "9" "12" "62" "3" "A-NKY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSI" BY REFERENCE W-SSI "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKC" "9" "12" "73" "1" "A-SSI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKC" BY REFERENCE W-SKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKR" "9" "12" "75" "4" "A-SKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKR" BY REFERENCE W-SKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-ACP" " " "13" "0" "22" "10C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGT" "9" "13" "13" "1" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGT" BY REFERENCE W-SGT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGR" "9V9" "13" "15" "2" "A-SGT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGR" BY REFERENCE W-SGR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STT" "9" "13" "31" "1" "A-SGR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STT" BY REFERENCE W-STT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STR" "9V9" "13" "33" "2" "A-STT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STR" BY REFERENCE W-STR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YG" "9" "13" "47" "6" "A-STR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YG" BY REFERENCE W-YG "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UTC1" "9" "13" "63" "1" "A-YG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UTC1" BY REFERENCE W-UTC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UTK1" "9" "13" "65" "4" "A-UTC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UTK1" BY REFERENCE W-UTK1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UTC2" "9" "13" "73" "1" "A-UTK1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UTC2" BY REFERENCE W-UTC2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UTK2" "9" "13" "75" "4" "A-UTC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UTK2" BY REFERENCE W-UTK2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNA" "N" "15" "17" "52" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNA" BY REFERENCE W-SNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-ACP" " " "16" "0" "40" "A-SNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJSU" "N" "16" "7" "40" " " "13C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJSU" BY REFERENCE W-SJSU "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-ACP" " " "17" "0" "40" "13C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJSS" "N" "17" "7" "40" " " "14C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJSS" BY REFERENCE W-SJSS "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "15C-ACP" " " "18" "0" "36" "14C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SUNO" "X" "18" "5" "8" " " "15C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SUNO" BY REFERENCE W-SUNO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STEL" "X" "18" "21" "14" "A-SUNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STEL" BY REFERENCE W-STEL "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SFAX" "X" "18" "43" "14" "A-STEL" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SFAX" BY REFERENCE W-SFAX "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNA" "N" "19" "17" "32" "15C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNA" BY REFERENCE W-TNA "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NTCD" "9" "21" "11" "4" "A-TNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NTCD" BY REFERENCE W-NTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "41" "1" "A-NTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "19C-ACP" " " "12" "0" "8" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "12" "28" "4" " " "19C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "12" "38" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "58" "1" "19C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "399" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-BCN" "N" "6" "66" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN" BY REFERENCE HKB-BMNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "7" "0" "10" "D-BCN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-FKC" "9" "7" "11" "2" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-FKC" BY REFERENCE W-FKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-FKN" "N" "7" "14" "8" "D-FKC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-FKN" BY REFERENCE HKB-FKNA "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-DSP" " " "9" "0" "24" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNC" "9" "9" "55" "2" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNC" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNN" "N" "9" "58" "22" "D-FKC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNN" BY REFERENCE HKB-TNNA "28" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-DSP" " " "10" "0" "4" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-DCC" "9" "10" "12" "1" " " "03C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-DCC" BY REFERENCE W-DCC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-BIK" "Z" "10" "19" "1" "D-DCC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BIK" BY REFERENCE W-BIK "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SSC" "Z" "10" "26" "1" "D-BIK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSC" BY REFERENCE W-SSC "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-KSC" "Z" "10" "37" "1" "D-SSC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KSC" BY REFERENCE W-KSC "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-DSP" " " "12" "0" "17" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SS" "Z" "12" "11" "2" " " "04C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SS" BY REFERENCE W-SS "2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SHD" "Z" "12" "21" "2" "D-SS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHD" BY REFERENCE W-SHD "2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SHC1" "Z" "12" "35" "1" "D-SHD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHC1" BY REFERENCE W-SHC1 "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SHC2" "Z" "12" "40" "1" "D-SHC1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHC2" BY REFERENCE W-SHC2 "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NKY" "Z" "12" "50" "3" "D-SHC2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NKY" BY REFERENCE W-NKY "3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SSI" "Z" "12" "62" "3" "D-NKY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSI" BY REFERENCE W-SSI "3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SKC" "Z" "12" "73" "1" "D-SSI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SKC" BY REFERENCE W-SKC "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SKR" "Z" "12" "75" "4" "D-SKC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SKR" BY REFERENCE W-SKR "4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-DSP" " " "13" "0" "24" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SGT" "Z" "13" "13" "1" " " "05C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SGT" BY REFERENCE W-SGT "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SGR" "Z.Z" "13" "15" "3" "D-SGT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SGR" BY REFERENCE W-SGR "2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-STT" "Z" "13" "31" "1" "D-SGR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-STT" BY REFERENCE W-STT "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-STR" "Z.Z" "13" "33" "3" "D-STT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-STR" BY REFERENCE W-STR "2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-YG" "Z" "13" "47" "6" "D-STR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-YG" BY REFERENCE W-YG "6" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-UTC1" "Z" "13" "63" "1" "D-YG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTC1" BY REFERENCE W-UTC1 "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-UTK1" "Z" "13" "65" "4" "D-UTC1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTK1" BY REFERENCE W-UTK1 "4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-UTC2" "Z" "13" "73" "1" "D-UTK1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTC2" BY REFERENCE W-UTC2 "1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-UTK2" "Z" "13" "75" "4" "D-UTC2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTK2" BY REFERENCE W-UTK2 "4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NNA" "N" "21" "25" "52" "05C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-PM" " " "0" "0" "256" "D-NNA" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-PM" "N" "1" "11" "48" " " "D-PM" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-PM" "X" "3" "26" "37" "01D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03D-PM" "X" "3" "52" "14" "02D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04D-PM" "X" "6" "15" "33" "03D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05D-PM" "X" "8" "15" "37" "04D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06D-PM" "X" "10" "17" "29" "05D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07D-PM" "X" "12" "17" "36" "06D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08D-PM" "X" "23" "41" "22" "07D-PM" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING
            "C-SPC" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-SPC" " " "10" "0" "8" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING
            "S-SNG" "X" "10" "62" "4" " " "01C-SPC" RETURNING RESU.
       CALL "SD_Init" USING
            "S-ENG" "X" "10" "76" "4" "S-SNG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NTCD" " " "21" "0" "56" "01C-SPC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NTCD" "X" "21" "11" "4" " " "D-NTCD" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NTCD" "X" "21" "25" "52" "01D-NTCD" " " 
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "734" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "734" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "22" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "N" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME8" "N" "24" "15" "28" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME9" " " "24" "0" "33" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME9" "X" "24" "15" "29" " " "E-ME9" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME9" "9" "24" "34" "4" "01E-ME9" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME9" BY REFERENCE T-NTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "18" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "25" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME12" "X" "24" "15" "25" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME13" "X" "24" "15" "25" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME14" "X" "24" "15" "24" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME25" "X" "24" "15" "25" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME26" "X" "24" "15" "25" "E-ME25" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME27" "X" "24" "15" "25" "E-ME26" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME29" "X" "24" "15" "24" "E-ME27" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME32" "X" "24" "15" "22" "E-ME29" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME33" "X" "24" "15" "24" "E-ME32" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME34" "X" "24" "15" "23" "E-ME33" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME37" "X" "24" "15" "26" "E-ME34" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME45" " " "24" "0" "50" "E-ME37" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME45" "X" "24" "15" "39" " " "E-ME45" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME45" "X" "24" "37" "11" "01E-ME45" " " 
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME45" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME51" "X" "24" "15" "18" "E-ME45" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME52" "X" "24" "15" "27" "E-ME51" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME55" "X" "24" "15" "26" "E-ME52" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME56" "X" "24" "15" "25" "E-ME55" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME59" "X" "24" "15" "35" "E-ME56" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME81" "X" "24" "15" "28" "E-ME59" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME82" "X" "24" "15" "21" "E-ME81" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME83" "X" "24" "15" "32" "E-ME82" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-010.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
	           CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           INITIALIZE W-DATA.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
           ELSE
               CALL "DB_F_Open" USING
                "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
               CALL "DB_F_Open" USING
                "I-O" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
                "TT-KEY" BY REFERENCE TT-KEY
               CALL "DB_F_Open" USING
                "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
               CALL "DB_F_Open" USING
                "I-O" TZNT-M_PNAME1 "SHARED" BY REFERENCE TZNT-M_IDLST
                "1" "TZNT-KEY" BY REFERENCE TZNT-KEY
               CALL "DB_F_Open" USING
                "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
                "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
                THT-KEY2
               CALL "DB_F_Open" USING
                "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
                "TSK-KEY" BY REFERENCE TSK-KEY
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           ACCEPT W-NGP FROM DATE.
           MOVE DATE-02R TO H-DATE H-NGP.
           MOVE ZERO TO W-PAGE.
       M-020.
           CALL "SD_Screen_Output" USING "SCHK50" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-020
           END-IF.
           IF  W-ACT = 9
               GO TO M-980
           END-IF.
           IF  W-ACT = 4
               GO TO M-700
           END-IF.
           IF  JS-SIGN = 0
               IF  W-ACT NOT = 1 AND 2 AND 3 AND 5
                   GO TO M-020
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               IF  W-ACT NOT = 5
                   GO TO M-020
               END-IF
           END-IF.
       M-030.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF.
           IF  W-ACP = 9
               GO TO M-020
           END-IF.
           IF  W-ACT = 5
               GO TO M-030
           END-IF.
           IF  W-ACT = 3
               GO TO M-540
           END-IF.
           IF  W-ACT NOT = 1
               GO TO M-410
           END-IF.
      *****     ÇvÇqÇhÇsÇd     *****                                    ********
       M-400.
           PERFORM WRI-RTN THRU WRI-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF.
           GO TO M-030.
      *****     ÇqÇdÇvÇqÇhÇsÇd     *****                                ********
       M-410.
           PERFORM REW-RTN THRU REW-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF.
           GO TO M-030.
      *****     ÇcÇdÇkÇdÇsÇd     *****                                  ********
       M-540.
           PERFORM DEL1-RTN THRU DEL1-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF.
           GO TO M-030.
      *****     çÏÅ@Å@ï\     *****                                      ********
       M-700.
           PERFORM PRI-RTN THRU PRI-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF.
           GO TO M-020.
      *****     ÇdÅ@ÇmÅ@Çc     *****                                    ********
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE THTM_IDLST THTM_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
	       CALL "DB_Close".
           STOP RUN.
      *****     Ç`ÇbÇbÇdÇoÇs   *****                                    ********
       ACP-RTN.
           MOVE 0 TO W-ACP.
           IF  JS-SIGN = 1
               GO TO ACP-010
           END-IF.
           MOVE "9999998" TO TC-KEY.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               GO TO ACP-EX
           END-IF.
       ACP-010.
           CALL "SD_Screen_Output" USING "SCHK50" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" 
                                  RETURNING RESU.
           INITIALIZE W-R W-TR.
           MOVE ALL "Å@" TO W-NAME W-JSU W-JSS W-TNA
                                W-SNA W-SJSU W-SJSS.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSU" A-JSU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSS" A-JSS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TNA" A-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNA" A-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJSU" A-SJSU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJSS" A-SJSS "p" RETURNING RESU.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-ACP
               GO TO ACP-EX
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF.
           IF  W-KEY = 9999
               GO TO ACP-040
           END-IF.
           MOVE W-KEY TO WT-KEY.
           MOVE W-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-080
           END-IF.
           MOVE T-R TO W-R.
           MOVE T-SS TO W-OSS.
           MOVE T-TNC TO W-OTNC.
           MOVE T-BC TO W-OBMC.
           MOVE T-DCC TO W-ODCC.
           MOVE W-NTCD TO W-NTCDB.
           IF  W-ACT = 5
               GO TO ACP-240
           END-IF.
           MOVE W-KEY TO TT-TCD.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO ACP-RTN
           END-IF.
           MOVE TT-R TO W-TR.
           GO TO ACP-240.
       ACP-080.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO ACP-RTN
           END-IF.
           MOVE W-KEY TO TT-TCD.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO W-TGC W-DNG
               GO TO ACP-340
           END-IF.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU.
           GO TO ACP-RTN.
      **********   ALL º≠≥æ≤     **********                             ********
      ********************************                                  ********
       ACP-240.
           PERFORM CRT-RTN THRU CRT-EX.
           IF  W-ACT = 5
               GO TO ACP-900
           END-IF.
           IF  W-ACT = 1
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               INITIALIZE W-TR
               GO TO ACP-040
           END-IF.
           IF  W-END = 9
               GO TO ACP-EX
           END-IF.
           IF  W-ACT NOT = 3
               GO TO ACP-300
           END-IF.
      *****     DELETE CHECK     *****                                  ********
           IF  ZERO NOT = WT-TZZ OR WT-TZZZ OR WT-TUZ OR WT-TUZZ
                      OR WT-TUA OR WT-TUAZ OR WT-TNB OR WT-TNBZ
                      OR WT-TNK OR WT-TNKZ OR WT-TUG
               CALL "SD_Output" USING "E-ME81" E-ME81 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
           IF  T-TGC = 1
               CALL "SD_Output" USING "E-ME82" E-ME82 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
           MOVE ZERO TO TZNT-KEY.
           MOVE W-KEY TO TZNT-TCD.
      *           START TZNT-M KEY NOT < TZNT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TZNT-M_PNAME1 "TZNT-KEY" " NOT < " TZNT-KEY RETURNING RET.
           IF  RET = 1
               GO TO ACP-900
           END-IF.
       ACP-260.
      *           READ TZNT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-900
           END-IF.
           IF  W-KEY NOT = TZNT-TCD
               GO TO ACP-900
           END-IF.
           MOVE ZERO TO CNT.
       ACP-280.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO ACP-260
           END-IF.
           IF  ZERO = TZNT-UD(CNT) AND TZNT-AD(CNT)
               GO TO ACP-280
           END-IF.
           CALL "SD_Output" USING "E-ME83" E-ME83 "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU.
           GO TO ACP-900.
       ACP-300.
           MOVE W-BC TO W-OBC.
           IF  W-BC > 1
               MOVE 1 TO W-OBC
           END-IF.
       ACP-340.
           MOVE W-NAME TO WB-NAME.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "52" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-NAME TO W-NAME
                   CALL "SD_Output" USING "A-NAME" A-NAME "p" 
                                         RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-340
           END-IF.
       ACP-380.
           MOVE W-KANA TO WB-KANA.
           CALL "SD_Output" USING "A-KANA" A-KANA "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KANA "A-KANA" "X" "36" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-KANA TO W-KANA
                   CALL "SD_Output" USING "A-KANA" A-KANA "p" 
                                         RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-340
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-380
           END-IF.
           IF  W-KANA = SPACE
               GO TO ACP-380
           END-IF.
       ACP-385.
           MOVE W-BC TO WB-BC.
           CALL "SD_Accept" USING BY REFERENCE A-BC "A-BC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-BC TO W-BC
                   CALL "SD_Output" USING "A-BC" A-BC "p" 
                                         RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-BC TO W-BC
                   CALL "SD_Output" USING "A-BC" A-BC "p" 
                                         RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-380
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-385
           END-IF.
           PERFORM BCN-RTN THRU BCN-EX.
           IF  W-INV NOT = 0
               GO TO ACP-385
           END-IF.
           IF  W-BC NOT = ZERO
               MOVE ZERO TO W-BIK
               CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU
           END-IF.
           MOVE W-BC TO WT-BC.
           IF  W-ACT = 2
               IF  T-BC NOT = W-BC
                   CALL "SD_Output" USING "E-ME8" E-ME8 "p" 
                                  RETURNING RESU
                   CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               END-IF
           END-IF.
       ACP-390.
           MOVE W-FKC TO WB-FKC.
           CALL "SD_Accept" USING BY REFERENCE A-FKC "A-FKC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-FKC TO W-FKC
                   CALL "SD_Output" USING "A-FKC" A-FKC "p" 
                                         RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-FKC TO W-FKC
                   CALL "SD_Output" USING "A-FKC" A-FKC "p" 
                                         RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-385
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-390
           END-IF.
           PERFORM FKN-RTN THRU FKN-EX.
           IF  W-INV NOT = 0
               GO TO ACP-390
           END-IF.
           MOVE W-FKC TO WT-FKC.
           IF  W-ACT = 1
               IF  W-JSU = SPACE
                   MOVE HKB-FKNA TO W-JSU
               END-IF
           END-IF.
       ACP-391.
           MOVE W-UNO TO WB-UNO.
           CALL "SD_Accept" USING BY REFERENCE A-UNO "A-UNO" "X" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-UNO TO W-UNO
                   CALL "SD_Output" USING "A-UNO" A-UNO "p" 
                                         RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-UNO TO W-UNO
                   CALL "SD_Output" USING "A-UNO" A-UNO "p" 
                                         RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-390
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-391
           END-IF.
           IF  W-UNO = SPACE
               IF  W-FKC NOT = 99
                   GO TO ACP-391
               END-IF
           END-IF.
       ACP-400.
           MOVE W-JSU TO WB-JSU.
           CALL "SD_Output" USING "A-JSU" A-JSU "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JSU "A-JSU" "N" "40" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-JSU TO W-JSU
                   CALL "SD_Output" USING "A-JSU" A-JSU "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-JSU TO W-JSU
                   CALL "SD_Output" USING "A-JSU" A-JSU "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-391
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-400
           END-IF.
       ACP-420.
           MOVE W-JSS TO WB-JSS.
           CALL "SD_Output" USING "A-JSS" A-JSS "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JSS "A-JSS" "N" "40" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-JSS TO W-JSS
                   CALL "SD_Output" USING "A-JSU" A-JSU "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-JSS TO W-JSS
                   CALL "SD_Output" USING "A-JSU" A-JSU "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-400
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-420
           END-IF.
       ACP-460.
           MOVE W-TEL TO WB-TEL.
           CALL "SD_Accept" USING BY REFERENCE A-TEL "A-TEL" "X" "14" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-TEL TO W-TEL
                   CALL "SD_Output" USING "A-TEL" A-TEL "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-TEL TO W-TEL
                   CALL "SD_Output" USING "A-TEL" A-TEL "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-420
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-460
           END-IF.
       ACP-480.
           MOVE W-FAX TO WB-FAX.
           CALL "SD_Accept" USING BY REFERENCE A-FAX "A-FAX" "X" "14" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-FAX TO W-FAX
                   CALL "SD_Output" USING "A-FAX" A-FAX "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-FAX TO W-FAX
                   CALL "SD_Output" USING "A-FAX" A-FAX "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-460
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-480
           END-IF.
       ACP-540.
           MOVE W-TNC TO WB-TNC.
           CALL "SD_Accept" USING BY REFERENCE A-TNC "A-TNC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-TNC TO W-TNC
                   CALL "SD_Output" USING "A-TNC" A-TNC "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-TNC TO W-TNC
                   CALL "SD_Output" USING "A-TNC" A-TNC "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-480
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-540
           END-IF.
           PERFORM TNC-RTN THRU TNC-EX.
           IF  W-INV NOT = 0
               GO TO ACP-540
           END-IF.
           MOVE W-TNC TO WT-TNC.
       ACP-560.
           MOVE W-ZEI TO WB-ZEI.
           CALL "SD_Output" USING "A-ZEI" A-ZEI "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ZEI "A-ZEI" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-ZEI TO W-ZEI
                   CALL "SD_Output" USING "A-ZEI" A-ZEI "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-ZEI TO W-ZEI
                   CALL "SD_Output" USING "A-ZEI" A-ZEI "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-540
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-560
           END-IF.
           IF  W-ZEI > 1
               GO TO ACP-560
           END-IF.
       ACP-562.
           MOVE W-DCC TO WB-DCC.
           CALL "SD_Output" USING "D-DCC" D-DCC "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DCC "A-DCC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-DCC TO W-DCC
                   CALL "SD_Output" USING "A-DCC" A-DCC "p" 
                                  RETURNING RESU
                   GO TO ACP-900
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-DCC TO W-DCC
                   CALL "SD_Output" USING "A-DCC" A-DCC "p" 
                                  RETURNING RESU
                   GO TO ACP-700
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-560
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-562
           END-IF.
           CALL "SD_Output" USING "D-DCC" D-DCC "p" 
                                  RETURNING RESU.
           MOVE W-DCC TO WT-DCC.
           IF  W-BC NOT = ZERO
               MOVE 0 TO W-BIK W-SSC W-KSC
               CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "D-SSC" D-SSC "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "D-KSC" D-KSC "p" 
                                  RETURNING RESU
               GO TO ACP-600
           END-IF.
       ACP-564.
           MOVE W-BIK TO WB-BIK.
           CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-BIK "A-BIK" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-BIK TO W-BIK
                   CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-BIK TO W-BIK
                   CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-562
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-564
           END-IF.
           IF  W-BIK > 1
               GO TO ACP-564
           END-IF.
           CALL "SD_Output" USING "D-BIK" D-BIK "p" 
                                  RETURNING RESU.
       ACP-566.
           MOVE W-SSC TO WB-SSC.
           CALL "SD_Output" USING "D-SSC" D-SSC "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SSC "A-SSC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SSC TO W-SSC
                   CALL "SD_Output" USING "D-SSC" D-SSC "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SSC TO W-SSC
                   CALL "SD_Output" USING "D-SSC" D-SSC "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-564
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-566
           END-IF.
           IF  W-SSC > 1
               GO TO ACP-566
           END-IF.
           CALL "SD_Output" USING "D-SSC" D-SSC "p" 
                                  RETURNING RESU.
       ACP-568.
           MOVE W-KSC TO WB-KSC.
           CALL "SD_Output" USING "D-KSC" D-KSC "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KSC "A-KSC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-KSC TO W-KSC
                   CALL "SD_Output" USING "D-KSC" D-KSC "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-KSC TO W-KSC
                   CALL "SD_Output" USING "D-KSC" D-KSC "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-566
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-568
           END-IF.
           IF  W-KSC > 1
               GO TO ACP-568
           END-IF.
           CALL "SD_Output" USING "D-KSC" D-KSC "p" 
                                  RETURNING RESU.
       ACP-570.
           MOVE W-SNG TO WB-SNG.
           CALL "SD_Accept" USING BY REFERENCE A-SNG "A-SNG" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SNG TO W-SNG
                   CALL "SD_Output" USING "A-SNG" A-SNG "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SNG TO W-SNG
                   CALL "SD_Output" USING "A-SNG" A-SNG "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-BC NOT = ZERO
                   GO TO ACP-562
               ELSE
                   GO TO ACP-568
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-570
           END-IF.
           IF  W-SNG = ZERO
               CALL "SD_Output" USING "S-SNG" S-SNG "p" 
                                  RETURNING RESU
               GO TO ACP-572
           END-IF.
           IF  W-SG < 1 OR > 12
               GO TO ACP-570
           END-IF.
       ACP-572.
           MOVE W-ENG TO WB-ENG.
           CALL "SD_Accept" USING BY REFERENCE A-ENG "A-ENG" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-ENG TO W-ENG
                   CALL "SD_Output" USING "A-ENG" A-ENG "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-ENG TO W-ENG
                   CALL "SD_Output" USING "A-ENG" A-ENG "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-570
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-572
           END-IF.
           IF  W-ENG = ZERO
               CALL "SD_Output" USING "S-ENG" S-ENG "p" 
                                  RETURNING RESU
               GO TO ACP-600
           END-IF.
           IF  W-EG < 1 OR > 12
               GO TO ACP-572
           END-IF.
       ACP-600.
           MOVE W-SS TO WB-SS.
           CALL "SD_Output" USING "D-SS" D-SS "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SS "A-SS" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SS TO W-SS
                   CALL "SD_Output" USING "D-SS" D-SS "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SS TO W-SS
                   CALL "SD_Output" USING "D-SS" D-SS "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-572
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-600
           END-IF.
           IF  W-SS > 31 AND < 99
               GO TO ACP-600
           END-IF.
           CALL "SD_Output" USING "D-SS" D-SS "p" 
                                  RETURNING RESU.
       ACP-605.
           MOVE W-SHD TO WB-SHD.
           CALL "SD_Output" USING "D-SHD" D-SHD "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SHD "A-SHD" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SHD TO W-SHD
                   CALL "SD_Output" USING "D-SHD" D-SHD "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SHD TO W-SHD
                   CALL "SD_Output" USING "D-SHD" D-SHD "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-600
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-605
           END-IF.
           IF  W-SS > 31 AND < 99
               GO TO ACP-605
           END-IF.
           CALL "SD_Output" USING "D-SHD" D-SHD "p" 
                                  RETURNING RESU.
       ACP-610.
           MOVE W-SHC1 TO WB-SHC1.
           CALL "SD_Output" USING "D-SHC1" D-SHC1 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SHC1 "A-SHC1" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SHC1 TO W-SHC1
                   CALL "SD_Output" USING "D-SHC1" D-SHC1 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SHC1 TO W-SHC1
                   CALL "SD_Output" USING "D-SHC1" D-SHC1 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-605
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-610
           END-IF.
           IF  W-SHC1 > 3
               GO TO ACP-610
           END-IF.
           CALL "SD_Output" USING "D-SHC1" D-SHC1 "p" 
                                  RETURNING RESU.
           IF  W-SHC1 = 0
               MOVE 0 TO W-SHC2
               CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" 
                                  RETURNING RESU
               GO TO ACP-617
           END-IF.
       ACP-615.
           MOVE W-SHC2 TO WB-SHC2.
           CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SHC2 "A-SHC2" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SHC2 TO W-SHC2
                   CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SHC2 TO W-SHC2
                   CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-610
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-615
           END-IF.
           IF  W-SHC2 > 3
               GO TO ACP-615
           END-IF.
           CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" 
                                  RETURNING RESU.
       ACP-617.
           MOVE W-NKY TO WB-NKY.
           CALL "SD_Output" USING "D-NKY" D-NKY "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NKY "A-NKY" "9" "3" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-NKY TO W-NKY
                   CALL "SD_Output" USING "D-NKY" D-NKY "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-NKY TO W-NKY
                   CALL "SD_Output" USING "D-NKY" D-NKY "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-SHC1 = 0
                   GO TO ACP-610
               ELSE
                   GO TO ACP-615
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-617
           END-IF.
           CALL "SD_Output" USING "D-NKY" D-NKY "p" 
                                  RETURNING RESU.
       ACP-618.
           MOVE W-SSI TO WB-SSI.
           CALL "SD_Output" USING "D-SSI" D-SSI "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SSI "A-SSI" "9" "3" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SSI TO W-SSI
                   CALL "SD_Output" USING "D-SSI" D-SSI "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SSI TO W-SSI
                   CALL "SD_Output" USING "D-SSI" D-SSI "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-617
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-618
           END-IF.
           CALL "SD_Output" USING "D-SSI" D-SSI "p" 
                                  RETURNING RESU.
       ACP-619.
           MOVE W-SKC TO WB-SKC.
           CALL "SD_Output" USING "D-SKC" D-SKC "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SKC "A-SKC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SKC TO W-SKC
                   CALL "SD_Output" USING "D-SKC" D-SKC "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SKC TO W-SKC
                   CALL "SD_Output" USING "D-SKC" D-SKC "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-618
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-619
           END-IF.
           CALL "SD_Output" USING "D-SKC" D-SKC "p" 
                                  RETURNING RESU.
       ACP-622.
           MOVE W-SKR TO WB-SKR.
           CALL "SD_Output" USING "D-SKR" D-SKR "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SKR "A-SKR" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SKR TO W-SKR
                   CALL "SD_Output" USING "D-SKR" D-SKR "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SKR TO W-SKR
                   CALL "SD_Output" USING "D-SKR" D-SKR "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-619
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-622
           END-IF.
           CALL "SD_Output" USING "D-SKR" D-SKR "p" 
                                  RETURNING RESU.
       ACP-623.
           MOVE W-SGT TO WB-SGT.
           CALL "SD_Output" USING "D-SGT" D-SGT "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SGT "A-SGT" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SGT TO W-SGT
                   CALL "SD_Output" USING "D-SGT" D-SGT "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SGT TO W-SGT
                   CALL "SD_Output" USING "D-SGT" D-SGT "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-622
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-623
           END-IF.
           CALL "SD_Output" USING "D-SGT" D-SGT "p" 
                                  RETURNING RESU.
           IF  W-SGT = 0
               GO TO ACP-625
           END-IF.
           IF  W-SGT > 2
               GO TO ACP-623
           END-IF.
       ACP-624.
           MOVE W-SGR TO WB-SGR.
           CALL "SD_Output" USING "D-SGT" D-SGR "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SGR "A-SGR" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SGR TO W-SGR
                   CALL "SD_Output" USING "D-SGR" D-SGR "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SGR TO W-SGR
                   CALL "SD_Output" USING "D-SGR" D-SGR "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-623
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-624
           END-IF.
           CALL "SD_Output" USING "D-SGR" D-SGR "p" 
                                  RETURNING RESU.
           IF  W-SGR = ZERO
               GO TO ACP-624
           END-IF.
       ACP-625.
           MOVE W-STT TO WB-STT.
           CALL "SD_Output" USING "D-STT" D-STT "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-STT "A-STT" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-STT TO W-STT
                   CALL "SD_Output" USING "D-STT" D-STT "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-STT TO W-STT
                   CALL "SD_Output" USING "D-STT" D-STT "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-SGT = 0
                   GO TO ACP-623
               ELSE
                   GO TO ACP-624
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-625
           END-IF.
           CALL "SD_Output" USING "D-STT" D-STT "p" 
                                  RETURNING RESU.
           IF  W-STT = 0
               GO TO ACP-640
           END-IF.
           IF  W-STT > 2
               GO TO ACP-625
           END-IF.
       ACP-626.
           MOVE W-STR TO WB-STR.
           CALL "SD_Output" USING "D-STR" D-STR "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-STR "A-STR" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-STR TO W-STR
                   CALL "SD_Output" USING "D-STR" D-STR "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-STR TO W-STR
                   CALL "SD_Output" USING "D-STR" D-STR "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-625
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-626
           END-IF.
           CALL "SD_Output" USING "D-STR" D-STR "p" 
                                  RETURNING RESU.
           IF  W-STR = ZERO
               GO TO ACP-626
           END-IF.
       ACP-640.
           MOVE W-YG TO WB-YG.
           CALL "SD_Accept" USING BY REFERENCE A-YG "A-YG" "9" "6" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-YG TO W-YG
                   CALL "SD_Output" USING "A-YG" A-YG "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-YG TO W-YG
                   CALL "SD_Output" USING "A-YG" A-YG "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-STT NOT = 0
                   GO TO ACP-626
               ELSE
                   GO TO ACP-625
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-640
           END-IF.
           CALL "SD_Output" USING "D-YG" D-YG "p" 
                                  RETURNING RESU.
       ACP-645.
           MOVE W-UTC1 TO WB-UTC1.
           CALL "SD_Output" USING "D-UTC1" D-UTC1 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-UTC1 "A-UTC1" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-UTC1 TO W-UTC1
                   CALL "SD_Output" USING "A-UTC1" A-UTC1 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-UTC1 TO W-UTC1
                   CALL "SD_Output" USING "A-UTC1" A-UTC1 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-640
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-645
           END-IF.
           CALL "SD_Output" USING "D-UTC1" D-UTC1 "p" 
                                  RETURNING RESU.
       ACP-650.
           MOVE W-UTK1 TO WB-UTK1.
           CALL "SD_Output" USING "D-UTK1" D-UTK1 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-UTK1 "A-UTK1" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-UTK1 TO W-UTK1
                   CALL "SD_Output" USING "D-UTK1" D-UTK1 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-UTK1 TO W-UTK1
                   CALL "SD_Output" USING "D-UTK1" D-UTK1 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-645
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-650
           END-IF.
           CALL "SD_Output" USING "D-UTK1" D-UTK1 "p" 
                                  RETURNING RESU.
       ACP-660.
           MOVE W-UTC2 TO WB-UTC2.
           CALL "SD_Output" USING "D-UTC2" D-UTC2 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-UTC2 "A-UTC2" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-UTC2 TO W-UTC2
                   CALL "SD_Output" USING "D-UTC2" D-UTC2 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-UTC2 TO W-UTC2
                   CALL "SD_Output" USING "D-UTC2" D-UTC2 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-650
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-660
           END-IF.
           CALL "SD_Output" USING "D-UTC2" D-UTC2 "p" 
                                  RETURNING RESU.
       ACP-670.
           MOVE W-UTK2 TO WB-UTK2.
           CALL "SD_Output" USING "D-UTK2" D-UTK2 "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-UTK2 "A-UTK2" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-UTK2 TO W-UTK2
                   CALL "SD_Output" USING "D-UTK2" D-UTK2 "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-UTK2 TO W-UTK2
                   CALL "SD_Output" USING "D-UTK2" D-UTK2 "p" 
                                  RETURNING RESU
                   GO TO ACP-700
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-660
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-670
           END-IF.
           CALL "SD_Output" USING "D-UTK2" D-UTK2 "p" 
                                  RETURNING RESU.
       ACP-700.
           MOVE W-SNA TO WB-SNA.
           CALL "SD_Output" USING "A-SNA" A-SNA "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SNA "A-SNA" "N" "52" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SNA TO W-SNA
                   CALL "SD_Output" USING "A-SNA" A-SNA "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SNA TO W-SNA
                   CALL "SD_Output" USING "A-SNA" A-SNA "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               IF  W-BC = 0
                   GO TO ACP-670
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-700
           END-IF.
       ACP-720.
           MOVE W-SJSU TO WB-SJSU.
           CALL "SD_Output" USING "A-SJSU" A-SJSU "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SJSU "A-SJSU" "N" "40" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SJSU TO W-SJSU
                   CALL "SD_Output" USING "A-SJSU" A-SJSU "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SJSU TO W-SJSU
                   CALL "SD_Output" USING "A-SJSU" A-SJSU "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-700
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-720
           END-IF.
           IF  W-SJSU = SPACE
               MOVE SPACE TO W-SJSS
               CALL "SD_Output" USING "A-SJSS" A-SJSS "p" 
                                  RETURNING RESU
               GO TO ACP-760
           END-IF.
       ACP-740.
           MOVE W-SJSS TO WB-SJSS.
           CALL "SD_Output" USING "A-SJSS" A-SJSS "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SJSS "A-SJSS" "N" "40" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SJSS TO W-SJSS
                   CALL "SD_Output" USING "A-SJSS" A-SJSS "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SJSS TO W-SJSS
                   CALL "SD_Output" USING "A-SJSS" A-SJSS "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-720
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-740
           END-IF.
       ACP-760.
           MOVE W-SUNO TO WB-SUNO.
           CALL "SD_Accept" USING BY REFERENCE A-SUNO "A-SUNO" "X" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SUNO TO W-SUNO
                   CALL "SD_Output" USING "A-SUNO" A-SUNO "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SUNO TO W-SUNO
                   CALL "SD_Output" USING "A-SUNO" A-SUNO "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-740
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-760
           END-IF.
       ACP-780.
           MOVE W-STEL TO WB-STEL.
           CALL "SD_Accept" USING BY REFERENCE A-STEL "A-STEL" "X" "14" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-STEL TO W-STEL
                   CALL "SD_Output" USING "A-STEL" A-STEL "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-STEL TO W-STEL
                   CALL "SD_Output" USING "A-STEL" A-STEL "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-760
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-780
           END-IF.
       ACP-800.
           MOVE W-SFAX TO WB-SFAX.
           CALL "SD_Accept" USING BY REFERENCE A-SFAX "A-SFAX" "X" "14" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-SFAX TO W-SFAX
                   CALL "SD_Output" USING "A-SFAX" A-SFAX "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-SFAX TO W-SFAX
                   CALL "SD_Output" USING "A-SFAX" A-SFAX "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-780
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-800
           END-IF.
       ACP-820.
           MOVE W-TNA TO WB-TNA.
           CALL "SD_Output" USING "A-TNA" A-TNA "p" 
                                  RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TNA "A-TNA" "N" "32" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1
                   MOVE WB-TNA TO W-TNA
                   CALL "SD_Output" USING "A-TNA" A-TNA "p" 
                                  RETURNING RESU
                   GO TO ACP-900
               END-IF
           END-IF.
           IF  ESTAT = FUK
               IF  W-ACT NOT = 1
                   MOVE WB-TNA TO W-TNA
                   CALL "SD_Output" USING "A-TNA" A-TNA "p" 
                                  RETURNING RESU
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-800
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-820
           END-IF.
       ACP-840.
           MOVE W-NTCD TO WB-NTCD.
           CALL "SD_Accept" USING BY REFERENCE A-NTCD "A-NTCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-820
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-840
           END-IF.
           IF  W-NTCD = ZERO
               MOVE W-KEY TO W-NTCD
               CALL "SD_Output" USING "D-NTCD" D-NTCD "p" 
                                  RETURNING RESU
               GO TO ACP-900
           END-IF.
           MOVE W-NTCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               GO TO ACP-840
           END-IF.
           CALL "SD_Output" USING "D-NNA" D-NNA "p" 
                                  RETURNING RESU.
           IF  T-NTCD NOT = T-TCD
               CALL "SD_Output" USING "E-ME9" E-ME9 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               GO TO ACP-840
           END-IF.
       ACP-900.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3 OR 5
                   GO TO ACP-040
               ELSE
                   GO TO ACP-840
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-900
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "E-ME7" E-ME7 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO ACP-RTN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO ACP-900
           END-IF.
       ACP-EX.
           EXIT.
      *****     âÊñ Å@ï\é¶     *****                                    ********
       CRT-RTN.
           CALL "SD_Output" USING "A-KEY" A-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DCC" A-DCC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSU" A-JSU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSS" A-JSS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UNO" A-UNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BC" A-BC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TEL" A-TEL "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FAX" A-FAX "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ZEI" A-ZEI "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNC" D-TNC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-FKC" D-FKC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHD" D-SHD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHC1" D-SHC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHC2" D-SHC2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NKY" D-NKY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SSI" D-SSI "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKR" D-SKR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SGT" D-SGT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SGR" D-SGR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-STT" D-STT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-STR" D-STR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YG" D-YG "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BIK" D-BIK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SSC" D-SSC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KSC" D-KSC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KANA" A-KANA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKC" D-SKC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTC1" D-UTC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTK1" D-UTK1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTC2" D-UTC2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTK2" D-UTK2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNA" A-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJSU" A-SJSU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJSS" A-SJSS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SUNO" A-SUNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-STEL" A-STEL "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SFAX" A-SFAX "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TNA" A-TNA "p" RETURNING RESU.
           PERFORM BCN-RTN THRU BCN-EX.
           PERFORM TNC-RTN THRU TNC-EX.
           PERFORM FKN-RTN THRU FKN-EX.
           IF  W-SNG = ZERO
               CALL "SD_Output" USING "S-SNG" S-SNG "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-SNG" A-SNG "p" RETURNING RESU
           END-IF.
           IF  W-ENG = ZERO
               CALL "SD_Output" USING "S-ENG" S-ENG "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-ENG" A-ENG "p" RETURNING RESU
           END-IF.
           IF  W-TCD = W-NTCD
               CALL "SD_Output" USING "D-NTCD" D-NTCD "p" 
                                              RETURNING RESU
               GO TO CRT-020
           END-IF.
           MOVE W-NTCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF.
           CALL "SD_Output" USING "A-NTCD" A-NTCD "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "D-NNA" D-NNA "p" 
                                  RETURNING RESU.
           MOVE W-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME10" E-ME10 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO CRT-EX
           END-IF.
       CRT-020.
           IF  W-ACT = 2
               MOVE ZERO TO W-BD
               MOVE W-TNC TO W-BTNC
               MOVE W-BC TO W-BBC
           END-IF.
       CRT-EX.
           EXIT.
      *****     ïîñÂÅ@ãÊï™     *****                                    ********
       BCN-RTN.
           MOVE 0 TO W-INV.
           MOVE SPACE TO HKB-KEY.
           MOVE "02" TO HKB-NO.
           MOVE W-BC TO HKB-BM.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-INV
               MOVE "Å@Å@Å@Å@Å@Å@" TO HKB-BMNA
           END-IF.
           CALL "SD_Output" USING "D-BCN" D-BCN "p" 
                                  RETURNING RESU.
       BCN-EX.
           EXIT.
      *****     ínãÊÅ@ãÊï™     *****                                    ********
      *****     íSìñÅ@ãÊï™     *****                                    ********
       TNC-RTN.
           MOVE 0 TO W-INV.
           MOVE SPACE TO HKB-KEY.
           MOVE "04" TO HKB-NO.
           MOVE W-TNC TO HKB-TNC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-INV
               MOVE SPACE TO HKB-TNNA
           END-IF.
           CALL "SD_Output" USING "D-TNN" D-TNN "p" 
                                  RETURNING RESU.
       TNC-EX.
           EXIT.
      *****   ìsìπï{åß  ãÊï™   *****                                    ********
       FKN-RTN.
           MOVE 0 TO W-INV.
           MOVE SPACE TO HKB-KEY.
           MOVE "01" TO HKB-NO.
           MOVE W-FKC TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-INV
               MOVE SPACE TO HKB-FKNA
           END-IF.
           CALL "SD_Output" USING "D-FKN" D-FKN "p" 
                                  RETURNING RESU.
       FKN-EX.
           EXIT.
      *****     ÇsÇbÇlÅ@ÇlÇnÇuÇd     *****                              ********
       TCM-RTN.
           MOVE W-NAME TO TC-NAME.
           MOVE W-JSU TO TC-JSU.
           MOVE W-JSS TO TC-JSS.
           MOVE W-UNO TO TC-UNO.
           MOVE W-TEL TO TC-TEL.
           MOVE W-FKC TO TC-FKC.
           MOVE W-BIK TO TC-BIK.
       TCM-EX.
           EXIT.
      *****     ÇsÇbÇlÅ@àÍäáÇqÇdÇvÇqÇhÇsÇd     *****                    ********
       TCA-RTN.
           MOVE SPACE TO TC-KEY.
           MOVE W-KEY TO TC-TCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               GO TO TCA-EX
           END-IF.
       TCA-010.
      *           READ TC-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TCA-EX
           END-IF.
           IF  TC-TCD NOT = W-KEY
               GO TO TCA-EX
           END-IF.
           IF  TC-BIK = W-BIK
               GO TO TCA-010
           END-IF.
           MOVE W-BIK TO TC-BIK.
      *           REWRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME27" E-ME27 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               MOVE 9 TO W-END
               GO TO TCA-EX
           END-IF.
           GO TO TCA-010.
       TCA-EX.
           EXIT.
      *****     ÇvÇqÇhÇsÇd     *****                                    ********
       WRI-RTN.
           MOVE W-R TO T-R.
      *           WRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1 
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME32" E-ME32 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-010
           END-IF.
           GO TO WRI-020.
       WRI-010.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           MOVE "TM           " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           GO TO WRI-RTN.
       WRI-020.
           MOVE W-TR TO TT-R.
      *           WRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1 
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME11" E-ME11 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-030
           END-IF.
           IF  W-BC NOT = 0
               GO TO WRI-EX
           END-IF.
           GO TO WRI-040.
       WRI-030.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           MOVE "TTM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           GO TO WRI-020.
       WRI-040.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO WRI-050
           END-IF.
           GO TO WRI-EX.
       WRI-050.
           INITIALIZE TC-R.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
           PERFORM TCM-RTN THRU TCM-EX.
      *           WRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME25" E-ME25 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-060
           END-IF.
           GO TO WRI-070.
       WRI-060.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO WRI-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL"p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           MOVE "TCM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL"p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           GO TO WRI-050.
       WRI-070.
           MOVE TC-KEY TO W-TCK.
           MOVE 1 TO W-ACTD.
       WRI-EX.
           EXIT.
      *****     ÇqÇdÇvÇqÇhÇsÇd     *****                                ********
       REW-RTN.
           MOVE W-KEY TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           IF  W-NTCD = W-NTCDB
               GO TO REW-010
           END-IF.
      *           DELETE T-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING T-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME34" E-ME34 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
       REW-002.
           MOVE W-R TO T-R.
      *           WRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1 
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME32" E-ME32 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-004
           END-IF.
           GO TO REW-015.
       REW-004.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           MOVE "TM           " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "C-CL" C-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           GO TO REW-002.
       REW-010.
           MOVE W-R TO T-R.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1 
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME33" E-ME33 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
       REW-015.
           MOVE W-TR TO TT-R.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1 
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME13" E-ME13 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           IF  W-OSS = 00 OR 99
               IF  T-SS NOT = W-OSS
                   IF (WT-TUZ NOT = ZERO) OR (WT-TUZZ NOT = ZERO) OR
                      (WT-TUA NOT = ZERO) OR (WT-TUAZ NOT = ZERO) OR
                      (WT-TNB NOT = ZERO) OR (WT-TNBZ NOT = ZERO) OR
                      (WT-TNK NOT = ZERO) OR (WT-TNKZ NOT = ZERO)
                       CALL "SD_Output" USING "E-ME59" E-ME59 "p" 
                                          RETURNING RESU
                       CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
                       CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU
           IF (W-TNC = W-OTNC) AND (W-BC = W-OBMC) AND (W-DCC = W-ODCC)
               GO TO REW-018.
           MOVE W-KEY TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-018
           END-IF.
           MOVE W-TNC TO TSK-TNC.
           MOVE W-BC TO TSK-BMC.
           MOVE W-DCC TO TSK-DCC.
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1 
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME55" E-ME55 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
       REW-018.
           MOVE W-BC TO W-NBC.
           IF  W-BC > 1
               MOVE 1 TO W-NBC
           END-IF.
           IF  W-NBC NOT = W-OBC
               GO TO REW-140
           END-IF.
           IF  W-BC NOT = ZERO
               GO TO REW-360
           END-IF.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-020
           END-IF.
           GO TO REW-080.
       REW-020.
           INITIALIZE TC-R.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
           PERFORM TCM-RTN THRU TCM-EX.
      *           WRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME25" E-ME25 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-040
           END-IF.
           GO TO REW-060.
       REW-040.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           MOVE "TCM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           GO TO REW-020.
       REW-060.
           MOVE TC-KEY TO W-TCK.
           MOVE 1 TO W-ACTD.
           GO TO REW-360.
       REW-080.
           PERFORM TCM-RTN THRU TCM-EX.
      *           REWRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME27" E-ME27 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           MOVE TC-KEY TO W-TCK.
           MOVE 2 TO W-ACTD.
           PERFORM TCA-RTN THRU TCA-EX.
           IF  W-END = 9
               GO TO REW-EX
           END-IF.
           GO TO REW-360.
       REW-140.
           IF  W-BC NOT = ZERO
               GO TO REW-280
           END-IF.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-220
           END-IF.
           PERFORM TCA-RTN THRU TCA-EX.
           IF  W-END = 9
               GO TO REW-EX
           END-IF.
           GO TO REW-360.
       REW-220.
           INITIALIZE TC-R.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
           PERFORM TCM-RTN THRU TCM-EX.
      *           WRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME26" E-ME26 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-240
           END-IF.
           GO TO REW-260.
       REW-240.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           MOVE "TCM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           GO TO REW-220.
       REW-260.
           MOVE TC-KEY TO W-TCK.
           MOVE 1 TO W-ACTD.
           GO TO REW-360.
       REW-280.
           PERFORM DEL2-RTN THRU DEL2-EX.
           IF  W-END = 9
               GO TO REW-EX
           END-IF.
       REW-360.
           IF  W-TNC NOT = W-BTNC
               IF  COMPLETION_CODE = 000
                   CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  100
               ELSE
                   IF  COMPLETION_CODE = 150
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  200
                   END-IF
               END-IF
           END-IF.
           MOVE ZERO TO W-CD.
           IF (W-BTNC NOT = W-TNC) AND (DATE-HTNC = 0)
               MOVE 1 TO W-C1
           END-IF.
           IF (W-BBC  NOT = W-BC ) AND (DATE-HBC  = 0)
               MOVE 1 TO W-C3
           END-IF.
           IF  W-CD = ZERO
               GO TO REW-EX
           END-IF.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING "E-ME51" E-ME51 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU
               GO TO REW-EX
           END-IF.
           IF  W-C1 = 1
               MOVE W-C1 TO DATE-HTNC
           END-IF.
           IF  W-C2 = 3
               MOVE W-C3 TO DATE-HBC
           END-IF.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME52" E-ME52 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       REW-EX.
           EXIT.
      *****   ÇcÇdÇkÇdÇsÇd  á@   *****                                  ********
       DEL1-RTN.
           MOVE W-KEY TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL1-EX
           END-IF.
       DEL1-020.
      *           DELETE T-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING T-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME34" E-ME34 "p" 
                                      RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL1-EX
           END-IF.
           MOVE SPACE TO T-KEY2.
           MOVE W-KEY TO T-NTCD.
      *           START T-M KEY NOT < T-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY2" " NOT < " T-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO DEL1-040
           END-IF.
       DEL1-025.
      *           READ T-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL1-040
           END-IF.
           IF  W-KEY NOT = T-NTCD
               GO TO DEL1-040
           END-IF.
           MOVE T-TCD TO T-NTCD.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1 
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME33" E-ME33 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL1-EX
           END-IF.
           GO TO DEL1-025.
       DEL1-040.
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 150
           ELSE
               IF  COMPLETION_CODE = 100
                   CALL "C3_Set_Jrcode"  USING 
                    USER_ID BY REFERENCE COMPLETION_CODE 200
               END-IF
           END-IF.
      *           DELETE TT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME14" E-ME14 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL1-EX
           END-IF.
           MOVE W-KEY TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL1-060
           END-IF.
      *           DELETE TSKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TSKF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME56" E-ME56 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL1-EX
           END-IF.
       DEL1-060.
           IF  W-BC = 0
               PERFORM DEL2-RTN THRU DEL2-EX
           END-IF.
       DEL1-EX.
           EXIT.
      *****   ÇcÇdÇkÇdÇsÇd  áA   *****                                  ********
       DEL2-RTN.
           MOVE ZERO TO TZNT-KEY.
           MOVE W-KEY TO TZNT-TCD.
      *           START TZNT-M KEY NOT < TZNT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TZNT-M_PNAME1 "TZNT-KEY" " NOT < " TZNT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL2-110
           END-IF.
       DEL2-100.
      *           READ TZNT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL2-110
           END-IF.
           IF  W-KEY NOT = TZNT-TCD
               GO TO DEL2-110
           END-IF.
      *           DELETE TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TZNT-M_PNAME1 TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME37" E-ME37 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
           GO TO DEL2-100.
       DEL2-110.
           MOVE ZERO TO THT-KEY.
           MOVE W-KEY TO THT-TCD.
      *           START THTM KEY NOT < THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL2-200
           END-IF.
       DEL2-130.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL2-200
           END-IF.
           IF  W-KEY NOT = THT-TCD
               GO TO DEL2-200
           END-IF.
      *           DELETE THTM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME45" E-ME45 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
           GO TO DEL2-130.
       DEL2-200.
           MOVE W-KEY TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL2-EX
           END-IF.
       DEL2-210.
           MOVE TC-KEY TO W-TCK.
      *           DELETE TC-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TC-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME29" E-ME29 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO DEL2-EX
           END-IF.
           MOVE 3 TO W-ACTD.
      *           READ TC-M NEXT RECORD  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL2-EX
           END-IF.
           IF  W-KEY = TC-TCD
               GO TO DEL2-210
           END-IF.
       DEL2-EX.
           EXIT.
      *****     çÏÅ@Å@ï\     *****                                      ********
       PRI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-PM" D-PM "p" 
                                         RETURNING RESU.
           MOVE 0000 TO W-STCD.
           MOVE 9999 TO W-ETCD.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" 
                                         RETURNING RESU.
       PRI-010.
           CALL "SD_Accept" USING BY REFERENCE A-MC "A-MC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-EX
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-010
           END-IF.
           IF  W-MC NOT = 1 AND 5
               GO TO PRI-010
           END-IF.
       PRI-020.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-010
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-020
           END-IF.
           IF  CHK > 2
               GO TO PRI-020
           END-IF.
       PRI-025.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-020
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-025
           END-IF.
           IF  W-SEN > 1
               GO TO PRI-025
           END-IF.
       PRI-030.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-025
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-030
           END-IF.
       PRI-040.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-030
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-040
           END-IF.
           IF  W-STCD > W-ETCD
               GO TO PRI-030
           END-IF.
       PRI-050.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-050
           END-IF.
           IF  W-DMM = 9
               GO TO PRI-RTN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO PRI-050
           END-IF.
           MOVE ZERO TO W-PAGE.
           MOVE W-STCD TO T-KEY.
      *           START T-M KEY NOT < T-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY" " NOT < " T-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO PRI-RTN
           END-IF.
       PRI-060.
      *           READ T-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO PRI-EX
           END-IF.
           IF  T-KEY = "9999"
               GO TO PRI-060
           END-IF.
           IF  T-KEY > W-ETCD
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO PRI-EX
           END-IF.
           IF  CHK = 1
               IF  T-BC NOT = ZERO
                   GO TO PRI-060
               END-IF
           END-IF.
           IF  CHK = 2
               IF  T-BC = ZERO
                   GO TO PRI-060
               END-IF
           END-IF.
           IF  W-SEN = 0
               IF  T-ENG NOT = ZERO
                   GO TO PRI-060
               END-IF
           END-IF.
           CALL "PR_Open" RETURNING RESP.
           IF  W-MC = 5
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "24" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE HEAD01 TO SP-R
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD02 TO SP-R
               CALL "PR_LineFeed" USING "10" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           PERFORM MID-010 THRU MID-EX.
       PRI-070.
           PERFORM LST-RTN THRU LST-EX.
           IF  W-END = 9
               GO TO PRI-EX
           END-IF.
       PRI-080.
      *           READ T-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO PRI-090
           END-IF.
           IF  T-KEY = "9999"
               GO TO PRI-080
           END-IF.
           IF  T-KEY > W-ETCD
               GO TO PRI-090
           END-IF.
           IF  CHK = 1
               IF  T-BC NOT = ZERO
                   GO TO PRI-080
               END-IF
           END-IF.
           IF  CHK = 2
               IF  T-BC = ZERO
                   GO TO PRI-080
               END-IF
           END-IF.
           IF  W-SEN = 0
               IF  T-ENG NOT = ZERO
                   GO TO PRI-080
               END-IF
           END-IF.
           GO TO PRI-070.
       PRI-090.
           CALL "PR_Close" RETURNING RESP.
       PRI-EX.
           EXIT.
      *****     å©èoÇµÅ@çÏï\     *****                                  ********
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
      *****     ÇcÇ`ÇsÇ`Å@çÏï\     *****                                ********
       LST-RTN.
           MOVE ZERO TO W-RTCD.
           MOVE SPACE TO W-P1 W-P2.
           MOVE ALL "Å@" TO P-NAME P-JSU P-JSS.
           MOVE T-KEY TO P-KEY.
           MOVE T-NAME TO P-NAME.
           MOVE T-JSU TO P-JSU.
           MOVE T-JSS TO P-JSS.
           MOVE T-UNO TO P-UNO.
           MOVE T-TEL TO P-TEL.
           MOVE T-FAX TO P-FAX.
           MOVE T-KANA TO P-KANA.
           MOVE T-TNC TO P-TNC.
           MOVE T-FKC TO P-FKC.
           MOVE T-SS TO P-SS.
           MOVE T-SHD TO P-SHD.
           MOVE T-SHC1 TO P-SHC1.
           MOVE T-SHC2 TO P-SHC2.
           MOVE T-NKY TO P-NKY.
           MOVE T-SSI TO P-SSI.
           MOVE T-SKC TO P-SKC.
           MOVE T-SKR TO P-SKR.
           MOVE T-SGT TO P-SGT.
           MOVE T-SGR TO P-SGR.
           MOVE T-STT TO P-STT.
           MOVE T-STR TO P-STR.
           MOVE T-BC TO P-BC.
           MOVE T-ZEI TO P-ZEI.
           MOVE T-UTC1 TO P-UTC1.
           MOVE T-UTK1 TO P-UTK1.
           MOVE T-UTC2 TO P-UTC2.
           MOVE T-UTK2 TO P-UTK2.
           IF  T-BC = ZERO
               MOVE T-BIK TO P-BIK
               MOVE T-SSC TO P-SSC
               MOVE T-KSC TO P-KSC
           END-IF.
           MOVE T-DCC TO P-DCC.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 0 TO W-NGC.
           IF  ZERO = T-SNG AND T-ENG
               MOVE 1 TO W-NGC
           END-IF.
           IF  SPACE = T-SNA AND T-SJSU AND T-SUNO AND
                      T-STEL AND T-SFAX
               GO TO LST-020
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE ALL "Å@" TO P-NAME P-JSU.
           MOVE T-SNA TO P-NAME.
           MOVE T-SJSU TO P-JSU.
           MOVE T-SUNO TO P-UNO.
           MOVE T-STEL TO P-TEL.
           MOVE T-SFAX TO P-FAX.
           IF  T-SNG NOT = ZERO
               MOVE T-SNG TO P-SNGD
           END-IF.
           IF  T-ENG NOT = ZERO
               MOVE T-ENG TO P-ENGD
           END-IF.
           MOVE 1 TO W-NGC.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       LST-020.
           IF (T-TNA = SPACE) AND (T-TCD = T-NTCD)
                              AND (T-SJSS = SPACE) AND (W-NGC = 1)
               GO TO LST-EX
           END-IF.
           MOVE SPACE TO W-P3.
           MOVE ALL "Å@" TO P-TNA P-NNA P-SJSS.
           MOVE T-TNA TO P-TNA.
           MOVE T-SJSS TO P-SJSS.
           IF  W-NGC = 0
               IF  T-SNG NOT = ZERO
                   MOVE T-SNG TO P-SNG
               END-IF
           END-IF.
           IF  W-NGC = 0
               IF  T-ENG NOT = ZERO
                   MOVE T-ENG TO P-ENG
               END-IF
           END-IF.
           IF  T-TCD = T-NTCD
               GO TO LST-040
           END-IF.
           MOVE ")" TO P-R.
           MOVE T-NTCD TO P-NTCD.
           MOVE T-TCD TO W-RTCD.
           MOVE T-NTCD TO T-TCD.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF.
           MOVE T-NAME TO P-NNA.
       LST-040.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-RTCD = ZERO
               GO TO LST-EX
           END-IF.
           MOVE W-RTCD TO T-TCD.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
       LST-EX.
           EXIT.
