000010*********************************************
000020*****                                   *****
000030*****       ¿ÝÏt@C@@        *****
000040*****                       85/03       *****
000050*********************************************
000060 FD  HN-SRUI
000070     BLOCK    3     RECORDS
000080     LABEL    RECORD   STANDARD
000090     VALUE    OF  IDENTIFICATION  "HN-SRUI".
000100*
000110 01  SRUI1-R.
000120     02  SRUI1-KEY.
000130       03  SRUI1-01          PIC 9(04).                           ¾ÓæCD
000140       03  SRUI1-02          PIC 9(06).                           út
000150       03  SRUI1-02R  REDEFINES  SRUI1-02.
000160         04  SRUI1-021       PIC 9(02).                           N
000170         04  SRUI1-022       PIC 9(02).                           
000180         04  SRUI1-023       PIC 9(02).                           ú
000190       03  SRUI1-03          PIC 9(06).                           `[
000200       03  SRUI1-04          PIC 9(03).                           SEQ-NO
000210       03  SRUI1-05          PIC 9(01).                           s
000220     02  SRUI1-06            PIC N(24).                           õl
000230     02  SRUI1-07            PIC S9(07).                          ÁïÅ
000240     02  FILLER              PIC X(09).
000250     02  SRUI1-99            PIC 9(01).                           oÍæª
000260**
000270 01  SRUI2-R.
000280     02  SRUI2-KEY.
000290       03  SRUI2-01          PIC 9(04).                           ¾ÓæCD
000300       03  SRUI2-02          PIC 9(06).                           út
000310       03  SRUI2-02R  REDEFINES  SRUI2-02.
000320         04  SRUI2-021       PIC 9(02).                           N
000330         04  SRUI2-022       PIC 9(02).                           
000340         04  SRUI2-023       PIC 9(02).                           ú
000350       03  SRUI2-03          PIC 9(06).                           `[
000360       03  SRUI2-04          PIC 9(03).                           SEQ-NO
000370       03  SRUI2-05          PIC 9(01).                           s
000380     02  SRUI2-06            PIC 9(06).                           i¼CD
000390     02  SRUI2-07            PIC 9(01).                           TCYæ
000400     02  SRUI2-08            PIC S9(06).                          Ê
000410     02  SRUI2-09            PIC 9(04).                           ¿
000420     02  SRUI2-10            PIC S9(08).                          àz
000430     02  SRUI2-11            PIC 9(02).                           `[æª
000440     02  SRUI2-12            PIC 9(03).                           ¼CD
000450     02  FILLER              PIC X(34).
000460     02  SRUI2-99            PIC 9(01).                           oÍæª
000470**
