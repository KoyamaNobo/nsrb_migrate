000010 IDENTIFICATION  DIVISION.
000020 PROGRAM-ID.     TEST.
000030 ENVIRONMENT          DIVISION.
000040 CONFIGURATION        SECTION.
000050 SOURCE-COMPUTER.     SYSTEM100.
000060 OBJECT-COMPUTER.     SYSTEM100.
000070 INPUT-OUTPUT SECTION.
000080 FILE-CONTROL.
000090     SELECT  IN-SW ASSIGN  TO  SIWAKED-MSD
000100                   ORGANIZATION SEQUENTIAL
000110                   ACCESS  MODE SEQUENTIAL.
000120     SELECT  OUT-SW ASSIGN  TO  SIWAKEH-MSD
000130                   ORGANIZATION SEQUENTIAL
000140                   ACCESS  MODE SEQUENTIAL.
000150     SELECT  PR-F  ASSIGN TO PR-PRN999.
000160 DATA            DIVISION.
000170 FILE            SECTION.
000180 FD  IN-SW
000190     BLOCK   3   RECORDS
000200     LABEL   RECORD  STANDARD
000210     VALUE   OF  IDENTIFICATION  IS  "SIWAKE-H".
000220 01  IN-REC.
000230     02  IN-KEY.
000240       03  IN-YMD   PIC 9(06).
000250       03  IN-DENNO PIC 9(06).
000260     02  FILLER  PIC X(03).
000270     02  IN-KEY2 PIC 9(04).
000280     02  FILLER  PIC X(66).
000290 FD  OUT-SW
000300     BLOCK   3   RECORDS
000310     LABEL   RECORD  STANDARD
000320     VALUE   OF  IDENTIFICATION  IS  "SW".
000330 01  OUT-REC.
000340     02  FILLER          PIC X(85).
000350 FD  PR-F LABEL RECORD OMITTED.
000360 01  PR-REC.
           02  PR-SIN     PIC X(02).
000370     02  PR-DENYMD  PIC 9(06).
000380     02  FILLER  PIC X(01).
000390     02  PR-DENNO   PIC 9(06).
000380     02  FILLER  PIC X(01).
           02  PR-DR   PIC ---,---,---,--9.
           02  PR-CR  PIC ---,---,---,--9.
000400 WORKING-STORAGE SECTION.
000410 01  W-FLG.
000420     02  SYORI-FLG  PIC 9(01).
000430     02  ERR-FLG    PIC X(03).
000440 01  W-KEY.
000450     02  W-YMD      PIC 9(06).
000460     02  W-DENNO    PIC 9(06).
000470 01  W-WORK.
000480     02  W-DATACNT  PIC 9(02).
000490     02  IX1      PIC 9(02).
000500     02  W-DRAMOUNT PIC S9(11).
000510     02  W-CRAMOUNT PIC S9(11).
       01  W-MD.
           02  W-DAYDR  PIC S9(11).
           02  W-DAYCR  PIC S9(11).
           02  W-MOTDR  PIC S9(11).
           02  W-MOTCR  PIC S9(11).
000520 01  W-ITEM.
000530     02  W-SW  OCCURS  40.
000540        03  FILLER  PIC X(14).
000550        03  W-DRCR  PIC 9(01).
000560        03  W-KEY2  PIC 9(04).
000570        03  FILLER  PIC X(13).
000580        03  W-AMOUNT PIC S9(10).
000590        03  FILLER   PIC X(43).
000600 PROCEDURE       DIVISION.
000610 ENTRY-RTN       SECTION.
000620   ENTRY-000.
000630     PERFORM INIT-RTN.
000640     PERFORM MAIN-RTN  UNTIL SYORI-FLG = 1.
000650     PERFORM FINAL-RTN.
000660   ENTRY-999.
000670     EXIT.
000680 INIT-RTN SECTION.
000690   INIT-000.
000700     OPEN INPUT IN-SW OUTPUT OUT-SW PR-F      .
000710     INITIALIZE W-ITEM.
000720     INITIALIZE W-WORK.
000730     INITIALIZE W-KEY.
000740   INIT-999.
000750     EXIT.
000760 MAIN-RTN SECTION.
000770   MAIN-000.
000780     PERFORM IN-RTN.
000790     MOVE  IN-KEY TO W-KEY.
000800     PERFORM SYORI-RTN UNTIL SYORI-FLG  = 1.
000810     PERFORM D-CHECK-RTN.
               MOVE 0      TO PR-DENYMD
               MOVE 0 TO PR-DENNO
               MOVE "**" TO PR-SIN
               MOVE W-MOTDR TO PR-DR
               MOVE W-MOTCR TO PR-CR
               MOVE 0 TO W-DAYDR    W-DAYCR
               WRITE PR-REC AFTER 2.
000820   MAIN-999.
000830     EXIT.
000840 IN-RTN SECTION.
000850   IN-000.
000860     READ   IN-SW  AT  END
000870         MOVE  1  TO  SYORI-FLG GO  TO IN-999.
000880     IF  IN-YMD  <  "580300" GO  TO IN-000.
000880     IF  IN-YMD  >  "580399" GO  TO IN-000.
000890   IN-999.
000900     EXIT.
000910 SYORI-RTN SECTION.
000920   SYORI-000.
000930     IF  IN-KEY NOT = W-KEY
000940         PERFORM D-CHECK-RTN.
000950     PERFORM SET-RTN.
000960     PERFORM IN-RTN.
000970   SYORI-999.
000980     EXIT.
000990 D-CHECK-RTN SECTION.
001000   D-CHECK-000.
001010     MOVE  0 TO IX1.
001020   D-CHECK-001.
001030         ADD    1      TO  IX1.
001040     IF    IX1  >  W-DATACNT
001050         PERFORM  OUT-RTN
001060       ELSE
001070         PERFORM  HANTEI-RTN
001080         GO  TO  D-CHECK-001.
001090     INITIALIZE    W-ITEM .
001100     INITIALIZE W-WORK.
001110     MOVE  SPACE TO     ERR-FLG.
001120     MOVE  IN-KEY  TO  W-KEY.
001130   D-CHECK-999.
001140     EXIT.
001150 HANTEI-RTN SECTION.
001160   HANTEI-000.
001170     IF  W-DRCR (IX1)   = 1
001180         ADD   W-AMOUNT (IX1) TO  W-DRAMOUNT
001180         ADD   W-AMOUNT (IX1) TO  W-DAYDR
001180         ADD   W-AMOUNT (IX1) TO  W-MOTDR.
001190     IF  W-DRCR (IX1)   = 2
001180         ADD   W-AMOUNT (IX1) TO  W-CRAMOUNT
001180         ADD   W-AMOUNT (IX1) TO  W-DAYCR
001180         ADD   W-AMOUNT (IX1) TO  W-MOTCR.
001210   HANTEI-999.
001220     EXIT.
001230 OUT-RTN SECTION.
001240   OUT-000.
001250     IF  W-DRAMOUNT NOT = W-CRAMOUNT
001260        MOVE  "ERR"  TO  ERR-FLG
001270       ELSE
001280        MOVE  SPACE TO   ERR-FLG.
001290     IF  ERR-FLG   = "ERR"
               MOVE  SPACE TO PR-SIN
               MOVE  W-DRAMOUNT TO PR-DR
               MOVE  W-CRAMOUNT TO PR-CR
001300         MOVE  W-YMD TO PR-DENYMD
001310         MOVE  W-DENNO TO PR-DENNO
001320         WRITE PR-REC AFTER  2
001330         GO  TO  OUT-002.
001340     MOVE  0  TO  IX1.
001350   OUT-001.
001360     ADD  1  TO  IX1.
001370     IF  IX1 NOT >  W-DATACNT
001380         MOVE  W-SW (IX1)  TO  OUT-REC
001390         WRITE OUT-REC
001400         GO  TO  OUT-001.
         OUT-002.
           IF  W-YMD NOT = IN-YMD
               MOVE W-YMD  TO PR-DENYMD
               MOVE 0 TO PR-DENNO
               MOVE " *" TO PR-SIN
               MOVE W-DAYDR TO PR-DR
               MOVE W-DAYCR TO PR-CR
               MOVE 0 TO W-DAYDR    W-DAYCR
               WRITE PR-REC AFTER 2.
001410   OUT-999.
001420     EXIT.
001430 SET-RTN SECTION.
001440   SET-000.
001450     ADD  1  TO  W-DATACNT.
001460     MOVE  IN-REC TO W-SW (W-DATACNT).
001470   SET-999.
001480     EXIT.
001490 FINAL-RTN SECTION.
001500   FINAL-000.
001510     CLOSE IN-SW OUT-SW.
001520     STOP RUN.
001530   FINAL-999.
001540     EXIT.
