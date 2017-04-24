000010 IDENTIFICATION                     DIVISION.
000020 PROGRAM-ID.                        TEGYMD.
000030*>=========================================================<*
000040*>                                                         <*
000050*>       USER     NAME.....                                <*
000060*>       PROGRAM  NAME.....                                <*
000070*>       PROGRAM  TITLE....                                <*
000080*>       AUTHOR   .........                                <*
000090*>       DATE     WRITTEN.  83/09/28                       <*
000100*>                                                         <*
000110*>=========================================================<*
000120*
000130 ENVIRONMENT                        DIVISION.
000140 CONFIGURATION                      SECTION.
000150 SOURCE-COMPUTER.                   SYSTEM100.
000160 OBJECT-COMPUTER.                   SYSTEM100.
000170 INPUT-OUTPUT                       SECTION.
000180*
000190 FILE-CONTROL.
000200*
000210***********************[ Ã¶ÞÀ        Ì§²Ù    ]
000220     SELECT            TM           ASSIGN   TO   TEGATA-MSD
000230                                    ORGANIZATION  INDEXED
000240                                    ACCESS  MODE  SEQUENTIAL
000250                                    RECORD  KEY   TM-KEY.
000260 DATA                  DIVISION.
000270*
000280 FILE                  SECTION.
000290*
000300     COPY              TEGATA.
000310*
000320******************************************************
000330 WORKING-STORAGE                SECTION.
000340******************************************************
000350*
000360 01  CRT-YY            PIC S9(02).
000370 01  CRT-MM            PIC S9(02).
000380 01  W-YMD.
000390     02  W-YY          PIC 9(02).
000400     02  W-MM          PIC 9(02).
000410     02  W-DD          PIC 9(02).
000420 01  WK-YMD.
000430     02  WK-YY         PIC 9(02).
000440     02  WK-MM         PIC 9(02).
000450     02  WK-DD         PIC 9(02).
000460 01  W-M1              PIC 9(02).
000470 01  W-M2              PIC 9(02).
000480*
000490******************************************************
000500 PROCEDURE                      DIVISION.
000510******************************************************
000520*
000530*------------[  ²Ø¸ÞÁ ¼®Ø   ]
000540*
000550 ENTRY-RTN                      SECTION.
000560   ENTRY-000.
000570     PERFORM INIT-RTN.
000580     PERFORM MAIN-RTN.
000590     PERFORM FINAL-RTN.
000600     STOP    RUN.
000610   ENTRY-999.
000620     EXIT.
000630*
000640*------------[  ¼®· ¼®Ø  ]
000650*
000660 INIT-RTN                    SECTION.
000670   INIT-000.
000680     OPEN    I-O     TM.
000690   INIT-999.
000700     EXIT.
000710*
000720*--------[ MAIN ¼®Ø ]
000730*
000740 MAIN-RTN                    SECTION.
000750   MAIN-000.
000760     DISPCRT (08,31)  ""K472F"".
000770     DISPCRT (10,31)  ""K376E"".
000780     ACEPCRT (08,37)    CRT-YY.
000790     ACEPCRT (10,37)    CRT-MM.
000800   MAIN-001.
000810     READ    TM          AT  END     GO  TO  MAIN-999.
000820     MOVE    FURYMD      TO     W-YMD.
000830     PERFORM     HIZUKESHORI-RTN.
000840     MOVE    WK-YMD      TO      FURYMD.
000850   MAIN-002.
000860     MOVE    KESYMD      TO      W-YMD.
000870     PERFORM     HIZUKESHORI-RTN.
000880     MOVE    WK-YMD      TO      KESYMD.
000890   MAIN-003.
000900     IF      TORYMD  =   000000
000910                         GO  TO  MAIN-004.
000920     MOVE    TORYMD      TO      W-YMD.
000930     PERFORM     HIZUKESHORI-RTN.
000940     MOVE    WK-YMD      TO      TORYMD.
000950   MAIN-004.
000960     REWRITE     TM-REC      INVALID  KEY
000970                             PERFORM     ERROR-RTN
000980                             GO  TO      MAIN-999.
000990     GO  TO  MAIN-001.
001000   MAIN-999.
001010     EXIT.
001020*
001030*-------------[ ËÂÞ¹ ÍÝº³   ¼®Ø ]
001040*
001050 HIZUKESHORI-RTN             SECTION.
001060   HIZUKE-000.
001130     ADD     CRT-YY   TO     W-YY.
001140     ADD     CRT-MM   TO     W-MM.
001150   HIZUKE-001.
           IF         W-MM  =  12
               MOVE   W-YMD    TO    WK-YMD
               GO  TO  HIZUKE-999.
001160     COMPUTE    W-M1  =  W-MM  /  12.
001170     COMPUTE    W-M2  =  W-M1  *  12.
001180     COMPUTE    WK-MM =  W-MM  -  W-M2.
001190     COMPUTE    WK-YY =  W-YY  +  W-M1.
001200     MOVE       W-DD     TO       WK-DD.
001210   HIZUKE-999.
001220     EXIT.
001230*
001240*-------------[ ´×°     ¼®Ø ]
001250*
001260 ERROR-RTN                   SECTION.
001270   ERR-000.
001280     DISPCRT (01,05)    "TM-REC  Ø×²Ä´×°     µÜØ".
001290   ERR-999.
001300     EXIT.
001310*
001320*----------[ ¼­³Ø®³ ¼®Ø ]
001330 FINAL-RTN               SECTION.
001340   FINAL-000.
001350     CLOSE     TM.
001360   FINAL-999.
001370     EXIT.
