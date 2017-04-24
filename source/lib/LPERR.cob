      *****************************
      *    ´×° DISPLAY (Ò²Ý)      * (µÝ×²Ý—p)
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           IF  ERR-LIN  =  ZERO
               MOVE  24    TO  ERR-LIN
           END-IF
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
      *
      *
