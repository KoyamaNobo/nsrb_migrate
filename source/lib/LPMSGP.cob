      *****************************
      *    ´×° DISPLAY (Ò²Ý)      *
      *****************************
       ERR-ENT.
           MOVE    ERR-STAT  TO  ERR-FLG.
           PERFORM CLSE-ENT THRU CLSE-EXT.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING
            "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
      *           GO TO ERR-010.
       ERR-EXT.
           EXIT.
      *****************************
      *    ŠY“–ŒŽŽæž‚Ýˆ—       *
      *****************************
       Z-RTN.
           MOVE    1         TO  ZI.
       Z-010.
           IF  ZI  >  15
               MOVE  99      TO  ZI
               GO    TO      Z-EXT.
           IF  Z-TOUF(ZI)  >  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010.
           IF  Z-TOUT(ZI)  <  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010.
       Z-EXT.
           EXIT.
      *
      *
