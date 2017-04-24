      *****************************************
      *    MESSEGE  AREA                      *
      *                                       *
      * WORKING-STORAGE SECTION  :  LSERR     *
      * PROCEDURE DIVISION       :  LSERR_P   *
      *****************************************
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "ERR-LIN" "0" "499" " " " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "ERR-LIN" "0" "60" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "ERR-LIN" "1" "60" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE" " " "ERR-LIN" "0" "60" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "ERR-LIN" "1" "60" " "
            "DISP-MSG-SPACE" RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "ERR-LIN" "0" "5" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-B" "X" "ERR-LIN" "80" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-J" " " "ERR-LIN" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-J" "X" "ERR-LIN" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-M01" " " "ERR-LIN" "0" "44" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "ERR-LIN" "1" "24" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02NOR-M01" "X" "ERR-LIN" "13" "20" "01NOR-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "ERR-LIN" "0" "44" "NOR-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "ERR-LIN" "1" "24" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02NOR-D01" "X" "ERR-LIN" "13" "20" "01NOR-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "ERR-LIN" "0" "44" "NOR-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "ERR-LIN" "1" "24" " " "INV-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02INV-M01" "X" "ERR-LIN" "13" "20" "01INV-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "ERR-LIN" "0" "44" "INV-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "ERR-LIN" "1" "24" " " "INV-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02INV-D01" "X" "ERR-LIN" "13" "20" "01INV-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-01" " " "ERR-LIN" "0" "28" "INV-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01OK-01" "X" "ERR-LIN" "1" "28" " " "OK-01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CAN-01" " " "ERR-LIN" "0" "36" "OK-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CAN-01" "X" "ERR-LIN" "1" "20" " " "CAN-01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CAN-01" "X" "ERR-LIN" "11" "16" "01CAN-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-01" " " "ERR-LIN" "0" "36" "CAN-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-01" "X" "ERR-LIN" "1" "20" " " "ERR-01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-01" "X" "ERR-LIN" "11" "16" "01ERR-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "ERR-LIN" "0" "22" "ERR-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "ERR-LIN" "1" "22" " " "ERR-02"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "ERR-LIN" "0" "71" "ERR-02" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DIS" "X" "ERR-LIN" "2" "5" " " "ERR-DIS"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-DIS" "X" "ERR-LIN" "7" "12" "01ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-DIS" "X" "ERR-LIN" "19" "1" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ERR-DIS" "X" "ERR-LIN" "20" "11" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ERR-DIS" "X" "ERR-LIN" "31" "2" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06ERR-DIS" "X" "ERR-LIN" "33" "5" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07ERR-DIS" "X" "ERR-LIN" "38" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08ERR-DIS" "X" "ERR-LIN" "43" "30" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
      **
