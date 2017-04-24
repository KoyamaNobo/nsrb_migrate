      *****************************************
      *    MESSEGE  AREA                      *
      *                                       *
      * WORKING-STORAGE SECTION  :  LSMSG     *
      * PROCEDURE DIVISION       :  LSMSG_P   *
      *****************************************
       CALL "SD_Init" USING
           "DISP-ERR-AREA" " " "24" "0" "401" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-MSG-01" " " "24" "0" "60" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-MSG-01" "X" "24" "1" "60" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING
           "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-MSG-SPACE" " " "24" "0" "60" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-MSG-SPACE" "X" "24" "1" "60" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING
           "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-MSG-SPACES" " " "24" "0" "40" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-MSG-SPACES" "X" "24" "1" "40" " " "DISP-MSG-SPACES"
            RETURNING RESU.
       CALL "SD_From" USING
           "01DISP-MSG-SPACES" BY REFERENCE ERR-SPACES "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACES" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-BUZ-B" "X" "24" "80" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING
           "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01" RETURNING RESU.
       CALL "SD_Init" USING
           "NOR-D01" " " "24" "0" "22" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01" RETURNING RESU.
       CALL "SD_Init" USING
           "INV-M01" " " "24" "0" "22" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-M01" "X" "24" "1" "22" " " "INV-M01" RETURNING RESU.
       CALL "SD_Init" USING
           "INV-D01" " " "24" "0" "22" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-D01" "X" "24" "1" "22" " " "INV-D01" RETURNING RESU.
       CALL "SD_Init" USING
           "OK-01" " " "24" "0" "14" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01OK-01" "X" "24" "1" "14" " " "OK-01" RETURNING RESU.
       CALL "SD_Init" USING
           "CAN-01" " " "24" "0" "18" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CAN-01" "X" "24" "1" "18" " " "CAN-01" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-01" " " "24" "0" "18" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-01" "X" "24" "1" "18" " " "ERR-01" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-02" " " "24" "0" "22" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-02" "X" "24" "1" "22" " " "ERR-02" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-DIS" " " "24" "0" "71" "ERR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-DIS" "X" "24" "2" "5" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_Init" USING
           "02ERR-DIS" "X" "24" "7" "12" "01ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03ERR-DIS" "X" "24" "19" "1" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04ERR-DIS" "X" "24" "20" "11" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "05ERR-DIS" "X" "24" "31" "2" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "06ERR-DIS" "X" "24" "33" "5" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "07ERR-DIS" "X" "24" "38" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "08ERR-DIS" "X" "24" "43" "30" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "08ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
      **
