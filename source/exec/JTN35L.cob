       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JTN35L.
      **************************************************
      *    PROGRAM      :    出荷指図入力リスト        *
      *    DATA WRITTEN :    98/ 6/19                  *
      *    SCREEN  USED :    ______                    *
      *    COMPILE TYPE :    COBOL                     *
      *    W-JS         :    指図=0 , 確定=1           *
      *                 :    ﾅﾌｺ =3 , ﾜｰｸﾏﾝ=4 , ﾄﾗｽｺ=6 *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT               PIC  X(02).
       77  DIS-SW                 PIC  9(01).
       77  JT-SW                  PIC  9(01).
       77  W-JS                   PIC  9(01).
       77  JS-SIGN                PIC  9(01).
       77  INP-SW                 PIC  9(01).
       77  FU-SW                  PIC  X(02).
       77  2K-SW                  PIC  X(03)  VALUE  "OFF".
       77  WRI-SW                 PIC  9(01).
       77  PCNT                   PIC  9(03)  VALUE  ZERO.
       77  PRN-SW                 PIC  9(01)  VALUE  0.
       01  W-M                    PIC  N(02).
       01  N-24                   PIC  N(24)  VALUE  ALL "　".
       01  Z-SW                   PIC  9(02).
       01  STN-NO.
           02  STN-NO-01          PIC  X(03).
           02  STN-NO-02          PIC  X(03).
       01  MID1.
           02  F                  PIC  X(05)  VALUE X"1A24212474".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  M-01               PIC  N(05).
           02  F                  PIC  X(26)  VALUE  SPACE.
           02  F                  PIC  N(07)  VALUE
                "＊＊＊　　出荷".
           02  M-02               PIC  N(02).
           02  F                  PIC  N(11)  VALUE
                "　入力リスト　　＊＊＊".
           02  F                  PIC  X(26)  VALUE  SPACE.
           02  F                  PIC  X(05)  VALUE  "DATE.".
           02  M-YY               PIC  9(02).
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-MM               PIC Z9.
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-DD               PIC Z9.
           02  F                  PIC  X(10)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE  "P.".
           02  WPCNT              PIC ZZ9.
           02  F                  PIC  X(02)  VALUE  SPACE.
       01  MID1A.
           02  F                  PIC  X(05)  VALUE X"1A24212474".
           02  M-NW               PIC  N(10)  VALUE  SPACE.
           02  F                  PIC  X(20)  VALUE  SPACE.
           02  F                  PIC  N(20)  VALUE
               "＊＊＊　　出荷指図　変換リスト　　＊＊＊".
           02  F                  PIC  X(26)  VALUE  SPACE.
           02  F                  PIC  X(05)  VALUE  "DATE.".
           02  M-YYA              PIC  9(02).
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-MMA              PIC Z9.
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-DDA              PIC Z9.
           02  F                  PIC  X(10)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE  "P.".
           02  WPCNTA             PIC ZZ9.
           02  F                  PIC  X(02)  VALUE  SPACE.
       01  MID2.
           02  F                  PIC  X(05)  VALUE X"1A24212078".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "指図№　".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "伝区".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "日　付　".
           02  F                  PIC  X(13)  VALUE  "  ｾｯﾄ    ｺｰﾄﾞ".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(10)  VALUE
                                              "得　　意　　先　　名".
           02  F                  PIC  X(25)  VALUE  SPACE.
           02  F                  PIC  N(10)  VALUE
                                              "直　　送　　先　　名".
           02  F                  PIC  X(25)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "倉　　庫".
           02  F                  PIC  X(06)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "個数".
           02  F                  PIC  X(02)  VALUE  SPACE.
       01  MID3.
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "-".
           02  F                  PIC  N(02)  VALUE "行　".
           02  F                  PIC  X(06)  VALUE  "ｺｰﾄﾞ  ".
           02  F                  PIC  N(06)  VALUE "品　　　名　".
           02  F                  PIC  X(28)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "1".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "３号".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "２号".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "１号".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "０号".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "　中".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "　大".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "特大".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "28.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "29.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "30.0".
           02  F                  PIC  X(27)  VALUE  SPACE.
       01  MID4.
           02  F                  PIC  X(51)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "2".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "12.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "14.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "15.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "16.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "17.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "18.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "19.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "20.0".
           02  F                  PIC  X(27)  VALUE  SPACE.
       01  MID5.
           02  F                  PIC  X(51)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "3".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(32)  VALUE  SPACE.
       01  MID6.
           02  F                  PIC  X(51)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "4".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.0".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.5".
           02  F                  PIC  X(14)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "　計".
           02  F                  PIC  N(04)  VALUE "　受注№".
           02  F                  PIC  X(01)  VALUE  "-".
           02  F                  PIC  N(02)  VALUE "行　".
           02  F                  PIC  N(04)  VALUE "備　考　".
           02  F                  PIC  X(04)  VALUE  SPACE.
       01  MID9.
           02  F                  PIC  X(50)  VALUE
                "･･････････････････････････････････････････････････".
           02  F                  PIC  X(50)  VALUE
                "･･････････････････････････････････････････････････".
           02  F                  PIC  X(34)  VALUE
                "･･････････････････････････････････".
       01  W-PR1.
           02  P1-15B             PIC X(05).
           02  P1-00              PIC N(02).
           02  F                  PIC X(01).
           02  P1-01              PIC 9(06).
           02  F                  PIC X(02).
           02  P1-02              PIC N(02).
           02  F                  PIC X(01).
           02  P1-031             PIC 9(02).
           02  P1-A               PIC X(01).
           02  P1-032             PIC Z9.
           02  P1-B               PIC X(01).
           02  P1-033             PIC Z9.
           02  P1-04              PIC ----.
           02  F                  PIC X(02).
           02  P1-051             PIC 9(04).
           02  P1-C               PIC X(01).
           02  P1-052             PIC 9(03).
           02  F                  PIC X(01).
           02  P1-06              PIC N(26).
           02  F                  PIC X(01).
           02  P1-07              PIC N(26).
           02  F                  PIC X(01).
           02  P1-081             PIC 9(01).
           02  F                  PIC X(01).
           02  P1-082             PIC N(06).
           02  P1-09              PIC ZZZ9.
           02  F                  PIC X(02).
           02  P1-2B              PIC X(05).
       01  W-PR2.
           02  P2-15B             PIC X(05).
           02  F                  PIC X(04).
           02  P2-D               PIC X(01).
           02  P2-01              PIC 9(01).
           02  F                  PIC X(01).
           02  P2-02              PIC 9(06).
           02  F                  PIC X(01).
           02  P2-03              PIC N(24).
           02  F                  PIC X(01).
           02  P2-04              PIC 9(01).
           02  P2-051             PIC --,---.
           02  P2-052             PIC --,---.
           02  P2-053             PIC --,---.
           02  P2-054             PIC --,---.
           02  P2-055             PIC --,---.
           02  P2-056             PIC --,---.
           02  P2-057             PIC --,---.
           02  P2-058             PIC -----.
           02  P2-059             PIC -----.
           02  P2-050             PIC -----.
           02  P2-06              PIC ---,---.
           02  F                  PIC X(01).
           02  P2-071             PIC 9(06).
           02  P2-E               PIC X(01).
           02  P2-072             PIC 9(01).
           02  F                  PIC X(01).
           02  P2-08              PIC X(10).
           02  P2-2B              PIC X(05).
       01  W-PR3.
           02  P3-15B             PIC X(05).
           02  F                  PIC X(07).
           02  P3-01              PIC N(02).
           02  P3-F               PIC X(01).
           02  P3-02.
             03  P3-021           PIC N(09).
             03  P3-022           PIC N(01).
           02  F                  PIC X(01).
           02  P3-03              PIC N(02).
           02  P3-G               PIC X(01).
           02  P3-04.
             03  P3-041           PIC N(23).
             03  P3-042           PIC N(01).
           02  F                  PIC X(01).
           02  P3-05              PIC N(02).
           02  P3-H               PIC X(01).
           02  P3-061             PIC 9(01).
           02  F                  PIC X(01).
           02  P3-062             PIC N(06).
           02  F                  PIC X(02).
           02  P3-07              PIC N(04).
           02  P3-I               PIC X(01).
           02  P3-081             PIC 9(06).
           02  P3-J               PIC X(01).
           02  P3-082             PIC 9(02).
           02  F                  PIC X(03).
           02  P3-09              PIC N(02).
           02  P3-K               PIC X(01).
           02  P3-10              PIC ----,---.
           02  F                  PIC X(06).
           02  P3-11              PIC N(02).
           02  P3-L               PIC X(01).
           02  P3-12              PIC ----,---.
           02  F                  PIC X(02).
           02  P3-2B              PIC X(05).
       01  W-AREA1.
           02  WYMD.
             03  WYY              PIC  9(02).
             03  WMM              PIC  9(02).
             03  WDD              PIC  9(02).
           02  K-1                PIC  X(05)   VALUE  X"1A24212078".
           02  K-2                PIC  X(05)   VALUE  X"1A24212474".
       01  W-AREA2.
           02  W-SNO              PIC  9(03).
           02  W-SEN1             PIC  9(01).
           02  W-SEN2             PIC  9(01).
           02  W-DEN.
             03  W-FROM           PIC  9(06).
             03  W-TO             PIC  9(06).
           02  W-DMM              PIC  9(01).
           02  CHK                PIC  9(01).
           02  W-NO               PIC  9(06).
           02  W-KEI              PIC S9(06).
      *
           COPY     LWMSG.
      *
           COPY     L-JSTR.
           COPY     LIHIM2.
           COPY     LITCM.
           COPY     L-JCON.
           COPY     LOKJF.
      *FD  SP-F
       77  SP-R                   PIC  X(180).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER  PIC  X(20) VALUE
               "出荷　　　入力リスト".
           02  FILLER  PIC  N(02).
           02  FILLER.
             03  FILLER  PIC  X(15) VALUE  "入力画面№     ".
             03  FILLER  PIC  X(09) VALUE  "(ALL=999)".
           02  FILLER.
             03  FILLER  PIC  X(22) VALUE  "教　育=0 , 一　般=1 , ".
             03  FILLER  PIC  X(14) VALUE  "ＡＬＬ=9 ...  ".
           02  FILLER  PIC  X(12) VALUE  "０　未発行分".
           02  FILLER.
               03  FILLER  PIC  X(16) VALUE "１　発行済分　　".
               03  FILLER  PIC  X(01) VALUE   "[".
               03  FILLER  PIC  X(01) VALUE   "]".
           02  FILLER  PIC  X(06) VALUE  "指図№".
           02  FILLER  PIC  X(08) VALUE  "ＦＲＯＭ".
           02  FILLER  PIC  X(04) VALUE  "ＴＯ".
           02  FILLER.
               03  FILLER  PIC  X(06) VALUE  "確認（".
               03  FILLER  PIC  X(09) VALUE    "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "）".
               03  FILLER  PIC  X(10) VALUE    "--->  ﾘﾀｰﾝ".
       01  DSP-AREA1.
           02  FILLER  PIC  X(12) VALUE  "０　未生成分".
           02  FILLER  PIC  X(12) VALUE  "１　生成済分".
           02  FILLER.
               03  FILLER  PIC  X(16) VALUE "９　全　　件　　".
               03  FILLER  PIC  X(01) VALUE   "[".
               03  FILLER  PIC  X(01) VALUE   "]".
       01  DSP-AREA3.
           02  FILLER  PIC  X(34) VALUE
               "ナフコＥＯＳ　出荷指図　変換リスト".
       01  DSP-AREA4.
           02  FILLER  PIC  X(38) VALUE
               "ワークマンＥＯＳ　出荷指図　変換リスト".
       01  DSP-AREA6.
           02  FILLER  PIC  X(36) VALUE
               "トラスコ他入力　出荷指図　変換リスト".
       01  ACP-AREA.
           02  ACP-SNO     PIC  9(03).
           02  ACP-SIGN    PIC  9(01).
           02  ACP-SEN1    PIC  9(01).
           02  ACP-SEN2    PIC  9(01).
           02  ACP-FROM    PIC  9(06).
           02  ACP-TO      PIC  9(06).
           02  ACP-DMM     PIC  9(01).
       01  DSP-CLE.
           02  FILLER  PIC  X(06) VALUE  "      ".
           02  FILLER  PIC  X(06) VALUE  "      ".
       01  DSP-ERR.
           02  ERR-JS     PIC  X(30)
               VALUE "＊　ＪＳ－ＳＩＧＮ　エラー　＊".
           02  ERR-READ   PIC  X(32)
               VALUE "＊　ＪＳＴＲ　ＲＥＡＤエラー　＊".
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(60).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  ERR-02.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　なし　　＊".
           02  ERR-DIS.
               03  FILLER  PIC X(05) VALUE
               "<<<  ".
               03  FILLER  PIC X(12).
               03  FILLER  PIC X(01).
               03  FILLER  PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  FILLER  PIC X(02).
               03  FILLER  PIC X(05) VALUE
               "  >>>".
               03  FILLER  PIC X(05) VALUE
               " KEY=".
               03  FILLER  PIC X(30).
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RX" "1" "26" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "1" "30" "4" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02DSP-AREA" BY REFERENCE W-M "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" " " "5" "0" "24" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0103DSP-AREA" "X" "5" "25" "15" " " "03DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0203DSP-AREA" "X" "5" "44" "9" "0103DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" " " "7" "0" "36" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0104DSP-AREA" "X" "7" "25" "22" " " "04DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0204DSP-AREA" "X" "7" "47" "14" "0104DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "X" "10" "25" "12" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" " " "11" "0" "18" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0106DSP-AREA" "X" "11" "25" "16" " " "06DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0206DSP-AREA" "X" "11" "41" "1" "0106DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0306DSP-AREA" "X" "11" "43" "1" "0206DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "X" "15" "31" "6" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-AREA" "X" "16" "21" "8" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-AREA" "X" "17" "21" "4" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-AREA" " " "24" "0" "27" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0110DSP-AREA" "X" "24" "41" "6" " " "10DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0210DSP-AREA" "X" "24" "47" "9" "0110DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0310DSP-AREA" "X" "24" "56" "2" "0210DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0410DSP-AREA" "X" "24" "58" "10" "0310DSP-AREA" " "
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING
            "DSP-AREA1" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA1" "X" "10" "48" "12" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA1" "X" "11" "48" "12" "01DSP-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA1" " " "12" "0" "18" "02DSP-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0103DSP-AREA1" "X" "12" "48" "16" " " "03DSP-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0203DSP-AREA1" "X" "12" "64" "1" "0103DSP-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0303DSP-AREA1" "X" "12" "66" "1" "0203DSP-AREA1" " "
            RETURNING RESU.
      *DSP-AREA3
       CALL "SD_Init" USING
            "DSP-AREA3" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA3" "RX" "1" "12" "34" " " "DSP-AREA3"
            RETURNING RESU.
      *DSP-AREA4
       CALL "SD_Init" USING
            "DSP-AREA4" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA4" "RX" "1" "10" "38" " " "DSP-AREA4"
            RETURNING RESU.
      *DSP-AREA6
       CALL "SD_Init" USING
            "DSP-AREA6" " " "0" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA6" "RX" "1" "10" "36" " " "DSP-AREA6"
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-SNO" "9" "5" "37" "3" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-SNO" BY REFERENCE W-SNO "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-SIGN" "9" "7" "60" "1" "ACP-SNO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-SEN1" "9" "11" "42" "1" "ACP-SIGN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-SEN1" BY REFERENCE W-SEN1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-SEN2" "9" "12" "65" "1" "ACP-SEN1" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-SEN2" BY REFERENCE W-SEN2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-FROM" "9" "16" "31" "6" "ACP-SEN2" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TO" "9" "17" "31" "6" "ACP-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-DMM" "9" "24" "63" "1" "ACP-TO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING
            "DSP-CLE" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLE" "X" "16" "31" "6" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-CLE" "X" "17" "31" "6" "01DSP-CLE" " "
            RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING
            "DSP-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-JS" "X" "24" "1" "30" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-READ" "X" "24" "1" "32" "ERR-JS" " " RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING
            "DISP-ERR-AREA" " " "24" "0" "203" " " " " RETURNING RESU.
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
            "DISP-MSG-SPACE" " " "24" "0" "40" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-SPACE" "X" "24" "1" "40" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
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
            "ERR-02" " " "24" "0" "22" "DISP-BUZ-J" " " RETURNING RESU.
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
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MEIN.
           PERFORM  GMN-RTN    THRU  GMN-EX.
           IF  ESTAT      =  "P9"
               GO  TO  MR999
           END-IF
           PERFORM  OPN-RTN    THRU  OPN-EX.
           PERFORM  PRC-RTN    THRU  PRC-EX.
           PERFORM  END-RTN    THRU  END-EX.
       MR999.
           CALL "DB_Close".
           STOP     RUN.
       OPN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "PR_Open" RETURNING RESP.
           IF  W-JS         =  0
               CALL "DB_F_Open" USING
                "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
                "OKJF-KEY" BY REFERENCE OKJF-KEY
           END-IF.
       OPN-EX.
           EXIT.
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  W-JS         =  0
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
       GMN-RTN.
           ACCEPT W-JS FROM ARGUMENT-VALUE.
           IF  W-JS     NOT =  0  AND  1  AND  3  AND  4  AND  6
               CALL "SD_Output" USING "ERR-JS" ERR-JS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-JS         =  0
               MOVE  "指図"     TO  W-M
           ELSE
               IF  W-JS         =  1
                   MOVE  "確定"     TO  W-M
               ELSE
                   IF  W-JS         =  3
                       MOVE  "【ナフコＥＯＳ】　　"   TO  M-NW
                   ELSE
                       IF  W-JS         =  4
                           MOVE  "【ワークマンＥＯＳ】"   TO  M-NW
                       ELSE
                           IF  W-JS         =  6
                               MOVE  "【トラスコ他入力】　"   TO  M-NW
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE  W-M        TO  M-02.
           CALL     "CBLSTNNO"     USING  STN-NO USER_ID.
           MOVE  ZERO       TO  W-AREA2.
           ACCEPT   WYMD       FROM  DATE.
           MOVE  WYY        TO  M-YY  M-YYA.
           MOVE  WMM        TO  M-MM  M-MMA.
           MOVE  WDD        TO  M-DD  M-DDA.
       GMN-000.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           IF  W-JS         =  3
               CALL "SD_Output" USING
                "DSP-AREA3" DSP-AREA3 "p" RETURNING RESU
               MOVE  999        TO  W-SNO
               MOVE  9          TO  JS-SIGN
               MOVE  0          TO  W-SEN1  W-SEN2
               GO  TO  GMN-EX
           END-IF
           IF  W-JS         =  4
               CALL "SD_Output" USING
                "DSP-AREA4" DSP-AREA4 "p" RETURNING RESU
               MOVE  999        TO  W-SNO
               MOVE  9          TO  JS-SIGN
               MOVE  0          TO  W-SEN1  W-SEN2
               GO  TO  GMN-EX
           END-IF
           IF  W-JS         =  6
               CALL "SD_Output" USING
                "DSP-AREA6" DSP-AREA6 "p" RETURNING RESU
               MOVE  999        TO  W-SNO
               MOVE  9          TO  JS-SIGN
               MOVE  0          TO  W-SEN1  W-SEN2
               GO  TO  GMN-EX
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           IF  W-JS         =  0
               CALL "SD_Output" USING
                "DSP-AREA1" DSP-AREA1 "p" RETURNING RESU
           END-IF.
       GMN-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNO "ACP-SNO"
            "9" "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  GMN-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-005
           END-IF
           CALL "SD_Output" USING "ACP-SNO" ACP-SNO "p" RETURNING RESU.
       GMN-007.
           CALL "SD_Accept" USING BY REFERENCE ACP-SIGN "ACP-SIGN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  GMN-005
           END-IF
           IF  ESTAT      =  "P9"
               GO  TO  GMN-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-007
           END-IF
           CALL "SD_Output" USING
            "ACP-SIGN" ACP-SIGN "p" RETURNING RESU.
           IF  JS-SIGN NOT =  0  AND  1  AND  9
               GO  TO  GMN-007
           END-IF.
       GMN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN1 "ACP-SEN1"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  GMN-007
           END-IF
           IF  ESTAT      =  "P9"
               GO  TO  GMN-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-010
           END-IF
           CALL "SD_Output" USING
            "ACP-SEN1" ACP-SEN1 "p" RETURNING RESU.
           IF  W-SEN1 NOT =  0  AND  1
               GO  TO  GMN-010
           END-IF
           IF  W-JS   NOT =  0
               GO  TO  GMN-018
           END-IF.
       GMN-015.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN2 "ACP-SEN2"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  GMN-010
           END-IF
           IF  ESTAT      =  "P9"
               GO  TO  GMN-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-015
           END-IF
           CALL "SD_Output" USING
            "ACP-SEN2" ACP-SEN2 "p" RETURNING RESU.
           IF  W-SEN2 NOT =  0  AND  1  AND  9
               GO  TO  GMN-015
           END-IF.
       GMN-018.
           IF  W-SEN1     =  0
               CALL "SD_Output" USING
                "DSP-CLE" DSP-CLE "p" RETURNING RESU
               GO  TO  GMN-090
           END-IF.
       GMN-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM "ACP-FROM"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-JS       =  0
                   GO  TO  GMN-015
               ELSE
                   GO  TO  GMN-010
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-020
           END-IF
           CALL "SD_Output" USING
            "ACP-FROM" ACP-FROM "p" RETURNING RESU.
       GMN-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO "ACP-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  GMN-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-030
           END-IF
           CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU.
           IF  W-FROM     >  W-TO
               GO  TO  GMN-030
           END-IF.
       GMN-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-DMM "ACP-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-SEN1     =  1
                   GO  TO  GMN-030
               ELSE
                   IF  W-JS       =  0
                       GO  TO  GMN-015
                   ELSE
                       GO  TO  GMN-010
                   END-IF
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  GMN-090
           END-IF
           CALL "SD_Output" USING "ACP-DMM" ACP-DMM "p" RETURNING RESU.
           IF  W-DMM      =  9
               GO  TO  GMN-000
           END-IF
           IF  W-DMM  NOT =  1
               GO  TO  GMN-090
           END-IF.
       GMN-EX.
           EXIT.
       PRC-RTN.
           IF  W-SEN1     =  0
               MOVE  ZERO        TO  W-FROM
               MOVE  999999      TO  W-TO
           END-IF
           MOVE  SPACE       TO  JSTR-KEY.
           MOVE  W-FROM      TO  JSTR-01.
           IF  JS-SIGN    =  1
               IF  W-FROM     <  100000
                   MOVE  100000    TO  JSTR-01
               END-IF
           END-IF
      *           START    JSTR KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   PRC-EX
           END-IF.
       PRC-010.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   PRC-EX
           END-IF
           IF  JS-SIGN    =  0
               IF  JSTR-01     >  099999
                   GO  TO  PRC-EX
               END-IF
           END-IF
           IF  JS-SIGN    =  1
               IF  JSTR-01     >  199999
                   GO  TO  PRC-EX
               END-IF
           END-IF
           IF  JSTR-01         >  W-TO
               GO  TO  PRC-EX
           END-IF
           IF  W-JS       =  0  OR  1
               IF  JSTR-30   NOT  =  0  AND  2  AND 5
                   GO  TO  PRC-010
               END-IF
           END-IF
           IF  W-JS       =  3  OR  4
               IF  JSTR-30   NOT  =  1
                   GO  TO  PRC-010
               END-IF
           END-IF
           IF  W-JS       =  6
               IF  JSTR-30   NOT  =  4
                   GO  TO  PRC-010
               END-IF
           END-IF
           IF  W-SEN1          =  0
               IF  W-JS       =  0
                   IF  JSTR-4012       =  1
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  0
               IF  W-JS       =  1
                   IF  JSTR-4022   NOT =  1
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  1
               IF  W-JS       =  0
                   IF  JSTR-4012   NOT =  1
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  1
               IF  W-JS       =  1
                   IF  JSTR-4022   NOT =  2
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SNO       NOT =  999
                   IF  JSTR-4011   NOT =  W-SNO
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  1
               IF  W-SNO       NOT =  999
                   IF  JSTR-4021   NOT =  W-SNO
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SEN2     =  0
                   IF (JSTR-14     NOT =  9)  AND (JSTR-03   NOT  =  0)
                       IF  JSTR-17     NOT =  0
                           GO  TO  PRC-010
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SEN2     =  1
                   IF  JSTR-17         =  0
                       GO  TO  PRC-010
                   END-IF
               END-IF
           END-IF
      *
           PERFORM  MID-RTN    THRU  MID-EX.
       PRC-020.
           MOVE     JSTR-01    TO    W-NO.
           MOVE     ZERO       TO    W-KEI  CHK.
      *
           MOVE     SPACE      TO    W-PR3.
           MOVE     K-1        TO    P3-15B.
           MOVE     K-2        TO    P3-2B.
           MOVE     SPACE      TO    P3-021 P3-022 P3-041 P3-042 P3-062
                                     P3-11.
           MOVE   "配達"     TO    P3-01.
           MOVE   "摘要"     TO    P3-03.
           MOVE     SPACE      TO    P3-05  P3-07  P3-062.
           IF  JSTR-03         =  0
               MOVE   "運送"     TO    P3-05
               MOVE   "送り状№" TO    P3-07
           END-IF
           MOVE   "小計"     TO    P3-09.
           MOVE     ":"        TO    P3-F  P3-G  P3-H  P3-I  P3-K.
           MOVE     JSTR-14D   TO    P3-021.
           MOVE     JSTR-15    TO    P3-041.
           IF  JSTR-03    NOT  =  0
               GO  TO  PRC-030
           END-IF
           MOVE     JSTR-14    TO    P3-061.
           MOVE     2          TO    JCON2-01.
           MOVE     JSTR-14    TO    JCON2-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON2-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON2-03
           END-IF
           MOVE     JCON2-03   TO    P3-062.
           MOVE     JSTR-14B   TO    P3-081.
           MOVE     "-"        TO    P3-J.
           MOVE     JSTR-14C   TO    P3-082.
           IF  W-JS       NOT  =  0
               GO  TO  PRC-030
           END-IF
           IF  JSTR-14    NOT  =  6
               GO  TO  PRC-030
           END-IF
           IF  JSTR-14C   NOT  =  ZERO
               GO  TO  PRC-030
           END-IF
           MOVE   JSTR-14B     TO    OKJF-KEY.
      *           READ     OKJF       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     ZERO       TO    OKJF-12
           END-IF
           MOVE   "代引"     TO    P3-11.
           MOVE     ":"        TO    P3-L.
           MOVE     OKJF-12    TO    P3-12.
       PRC-030.
           IF  CHK        =  0
               PERFORM  PR1-RTN    THRU  PR1-EX
           END-IF
           PERFORM  PR2-RTN    THRU  PR2-EX.
           PERFORM  UPD-RTN    THRU  UPD-EX.
       PRC-040.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   PRC-090
           END-IF
           IF  JS-SIGN    =  0
               IF  JSTR-01     >  099999
                   GO  TO  PRC-090
               END-IF
           END-IF
           IF  JS-SIGN    =  1
               IF  JSTR-01     >  199999
                   GO  TO  PRC-090
               END-IF
           END-IF
           IF  JSTR-01         >  W-TO
               GO  TO  PRC-090
           END-IF
           IF  W-JS       =  0  OR  1
               IF  JSTR-30   NOT  =  0  AND  2  AND 5
                   GO  TO  PRC-040
               END-IF
           END-IF
           IF  W-JS       =  3  OR  4
               IF  JSTR-30   NOT  =  1
                   GO  TO  PRC-040
               END-IF
           END-IF
           IF  W-JS       =  6
               IF  JSTR-30   NOT  =  4
                   GO  TO  PRC-040
               END-IF
           END-IF
           IF  W-SEN1          =  0
               IF  W-JS       =  0
                   IF  JSTR-4012       =  1
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  0
               IF  W-JS       =  1
                   IF  JSTR-4022   NOT =  1
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  1
               IF  W-JS       =  0
                   IF  JSTR-4012   NOT =  1
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-SEN1          =  1
               IF  W-JS       =  1
                   IF  JSTR-4022   NOT =  2
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SNO       NOT =  999
                   IF  JSTR-4011   NOT =  W-SNO
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  1
               IF  W-SNO       NOT =  999
                   IF  JSTR-4021   NOT =  W-SNO
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SEN2     =  0
                   IF (JSTR-14     NOT =  9)  AND (JSTR-03   NOT  =  0)
                       IF  JSTR-17     NOT =  0
                           GO  TO  PRC-040
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-JS       =  0
               IF  W-SEN2     =  1
                   IF  JSTR-17         =  0
                       GO  TO  PRC-040
                   END-IF
               END-IF
           END-IF
      *
           IF  JSTR-01         =  W-NO
               GO  TO  PRC-030
           END-IF
           PERFORM  PR3-RTN    THRU  PR3-EX.
           GO  TO  PRC-020.
       PRC-090.
           PERFORM  PR3-RTN    THRU  PR3-EX.
       PRC-EX.
           EXIT.
       UPD-RTN.
           IF  W-JS            =     0
               IF  JSTR-4012       =     1
                   GO  TO  UPD-EX
               END-IF
           END-IF
           IF  W-JS            =     1
               IF  JSTR-4022       =     2
                   GO  TO  UPD-EX
               END-IF
           END-IF
      *           READ     JSTR       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-READ" ERR-READ "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-JS            =     0
               MOVE     1          TO    JSTR-4012
           END-IF
           IF  W-JS            =     1
               MOVE     2          TO    JSTR-4022
           END-IF
           IF  W-JS            =     3  OR  4
               MOVE     2          TO    JSTR-30
           END-IF
           IF  W-JS            =     6
               MOVE     5          TO    JSTR-30
           END-IF
      *           REWRITE  JSTR-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       UPD-EX.
           EXIT.
       PR1-RTN.
           MOVE     SPACE      TO    W-PR1.
           MOVE     SPACE      TO    P1-00  P1-02  P1-06  P1-07  P1-082.
           MOVE     K-1        TO    P1-15B.
           IF  W-JS        NOT =  0  AND  3  AND  4  AND  6
               IF  JSTR-4023      =  2
                   MOVE     "訂正"   TO    P1-00
               ELSE
                   MOVE     "取消"   TO    P1-00
               END-IF
           END-IF
           MOVE     JSTR-01    TO    P1-01.
           IF  JSTR-03        =  0
               MOVE     "出荷"   TO    P1-02
           END-IF
           IF  JSTR-03        =  3
               MOVE     "訂正"   TO    P1-02
           END-IF
           IF  JSTR-03        =  5
               MOVE     "返品"   TO    P1-02
           END-IF
           IF  JSTR-03        =  6
               MOVE     "不良"   TO    P1-02
           END-IF
           MOVE     "/"        TO    P1-A    P1-B.
           IF  W-JS            =  0  OR  3  OR  4  OR  6
               MOVE     JSTR-0412  TO    P1-031
               MOVE     JSTR-042   TO    P1-032
               MOVE     JSTR-043   TO    P1-033
           ELSE
               MOVE     JSTR-0512  TO    P1-031
               MOVE     JSTR-052   TO    P1-032
               MOVE     JSTR-053   TO    P1-033
           END-IF
           MOVE     JSTR-14A   TO    P1-04.
           MOVE     JSTR-061   TO    P1-051.
           MOVE     "-"        TO    P1-C.
           MOVE     JSTR-062   TO    P1-052.
           MOVE     JSTR-061   TO    TC-TCD.
           MOVE     001        TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    P1-06.
           IF  JSTR-062        =  001
               GO  TO  PR1-010
           END-IF
           MOVE     JSTR-061   TO    TC-TCD.
           MOVE     JSTR-062   TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    P1-07.
       PR1-010.
           MOVE     JSTR-07    TO    P1-081.
           MOVE     3          TO    JCON3-01.
           MOVE     JSTR-07    TO    JCON3-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON3-03
           END-IF
           MOVE     JCON3-03   TO    P1-082.
           IF  W-JS        NOT =  0  AND  3  AND  4  AND  6
               MOVE     JSTR-15A   TO    P1-09
           END-IF
           MOVE     K-2        TO    P1-2B.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE   SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM   MID-RTN     THRU   MID-EX
           END-IF
           MOVE     SPACE      TO    SP-R.
           MOVE     W-PR1      TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     1          TO    CHK.
       PR1-EX.
           EXIT.
       PR2-RTN.
           MOVE     SPACE      TO    W-PR2.
           MOVE     SPACE      TO    P2-03.
           MOVE     "-"        TO    P2-D.
           MOVE     JSTR-02    TO    P2-01.
           MOVE     JSTR-09    TO    P2-02.
           MOVE     JSTR-09    TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    HI-NAME
           END-IF
           MOVE     HI-NAME    TO    P2-03.
           MOVE     JSTR-10    TO    P2-04.
           IF  W-JS            =  0  OR  3  OR  4  OR  6
               MOVE     JSTR-1111(01) TO    P2-051
               MOVE     JSTR-1111(02) TO    P2-052
               MOVE     JSTR-1111(03) TO    P2-053
               MOVE     JSTR-1111(04) TO    P2-054
               MOVE     JSTR-1111(05) TO    P2-055
               MOVE     JSTR-1111(06) TO    P2-056
               MOVE     JSTR-1111(07) TO    P2-057
               MOVE     JSTR-1111(08) TO    P2-058
               MOVE     JSTR-1111(09) TO    P2-059
               MOVE     JSTR-1111(10) TO    P2-050
               MOVE     JSTR-112   TO    P2-06
           ELSE
               MOVE     JSTR-1211(01) TO    P2-051
               MOVE     JSTR-1211(02) TO    P2-052
               MOVE     JSTR-1211(03) TO    P2-053
               MOVE     JSTR-1211(04) TO    P2-054
               MOVE     JSTR-1211(05) TO    P2-055
               MOVE     JSTR-1211(06) TO    P2-056
               MOVE     JSTR-1211(07) TO    P2-057
               MOVE     JSTR-1211(08) TO    P2-058
               MOVE     JSTR-1211(09) TO    P2-059
               MOVE     JSTR-1211(10) TO    P2-050
               MOVE     JSTR-122   TO    P2-06
           END-IF
           IF  JSTR-081   NOT  =   ZERO
               MOVE  JSTR-081     TO P2-071
               MOVE  "-"          TO P2-E
               MOVE  JSTR-082     TO P2-072
           END-IF
           IF  JSTR-20    NOT  =   SPACE
               MOVE  JSTR-20      TO P2-08
           END-IF
           MOVE     K-1        TO    P2-15B.
           MOVE     K-2        TO    P2-2B.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE   SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM   MID-RTN     THRU   MID-EX
           END-IF
           MOVE     SPACE      TO    SP-R.
           MOVE     W-PR2      TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-JS            =  0  OR  3  OR  4  OR  6
               ADD      JSTR-112   TO    W-KEI
           ELSE
               ADD      JSTR-122   TO    W-KEI
           END-IF.
       PR2-EX.
           EXIT.
       PR3-RTN.
           MOVE     W-KEI      TO    P3-10.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE   SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM   MID-RTN     THRU   MID-EX
           END-IF
           MOVE     SPACE      TO    SP-R.
           MOVE     W-PR3      TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE      TO    SP-R.
           IF  W-JS            =  0  OR  3  OR  4  OR  6
               MOVE     MID9       TO    SP-R
           END-IF
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE      TO    SP-R.
       PR3-EX.
           EXIT.
       MID-RTN.
           ADD   1   TO    PCNT.
           IF  W-JS            =  3  OR  4  OR  6
               MOVE   PCNT    TO    WPCNTA
           ELSE
               MOVE  PCNT  TO  WPCNT
           END-IF
           IF  JS-SIGN  =  ZERO
               MOVE  "【教　育】"     TO  M-01
           END-IF
           IF  JS-SIGN  =  1
               MOVE  "【一　般】"     TO  M-01
           END-IF
           IF  W-JS            =  3  OR  4  OR  6
               MOVE   MID1A   TO    SP-R
           ELSE
               MOVE   MID1    TO    SP-R
           END-IF
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID2    TO    SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   MID3    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID4    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID5    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID6    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
      *
       MID-EX.
           EXIT.
      *
           COPY    LPMSG.
