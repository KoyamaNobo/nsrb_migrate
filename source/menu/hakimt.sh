/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　履物棚卸処理              JIP
01 HMN010LM履物棚卸ファイル　クリア        CHAINN  ANO exec
02 ******  ******************************  CHAINN  ANO
03 HMN110LM履物棚卸伝票　入力              CHAINN  ANO exec
04 ******                                  CHAINN  ANO
05 HN10  JS品名別棚卸チェックリスト        CHAINN  A   job
06 ******                                  CHAINN  ANO
07 HN05  JS品名統計ファイル　棚卸セット    CHAINN  A   job
08 ******                                  CHAINN  ANO
09 HN15  JS品名サイズ別　棚卸明細表        CHAINN  A   job
10 HN20  JS倉庫品名サイズ別　棚卸明細表    CHAINN  A   job
11 HN25  JS製品　分類棚卸表                CHAINN  A   job
12 ******                                  CHAINN  ANO
13 HN35  JS棚卸 親子コード チェック表      CHAINN  A   job
14 ******                                  CHAINN  ANO
15 HN30  JS履物分類別・品種別 棚卸誤差表   CHAINN  A   job
16 HN65  JS品名サイズ別 棚卸誤差表         CHAINN  A   job
17 ******  ･･････････････････････････････  CHAINN  ANO
18 HMN120LM品名一括変更入力（親子）        CHAINN  ANO exec
19 JTT510JSバーコード分セット              CHAINN  A   job
20 JTT710JSバーコードなし分セット（玉島他  CHAINN  A   job
21 HN60  JS◎　履物前月残高　棚卸更新　◎  CHAINN  A   job
22 ******                                  CHAINN  ANO
23 ******  ======  決算単価処理  ========  CHAINN  ANO
24 HN40  JS棚卸表　（決算単価入力用）      CHAINN  A   job
25 ******                                  CHAINN  ANO
26 HMN850LM棚卸 決算用単価 入力            CHAINN  ANO exec
27 ******                                  CHAINN  ANO
28 HN45  JS決算用　倉庫別　棚卸明細表      CHAINN  A   job
29 HN55  JS分類決算用　棚卸差額集計表      CHAINN  A   job
30 HN50  JS決算用　棚卸差額　明細表        CHAINN  A   job
31 ******                                  CHAINN  ANO
32 ******  =====　製品廃棄処理　=========  CHAINN  ANO
33 HH10  JS在庫表エクセル変換  (HZAIKO)    CHAINN  A   job
34 ******                                  CHAINN  ANO
35 HH20  JS廃棄品ワーク作成 (HAIK-WK0064)  CHAINN  A   job
36 ******                                  CHAINN  ANO
37 HH50  JS分類・品名別在庫廃棄明細表      CHAINN  A   job
38 ******                                  CHAINN  ANO
39 ******                                  CHAINN  ANO
40 HH90  JSセーブ・在庫更新 (HUHM･HHTF)    CHAINN  A   job
/>
