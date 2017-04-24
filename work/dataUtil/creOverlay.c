#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>
#include <math.h> 
#include <time.h>                /* localtimeに必要 */
#include <mysql.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/
#include <sys/types.h>           //ファイルの情報を取得するため
#include <sys/stat.h>            //ファイルの情報を取得するため
#include <iconv.h>               //プリンタへ文字を送る時の文字コード変換
#include <libcob.h>
#include "hpdf.h"
#include <confheader.h>
#include <ctype.h>
#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN];
int MT_Initialize();
#endif
//////MIN - 最小値取得 -
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#ifndef CONF_PATH
#define CONF_PATH
const char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";
const char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif

//DBのタグ名
#ifndef CONF_DB_TAG
#define CONF_DB_TAG
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
const char* CONF_DB_PORT = "dbPort";
const char* ITEM_TBL_NAME = "dbIEName";
const char* MANA_TBL_NAME = "dbTMName";
#endif


//DBの設定(長さ制限の倍まで取れるようにしておく(mysqlの仕様を基準とする))
const char* PR_DEFAULT_ENCODE = "90ms-RKSJ-H";
const char* PR_TEMP_PATH = "prnTempPath";
const char* PR_BASE_PATH = "prnBasePath";
const char* PR_PAGE_MAX  = "prnPageMax";
const char* PR_OBJ_MAX   = "prnObjMax";
const char* PR_TMP_FNAME = "FRTXXXXXX";
const char* PR_PRN_FNAME = "PRTXXXXXX";
char* CHAR_SET_DEFPATH  = "//conf/fontSet/fontElem";
#define CMARGIN 2
#define FILE_LENGTH 128
#define MARGIN_CONDITION 0.5

char PR_db_server[64];
char PR_db_user[34];
char PR_db_password[64];
char PR_db_database[64];
int  PR_db_port;

//デバグ用マクロ名（マクロ名あり=デバグモード、マクロ名なし=通常モード）
//#define DEBUG
//フォーマット(用紙サイズ)判定用マクロ名（マクロ名あり=SD定義より用紙サイズ判定、マクロ名なし=M_FORM、size指定に従う）
//#define FORMAT

HPDF_Doc  pdf;
//HPDF_Page PR_page[2048];
HPDF_Page PR_page;
HPDF_PageSizes PR_pagesize;         //ページのサイズ
HPDF_PageDirection PR_pagestyle;    //ページの縦横

//pdfの用紙設定(フォントの設定を含む)
char PR_Lineprint = 0;
float PR_fontsize = 0;                //pdfのフォントサイズ
float PR_linepitch = 0,PR_charpitch = 0;         //ページに対する文字配置を設定
float PR_topmargin = 0,PR_leftmargin = 0;        //余白の基準(上下,左右は同じ幅とする)
char PR_fontname[31];                       //pdfのフォント名を格納
const char* PR_font;                       //pdfのフォントを格納
//pdfのオブジェクトのプロパティ
int PR_currentLine = 0;                     //次に書き込みする行番号
//int PR_currentPage = 0;                     //現在書き込み中のページ番号
int PR_currentObj = 0;                     //現在書き込み中のファイル番号
int PR_maxPage     = 0;                         //ファイル番号の最大
int PR_maxObj      = 0;                         //ファイル番号の最大
char *PR_fileArray[100] = {0};        //ファイルポインタの配列最大ページ数はPR_maxObj*100になる

char PR_id[21];						//pdf設定のID
char PR_overlayfile[128] = "";		//書式オーバレイファイル(パス+ファイル)
//char PR_overlayfileid[128] = "";	//書式オーバレイファイルID(拡張子なし)
char PR_overlaysearch[128] = "";	//書式オーバレイID検索用文字列(テーブル：M_FORMを参照する)
char PR_overlaypdf[128] = "";		//書式オーバレイpdfファイルID

char PR_temppath[128] = "";        //作成されるpdfの名前
char PR_tempname[128] = "";        //作成されるpdfの名前
char PR_bindname[128] = "";         //結合後のpdfの名前
char PR_basepath[128] = "";                  //背景となるpdfまでのパス
char PR_basename[128] = "";                  //背景となるpdfの名前
char PR_printername[31] = "";
jmp_buf PR_env;    //エラー処理用

int gfuncid = 0;	//関数ID
int gtermid = 0;	//処理ID

char gcutbuf[351];	//文字列切り出し(getStr)で使用する変数
//char gstrup[2048];	//大文字→小文字(toLower)で使用する変数

//印字サイズ制御コード用変数
HPDF_REAL PR_lineheight = 0;		//行高
HPDF_REAL PR_horizontalScale = 0;	//水平割合
HPDF_Font PR_definition_font;		//フォント
int PR_ctrlCodeNoSave = 0;			//制御コード番号(前回判定用保存)(文字ピッチ用)（1:全角文字=1.5バイト、2:全角文字=2バイト）
int PR_ctrl2CodeNoSave = 0;			//制御コード番号(前回判定用保存)(横倍、縦倍、２倍指定用)（3:横2倍文字、4:縦2倍文字、5:2倍文字、6:横方向リセット、7:2倍文字リセット、8:縦方向リセット）
float PR_charsize = 0;				//半角文字の横幅ポイント値

#define OVERLAY_REC_LEN 72	//書式オーバレイファイルの最大レコード長
#define FDD_LEN 35			//定義FD使用領域長
#define SDD_LEN 29			//定義FD使用領域長
#define ITD_LEN 56			//定義FD使用領域長
#define CAPTION_LEN 8		//書見出し値の長(桁：49～56)
#define CONTINUE_LEN 46		//書見出し値の継続最大長(桁：11～56)
float ranhabaUnit = 0.72;	//欄幅単位(欄幅=1(=1/100インチ)のポイント数：1インチ=72ポイント)

float A3_Height = 16.54;	//A3長
float A3_Width = 11.69;		//A3幅
float B4_Height = 14.33;	//B4長
float B4_Width = 10.12;		//B4幅
float A4_Height = 11.69;	//A4長
float A4_Width = 8.27;		//A4幅

float divCharSpace = 10;	//CharSpace分割値
float rateBold = 1.25;		//太線率
float rateMed = 1.125;		//中線率
float rateThin = 1.0;		//細線率

//書式オーバレイファイルの内容(構造体定義)
//(定義FDコーディング用紙は51桁まで／実使用は35桁)
//(定義SDコーディング用紙は51桁まで／実使用は29桁)
//(定義ITコーディング用紙は72桁まで／実使用は56桁、見出し値の継続値を連結保存するので72+350桁確保しておく)
//(書式オーバレイ定義ファイルの行数が40行くらいなので100行用意しておく)
typedef struct overlay_data{
	char fdd[51];			//FD定義
	char sdd[51];			//SD定義
	char itd[423][100];		//IT定義
	int itd_cnt;			//IT定義件数
}OVERLAY_DATA;
#define ITD_LEN_MAX 422	//IT定義の最大長
#define GOVERLAY_DATA_MAX 100	//書式オーバレイファイルの定義ITの最大件数

//書式オーバレイファイルの内容(構造体定義)
//(見出し値は54桁(通常=8桁+継続=46))
//(書式オーバレイpdfファイルの内容は編集によって書式オーバレイ定義ファイルの行数より増えるので200行用意しておく)
typedef struct pdf_data{
	int no;					//連番
	int levelNo;			//レベル(番号)
	int beginRow;			//開始位置・行
	int beginCol;			//開始位置・桁
	int sizeRow;			//大きさ・行
	int sizeCol;			//大きさ・桁
	int lineStyl;			//線形　(1:縦線(=V)、2:横線(=H)、3:長方形(=B)、5:斜線(=O))
	int lineVar;			//線種　(1:太線、2:細線、3:太点線、4:細点線、9:太一点鎖線、10:細一点鎖線(=A)、11:中線(=O)、12:中点線(=P)、13:中一点鎖線(=S))
	int linePos;			//位置　(0:下側(=D)、1:中央(=C))
	int endRow;				//終了位置・行
	int endCol;				//終了位置・桁
	float point;			//ポイント　(0.8:80%文字(8=9.0P)、0.9:90%文字(9=9.6P)、1.5:150%文字(Y=14.4P)、2.0:200%文字(Z=216P)、1.0:100%文字(デフォルト、その他指定のとき))
	int zoom;				//拡大　(0:なし、1:長体(=T)、2:平体(=F))
	int ranhaba;			//欄幅　(文字左端から次の文字の左端までの大きさを指定、単位:1/100インチ)
	int interval;			//同一行単語間のバイト数(先頭の単語はゼロを設定)
	float spaceCharLen;		//文字間空白（単位=文字数(小数可)：見出し値、長方形付属用）
	char text[351];			//見出し値
}PDF_DATA;
int text_len = 350;			//見出し値の最大文字数

#define GPDF_DATA_MAX 500	//書式オーバレイpdfファイルの内容の最大件数
PDF_DATA gpdf_data[GPDF_DATA_MAX];	//書式オーバレイpdfファイルの内容(構造体実体)
int gpdf_data_cnt = 0;		//書式オーバレイpdfファイルの内容の件数

//IT定義・罫線のレベル内容(構造体定義)
//(配列要素の添え字 + 1 がレベル番号に対応する)
typedef struct level_data{
	int levelNo;			//レベル(番号)
	int lineStyl;			//線形　(1:縦線(=V)、2:横線(=H)、3:長方形(=B)、5:斜線(=O))
	int beginRow;			//開始位置・行
	int beginCol;			//開始位置・桁
	int sizeRow;			//大きさ・行
	int sizeCol;			//大きさ・桁
}LEVEL_DATA;
int level_data_len = 10;	//IT定義・罫線のレベル内容の最大件数

LEVEL_DATA glevel_data[9];	//IT定義・罫線のレベル内容(構造体実体)

//半角→全角　変換用
#define ISKANJI(c)       (c >= 0x81 && c <= 0x9f || c >=0xe0 && c <= 0xfc)	//漢字コード範囲
#define ISALPH(c)        (c >= 0x20 && c <= 0x7e)	//半角英数字コード範囲
#define ISKANA(c)        (c >= 0xa1 && c <= 0xdf)	//半角カナコード範囲
#define ISDAKU(c)        ((c & 0xff) == 0xde)	//半角カナ濁点コード
#define ISHANDAKU(c)     ((c & 0xff) == 0xdf)	//半角カナ半濁点コード
#define MAYBEDAKU(c)     (c >= 0xb6 && c <= 0xc4 || c >= 0xca && c <= 0xce)	//濁点が付く可能性のある半角カナコード範囲
#define MAYBEHANDAKU(c)  (c >= 0xca && c <= 0xce)	//半濁点が付く可能性のある半角カナコード範囲
unsigned char *ALPH[] = {"　", "！", "”", "＃", "＄", "％", "＆", "’", "（", "）", "＊", "＋", "，", "－", "．", "／", 
						"０", "１", "２", "３", "４", "５", "６", "７", "８", "９", 
						"：", "；", "＜", "＝", "＞", "？", "＠", 
						"Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ", "Ｆ", "Ｇ", "Ｈ", "Ｉ", "Ｊ", "Ｋ", "Ｌ", "Ｍ", "Ｎ", "Ｏ", "Ｐ", "Ｑ", "Ｒ", "Ｓ", "Ｔ", "Ｕ", "Ｖ", "Ｗ", "Ｘ", "Ｙ", "Ｚ", 
						"［", "￥", "］", "＾", "＿", "‘", 
						"ａ", "ｂ", "ｃ", "ｄ", "ｅ", "ｆ", "ｇ", "ｈ", "ｉ", "ｊ", "ｋ", "ｌ", "ｍ", "ｎ", "ｏ", "ｐ", "ｑ", "ｒ", "ｓ", "ｔ", "ｕ", "ｖ", "ｗ", "ｘ", "ｙ", "ｚ", 
						"｛", "｜", "｝", "～"};
//ソ=配列の31番目
unsigned char *KANA[] = {"。", "「", "」", "、", "・", 
						"ヲ", "ァ", "ィ", "ゥ", "ェ", "ォ", "ャ", "ュ", "ョ", "ッ", "ー", 
						"ア", "イ", "ウ", "エ", "オ", 
						"カ", "キ", "ク", "ケ", "コ", 
						"サ", "シ", "ス", "セ", "　", 
						"タ", "チ", "ツ", "テ", "ト", 
						"ナ", "ニ", "ヌ", "ネ", "ノ", 
						"ハ", "ヒ", "フ", "ヘ", "ホ", 
						"マ", "ミ", "ム", "メ", "モ", 
						"ヤ", "ユ", "ヨ", "ラ", "リ", 
						"ル", "レ", "ロ", "ワ", "ン", 
						"゛", "゜"};
unsigned char *DAKU[] = {"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", "　", "　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"ガ", "ギ", "グ", "ゲ", "ゴ", 
						"ザ", "ジ", "ズ", "ゼ", "ゾ", 
						"ダ", "ヂ", "ヅ", "デ", "ド", 
						"　", "　", "　", "　", "　", 
						"バ", "ビ", "ブ", "ベ", "ポ", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　"};
unsigned char *HDAK[] = {"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", "　", "　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"パ", "ピ", "プ", "ペ", "ポ", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　", "　", "　", "　", 
						"　", "　"};

HPDF_Point mcurpos;

//エラー処理
#ifdef HPDF_DLL
void  __stdcall
#else
void
#endif
error_handler(HPDF_STATUS error_no,HPDF_STATUS detail_no,void *user_data){
    fprintf(
    	stderr,
    	"  Error C [%02d-%02d]: error_no=%04X, detail_no=%u : %s\n", 
    	88, 88,
    	(HPDF_UINT)error_no,
    	(HPDF_UINT)detail_no,
    	map_source_func
    );
    longjmp(PR_env, 1);
}

static int (*func)(char *errno, const char *errmsg);
void mybacktrace(void);    //debug用関数

//==============================================================================
//【設定を取得して、書式オーバレイファイルを順次読み込み書式オーバレイpdfファイル作成用の構造体に値を編集】（01）
int PR_Initialize();
//【pdfを作成】（02）
int PR_Open();
//【書式オーバレイpdfファイル(構造体)を参照してオーバレイpdfファイルを出力する】（03）
int PR_Write();
//【pdfを出力する】（04）
int PR_Close();
//【ページの生成】（05）
int PR_NewPage();
//【pdfを書き込み、保存】（06）
int PR_Save();
//【xml設定ファイルの読み込み】（07）
int PR_conf_read();
//【db帳票データを読み込んで設定値を設定】（08）
int PR_setProperty();
//【書式オーバレイpdfファイルの初期作成】（09）
int PR_putInitoverlaypdf();
//【書式オーバレイファイルの読み込みと構造体への格納】（10）
int PR_getoverlaydata(OVERLAY_DATA *loverlay_data);
//【書式オーバレイ定義ファイルから作成した書式オーバレイファイルの内容構造体を順次参照して
//　書式オーバレイpdfファイルの内容構造体にデータを作成する】（11）
int PR_edtpdfData(OVERLAY_DATA *loverlay_data);
//【書式オーバレイpdfファイルの内容(構造体)に見出し値データを作成】（12）
int PR_edtpdfDataText(OVERLAY_DATA *loverlay_data, int pidx, int boxF, int *befRow, int *cnt);
//【書式オーバレイpdfファイルの内容構造体に罫線データを作成】（13）
int PR_edtpdfDataLine(OVERLAY_DATA *loverlay_data, int pidx, int *cnt);
//【書式オーバレイファイルの内容(構造体)の見出し値要素を開始位置・行、桁で並べ替え】（14）
void PR_sortoverlaydata(OVERLAY_DATA *loverlay_data);
//【書式オーバレイpdfファイルの内容構造体に罫線データを作成(レベル=01、長方形の繰り返し用)】（15）
int PR_edtpdfDataLineLoop(OVERLAY_DATA *loverlay_data, int pidx, int *cnt);
//【引数文字列(書式オーバレイID)からファイル名、ID検索用文字列、pdf名を編集】（16）
int PR_edtFileID(char *parastr);

//【フォント等、用紙指定】（51）
int fontSetting();
//【設定ファイルのパスを取得】（52）
char *getConfFilename(char *strConfPath);
//【フォントファイルのパス＋ファイル名を取得】（53）
char *getFontFilePath(char *fontname,char *filePath);
//【ファイルの有無を確認】（54）
int chkFileExist(char *chkFile);
//【現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す】（56）
char *local_server_time(char *retStr);
//【半角→全角変換】（57）
void cnvHanToZen(const char *han, char *zen);
//【左上基点(ポイント)、桁位置、文字幅(ポイント)から桁位置のポイント値を求める】（58）
HPDF_REAL getColpos(HPDF_Point basPos, int Col, HPDF_REAL charWidth);
//【左上基点(ポイント)、行位置、文字幅(ポイント)から行位置のポイント値を求める】（59）
HPDF_REAL getRowpos(HPDF_Point basPos, int Row, HPDF_REAL lineHeight, int drawline, int centerline);
//【文字列の指定位置から指定長部分を取り出す】（60）
char *getStr(char *sbuf, int pos, int getlen);
//【文字列の指定位置から指定長部分を取り出して数値化(int型のみ)】（61）
int getNum(char *sbuf, int pos, int len);
//【文字列の文字数を数える(全角文字も１文字)】（62）
int getCharCnt(char *sbuf);
//【書式オーバレイpdfファイルの内容(構造体実体)件数加算】（63）
int addpdf_dataCnt(int *cnt);
//【書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素を初期化】（64）
void clrpdf_data(int idx);
//【IT定義・罫線のレベル内容(構造体実体)を初期化】（65）
void clrlevel_data();
//【見出し値の編集（縦書き・横書き）】（66）
int edtpdfText(OVERLAY_DATA *loverlay_data, int pidx, int *cnt, char *buf, int *retRow);
//【ポイント・拡大によるデフォルト文字高さをこえる場合に１行加算】（67）
void sortoverlaydataRowSet(char *s_itd, int *Row);
//【書式オーバレイpdfファイルの内容構造体の行、桁を調整する】（68）
int setpdfDataColRow(int *cnt);
//【レベル指定配下のIT指定が表形式かチェック】（69）
int chkHyo(int lineStyl, int sizeRow, int sizeCol, int yrep, int xrep);
//【文字列右側の空白文字を削除】（70）
void RTrim(char *str);
//【文字列の大文字を小文字に変換】（71）
char *strToLower(char *str);
//==============================================================================

//【creOverlay入り口】
int main(int argc, char *argv[]){
	int ret = 0;
	char strTime[] = "00/00/00 00:00:00";	//時間の文字列を格納

	//引数の数を確認(書式オーバレイID)
	if (argc < 2){
		cob_runtime_error( " Error C [%02d-%02d] %s : Option is nothing ", 00, 01, local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//引数の長さを確認(14バイト以内であること)
	if (strlen(argv[1]) > 14){
		cob_runtime_error( " Error C [%02d-%02d] %s : Option is too long", 00, 02, local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//引数文字列(書式オーバレイID)からファイル名、ID検索用文字列、pdf名を取得
	PR_edtFileID(argv[1]);

	//書式オーバレイファイルの存在チェック
	ret = chkFileExist(PR_overlayfile);
	if (ret != 0){
		cob_runtime_error(" Error C [%02d-%02d] %s : Overlay File is not exist[%s]", 00, 02, local_server_time(strTime), PR_overlayfile);
		exit(EXIT_FAILURE);
	}

	printf(">> Overlay PDF maker program start \n");

	//書式オーバレイpdfファイル作成の初期処理
	ret = PR_Initialize();
#ifdef DEBUG
printf(">> main     PR_Initialize ret : %d\n" , ret);
#endif
	if (ret != 0){
		exit(EXIT_FAILURE);
	}
#ifdef DEBUG
printf(">> main     PR_basename : %s\n" , PR_basename);
#endif

	//書式オーバレイpdfファイルを開く(pdfファイルとして作成)
	ret = PR_Open();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	//書式オーバレイpdfファイルの内容構造体から書式オーバレイpdfへ見出し値・罫線を出力する
	ret = PR_Write();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	//書式オーバレイpdfファイルを閉じる(編集したpdfファイルを書き込み)
	ret = PR_Close();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	cob_runtime_error(">> Overlay PDF maker program stop (status : %d) ", ret);

	return ret;
}

//【設定を取得して、書式オーバレイファイルを順次読み込み書式オーバレイpdfファイル作成用の構造体に値を編集】
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_Initialize(){
	float pHeight = 0;		//用紙長
	float pWidth = 0;		//用紙幅
	float pcmp = 0;
	int ret = 0;
	
	OVERLAY_DATA loverlay_data;		//書式オーバレイファイルの内容(構造体実体)
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Initialize :");
	gfuncid = 1;
	gtermid = 0;
	
	//xml設定の読み込み(グローバル変数へ格納)
	ret = PR_conf_read();
	if (ret != 0){
		return ret;
	}
	
	//db(M_FORMテーブル)から帳票データを取得
	ret = PR_setProperty();
	if (ret != 0){
		return ret;
	}

	//通常の横幅(拡大率)
	PR_horizontalScale = HPDF_Page_GetHorizontalScalling(PR_page);
	
	//書式オーバレイpdfファイルを初期作成
	ret = PR_putInitoverlaypdf();
	if (ret != 0){
		return ret;
	}

	//書式オーバレイファイルを読み込んで構造体へ格納
	ret = PR_getoverlaydata(&loverlay_data);
	if (ret != 0){
		return ret;
	}

	//書式オーバレイファイルの内容(構造体)の見出し値要素を開始位置・行、桁をキーとして昇順に並べ替え
	PR_sortoverlaydata(&loverlay_data);

	//SD定義から用紙長・幅を取得して、その値からページサイズ・方向を決定
	pHeight = (float)getNum(loverlay_data.sdd, 16, 3) / 10;
	pWidth = (float)getNum(loverlay_data.sdd, 19, 3) / 10;
#ifdef DEBUG
printf(">> PR_Initialize  pHeight : %f\n" , pHeight);
printf(">> PR_Initialize  pWidth  : %f\n" , pWidth);
#endif
#ifdef FORMAT
//M_FORMで取得した用紙設定を採用～SD指定による設定をしない(デバグ時)
	//(用紙長・幅からページ方向を決定)
	if (pHeight > pWidth){
		//(用紙・縦)
		PR_pagestyle = HPDF_PAGE_PORTRAIT;
	}else{
		//(用紙・横)
		PR_pagestyle = HPDF_PAGE_LANDSCAPE;
		//(ページサイズ決定のため縦横値を入れ替え)
		pcmp = pHeight;
		pHeight = pWidth;
		pWidth = pcmp;
	}
	//(用紙長・幅からページサイズを決定)
	//(A4)
	if (pHeight <= A4_Height && pWidth <= A4_Width){
		PR_pagesize = HPDF_PAGE_SIZE_A4;
	//(B4)
	}else if (pHeight <= B4_Height && pWidth <= B4_Width){
		PR_pagesize = HPDF_PAGE_SIZE_B4;
	//(B4以上ならA3とする)
	}else{
		PR_pagesize = HPDF_PAGE_SIZE_A3;
//#ifdef DEBUG
//A3サイズでもA4にする(デバグ時)
//PR_pagesize = HPDF_PAGE_SIZE_A4;
//#endif
	}
#endif

	//書式オーバレイファイルの内容を格納した構造体からオーバレイpdfファイル作成用の書式オーバレイpdfファイル構造体を編集
	ret = PR_edtpdfData(&loverlay_data);
	if (ret != 0){
		return ret;
	}
#ifdef DEBUG
int i = 0;
for (i=0; i < gpdf_data_cnt; i++){
printf(">> gpdf_data[%d] no:%d/bRow:%d/bCol:%d/sizeRow:%d/sizeCol:%d \n",
	   i, gpdf_data[i].no, gpdf_data[i].beginRow, gpdf_data[i].beginCol,
	   gpdf_data[i].sizeRow, gpdf_data[i].sizeCol);
printf(">>               lineStyl:%d/lineVar:%d/eRow:%d/eCol:%d/zoom:%d/ranhaba%d \n",
	   gpdf_data[i].lineStyl, gpdf_data[i].lineVar, 
	   gpdf_data[i].endRow, gpdf_data[i].endCol, gpdf_data[i].zoom, gpdf_data[i].ranhaba);
printf(">>               interval:%d/text:[%s] \n", gpdf_data[i].interval, gpdf_data[i].text);
}
#endif

	return ret;
}

//【pdfを作成】
//　(返値)(0=正常、-1=エラー時のpdf実体破棄エラー、-2=pdf生成エラー、以外=他の関数からの返値)
int PR_Open(){
	char fontFilePath[256];
	const char *fontPath = fontFilePath;
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Open :");
	gfuncid = 2;
	gtermid = 0;
	
	//pdfの実体を生成
	pdf = HPDF_New(error_handler, NULL);
	if (setjmp(PR_env)) {
		//エラーならpdfの実体を破棄
		gtermid = 1;
		HPDF_Free(pdf);
		cob_runtime_error(" Error C [%02d-%02d]: pdf free ", gfuncid, gtermid);
		return 1;
	}
	
	//日本語エンコードを許可
	HPDF_UseJPEncodings(pdf);
	//エンコードを設定
	HPDF_SetCurrentEncoder(pdf,PR_DEFAULT_ENCODE);  
	//日本語が使えるフォントを追加
	HPDF_UseJPFonts(pdf);
	getFontFilePath(PR_fontname,fontFilePath);
	PR_font = HPDF_LoadTTFontFromFile(pdf, fontPath, HPDF_TRUE);
	
	if (!pdf) {
		//pdf実体の生成失敗
		gtermid = 2;
		cob_runtime_error(" Error C [%02d-%02d]: cannot create PdfDoc object", gfuncid, gtermid);
		return 1;
	}
	
	//ページを生成
	ret = PR_NewPage();
	if (ret != 0){
		return ret;
	}
	
	return ret;
}

//【書式オーバレイpdfファイル(構造体)を参照してオーバレイpdfファイルを出力する】
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_Write(){
	HPDF_UINT16 DASH_MODE1[] = {1};				//点線指定で使用
	HPDF_UINT16 DASH_MODE2[] = {3, 7};
	HPDF_UINT16 DASH_MODE3[] = {8, 7, 2, 7};	//一点鎖線指定で使用
	HPDF_Font definition_font;          //pdfのフォント
	HPDF_Point basPos;			//初期位置
	HPDF_Point curPos;			//(見出し値出力後の)現在位置
	HPDF_REAL charWidth = 0;	//文字幅
	HPDF_REAL rcharWidth = 0;	//文字幅(欄幅算出用)
	HPDF_REAL lineHeight = 0;	//改行値(×２→行高)
	HPDF_REAL Col = 0;
	HPDF_REAL befCol = 0;
	HPDF_REAL Row = 0;
	HPDF_REAL cmpRow = 0;
	HPDF_REAL rWidth = 0;
	HPDF_REAL rHeight = 0;
	HPDF_REAL wx = 0;
	HPDF_REAL wy = 0;
	float ranhaba = 0;
	float saCol = 0;			//出力桁位置の調整用差分(単位：ポイント)
	int idx = 0;
	int ret = 0;
	HPDF_STATUS retCode;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Write :");
	gfuncid = 3;
	gtermid = 0;

	//行の高さを格納(印字行と行送りの構成になっているので、実際の行高は２倍)
	lineHeight = HPDF_Page_GetTextLeading(PR_page);
	//文字幅を格納
	charWidth = HPDF_Page_TextWidth(PR_page, " ");
#ifdef DEBUG
//printf(">> PR_Write charWidth:%f/lineHeight:%f\n", charWidth, lineHeight);
#endif

	//初期位置を格納
	basPos = HPDF_Page_GetCurrentTextPos(PR_page);
#ifdef DEBUG
printf(">> PR_Write (fst)basX:%f/basY:%f\n", basPos.x, basPos.y);
#endif

	//《見出し値》
	//書式オーバレイpdfファイルの内容(構造体)を順次参照
	cmpRow = 0;
	befCol = 0;
	for (idx = 0; idx < gpdf_data_cnt; idx++){
		//見出し値・値がないものは除く
		if (strlen(gpdf_data[idx].text) > 0){
			//行が変わったら全項目の出力位置(befCol)をクリア
			if (gpdf_data[idx].beginRow != cmpRow){
				befCol = 0;
				cmpRow = gpdf_data[idx].beginRow;
			}
			//指定された位置に見出し値を出力
			//桁位置(ポイント)を算出
			Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
			if (Col < befCol){
				//(桁位置(ポイント)が前出力の単語後位置にかかる場合元の単語の間隔分後ろへ出力)
				if (gpdf_data[idx].interval > 0){
					Col = befCol + (float)gpdf_data[idx].interval * charWidth;
				}
			}
			//行位置(ポイント)を算出
			Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 0, 0);
#ifdef DEBUG
printf(">> PR_Write gpdf_data befCol:%f\n", befCol);
printf(">> PR_Write gpdf_data beginXY[%d] beginCol:%d/beginRow:%d/Col:%f/Row:%f\n", idx, gpdf_data[idx].beginCol, gpdf_data[idx].beginRow, Col, Row);
printf(">> PR_Write gpdf_data text[%d] text : [%s]\n", idx, gpdf_data[idx].text);
#endif
			//ポイント(文字のフォントを変更して実施)設定
			definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
			retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * gpdf_data[idx].point);
			//拡大設定
			if (gpdf_data[idx].zoom == 0){
				//拡大なし
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 1);
				//拡大指定がないときは出力桁位置の差分なし
				saCol = 0;
			}else if  (gpdf_data[idx].zoom == 1){
				//長体(文字フォントを２倍して水平割合を1/2する)
				definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
				retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * 2);
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 0.5);
				//出力桁位置はSetHorizontalScalling指定による伸縮が加味されない
				//そのためフォントを２倍にしているので出力桁位置の差分に元の文字列分のマイナスを設定
				saCol = strlen(gpdf_data[idx].text) * charWidth * -1;
			}else if  (gpdf_data[idx].zoom == 2){
				//平体(文字フォントを1/2して水平割合を２倍する)
				definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
				retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * gpdf_data[idx].point * 0.5);
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 2);
				//出力桁位置はSetHorizontalScalling指定による伸縮が加味されない
				//そのためフォントを1/2にしているので出力桁位置の差分に元の文字列分1/2のプラスを設定
				saCol = strlen(gpdf_data[idx].text) / 2 * charWidth;
			}
			//文字間隔を設定
			if (gpdf_data[idx].ranhaba > 0){
				//欄幅指定のとき
				//(欄幅指定対象の見出し値のサイズを求める～全角か半角か判定)
				unsigned char c1 = gpdf_data[idx].text[0];
				if (ISKANJI(c1)){
					//全角
					rcharWidth = charWidth * 2;
				}else{
					//半角
					rcharWidth = charWidth;
				}
				//ポイント(文字サイズ)指定値を反映
				rcharWidth *= gpdf_data[idx].point;
				//欄幅のサイズを求める
				ranhaba = (float)gpdf_data[idx].ranhaba * ranhabaUnit;
				if ((ranhaba - rcharWidth) > 0){
					//文字間隔値が正のとき
					retCode = HPDF_Page_SetCharSpace(PR_page, (ranhaba - rcharWidth));
					//SetCharSpace指定で文字間の空白を設定すると末尾文字の後にも文字間空白が取られる
					//そのため文字間空白分を出力桁位置の差分にマイナス設定
					saCol = (ranhaba - rcharWidth) * -1;
				}else{
					//文字間隔値が負のとき
					retCode = HPDF_Page_SetCharSpace(PR_page, 0);
				}
			}else if (gpdf_data[idx].spaceCharLen > 0){
				//見出し値、長方形が付属の桁一様分散指定のとき
				retCode = HPDF_Page_SetCharSpace(PR_page, (charWidth * gpdf_data[idx].spaceCharLen));
			}else{
				//指定なし
				retCode = HPDF_Page_SetCharSpace(PR_page, 0);
			}
			//見出し値を出力
			retCode = HPDF_Page_TextOut(PR_page, Col, Row, gpdf_data[idx].text);

			//初期位置に戻す(x, y片方ずつ戻さないとうまく戻らない)
			//(テキストを出力した現在位置を取得)
			curPos = HPDF_Page_GetCurrentTextPos(PR_page);
			//(桁位置を保存／文字ポイント変更等対応～先の見出し出力から元の単語間隔分後ろに出力する)
			befCol = curPos.x;
			befCol += saCol;	//出力桁位置の差分を加算して出力桁位置を調整
#ifdef DEBUG
//retCode = HPDF_Page_TextOut(PR_page, befCol, curPos.y, "■");
#endif
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write curX/curY(   ) : %f / %f\n", curPos.x, curPos.y);
#endif
			//(y軸を元に戻す)
			wx = 0;
			wy = basPos.y - curPos.y;
#ifdef DEBUG
//printf("PR_Write wx/wy(   )     : %f / %f\n", wx, wy);
#endif
			retCode = HPDF_Page_MoveTextPos(PR_page, wx, wy);
			//(テキストを出力した現在位置を取得)
			curPos = HPDF_Page_GetCurrentTextPos(PR_page);
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write curX/curY(n  ) : %f / %f\n", curPos.x, curPos.y);
#endif
			//(x軸を元に戻す)
			wx = basPos.x - curPos.x;
			wy = 0;
#ifdef DEBUG
//printf(">> PR_Write wx/wy(n  )     : %f / %f\n", wx, wy);
#endif
			retCode = HPDF_Page_MoveTextPos(PR_page, wx, wy);
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write basX/basY(re ) : %f / %f\n", curPos.x, curPos.y);
#endif
		}
	}

#ifdef DEBUG
printf(">> PR_Write before line output : \n");
#endif

	//描画モードに移行
	HPDF_Page_EndText(PR_page);

	//《罫線》
	//書式オーバレイpdfファイルの内容の構造体を順次参照
	for (idx = 0; idx < gpdf_data_cnt; idx++){
		if (gpdf_data[idx].lineStyl != 0){
			//線種を設定
			if (gpdf_data[idx].lineVar == 1){
				//(太線)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 2){
				//(細線)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 3){
				//(太点線)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 4){
				//(細点線)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 9){
				//(太一点鎖線)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 10){
				//(細一点鎖線)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 11){
				//(中線)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else if (gpdf_data[idx].lineVar == 12){
				//(中点線)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else if (gpdf_data[idx].lineVar == 13){
				//(中一点鎖線)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else{
				//(規定以外→細線)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}
			if (gpdf_data[idx].lineStyl == 1){
				//縦線
				//開始桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//行位置(ポイント)を算出
				Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				//縦線はカラムの上部から引かれるので１行分上へ
				Row += (lineHeight * 2);
				//開始桁位置を設定
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//終了桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//行位置(ポイント)を算出
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				//縦線はカラムの上部から引かれるので１行分上へ
				Row += (lineHeight * 2);
				HPDF_Page_LineTo(PR_page, Col, Row);
			}else if (gpdf_data[idx].lineStyl == 2){
				//横線
				//開始桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//行位置(ポイント)を算出
				if (gpdf_data[idx].linePos == 1){
					//位置が中央指定
					Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 1);
				}else{
					//位置が中央指定でない
					Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				}
				//開始桁位置を設定
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//終了桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//行位置(ポイント)を算出
				//(微調整：1/2行分上へ)
				//行位置(ポイント)を算出
				if (gpdf_data[idx].linePos == 1){
					//位置が中央指定
					Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 1);
				}else{
					//位置が中央指定でない
					Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				}
				HPDF_Page_LineTo(PR_page, Col, Row);
			}else if (gpdf_data[idx].lineStyl == 3){
				//長方形
				//終了桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//行位置(ポイント)を算出
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				//長方形はカラムの上部から引かれるので１行分上へ
				Row += (lineHeight * 2);
				//長方形の幅を算出
				rWidth = (float)gpdf_data[idx].sizeCol * charWidth;
				//長方形の高さを算出
				rHeight = (float)gpdf_data[idx].sizeRow * (lineHeight * 2);
				HPDF_Page_Rectangle(PR_page, Col, Row, rWidth, rHeight);
			}else if (gpdf_data[idx].lineStyl == 5){
				//斜線
				//開始桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//行位置(ポイント)を算出
				Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				//開始桁位置を設定
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//終了桁位置(ポイント)を算出
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//行位置(ポイント)を算出
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				HPDF_Page_LineTo(PR_page, Col, Row);
			}
			HPDF_Page_Stroke(PR_page);
		}
	}
	ret = HPDF_Page_BeginText(PR_page);

	return ret;
}

//【pdfを出力する】
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_Close(){
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Close :");
	gfuncid = 4;
	gtermid = 0;

	ret = PR_Save();
	if (ret == 1){
		return 1;
	}
	
	return ret;
}

//【ページの生成】
//　(返値)(0=正常)
int PR_NewPage(){
	HPDF_STATUS retCode;
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_NewPage :");
	gfuncid = 5;
	gtermid = 0;

	//pageオブジェクトの作成
	PR_page = HPDF_AddPage(pdf);
	
	//ページ設定(initializeのxml設定から取得または書式オーバレイファイルのSD定義から判定したもの)
	retCode = HPDF_Page_SetSize(PR_page, PR_pagesize, PR_pagestyle);
	
	//フォント設定だけを分割
	retCode = fontSetting();
	
	retCode = HPDF_Page_BeginText(PR_page);
	retCode = HPDF_Page_MoveTextPos(PR_page, PR_leftmargin, HPDF_Page_GetHeight(PR_page) - PR_topmargin);
				mcurpos = HPDF_Page_GetCurrentTextPos(PR_page);
	retCode = HPDF_Page_ShowText(PR_page, "");
	
	//次に書き込む行をセット
	PR_currentLine = 1;

	return ret;
}

//【pdfを書き込み、保存】
//　(返値)(0=正常、-1=保存エラー)
int PR_Save(){
	HPDF_STATUS temp;
	char cmd[256]="";
	struct stat st;
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Save :");
	gfuncid = 6;
	gtermid = 0;

	if (strcmp(PR_id, "999") != 0){
		//指定の書式オーバレイ設定が存在したとき
		HPDF_Page_EndText(PR_page);
		temp = HPDF_SaveToFile(pdf, PR_overlaypdf);

		//正常に保存できなければ終了
		if (temp != HPDF_OK){
			return 1;
		}

		/* clean up */
		HPDF_Free(pdf);
	}else{
		//指定の書式オーバレイ設定が存在しなかったとき(標準(id=999)を使用する)
		//tempファイル名を生成
		strcpy(PR_tempname, PR_temppath);
		strcat(PR_tempname, PR_TMP_FNAME);

		//tempファイルの生成
		if (strstr(PR_tempname, "XXXXXX") != NULL){
			if(mkstemp(PR_tempname) == -1){
				gtermid = 1;
				cob_runtime_error(" Error C [%02d-%02d]: don't create temp File(%s) ", gfuncid, gtermid, PR_tempname);
				ret = 1;
			}else{
				ret = chmod(PR_tempname, 
				            S_IRUSR | S_IWUSR | S_IXUSR |	//rwx
				            S_IRGRP | S_IWGRP | S_IXGRP |	//rwx
				            S_IROTH | S_IWOTH | S_IXOTH		//rwx
				            );
				if (ret != 0){
					//作成したtempファイルを削除
					remove(PR_tempname);
					
					gtermid = 2;
					cob_runtime_error(" Error C [%02d-%02d]: cannot authorize to temp File(%s) ", gfuncid, gtermid, PR_tempname);
					ret = -1;
				}
			}
		}
		if (ret != 0){
			return ret;
		}

		//生成したpdf内容をtempファイルに出力
		HPDF_Page_EndText(PR_page);
		temp = HPDF_SaveToFile(pdf, PR_tempname);

		//正常に保存できなければ終了
		if (temp != HPDF_OK){
			return 1;
		}

		/* clean up */
		HPDF_Free(pdf);

		//生成したpdfファイル(tempファイル)に背景となるpdfを結合して書式オーバレイpdfファイルに出力(pdftkコマンドを発行)
		sprintf(cmd, "pdftk %s background %s output %s", PR_tempname, PR_basename, PR_overlaypdf);
		ret = system(cmd);
		if (ret != 0){
			gtermid = 3;
			cob_runtime_error(" Error C [%02d-%02d]: cannot join Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
			return 1;
		}

		//tempファイルを削除する
		ret = stat(PR_tempname, &st);
		if(ret == 0){
			sprintf(cmd, "rm %s ", PR_tempname);
			ret = system(cmd);
			if (ret != 0){
				gtermid = 4;
				cob_runtime_error(" Error C [%02d-%02d]: cannot delete temp File(%s) ", gfuncid, gtermid, PR_tempname);
				return 1;
			}
		}
	}

	return 0;
}

//【xml設定ファイルの読み込み】
int PR_conf_read(){
	int i;
	char strConfPath[1024];
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_conf_read :");
	gfuncid = 7;
	gtermid = 0;

	//ファイルネームを元にリーダポインタを作成
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//リーダをリードできる状態に
	xmlTextReaderRead(reader);
	//現在のノードのポインタをセット？
	xmlTextReaderExpand(reader);
	//現在のノードからDOMを取り出している?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return 1;
	//ドキュメントからコンテキスト()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
    if (!ctx) return 1;
	//xpathで指定したノードリストを取得
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
	if (!xpobj) return 1;
	//ノードリストをノードの配列のようなものに
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//ノード数の取得(取得できないなら0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//ノードリストから値を表示
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->content) {
				//設定ファイルからDBのホスト名を取得
				if (strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(PR_db_server,node->content);
#ifdef DEBUG
printf(">>           dbHost : %s\n" , PR_db_server);
#endif
				}
				//設定ファイルからDBのユーザ名を取得
				if (strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(PR_db_user,node->content);
				}
				//設定ファイルからDBのパスワードを取得
				if (strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(PR_db_password,node->content);
				}
				//設定ファイルからDB名を取得
				if (strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(PR_db_database,node->content);
				}
				//設定ファイルから背景のパスを取得
				if (strcmp(node->parent->name,PR_BASE_PATH) == 0){
					//後で後ろにファイル名をつなぐ
					strcpy(PR_basepath,node->content);
				}
				if (strcmp(node->parent->name,CONF_DB_PORT) == 0){
					PR_db_port = atoi(node->content);
				}
				//設定ファイルからTEMPディレクトリのパスを取得
				if (strcmp(node->parent->name,PR_TEMP_PATH) == 0){
					//後で後ろにファイル名をつなぐ
					strcpy(PR_temppath,node->content);
					strcpy(PR_bindname,node->content);
				}
				//設定ファイルからファイル数の最大を取得
				if (strcmp(node->parent->name,PR_OBJ_MAX) == 0){
					//後で後ろにファイル名をつなぐ
					PR_maxObj = atoi(node->content);
				}
				//設定ファイルからページの最大を取得
				if (strcmp(node->parent->name,PR_PAGE_MAX) == 0){
					//後で後ろにファイル名をつなぐ
					PR_maxPage = atoi(node->content);

				}
			} else {
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				return 1;
			}
		}
	}
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	if (PR_maxPage == 0){
		gtermid = 1;
		cob_runtime_error( " Error C [%02d-%02d] : Pagemax value has not been set", gfuncid, gtermid);
		return 1;
	}
	return 0;
}

//【db帳票データを読み込んで設定値を設定】
//　(返値)(0=正常、1=DB接続エラー、-1=query発行エラー、-2=pdf情報取得不可)
int PR_setProperty(){
	MYSQL PR_conn, *PR_mysql = &PR_conn;
	MYSQL_RES *res;
	MYSQL_ROW row;
	char query[512] = "";	//初期設定を読み込むSQLの格納
	char temp[256] = "";	//sql用temporary
	
	//関数名を共有変数にセット
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_setProperty : searchID %s ", PR_overlaysearch);
	gfuncid = 8;
	gtermid = 0;

	//DB接続
	if ((PR_mysql = mysql_init(&PR_conn)) == NULL) {
		cob_runtime_error( "%s", mysql_error(PR_mysql));
		return 1;
	}
//	mybacktrace();
	
	//db接続
	gtermid = 1;
	if (!mysql_real_connect(&PR_conn, PR_db_server, PR_db_user, PR_db_password, PR_db_database, PR_db_port, NULL, 0)) {  
		cob_runtime_error( " Error C [%02d-%02d]:db_connect server :%s user:%s pass:%s database:%s ", gfuncid, gtermid, PR_db_server,PR_db_user,PR_db_password,PR_db_database);
		return 1;
	}

	//↓項目と行添え字の対比一覧
	// size=row[0]
	// page_style=row[1]
	// font=row[2]
	// font_size=row[3]
	// base_pdf=row[4]
	// line_pitch=row[5]
	// char_pitch=row[6]
	// top_margin=row[7]
	// left_margin=row[8]
	// id=row[9]
	strcat(temp," SELECT mf.size, mf.page_style, mf.font ");
	strcat(temp," , mf.font_size, mf.base_pdf, mf.line_pitch,mf.char_pitch ");
	strcat(temp," , mf.top_margin, mf.left_margin, mf.id ");
	strcat(temp," FROM M_FORM mf ");
	strcat(temp," WHERE id like '%s' or ");
//	strcat(temp," WHERE id = '999-%s' or ");	//'999-overlayfileid'の形式のみ可とする
	strcat(temp,"       id = '999' ");
	strcat(temp," ORDER BY id DESC");
	sprintf(query, temp, PR_overlaysearch);
#ifdef DEBUG
printf(">> PR_setProperty     query : %s\n" , query);
#endif
	
//	printf("%s %s \n",query,PR_db_database);
//	printf("%s \n",query);
	//DB帳票データを抽出
	if (mysql_query(PR_mysql, query)!=0) {
		fprintf(stderr, "%s\n", mysql_error(PR_mysql));
		return 1;
	}
	
	res = mysql_store_result(PR_mysql);
	
	//書式オーバレイファイルIDを含むidがあればそちらから、なければ標準(id='999')から取得
	while (row = mysql_fetch_row(res) ){ 
		//ページサイズの取得
		if (strcmp(row[0],"A4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A4;
		}else if (strcmp(row[0],"A5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A5;
		}else if (strcmp(row[0],"B4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B4;
		}else if (strcmp(row[0],"B5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B5;
		}
#ifdef DEBUG
printf(">> PR_setProperty pagesize : %s\n" , row[0]);
#endif
		
		//ページのスタイルを取得(縦横)
		if (strcmp(row[1],"HPDF_PAGE_LANDSCAPE") == 0){
			//横向き
			PR_pagestyle=HPDF_PAGE_LANDSCAPE;
		}else if (strcmp(row[1],"HPDF_PAGE_PORTRAIT") == 0){
			//縦向き
			PR_pagestyle=HPDF_PAGE_PORTRAIT;
		}
		
		//取得していた設定をクリア
		memset(PR_fontname,'\0',strlen(PR_fontname));
		//フォント名を格納
		strcpy(PR_fontname,row[2]);
		//文字列→数字でフォントサイズを格納
		PR_fontsize = atof(row[3]);
		
		//取得していた設定をクリア
		memset(PR_basename,'\0',strlen(PR_basename));
		//背景となるpdfのパスと名前を格納
		strcat(PR_basename,PR_basepath);
		strcat(PR_basename,row[4]);
		//ページの行数を格納
		PR_linepitch = atof(row[5]);
		//行の文字数を格納
		PR_charpitch = atof(row[6]);
#ifdef DEBUG
printf(">> PR_setProperty (string)PR_linepitch/PR_charpitch(string) : %s / %s\n" , row[5], row[6]);
printf(">> PR_setProperty  (float)PR_linepitch/PR_charpitch : %f / %f\n" , atof(row[5]), atof(row[6]));
printf(">> PR_setProperty         PR_linepitch/PR_charpitch : %f / %f\n" , PR_linepitch, PR_charpitch);
#endif
		
		//上下の余白を格納
		PR_topmargin = atof(row[7]);
		//左右の余白を格納
		PR_leftmargin = atof(row[8]);
		//IDを格納
		memset(PR_id, '\0', sizeof(PR_id));
		strcpy(PR_id, row[9]);
		
		break;
	}
	mysql_free_result(res);
	mysql_close(PR_mysql);
	
	//失敗したらエラーを返す
	if (strlen(PR_basename) == 0){
		gtermid = 2;
		cob_runtime_error( " Error C [%02d-%02d]: cannot read database for PDF ", gfuncid, gtermid);
		return 1;
	}
	
	return 0;
}

//【書式オーバレイpdfファイルの初期作成】
//　(返値)(0=正常に作成、-1=ファイル作成失敗、-2=権限設定失敗)
int PR_putInitoverlaypdf(){
	FILE *fp;
	char cmd[256]="";
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_putInitoverlaypdf :");
	gfuncid = 9;
	gtermid = 0;

	//書式オーバレイpdfファイルを空で作成(touchコマンドを発行)
	sprintf(cmd, "touch %s ", PR_overlaypdf);
	ret = system(cmd);
	if (ret != 0){
		gtermid = 1;
		cob_runtime_error( " Error C [%02d-%02d]: cannot create Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
		return 1;
	}
	
	//作成した書式オーバレイpdfファイルに権限を設定
	//(権限指定=rw-rw-rw-)
	ret = chmod(PR_overlaypdf, 
	            S_IRUSR | S_IWUSR |		//rw-
	            S_IRGRP | S_IWGRP |		//rw-
	            S_IROTH | S_IWOTH		//rw-
	            );
	if (ret != 0){
		//作成した書式オーバレイpdfファイルを削除
		remove(PR_overlaypdf);
		
		gtermid = 2;
		cob_runtime_error( " Error C [%02d-%02d]: cannot authorize to Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
		return 1;
	}
	
	return 0;
}

//【書式オーバレイファイルの読み込みと構造体への格納】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　(返値)(0=正常、-1=書式オーバレイファイルが開けない)
int PR_getoverlaydata(OVERLAY_DATA *loverlay_data){
	FILE *fp;
	char sbuf[OVERLAY_REC_LEN - 1];
	char *cbuf;		//見出し値文字列
	int cap_len = 0;
	int idx = 0;
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_getoverlaydata :");
	gfuncid = 10;
	gtermid = 0;
	
	//書式オーバレイファイルを読み込み
	fp = fopen(PR_overlayfile, "r");
	if (fp != NULL){
		idx = 0;
		while (fgets(sbuf, OVERLAY_REC_LEN, fp) != NULL){
			//改行コードを削除
			if (strchr(sbuf,'\r') != NULL ){
				*strchr(sbuf,'\r') = '\0';
			}else if(strchr(sbuf,'\n') != NULL ){
				*strchr(sbuf,'\n') = '\0';
			}
#ifdef DEBUG
//printf(">> PR_getoverlaydata     sbuf : %s\n" , sbuf);
#endif
			if (strcmp(getStr(sbuf, 6, 2), "FD") == 0){
				//FD定義
				strcpy(loverlay_data->fdd, getStr(sbuf, 1, FDD_LEN));
			}else if (strcmp(getStr(sbuf, 6, 2), "SD") == 0){
				//SD定義
				strcpy(loverlay_data->sdd, getStr(sbuf, 1, SDD_LEN));
			}else if (strcmp(getStr(sbuf, 6, 2), "IT") == 0 || strcmp(getStr(sbuf, 6, 2), "  ") == 0){
				//IT定義(空白は省略なのでITとみなす)
				if (strcmp(getStr(sbuf, 10, 1), "-") == 0){
					//見出し値の継続
					cbuf = getStr(sbuf, 11, CONTINUE_LEN);
					if (cap_len + strlen(cbuf) > text_len){
						gtermid = 1;
						cob_runtime_error( " Error C [%02d-%02d]: Caption length over length=%s (%s)", gfuncid, gtermid, text_len, sbuf);
						ret = 1;
						break;
					}
					cap_len += strlen(cbuf);
					//継続の見出し値を連結
					strcat(loverlay_data->itd[idx - 1], cbuf);
				}else{
					idx += 1;
					if (idx > GOVERLAY_DATA_MAX){
						gtermid = 2;
						fprintf(stderr," Error C [%02d-%02d]: Overlay File structure table overflow (%s)\n", gfuncid, gtermid, sbuf);
						ret = 1;
						break;
					}
#ifdef DEBUG
printf(">> PR_getoverlaydata sbuf / sbuf length : [%s] / %d\n", sbuf, strlen(sbuf));
#endif
					strcpy(loverlay_data->itd[idx - 1], getStr(sbuf, 1, ITD_LEN));
					cbuf = getStr(sbuf, 49, CAPTION_LEN);
					//見出し値の桁数を保存
					cap_len = strlen(cbuf);
#ifdef DEBUG
printf(">> PR_getoverlaydata loverlay_data->itd[%d] length : %d\n" , idx - 1, strlen(loverlay_data->itd[idx - 1]));
#endif
				}
			}
		}
		//IT定義の件数を格納
		loverlay_data->itd_cnt = idx;
#ifdef DEBUG
//printf(">> PR_getoverlaydata loverlay_data->itd_cnt : %d\n" , loverlay_data->itd_cnt);
#endif
	}else{
		gtermid = 3;
		fprintf(stderr," Error C [%02d-%02d]: cannot open Overlay File(%s) \n", gfuncid, gtermid, PR_overlayfile);
		ret = -1;
	}
	fclose(fp);
#ifdef DEBUG
//printf("fdd : %s\n" , loverlay_data->fdd);
//printf("sdd : %s\n" , loverlay_data->sdd);
//for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
//printf("itd : %s\n" , loverlay_data->itd[idx]);
//}
#endif
	
	return ret;
}

//【書式オーバレイ定義ファイルから作成した書式オーバレイファイルの内容構造体を順次参照して
//　書式オーバレイpdfファイルの内容構造体にデータを作成する】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_edtpdfData(OVERLAY_DATA *loverlay_data){
	int level = 0;		//書式オーバレイファイル・定義IT行のレベル
	int pdfCnt = 0;		//書式オーバレイpdfファイルの編集行数(-1すれば添字となる)
	int boxF = 0;		//見出しに長方形の指定が付属（0=付属していない、1=付属している）
	int befRow = 0;		//行が変わったかチェック用の変数(行値を保存、初回はゼロ)
	int chkF = 0;
	int idx = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfData :");
	gfuncid = 11;
	gtermid = 0;
	
	//書式オーバレイファイルの内容を格納した構造体のIT定義を順次参照
	pdfCnt = 0;
	befRow = 0;
	//IT定義・罫線のレベル内容(構造体実体)初期化
	clrlevel_data();
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		//線形が長方形か判定(見出し値付属の長方形を判定)
		if (strcmp(getStr(loverlay_data->itd[idx], 33, 1), "B") == 0){
			boxF = 1;
		}else{
			boxF = 0;
		}
		//罫線・線形、見出し値・タイプ別に書式オーバレイpdfファイルを編集
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") == 0){
#ifdef DEBUG
printf(">> PR_edtpdfData type is %s\n", getStr(loverlay_data->itd[idx], 36, 1));
#endif
			//見出し値・タイプ：英数カナ、日本語、日本語カタカナ定数、:日本語１６進定数 → 見出し値と判定
			ret = PR_edtpdfDataText(loverlay_data, idx, boxF, &befRow, &pdfCnt);
			if (ret != 0){
				return ret;
			}
			//長方形指定が付属していたら罫線処理をする
			if (boxF == 1){
				ret = PR_edtpdfDataLine(loverlay_data, idx, &pdfCnt);
				if (ret != 0){
					return ret;
				}
			}
		}else{
			//見出し値・タイプ：値がない → 罫線と判定
			chkF = 0;
			if (getNum(loverlay_data->itd[idx], 8, 2) == 01 && strcmp(getStr(loverlay_data->itd[idx], 33, 1), "B") == 0){
				//レベル=01で、線形=長方形
				if (getNum(loverlay_data->itd[idx], 23, 2) != 0 || getNum(loverlay_data->itd[idx], 27, 2) != 0){
					//レベル=01、線形=長方形で繰り返し指定があるとき→下位指定も全て繰り返し内に入る
					ret = PR_edtpdfDataLineLoop(loverlay_data, idx, &pdfCnt);
					if (ret != 0){
						return ret;
					}
					chkF = 1;
				}
			}
			if (chkF == 0){
				//上記以外(通常および単体繰り返し)
				ret = PR_edtpdfDataLine(loverlay_data, idx, &pdfCnt);
				if (ret != 0){
					return ret;
				}
			}
		}
#ifdef DEBUG
//printf("PR_edtpdfData itd[%d]/itd length : [%s] / %d\n", idx, getStr(loverlay_data->itd[idx], 11, 46), strlen(getStr(loverlay_data->itd[idx], 11, 46)));
#endif
	}

	//書式オーバレイpdfファイルの内容の行、桁の調整を行う
	ret = setpdfDataColRow(&pdfCnt);

	//書式オーバレイpdfファイルの内容の件数を格納
	gpdf_data_cnt = pdfCnt;

#ifdef DEBUG
for (idx = 0; idx < level_data_len; idx++){
printf(">> PR_edtpdfData - glevel_data[%d].levelNo/beginRow/beginCol  : %d / %d / %d\n" , idx, glevel_data[idx].levelNo, glevel_data[idx].beginRow, glevel_data[idx].beginCol);
printf(">> PR_edtpdfData - glevel_data[%d].sizeRow/sizeCol  : %d / %d\n" , idx, glevel_data[idx].sizeRow, glevel_data[idx].sizeCol);
}
#endif

	return ret;
}

//【書式オーバレイpdfファイルの内容(構造体)に見出し値データを作成】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　int pidx                   ：書式オーバレイファイルの内容(構造体)カレントの添え字(I)
//　int boxF                   ：見出し値に長方形指定が付属しているかフラグ(I)
//　int *befRow                ：書式オーバレイファイルの内容(構造体)の１つ前の要素の行位置(I-O)
//　int pidx                   ：書式オーバレイファイルの内容(構造体)カレントの添え字(I)
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_edtpdfDataText(OVERLAY_DATA *loverlay_data, int pidx, int boxF, int *befRow, int *cnt){
	char *getbuf;
	char buf[351];
	char *wbuf;
	unsigned char cnvbuf[701];	//半角→全角変換エリア
	unsigned int cnvint = 0;	//１６進→日本語変換用
	int beginRow = 0;
	int beginCol = 0;
	int sizeRow = 0;
	int sizeCol = 0;
	int rowPos = 0;
	int colPos = 0;
	int edtRow = 0;
	int edtCol = 0;
	int curRow = 0;
	int xrep = 0;		//横方向繰り返し回数
	int yrep = 0;		//縦方向繰り返し回数
	int xival = 0;		//横方向繰り返しの桁間隔
	int yival = 0;		//縦方向繰り返しの行間隔
	int charCnt = 0;
	int curidx = 0;
	int idx = 0;
	int xidx = 0;
	int yidx = 0;
	int pos = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataText :");
	gfuncid = 12;
	gtermid = 0;

	beginRow = getNum(loverlay_data->itd[pidx], 11, 3);	//開始位置・行
	beginCol = getNum(loverlay_data->itd[pidx], 14, 3);	//開始位置・桁

	//見出し値(印字するテキスト)を編集
	//(見出し値(印字するテキスト)を切り出し)
	getbuf = getStr(loverlay_data->itd[pidx], 49, text_len);
	strcpy(buf, getbuf);
	RTrim(buf);	//継続は連結済みなので末尾の空白は取り去る
	//(件数加算)
	ret = addpdf_dataCnt(cnt);
#ifdef DEBUG
printf(">> PR_edtpdfDataText - cnt/buf/buf length : %d / [%s] / %d\n", *cnt, buf, strlen(buf));
#endif
	if (ret != 0){
		return ret;
	}
	clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
	gpdf_data[*cnt - 1].no = *cnt;		//連番
#ifdef DEBUG
printf(">> PR_edtpdfDataText boxF : %d\n", boxF);
#endif
	if (boxF == 0){
		//通常の見出し指定
		gpdf_data[*cnt - 1].beginRow = beginRow;	//開始位置・行
		gpdf_data[*cnt - 1].beginCol = beginCol;	//開始位置・桁
	}else{
		//見出しに長方形指定が付属
		//(大きさ・行、桁の値を取得)
		sizeRow = getNum(loverlay_data->itd[pidx], 17, 3);	//大きさ・行
		sizeCol = getNum(loverlay_data->itd[pidx], 20, 3);	//大きさ・桁
		//(配置・行位置指定から見出し値の出力行を設定)
		if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " U") == 0){
			//(最上部)
			gpdf_data[*cnt - 1].beginRow = beginRow;
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " C") == 0){
			//(中央)
			gpdf_data[*cnt - 1].beginRow = beginRow + (sizeRow / 2);
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " D") == 0){
			//(最下部)
			gpdf_data[*cnt - 1].beginRow = beginRow + sizeRow - 1;
		}else{
			//(数値で行指定)
			edtRow = getNum(loverlay_data->itd[pidx], 44, 2);
			if (edtRow < 1 || edtRow > (beginRow + sizeRow - 1)){
				//行指定が１以下または長方形の最下部をこえているときは最上部とする
				edtRow = 1;
			}
			gpdf_data[*cnt - 1].beginRow = beginRow + edtRow - 1;
		}
		//(配置・桁位置指定から見出し値の出力桁を設定)
		if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " L") == 0){
			//(左詰め)
			gpdf_data[*cnt - 1].beginCol = beginCol;
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " R") == 0){
			//(右詰め)
			gpdf_data[*cnt - 1].beginCol = beginCol + sizeCol - strlen(buf);
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " D") == 0){
			//(一様分散)
			//(見出し値の文字列数をカウント、出力する長方形の横幅に均等になるように文字間の文字数を計算)
			charCnt = getCharCnt(buf);
			gpdf_data[*cnt - 1].spaceCharLen = ((float)sizeCol - (float)strlen(buf)) / ((float)charCnt + 1);
			//(見出し値の前に半角空白を付加して１桁前を出力位置とする(均等出力のための空白を出力するため))
			wbuf = (char *)malloc(strlen(buf) + 1);
			memset(wbuf, '\0', charCnt + 1);
			sprintf(wbuf, " %s", buf);
			strcpy(buf, wbuf);
			free(wbuf);
			gpdf_data[*cnt - 1].beginCol = beginCol - 1;
		}else{
			//(数値で桁指定)
			edtCol = getNum(loverlay_data->itd[pidx], 46, 2);
			if (edtCol < 1 || edtCol > (beginCol + sizeCol - strlen(buf) - 1)){
				//桁指定が１以下または長方形の右詰めに収まらないときは左詰めとする
				edtCol = 1;
			}
			gpdf_data[*cnt - 1].beginCol = beginCol + edtCol - 1;
		}
	}
	//ポイント
	if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "8") == 0){
		gpdf_data[*cnt - 1].point = 0.8;		//(80%文字)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "9") == 0){
		gpdf_data[*cnt - 1].point = 0.9;		//(90%文字)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") == 0){
		gpdf_data[*cnt - 1].point = 1.5;		//(150%文字)
		gpdf_data[*cnt - 1].beginRow += 1;		//(拡大文字は拡大分上行に出力されるので開始行に+1する)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") == 0){
		gpdf_data[*cnt - 1].point = 2.0;		//(200%文字)
		gpdf_data[*cnt - 1].beginRow += 1;		//(拡大文字は拡大分上行に出力されるので開始行に+1する)
	}else{
		gpdf_data[*cnt - 1].point = 1.0;		//(100%文字)
	}
	//拡大
	if (strcmp(getStr(loverlay_data->itd[pidx], 40, 1), "T") == 0){
		gpdf_data[*cnt - 1].zoom = 1;			//(長体)
		if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") != 0 &&
			strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") != 0){
			//拡大文字は除く
			gpdf_data[*cnt - 1].beginRow += 1;		//(長体は縦に長くなった分、上行に出力されるので開始行に+1する)
		}
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 40, 1), "F") == 0){
		gpdf_data[*cnt - 1].zoom = 2;			//(平体)
		//(平体は縦の長を1/2するので拡大文字(150, 200%文字)が対象のとき、開始行に+1された分を-1する)
		if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") == 0 ||
			strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") == 0){
			gpdf_data[*cnt - 1].beginRow -= 1;
		}
	}else{
		gpdf_data[*cnt - 1].zoom = 0;			//(なし)
	}
	gpdf_data[*cnt - 1].ranhaba = getNum(loverlay_data->itd[pidx], 41, 2);	//欄幅

	//同一行の単語間のバイト数を取得～設定(先頭の単語にはゼロを編集)
	if (gpdf_data[*cnt - 1].beginRow != *befRow){
		gpdf_data[*cnt - 1].interval = 0;
		*befRow = gpdf_data[*cnt - 1].beginRow;
	}else{
		//１つ前の項目が線ならゼロを編集
		if (gpdf_data[*cnt - 1 - 1].lineStyl != 0){
			gpdf_data[*cnt - 1].interval = 0;
		}else{
			//１つ前の項目の開始位置・桁、見出し値のバイト数、現在の項目の開始位置・桁から単語間のバイト数を算出
			//(現、開始位置・桁 - (前、開始位置・桁 + 前、見出し値のバイト数))
			gpdf_data[*cnt - 1].interval = gpdf_data[*cnt - 1].beginCol - (gpdf_data[*cnt - 1 - 1].beginCol + strlen(gpdf_data[*cnt - 1 - 1].text));
		}
	}

	//日本語１６進定数の指定のとき日本語に変換
	if (strcmp(getStr(loverlay_data->itd[pidx], 36, 1), "X") == 0){
		wbuf = (char *)malloc(strlen(buf));
		memset(wbuf, '\0', strlen(buf));
		strcpy(wbuf, buf);
		//日本語１６進定数の指定
		pos = 0;
		for (idx = 0; idx < strlen(wbuf) && wbuf[idx] != '\0'; idx++){
			if (wbuf[idx] == '\0'){
				break;
			}
			sscanf(&wbuf[idx], "%02x", &cnvint);
			buf[pos] = cnvint;
			idx += 1;	//１バイト分加算
			pos += 1;	//編集位置加算
		}
		buf[pos] = '\0';
#ifdef DEBUG
//printf(">> PR_edtpdfDataText     str : [%s]\n", wbuf);
#endif
		free(wbuf);
	}

	//日本語カタカナ定数の指定のとき半角から全角に変換
	if (strcmp(getStr(loverlay_data->itd[pidx], 36, 1), "K") == 0){
		cnvHanToZen(buf, cnvbuf);
		strcpy(buf, cnvbuf);
	}

	//見出し値の編集
	curidx = *cnt - 1;	//現在の見出し値要素(繰り返しのときの基準要素)の添え字を保存
	ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);

	//繰り返し(書式オーバレイpdfファイルの内容(構造体)へ繰り返し分の要素を作成する)
	yrep = getNum(loverlay_data->itd[pidx], 23, 2);	//(縦方向繰り返し回数)
	if (yrep > 0){
		yival = getNum(loverlay_data->itd[pidx], 25, 2);	//縦方向繰り返し行間隔
		if (strcmp(getStr(loverlay_data->itd[pidx], 48, 1), "Y") == 0){
			yival += getCharCnt(buf);	//縦書き → 見出し値、縦書きなので元の見出し値の長が自分行となるので見出し値の長を加算
		}else{
			yival += 1;					//横書き → 見出し値、自行分は１行を加算
		}
	}
	xrep = getNum(loverlay_data->itd[pidx], 27, 2);	//(横方向繰り返し回数)
	if (xrep > 0){
		xival = getNum(loverlay_data->itd[pidx], 29, 2);	//横方向繰り返し桁間隔
		xival += strlen(gpdf_data[curidx].text);	//見出し値、自桁分を加算
		if (yrep <= 0){
			//横方向繰り返しありで、縦方向繰り返しなしのとき、コード上↓のforループがスルーされるので、
			//縦方向繰り返しに 1 を設定しておく(１回繰り返しなので影響はない)
			yrep = 1;
		}
	}
	for (yidx = 0; yidx < yrep; yidx++){
		//縦方向繰り返し
		if (yidx > 0){
			//現在の見出し値要素(繰り返しのときの基準要素)があるので１つ目は要素を作成しない
			ret = addpdf_dataCnt(cnt);
			if (ret != 0){
				return ret;
			}
			clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
			gpdf_data[*cnt - 1].no = *cnt;		//連番
			gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//開始位置・行
			gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol;	//開始位置・桁
			gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//大きさ・行
			gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//大きさ・桁
			gpdf_data[*cnt - 1].point = gpdf_data[curidx].point;		//ポイント
			gpdf_data[*cnt - 1].zoom = gpdf_data[curidx].zoom;			//拡大
			gpdf_data[*cnt - 1].ranhaba = gpdf_data[curidx].ranhaba;	//欄幅
			gpdf_data[*cnt - 1].interval = gpdf_data[curidx].interval;	//同一行単語間のバイト数(先頭の単語はゼロを設定)
			gpdf_data[*cnt - 1].spaceCharLen = gpdf_data[curidx].spaceCharLen;	//文字間空白（単位=文字数(小数可)：見出し値、長方形付属用）
			curRow = gpdf_data[*cnt - 1].beginRow;
			ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);
		}
		for (xidx = 0; xidx < xrep; xidx++){
			//横方向繰り返し
			if (xidx > 0){
				//現在の罫線要素(繰り返しのときの基準要素)があるので１つ目は要素を作成しない
				ret = addpdf_dataCnt(cnt);
				if (ret != 0){
					return ret;
				}
				clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
				gpdf_data[*cnt - 1].no = *cnt;		//連番
				gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//開始位置・行
				gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol + (xival * xidx);	//開始位置・桁
				gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//大きさ・行
				gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//大きさ・桁
				gpdf_data[*cnt - 1].point = gpdf_data[curidx].point;		//ポイント
				gpdf_data[*cnt - 1].zoom = gpdf_data[curidx].zoom;			//拡大
				gpdf_data[*cnt - 1].ranhaba = gpdf_data[curidx].ranhaba;	//欄幅
				gpdf_data[*cnt - 1].interval = gpdf_data[curidx].interval;	//同一行単語間のバイト数(先頭の単語はゼロを設定)
				gpdf_data[*cnt - 1].spaceCharLen = gpdf_data[curidx].spaceCharLen;	//文字間空白（単位=文字数(小数可)：見出し値、長方形付属用）
				curRow = gpdf_data[*cnt - 1].beginRow;
				ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);
			}
		}
	}

	return ret;
}

//【書式オーバレイpdfファイルの内容構造体に罫線データを作成】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　int pidx                   ：書式オーバレイファイルの内容(構造体)カレントの添え字(I)
//　int *cnt                   ：書式オーバレイpdfファイルの内容(構造体)の件数(I-O)
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_edtpdfDataLine(OVERLAY_DATA *loverlay_data, int pidx, int *cnt){
	char *buf;
	int beginRow = 0;
	int beginCol = 0;
	int sizeRow = 0;
	int sizeCol = 0;
	int levelNo = 0;
	int levelF = 0;		//レベル配下（0=配下にないか考慮不要、1=配下にある）
	int curidx = 0;		//繰り返し元の配列位置
	int repival = 0;	//繰り返し間隔
	int xrep = 0;		//横方向繰り返し回数
	int yrep = 0;		//縦方向繰り返し回数
	int xival = 0;		//横方向繰り返しの桁間隔
	int yival = 0;		//縦方向繰り返しの行間隔
	int idx = 0;
	int xidx = 0;
	int yidx = 0;
	int chkF = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataLine :");
	gfuncid = 13;
	gtermid = 0;

	//開始位置、大きさと繰り返し回数は、変数に切り出しておく
	beginRow = getNum(loverlay_data->itd[pidx], 11, 3);	//開始位置・行
	beginCol = getNum(loverlay_data->itd[pidx], 14, 3);	//開始位置・桁
	sizeRow = getNum(loverlay_data->itd[pidx], 17, 3);	//大きさ・行
	sizeCol = getNum(loverlay_data->itd[pidx], 20, 3);	//大きさ・桁
	yrep = getNum(loverlay_data->itd[pidx], 23, 2);	//縦方向繰り返し回数
	xrep = getNum(loverlay_data->itd[pidx], 27, 2);	//横方向繰り返し回数

	//(件数加算)
	ret = addpdf_dataCnt(cnt);
	if (ret != 0){
		return ret;
	}
	clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア

	gpdf_data[*cnt - 1].no = *cnt;			//連番
#ifdef DEBUG
printf(">> PR_edtpdfDataLine gpdf_data[%d] no : %d\n", *cnt - 1, *cnt);
#endif
	levelNo = getNum(loverlay_data->itd[pidx], 8, 2);
	gpdf_data[*cnt - 1].levelNo = levelNo;	//レベル番号
	//線形
	if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "V") == 0){
		gpdf_data[*cnt - 1].lineStyl = 1;		//(縦線)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "H") == 0){
		gpdf_data[*cnt - 1].lineStyl = 2;		//(横線)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "B") == 0){
		gpdf_data[*cnt - 1].lineStyl = 3;		//(長方形)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "O") == 0){
		gpdf_data[*cnt - 1].lineStyl = 5;		//(斜線)
	}
#ifdef DEBUG
//printf(">> PR_edtpdfDataLine gpdf_data[%d] lineStyle, overLayData_33 : %d / [%s]\n", *cnt - 1, gpdf_data[*cnt - 1].lineStyl, getStr(loverlay_data->itd[pidx], 33, 1));
#endif
	//線種
	if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "1") == 0){
		gpdf_data[*cnt - 1].lineVar = 1;	//太線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "2") == 0){
		gpdf_data[*cnt - 1].lineVar = 2;	//細線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "3") == 0){
		gpdf_data[*cnt - 1].lineVar = 3;	//太点線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "4") == 0){
		gpdf_data[*cnt - 1].lineVar = 4;	//細点線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "9") == 0){
		gpdf_data[*cnt - 1].lineVar = 9;	//太一点鎖線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "A") == 0){
		gpdf_data[*cnt - 1].lineVar = 10;	//細一点鎖線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "O") == 0){
		gpdf_data[*cnt - 1].lineVar = 11;	//中線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "P") == 0){
		gpdf_data[*cnt - 1].lineVar = 12;	//中点線
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "S") == 0){
		gpdf_data[*cnt - 1].lineVar = 13;	//中一点鎖線
	}

	//レベル番号指定ありのとき、罫線のレベル内容を設定する
	//(レベル番号の指定がない場合と位置指定等のカラムが異なる)
	if (levelNo > 0){
		if (levelNo > 10){
			//レベル番号が10以上は処理しない
			gtermid = 1;
			fprintf(stderr," Error C [%02d-%02d]: level no is 10 over (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
			return ret;
		}
		idx = levelNo - 1;
		if (levelNo == 1){
			//レベル=01
			//IT定義・罫線のレベル内容(構造体実体)初期化
			clrlevel_data();
			glevel_data[idx].levelNo = levelNo;		//レベル(番号)
			glevel_data[idx].lineStyl = gpdf_data[*cnt - 1].lineStyl;	//線形
			glevel_data[idx].beginRow = beginRow;	//開始位置・行
			glevel_data[idx].beginCol = beginCol;	//開始位置・桁
			glevel_data[idx].sizeRow = sizeRow;	//大きさ・行
			glevel_data[idx].sizeCol = sizeCol;	//大きさ・桁
		}else{
			//レベル=02以降
			if (glevel_data[idx].levelNo == 0 || gpdf_data[*cnt - 1].lineStyl != glevel_data[idx].lineStyl){
				//レベル内容が設定されていないとき、または線形が変わったときのみ値を設定する
				glevel_data[idx].levelNo = levelNo;		//レベル(番号)
				glevel_data[idx].lineStyl = gpdf_data[*cnt - 1].lineStyl;	//線形
				//表形式の判定
				chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
				if (chkF == 1){
					//(表形式)
					if (gpdf_data[*cnt - 1].lineStyl == 1){
						//(縦線)
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow;
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol + sizeCol - 1;
					}else if (gpdf_data[*cnt - 1].lineStyl == 2){
						//(横線)
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow + sizeRow - 1;
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol;
					}else{
						//(縦・横線以外は処理しない)
						gtermid = 2;
						fprintf(stderr," Error C [%02d-%02d]: tabular form's line is not vertical or horizontal (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
						return ret;
					}
				}else{
					//(表形式でない)
					if (beginRow == 0){
						//開始位置・行の値が設定されていないときは1つ前のレベルの開始位置・行の値を引き継ぐ
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow;
					}else{
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow + beginRow - 1;
					}
					if (beginCol == 0){
						//開始位置・桁の値が設定されていないときは1つ前のレベルの開始位置・桁の値を引き継ぐ
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol;
					}else{
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol + beginCol - 1;
					}
				}
				glevel_data[idx].sizeRow = sizeRow;	//大きさ・行
				glevel_data[idx].sizeCol = sizeCol;	//大きさ・桁
			}
		}
	}else{
		//レベル番号指定なし(=00)のとき、IT定義・罫線のレベル内容(構造体実体)初期化
		clrlevel_data();
	}

	//開始位置
	if (gpdf_data[*cnt - 1].levelNo <= 1){
		//レベル=なし, 01
		gpdf_data[*cnt - 1].beginRow = beginRow;
		gpdf_data[*cnt - 1].beginCol = beginCol;
	}else{
		//レベル=02～
		idx = levelNo - 1;
		//表形式の判定
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(表形式)
			if (gpdf_data[*cnt - 1].lineStyl == 1){
				//(縦線)
				//(行：親レベルの行を取得)
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow;
				//(桁：レベルまたは線種が１つ前のデータと変わったとき → 親レベルの桁 + 大きさ・桁 - １)
				//(桁：レベルまたは線種が１つ前のデータと同じとき → １つ前のデータの桁 + 大きさ・桁)
				if (gpdf_data[*cnt - 1].levelNo != gpdf_data[*cnt - 1 - 1].levelNo ||
					gpdf_data[*cnt - 1].lineStyl != gpdf_data[*cnt - 1 - 1].lineStyl){
					gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol + sizeCol - 1;
				}else{
					gpdf_data[*cnt - 1].beginCol = gpdf_data[*cnt - 1 - 1].beginCol + sizeCol;
				}
				//大きさ・行、桁に値を設定
				//(行：親レベルの大きさ・行を取得)
				gpdf_data[*cnt - 1].sizeRow = glevel_data[idx - 1].sizeRow;
				//(桁：親レベルとの桁差分が入っているのでゼロ)
				gpdf_data[*cnt - 1].sizeCol = 0;
			}else if (gpdf_data[*cnt - 1].lineStyl == 2){
				//(横線)
				//(行：レベルまたは線種が１つ前のデータと変わったとき → 親レベルの行 + 大きさ・行 - １)
				//(行：レベルまたは線種が１つ前のデータと同じとき → １つ前のデータの行 + 大きさ・行)
				if (gpdf_data[*cnt - 1].levelNo != gpdf_data[*cnt - 1 - 1].levelNo ||
					gpdf_data[*cnt - 1].lineStyl != gpdf_data[*cnt - 1 - 1].lineStyl){
					gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow + sizeRow - 1;
				}else{
					gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1 - 1].beginRow + sizeRow;
				}
				//(桁：親レベルの桁を取得)
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol;
				//大きさ・行、桁に値を設定
				//(行：親レベルとの行差分が入っているのでゼロ)
				gpdf_data[*cnt - 1].sizeRow = 0;
				//(桁：親レベルの大きさ・桁を取得)
				gpdf_data[*cnt - 1].sizeCol = glevel_data[idx - 1].sizeCol;
			}else{
				//(縦・横線以外は処理しない)
				gtermid = 3;
				fprintf(stderr," Error C [%02d-%02d]: tabular form's line is not vertical or horizontal (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
				return ret;
			}
		}else{
			//(表形式でない)
			if (beginRow == 0){
				//開始位置・行の値が設定されていないときは1つ前のレベルの開始位置・行の値を引き継ぐ
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow;
			}else{
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow + beginRow - 1;
			}
			if (beginCol == 0){
				//開始位置・桁の値が設定されていないときは1つ前のレベルの開始位置・桁の値を引き継ぐ
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol;
			}else{
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol + beginCol - 1;
			}
		}
	}

	//大きさ・行、桁
	if (gpdf_data[*cnt - 1].levelNo <= 1){
		//レベル=なし, 01
		gpdf_data[*cnt - 1].sizeRow = sizeRow;	//大きさ・行
		gpdf_data[*cnt - 1].sizeCol = sizeCol;	//大きさ・桁
	}else{
		//レベル=02～
		idx = levelNo - 1;
		//表形式の判定
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(表形式)
			//(表形式は開始位置の値設定時に設定済みのため、ここではしない)
		}else{
			//(表形式でない)
			if (gpdf_data[*cnt - 1].lineStyl == 1 || gpdf_data[*cnt - 1].lineStyl == 3){
				//(縦線または長方形)
				if (sizeRow == 0){
					//大きさ・行の値が設定されていないときは1つ前のレベルの大きさ・行の値を引き継ぐ
					gpdf_data[*cnt - 1].sizeRow = glevel_data[idx - 1].sizeRow;
				}else{
					gpdf_data[*cnt - 1].sizeRow = sizeRow;
				}
			}
			if (gpdf_data[*cnt - 1].lineStyl == 2 || gpdf_data[*cnt - 1].lineStyl == 3){
				//(横線または長方形)
				if (sizeCol == 0){
					//大きさ・桁の値が設定されていないときは1つ前のレベルの大きさ・桁の値を引き継ぐ
					gpdf_data[*cnt - 1].sizeCol = glevel_data[idx - 1].sizeCol;
				}else{
					gpdf_data[*cnt - 1].sizeCol = sizeCol;
				}
			}
		}
	}

	//終了位置
	if (gpdf_data[*cnt - 1].lineStyl == 1){
		//縦線
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//終了位置・行
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol;	//終了位置・桁
	}else if (gpdf_data[*cnt - 1].lineStyl == 2){
		//横線
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow;	//終了位置・行
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//終了位置・桁
		if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "C") == 0){
			gpdf_data[*cnt - 1].linePos = 1;	//位置
		}
	}else if (gpdf_data[*cnt - 1].lineStyl == 3){
		//長方形
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//終了位置・行
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol;	//終了位置・桁
	}else if (gpdf_data[*cnt - 1].lineStyl == 5){
		//斜線
		if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "R") == 0){
			//右下がり斜線
			gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1].beginRow - 1;	//開始位置・行
			gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//終了位置・行
			gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//終了位置・桁
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "L") == 0){
			//左下がり斜線
			gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow - 1;	//開始位置・行
			gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow - gpdf_data[*cnt - 1].sizeRow;	//終了位置・行
			gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//終了位置・桁
		}
	}

	//繰り返し(書式オーバレイpdfファイルの内容(構造体)へ繰り返し分の要素を作成する)
	curidx = *cnt - 1;	//現在の罫線要素(繰り返しのときの基準要素)の添え字を保存
	if (yrep > 0){
		//縦方向繰り返しの行間隔を取得
		//表形式の判定
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(表形式)
			yival = getNum(loverlay_data->itd[pidx], 17, 3);	//縦方向繰り返し行間隔(大きさ・行に指定されている)
		}else{
			//(表形式でない)
			yival = getNum(loverlay_data->itd[pidx], 25, 2);	//縦方向繰り返し行間隔
			if (gpdf_data[*cnt - 1].lineStyl == 2){
				//横線～縦方向繰り返しなら、自行の１行分を加算
				yival += 1;
			}
		}
		if (gpdf_data[*cnt - 1].lineStyl == 1 || gpdf_data[*cnt - 1].lineStyl == 3 || gpdf_data[*cnt - 1].lineStyl == 5){
			//縦線、長方形または斜線なら自身の長さを加算
			yival += gpdf_data[*cnt - 1].sizeRow;
		}
	}
	if (xrep > 0){
		//横方向繰り返しの桁間隔を取得
		//表形式の判定
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(表形式)
			xival = getNum(loverlay_data->itd[pidx], 20, 3);	//横方向繰り返し桁間隔(大きさ・桁に指定されている)
		}else{
			//(表形式でない)
			xival = getNum(loverlay_data->itd[pidx], 29, 2);	//横方向繰り返し桁間隔
			if (gpdf_data[*cnt - 1].lineStyl == 1){
				//縦線～横方向繰り返しなら、自桁の１桁分を加算
				xival += 1;
			}
		}
		if (gpdf_data[*cnt - 1].lineStyl == 2 || gpdf_data[*cnt - 1].lineStyl == 3 || gpdf_data[*cnt - 1].lineStyl == 5){
			//横線、長方形または斜線なら自身の長さを加算
			xival += gpdf_data[*cnt - 1].sizeCol;
		}
		if (yrep <= 0){
			//横方向繰り返しありで、縦方向繰り返しなしのとき、コード上↓のforループがスルーされるので、
			//縦方向繰り返しに 1 を設定しておく(１回繰り返しなので影響はない)
			yrep = 1;
		}
	}
	for (yidx = 0; yidx < yrep; yidx++){
		//縦方向繰り返し
		if (yidx > 0){
			//現在の罫線要素(繰り返しのときの基準要素)があるので１つ目は要素を作成しない
			ret = addpdf_dataCnt(cnt);
			if (ret != 0){
				return ret;
			}
			clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
			gpdf_data[*cnt - 1].no = *cnt;		//連番
			gpdf_data[*cnt - 1].levelNo = gpdf_data[curidx].levelNo;	//レベル(番号)
			gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//開始位置・行
			gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol;	//開始位置・桁
			gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//大きさ・行
			gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//大きさ・桁
			gpdf_data[*cnt - 1].lineStyl = gpdf_data[curidx].lineStyl;	//線形
			gpdf_data[*cnt - 1].lineVar = gpdf_data[curidx].lineVar;	//線種
			gpdf_data[*cnt - 1].endRow = gpdf_data[curidx].endRow + (yival * yidx);	//終了位置・行
			gpdf_data[*cnt - 1].endCol = gpdf_data[curidx].endCol;	//終了位置・桁
		}
		for (xidx = 0; xidx < xrep; xidx++){
			//横方向繰り返し
			if (xidx > 0){
				//現在の罫線要素(繰り返しのときの基準要素)があるので１つ目は要素を作成しない
				ret = addpdf_dataCnt(cnt);
				if (ret != 0){
					return ret;
				}
				clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
				gpdf_data[*cnt - 1].no = *cnt;		//連番
			gpdf_data[*cnt - 1].levelNo = gpdf_data[curidx].levelNo;	//レベル(番号)
				gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//開始位置・行
				gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol + (xival * xidx);	//開始位置・桁
				gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//大きさ・行
				gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//大きさ・桁
				gpdf_data[*cnt - 1].lineStyl = gpdf_data[curidx].lineStyl;	//線形
				gpdf_data[*cnt - 1].lineVar = gpdf_data[curidx].lineVar;	//線種
				gpdf_data[*cnt - 1].endRow = gpdf_data[curidx].endRow + (yival * yidx);	//終了位置・行
				gpdf_data[*cnt - 1].endCol = gpdf_data[curidx].endCol + (xival * xidx);	//終了位置・桁
			}
		}
	}

	return ret;
}

//【書式オーバレイファイルの内容(構造体)の見出し値要素を開始位置・行、桁で並べ替え】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
void PR_sortoverlaydata(OVERLAY_DATA *loverlay_data){
	int keya = 0;	//比較用キー１
	int keyb = 0;	//比較用キー２
	int Col = 0;
	int Row = 0;
	int loopF = 0;
	char s_itd[100][423];	//IT定義並べ替え用領域
	char wchr[423];
	int s_cnt = 0;
	int idx = 0;

	//関数名を共有変数にセット
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_sortoverlaydata :");
	gfuncid = 14;
	gtermid = 0;

#ifdef DEBUG
printf(">> PR_sortoverlaydata  loverlay_data->itd_cnt : %d\n", loverlay_data->itd_cnt);
#endif

	//IT定義並べ替え用領域初期化
	for (idx = 0; idx < GOVERLAY_DATA_MAX; idx++){
		memset(s_itd[idx], '\0', sizeof(s_itd[idx]));
	}
#ifdef DEBUG
printf(">> PR_sortoverlaydata  sizeof s_itd    : %d\n", sizeof(s_itd));
printf(">> PR_sortoverlaydata  sizeof s_itd[0] : %d\n", sizeof(s_itd[0]));
#endif

	//書式オーバレイファイルの内容(構造体)IT定義中の見出し値の要素のみを文字列配列に保存
	s_cnt = 0;
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") == 0){
			s_cnt += 1;
			strcpy(s_itd[s_cnt - 1], loverlay_data->itd[idx]);
		}
	}

	//見出し値要素の文字列配列を開始位置・行、桁をキーに並べ替え
	loopF = 1;
	while (loopF == 1){
		loopF = 0;
		for (idx = 0; idx < (s_cnt - 1); idx++){
			Row = getNum(s_itd[idx], 11, 3);
			sortoverlaydataRowSet(s_itd[idx], &Row);	//比較キー値にはポイント・拡大指定による行変更を反映して比較
			Col = getNum(s_itd[idx], 14, 3);
			keya = (Row * 1000) + Col;
			Row = getNum(s_itd[idx + 1], 11, 3);
			sortoverlaydataRowSet(s_itd[idx + 1], &Row);	//比較キー値にはポイント・拡大指定による行変更を反映して比較
			Col = getNum(s_itd[idx + 1], 14, 3);
			keyb = (Row * 1000) + Col;
			if (keya > keyb){
				strcpy(wchr, s_itd[idx]);
				memset(s_itd[idx], '\0', sizeof(s_itd[idx]));
				strcpy(s_itd[idx], s_itd[idx + 1]);
				memset(s_itd[idx + 1], '\0', sizeof(s_itd[idx + 1]));
				strcpy(s_itd[idx + 1], wchr);
				loopF = 1;
			}
		}
	}

	//書式オーバレイファイルの内容(構造体)IT定義中の見出し値以外(=罫線)の要素のみを見出し値要素の文字列配列の後に保存
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") != 0){
			s_cnt += 1;
			strcpy(s_itd[s_cnt - 1], loverlay_data->itd[idx]);
		}
	}

#ifdef DEBUG
for (idx = 0; idx < s_cnt; idx++){
printf(">> PR_sortoverlaydata  s_cnt[%03d] : [%s]\n", idx, s_itd[idx]);
}
printf(">> PR_sortoverlaydata                   s_cnt : %d\n", s_cnt);
#endif

	//IT定義を保存した文字列配列(見出し値は並べ替え済み)を書式オーバレイファイルの内容(構造体)IT定義へ戻す
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
    	memset(loverlay_data->itd[idx], '\0', ITD_LEN_MAX);
		strcpy(loverlay_data->itd[idx], s_itd[idx]);
	}
}

//【書式オーバレイpdfファイルの内容構造体に罫線データを作成(レベル=01、長方形の繰り返し用)】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　int pidx                   ：書式オーバレイファイルの内容(構造体)カレントの添え字(I)
//　int *cnt                   ：書式オーバレイpdfファイルの内容(構造体)の件数(I-O)
//　(返値)(0=正常、以外=他の関数からの返値)
int PR_edtpdfDataLineLoop(OVERLAY_DATA *loverlay_data, int pidx, int *cnt){
	char buf[423];
	char bufa[11];
	char bufb[7];
	char bufc[423];
	int yrepcnt = 0;
	int xrepcnt = 0;
	int yrepival = 0;
	int xrepival = 0;
	int ybasStr = 0;
	int xbasStr = 0;
	int ybasSize = 0;
	int xbasSize = 0;
	int ystr = 0;
	int xstr = 0;
	int yloop = 0;
	int xloop = 0;
	int loop = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataLineLoop :");
	gfuncid = 15;
	gtermid = 0;

	//繰り返し・縦、横・回数、間隔を取得
	yrepcnt = getNum(loverlay_data->itd[pidx], 23, 2);	//繰り返し・縦・回数
	yrepival = getNum(loverlay_data->itd[pidx], 25, 2);	//繰り返し・縦・間隔
	xrepcnt = getNum(loverlay_data->itd[pidx], 27, 2);	//繰り返し・横・回数
	xrepival = getNum(loverlay_data->itd[pidx], 29, 2);	//繰り返し・横・間隔
	if (yrepcnt == 0){
		yrepcnt = 1;	//繰り返し・縦・回数なしは１にする
	}
	if (xrepcnt == 0){
		xrepcnt = 1;	//繰り返し・横・回数なしは１にする
	}

	ybasStr = getNum(loverlay_data->itd[pidx], 11, 3);	//開始位置・行
	xbasStr = getNum(loverlay_data->itd[pidx], 14, 3);	//開始位置・桁
	ybasSize = getNum(loverlay_data->itd[pidx], 17, 3);	//大きさ・行
	xbasSize = getNum(loverlay_data->itd[pidx], 20, 3);	//大きさ・桁
	for (yloop = 0; yloop < yrepcnt; yloop++){	//繰り返し・縦・回数分ループ
		//開始位置・行の位置を繰り返しを考慮して算出
		ystr = ybasStr + ((ybasSize + yrepival) * yloop);
		for (xloop = 0; xloop < xrepcnt; xloop++){	//繰り返し・横・回数分ループ
			//開始位置・桁の位置を繰り返しを考慮して算出
			xstr = xbasStr + ((xbasSize + xrepival) * xloop);

			//レベル=01で線形=長方形の繰り返し指定がある書式オーバレイファイルのレコードの開始位置・行、桁の値を置換える
			memset(buf, '\0', sizeof(buf));
			memset(bufa, '\0', sizeof(bufa));
			memset(bufb, '\0', sizeof(bufb));
			memset(bufc, '\0', sizeof(bufc));
			sprintf(bufa, "%s", getStr(loverlay_data->itd[pidx], 1, 10));	//(先頭)～継続
			sprintf(bufb, "%s", getStr(loverlay_data->itd[pidx], 17, 6));	//大きさ・行、桁
			sprintf(bufc, "%s", getStr(loverlay_data->itd[pidx], 31, ITD_LEN_MAX));	//(未使用31バイト目)～最終桁まで
			//繰り返しの値は削除(=空白に置換え)
			sprintf(buf, "%s%3d%3d%s        %s", bufa, ystr, xstr, bufb, bufc);
			strcpy(loverlay_data->itd[pidx], buf);

			//レベル=01の指定で罫線データを作成
			ret = PR_edtpdfDataLine(loverlay_data, pidx, cnt);
			if (ret != 0){
				return ret;
			}

			//次の上位レベルが出現するまで(=配下の指定全て)ループして罫線データを作成
			for (loop = pidx + 1; pidx < loverlay_data->itd_cnt; loop++){
				if (getNum(loverlay_data->itd[loop], 8, 2) <= 01){
					//上位レベルまたはレベル指定なしがみつかったらループExit
					break;
				}
				//下位レベルの指定で罫線データを作成
				ret = PR_edtpdfDataLine(loverlay_data, loop, cnt);
				if (ret != 0){
					return ret;
				}
			}
		}
	}

	return ret;
}

//【引数文字列(書式オーバレイID)からファイル名、ID検索用文字列、pdf名を編集】
//→char *parastr：引数文字列(書式オーバレイID)(I)
//　(返値)(0=正常)
int PR_edtFileID(char *parastr){
	char overlaypath[] = "../print/";	//書式オーバレイファイルのパス
	char txtChar[] = ".txt";			//txt拡張子
	char pdfChar[] = ".pdf";			//pdf拡張子
	char overlayid[128] = "";	//書式オーバレイID
	char *str_top;	//ハイフン区切りの最初の文字列
	char *str_bot;	//ハイフン区切りの最後の文字列
	char *str_wk;
	char *strpara;
	int ret = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtFileID :");
	gfuncid = 16;
	gtermid = 0;

	str_top = (char *)malloc(sizeof(parastr));
	str_bot = (char *)malloc(sizeof(parastr));
	str_wk = (char *)malloc(sizeof(parastr));
	strpara = (char *)malloc(sizeof(parastr));
	memset(str_top, '\0', sizeof(parastr));
	memset(str_bot, '\0', sizeof(parastr));
	memset(str_wk, '\0', sizeof(parastr));
	memset(strpara, '\0', sizeof(parastr));
	
	//引数文字列(書式オーバレイID)の大文字を小文字化
	strcpy(strpara, parastr);
	//文字列中にハイフンがあるか？
	if (strchr(strpara, '-')){
		//最初のハイフン前の文字列を変数に格納
		str_top = strtok(strpara, "-");
		//以降にある最後の最分後の文字列を変数に格納
		str_bot = strtok(NULL, "-");
		strcpy(str_wk, str_bot);
		while (str_wk != NULL){
			str_wk = strtok(NULL, "-");
			if (str_wk != NULL){
				strcpy(str_bot, str_wk);
			}
		}
		//書式オーバレイ検索用文字列を編集
		sprintf(PR_overlaysearch, "%s%%%s", str_top, str_bot);
	}else{
		//最初の文字列を999、最後の文字列を引数文字列(書式オーバレイID)とする
		strcpy(str_top, "999");
		strcpy(str_bot, parastr);
		//書式オーバレイ検索用文字列を編集
		sprintf(PR_overlaysearch, "%s-%s", str_top, str_bot);
	}
	//
#ifdef DEBUG
printf(">> PR_edtFileID  str_top : %s\n" , str_top);
printf(">> PR_edtFileID  str_bot : %s\n" , str_bot);
#endif
	//書式オーバレイファイル(パス+ファイル)を編集
	sprintf(PR_overlayfile, "%s%s%s", overlaypath, strToLower(str_bot), txtChar);
	//書式オーバレイpdfファイルID
	sprintf(PR_overlaypdf, "%s%s", parastr, pdfChar);
#ifdef DEBUG
printf(">> PR_edtFileID  PR_overlayfile   : %s\n" , PR_overlayfile);
printf(">> PR_edtFileID  PR_overlaysearch : %s\n" , PR_overlaysearch);
printf(">> PR_edtFileID  PR_overlaypdf    : %s\n" , PR_overlaypdf);
#endif

	return ret;
}

//【フォント等、用紙指定】
int fontSetting(){
	HPDF_REAL lineheight, charwidth, pageheight, pagewidth;
	HPDF_Font definition_font;          //pdfのフォント
	HPDF_STATUS retCode;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," fontSetting :");
	gfuncid = 51;
	gtermid = 0;

	//フォント設定
	definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
	retCode = HPDF_Page_SetFontAndSize (PR_page, definition_font, PR_fontsize);
	
	//書き込み開始
//	retCode = HPDF_Page_BeginText(PR_page);
	retCode = HPDF_Page_SetFontAndSize (PR_page, definition_font, PR_fontsize);
	
	//一行の高さの設定
	pageheight = HPDF_Page_GetHeight(PR_page);
	//上下のマージンだが、調整のため1.8倍とする
	lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / PR_linepitch );
#ifdef DEBUG
printf(">> fontSetting PR_linepitch/lineheight : %d / %f\n" , PR_linepitch, lineheight);
#endif
	//半角だから/2?
	retCode = HPDF_Page_SetTextLeading(PR_page,(lineheight / 2));

	//半角１文字分の横幅ポイントを取得
	PR_charsize = HPDF_Page_TextWidth(PR_page, " ");
	//行高を取得
	PR_lineheight = HPDF_Page_GetTextLeading(PR_page);
#ifdef DEBUG
printf(">> fontSetting PR_charsize/PR_lineheight : %f / %f\n" , PR_charsize, PR_lineheight);
#endif
	//フォント設定を取得
	PR_definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);

	return retCode;
}

//【設定ファイルのパスを取得】
//カレントdirのconfだけを開くとどこででも実行できないので
char *getConfFilename(char *strConfPath){
	char strTime[] = "00/00/00 00:00:00";
	FILE *fpFileExist;      //20150828 add koyama 

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getConfFilename :");
	gfuncid = 52;
	gtermid = 0;

	if ((fpFileExist = fopen(CONF_FLIEPATH, "r")) == NULL){
		if ((fpFileExist = fopen(CONF_SHREPATH, "r")) == NULL){
			fprintf(stderr, "Error C [%02d]:conf read Error %s \n", local_server_time(strTime));
			exit (1);
		}else{
			strcpy(strConfPath,CONF_SHREPATH);
		}
	}else{
		strcpy(strConfPath,CONF_FLIEPATH);
	}
	fclose(fpFileExist);
	
	return strConfPath;
}

//【フォントファイルのパス＋ファイル名を取得】
char *getFontFilePath(char *fontname,char *filePath){
	char strConfPath[1024]; //20150828 add koyama ファイル名を受け取るためのポインタ

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getFontFilePath :font %s filepath %.15s ",fontname,filePath);
	gfuncid = 53;
	gtermid = 0;

	int i;
	//ファイルネームを元にリーダポインタを作成
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//リーダをリードできる状態に
	xmlTextReaderRead(reader);
	//現在のノードのポインタをセット？
	xmlTextReaderExpand(reader);
	//現在のノードからDOMを取り出している?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return NULL;
	//ドキュメントからコンテキスト()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
	if (!ctx) return NULL;
	//xpathで指定したノードリストを取得
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CHAR_SET_DEFPATH, ctx);
	//xpathで指定したノードリストを取得
	if (!xpobj) return NULL;
	//ノードリストをノードの配列のようなものに
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//ノード数の取得(取得できないなら0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//ノードリストから値を表示
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->children->next->children->content) {
				//設定ファイルからDBのホスト名を取得
				if (i == 0){
					//ファイルパスの挿入(テキストノード分ずれる)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
				if (strcmp(node->children->next->children->content,fontname) == 0){
					//ファイルパスの挿入(テキストノード分ずれる)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
			} else {
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				return NULL;
			}
		}
	}
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	return filePath;
}

//【ファイルの有無を確認】
//→char *chkFile：ファイルID(パス＋ファイル名)(I)
//　(返値)ファイルの有無（0=ファイルあり、1=ファイルなし)
int chkFileExist(char *chkFile){
	int ret = 0;
	struct stat st;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," chkFileExist :format %s ",chkFile);
	gfuncid = 54;
	gtermid = 0;

	//ファイルの情報を取得することによりファイルの有無を確認
	ret = stat(chkFile, &st);
	
	return ret;
}

//【現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す】
char *local_server_time(char *retStr){
	time_t timer;
	struct tm *date;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," local_server_time :");
	gfuncid = 56;
	gtermid = 0;

	timer = time(NULL);
	date = localtime(&timer);
	strftime(retStr,strlen("00/00/00 00:00:00"),"%y/%m/%d %H:%M:%S", date);
	return retStr;
}

//【半角→全角変換】
//→char *han：半角文字列(I)
//　char *zen：全角文字列(O)
void cnvHanToZen(const char *han, char *zen)
{
	unsigned char c1;	//(全角単位で)1バイト目
	unsigned char c2;	//(全角単位で)2バイト目
	int idx = 0;
	int pos = 0;
	int i = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," cnvHanToZen :");
	gfuncid = 57;
	gtermid = 0;

	idx = 0;
	pos = 0;
	//引数の文字列を１バイトごとに変換
	while (han[idx] != '\0'){
		c1 = han[idx];		//１バイト目
		c2 = han[idx + 1];	//次の１バイト
#ifdef DEBUG
printf(">>          cnvHanToZen c1 / c2 : %c / %c (hex) %02x / %02x\n", c1, c2, c1, c2);
#endif
		if (ISKANJI(c1)){
			//全角文字と判定
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kanji\n");
#endif
			zen[pos] = c1;
			zen[pos + 1] = c2;
			pos += 2;
			idx += 2;
		}else if (ISALPH(c1)){
			//半角英数字と判定
			i = c1 - 0x20;
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is alphabet : [%s](%d)\n", ALPH[i], i);
printf(">>        >>cnvHanToZen ALPH_1(%d) : %02x\n", i, ALPH[i][0]);
printf(">>        >>cnvHanToZen ALPH_2(%d) : %02x\n", i, ALPH[i][1]);
#endif
			zen[pos] = ALPH[i][0];
			zen[pos + 1] = ALPH[i][1];
			pos += 2;
			idx += 1;
		}else if (ISKANA(c1)){
			i = c1 - 0xa1;
			if (MAYBEDAKU(c1) && ISDAKU(c2)){
				//半角カナ濁点文字と判定
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana dakuten\n");
#endif
				zen[pos] = DAKU[i][0];
				zen[pos + 1] = DAKU[i][1];
				pos += 2;
				idx += 2;
			}else if (MAYBEHANDAKU(c1) && ISHANDAKU(c2)){
				//半角カナ半濁点文字と判定
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana handakuten\n");
#endif
				zen[pos] = HDAK[i][0];
				zen[pos + 1] = HDAK[i][1];
				pos += 2;
				idx += 2;
			}else{
				//半角カナ文字と判定
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana nomal\n");
#endif
				if (i == 30){
					//全角「ソ(0x835c)」はコードにより固定値指定できないので直接編集
					zen[pos] = 0x83;
					zen[pos + 1] = 0x5c;
					pos += 2;
					idx += 1;
				}else{
					zen[pos] = KANA[i][0];
					zen[pos + 1] = KANA[i][1];
					pos += 2;
					idx += 1;
				}
			}
		}
	}

	zen[pos] = '\0';
}

//【左上基点(ポイント)、桁位置、文字幅(ポイント)から桁位置のポイント値を求める】
//→HPDF_Point basPos  ：基点ポイント(I)
//　int Col            ：桁位置(I)
//　HPDF_REAL charWidth：半角１文字分の幅(ポイント)(I)
//　(返値)桁の位置(ポイント)
HPDF_REAL getColpos(HPDF_Point basPos, int Col, HPDF_REAL charWidth){
	HPDF_REAL fColpos = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getColpos :");
	gfuncid = 58;
	gtermid = 0;

	//微調整：1/2文字分前へ
	fColpos = basPos.x + (float)Col * charWidth - (charWidth / 2);
	//微調整：1/3文字分前へ
	fColpos -= charWidth / 3;

	return fColpos;
}

//【左上基点(ポイント)、行位置、文字幅(ポイント)から行位置のポイント値を求める】
//→HPDF_Point basPos  ：基点ポイント(I)
//　int Row            ：行位置(I)
//　HPDF_REAL charWidth：半角１文字分の幅(ポイント)(I)
//　int drawline       ：罫線（0=見出し値、1=罫線）
//　int centerline     ：横線～行中央（0=行中央でない、1=行中央）
//　(返値)行の位置(ポイント)
HPDF_REAL getRowpos(HPDF_Point basPos, int Row, HPDF_REAL lineHeight, int drawline, int centerline){
	HPDF_REAL fRowpos = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getRowpos :");
	gfuncid = 59;
	gtermid = 0;

	//微調整：1/2行分上へ
	fRowpos = basPos.y - (float)Row * (lineHeight * 2) + lineHeight;

	//罫線(微調整：1/3行分下へ)
	if (drawline == 1){
		fRowpos -= lineHeight / 3;
	}

	//行中央～横線(位置が中央指定なら1/2行上へ)
	if (centerline == 1){
		fRowpos += lineHeight;
	}

	return fRowpos;
}

//【文字列の指定位置から指定長部分を取り出す】
//→char *sbuf：切り出し元文字列(I)
//　int pos   ：切り出し開始位置(I)
//　int getlen：切り出し長(I)
//(返値)切り出した文字列
char *getStr(char *sbuf, int pos, int getlen){
	int maxlen = 0;
	int len = 0;
	int idx = 0;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getStr :");
	gfuncid = 60;
	gtermid = 0;

#ifdef DEBUG
//printf(">> getStr - sbuf/pos/getlen : %s / %d / %d\n" , sbuf, pos, getlen);
#endif
	//切り出し元文字列の長を取得
	maxlen = strlen(sbuf);
#ifdef DEBUG
//printf("getStr - maxlen : %d\n" , maxlen);
#endif

	len = 0;
	memset(gcutbuf, '\0', sizeof(gcutbuf));
	//切り出し位置から切り出し長分、文字列を切り出し
	if (pos <= maxlen){
		//切り出し元文字列の長以内のときのみ切り出しを行う
		for (idx = 0; idx < 100; idx++){
			//指定桁数に達したら終了
			if (len >= getlen){
				break;
			}
			//切り出し元文字列の長に達したら終了
			if (idx > (maxlen - 1)){
				gcutbuf[idx] = '\0';
				break;
			}
			//切り出し元文字列から切り出し先文字列へ１文字ずつ編集(NULLを見つけたら終了する)
			gcutbuf[idx] = sbuf[pos - 1];
			if (gcutbuf[idx] == '\0'){
				break;
			}
			pos += 1;
			len += 1;
		}
	}
#ifdef DEBUG
//printf(">> getStr - idx/pos/len : %d / %d / %d\n" , idx, pos, len);
//printf(">> getStr - gcutbuf : %s\n" , gcutbuf);
#endif

	return gcutbuf;
}

//【文字列の指定位置から指定長部分を取り出して数値化(int型のみ)】
//→char *sbuf：切り出し元文字列(I)
//　int pos   ：切り出し開始位置(I)
//　int getlen：切り出し長(I)
//(返値)数値化された切り出し文字列
int getNum(char *sbuf, int pos, int len){
	char buf[20];
	int maxlen = 0;
	int idx = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getNum :");
	gfuncid = 61;
	gtermid = 0;

	//切り出し元文字列の長を取得
	maxlen = strlen(sbuf);
	//切り出し元文字列をこえるときはゼロを返す
	if (pos > maxlen){
		return ret;
	}
	if ((pos + len - 1) > maxlen){
		return ret;
	}

	memset(buf, '\0', 20);
	strncpy(buf, sbuf + (pos - 1), len);
#ifdef DEBUG
//printf(">> getNum     buf(bef.) : %s\n" , buf);
#endif
	for (idx = 0; idx < 20 || buf[idx] != '\0'; idx++){
		if (buf[idx] == ' '){
			buf[idx] = '0';
		}
	}
#ifdef DEBUG
//printf(">> getNum     buf(aft.) : %s\n" , buf);
#endif
	ret = atoi(buf);

	return ret;
}

//【文字列の文字数を数える(全角文字も１文字)】
//→char *sbuf：カウント対象文字列(I)
//　(返値)文字列の文字数
int getCharCnt(char *sbuf){
	unsigned char ch;
	int cnt = 0;
	int idx = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getCharCnt :");
	gfuncid = 62;
	gtermid = 0;

	cnt = 0;
	for (idx = 0; idx < strlen(sbuf) && sbuf[idx] != '\0'; idx++){
		ch = sbuf[idx];
		//全角判定
		//(１バイト目)
		if ((ch >= 0x81 && ch <= 0x9f) || (ch >= 0xe0 && ch <= 0xfc)){
			//(２バイト目)
			ch = sbuf[idx + 1];
			if (ch >= 0x40 && ch <= 0xfc){
				idx += 1;	//全角文字なので１バイト加算して次文字へ
			}
		}
		cnt += 1;
	}
	ret = cnt;

	return ret;
}

//【書式オーバレイpdfファイルの内容(構造体実体)件数加算】
//→int *cnt：書式オーバレイpdfファイルの内容件数(I-O)
//　(返値)(0=要素加算成功、-1=構造体最大数オーバー)
int addpdf_dataCnt(int *cnt){
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"addpdf_dataCnt :");
	gfuncid = 63;
	gtermid = 0;

	*cnt += 1;
#ifdef DEBUG
//printf(">> addpdf_dataCnt     cnt : %d\n", *cnt);
#endif
	if (*cnt > GPDF_DATA_MAX){
		gtermid = 1;
		fprintf(stderr," Error C [%02d-%02d]: pdf File structure table overflow\n", gfuncid, gtermid);
		return 1;
	}
	
	return ret;
}

//【書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素を初期化】
void clrpdf_data(int idx){

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," clrpdf_data :");
	gfuncid = 64;
	gtermid = 0;

	gpdf_data[idx].no = 0;			//連番
	gpdf_data[idx].levelNo = 0;		//レベル(番号)
	gpdf_data[idx].beginRow = 0;	//開始位置・行
	gpdf_data[idx].beginCol = 0;	//開始位置・桁
	gpdf_data[idx].sizeRow = 0;		//大きさ・行
	gpdf_data[idx].sizeCol = 0;		//大きさ・桁
	gpdf_data[idx].lineStyl = 0;	//線形　(1:縦線(=V)、2:横線(=H)、3:長方形(=B)、5:斜線(=O))
	gpdf_data[idx].lineVar = 0;		//線種　(1:太線、2:細線、3:太点線、4:細点線、9:太一点鎖線、10:細一点鎖線(=A)、11:中線(=O)、12:中点線(=P)、13:中一点鎖線(=S))
	gpdf_data[idx].linePos = 0;		//位置　(0:下側(=D)、1:中央(=C))
	gpdf_data[idx].endRow = 0;		//終了位置・行
	gpdf_data[idx].endCol = 0;		//終了位置・桁
	gpdf_data[idx].point = 0;		//ポイント
	gpdf_data[idx].zoom = 0;		//拡大
	gpdf_data[idx].ranhaba = 0;		//欄幅
	gpdf_data[idx].interval = 0;	//同一行単語間のバイト数(先頭の単語はゼロを設定)
	gpdf_data[idx].spaceCharLen = 0;	//文字間空白（単位=文字数(小数可)：見出し値、長方形付属用）
	memset(gpdf_data[idx].text, '\0', 349);	//見出し値
}

//【IT定義・罫線のレベル内容(構造体実体)を初期化】
void clrlevel_data(){
	int idx = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," clrlevel_data :");
	gfuncid = 65;
	gtermid = 0;

	for (idx = 0; idx < level_data_len; idx++){
		glevel_data[idx].levelNo = 0;	//レベル(番号)
		glevel_data[idx].lineStyl = 0;	//線形
		glevel_data[idx].beginRow = 0;	//開始位置・行
		glevel_data[idx].beginCol = 0;	//開始位置・桁
		glevel_data[idx].sizeRow = 0;	//大きさ・行
		glevel_data[idx].sizeCol = 0;	//大きさ・桁
	}
}

//【見出し値の編集（縦書き・横書き）】
//→OVERLAY_DATA *loverlay_data：書式オーバレイファイルの内容(構造体)(I)
//　int pidx                   ：書式オーバレイファイルの内容(構造体)カレントの添え字(I)
//　int *cnt                   ：書式オーバレイpdfファイルの内容(構造体)の件数(I-O)
//　char *buf                  ：見出し値の内容(I)
//　int *retRow                ：見出し値を出力した現在行(I-O)
//　(返値)(0=正常)
int edtpdfText(OVERLAY_DATA *loverlay_data, int pidx, int *cnt, char *buf, int *retRow){
	unsigned char ch;
	int fstF = 0;
	int charLen = 0;
	int curCnt = 0;
	int edtRow = 0;
	int idx = 0;
	int j = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," edtpdfText :");
	gfuncid = 66;
	gtermid = 0;

	//見出し値(縦書きあり)
	if (strcmp(getStr(loverlay_data->itd[pidx], 48, 1), "Y") == 0){
		//縦書き
		if (strlen(buf) > 0){
			curCnt = *cnt - 1;	//1文字目の配列要素添え字値を保存
			edtRow = gpdf_data[curCnt].beginRow;	//1文字目の開始位置・行を変数に設定
			fstF = 1;
			for (idx = 0; idx < strlen(buf); idx++){
				if (buf[idx] == '\0'){
					break;
				}
				if (fstF != 1){
					//(件数加算)
					ret = addpdf_dataCnt(cnt);
					if (ret != 0){
						return ret;
					}
					clrpdf_data(*cnt - 1);	//書式オーバレイpdfファイルの内容(構造体実体)指定添え字要素をクリア
					//(見出し値以外は同一の値を格納)
					gpdf_data[*cnt - 1].no = *cnt;		//連番
					edtRow += 1;
					gpdf_data[*cnt - 1].beginRow = edtRow;						//開始位置・行
					gpdf_data[*cnt - 1].beginCol = gpdf_data[curCnt].beginCol;	//開始位置・桁
					gpdf_data[*cnt - 1].point = gpdf_data[curCnt].point;		//ポイント
					gpdf_data[*cnt - 1].zoom = gpdf_data[curCnt].zoom;			//拡大
					gpdf_data[*cnt - 1].ranhaba = gpdf_data[curCnt].ranhaba;	//欄幅
				}
				charLen = 1;
				//全角判定
				//(１バイト目)
				ch = buf[idx];
				if ((ch >= 0x81 && ch <= 0x9f) || (ch >= 0xe0 && ch <= 0xfc)){
					//(２バイト目)
					ch = buf[idx + 1];
					if (ch >= 0x40 && ch <= 0xfc){
						charLen = 2;
					}
				}
				//文字バイト分切り出して編集
				for (j = 0; j < charLen; j++){
					gpdf_data[*cnt - 1].text[j] = buf[idx + j];	//見出し値
				}
				gpdf_data[*cnt - 1].text[j] = '\0';
				idx = idx + charLen - 1;	//全角文字なら１バイト加算して次文字へ

				fstF = 0;
			}
			*retRow = gpdf_data[*cnt - 1].beginRow;
		}
	}else{
		//横書き
		strcpy(gpdf_data[*cnt - 1].text, buf);	//見出し値
		*retRow = gpdf_data[*cnt - 1].beginRow;
	}

	return ret;
}

//【ポイント・拡大によるデフォルト文字高さをこえる場合に１行加算】
//→char *s_itd：IT定義の１要素(I)
//　int *Row   ：拡大指定により補正された行位置(I-O)
void sortoverlaydataRowSet(char *s_itd, int *Row){

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"sortoverlaydataRowSet :");
	gfuncid = 67;
	gtermid = 0;

	//Y,Z：ポイント100％以上
	//F：長体
	if (strcmp(getStr(s_itd, 39, 1), "Y") == 0 ||
		strcmp(getStr(s_itd, 39, 1), "Z") == 0){
		if (strcmp(getStr(s_itd, 40, 1), "F") != 0){
			*Row += 1;
		}
	}else if (strcmp(getStr(s_itd, 40, 1), "T") == 0){
		*Row += 1;
	}
}

//【書式オーバレイpdfファイルの内容構造体の行、桁を調整する】
//→int *cnt：書式オーバレイpdfファイルの内容(構造体)要素数
//　(返値)(0=正常)
int setpdfDataColRow(int *cnt){
	int idx = 0;
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"setpdfDataColRow :");
	gfuncid = 68;
	gtermid = 0;

	for (idx = 0; idx < *cnt; idx++){
		//行：実データ内容印刷との関係で１行上を設定する
		gpdf_data[idx].beginRow -= 1;	//開始位置・行
		gpdf_data[idx].endRow -= 1;	//終了位置・行
		//縦線：桁の右側に引く仕様なので桁に１加算する
		if (gpdf_data[idx].lineStyl == 1){
			gpdf_data[idx].beginCol += 1;	//開始位置・桁
			gpdf_data[idx].endCol += 1;	//終了位置・桁
		}
	}

	return ret;
}

//【レベル指定配下のIT指定が表形式かチェック】
//→int lineStyl：線形
//　int sizeRow ：大きさ・行
//　int sizeCol ：大きさ・桁
//　int yrep    ：繰り返し・縦・回数
//　int xrep    ：繰り返し・横・回数
//　(返値)(0=表形式でない、1=表形式)
int chkHyo(int lineStyl, int sizeRow, int sizeCol, int yrep, int xrep){
	int ret = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"chkHyo :");
	gfuncid = 69;
	gtermid = 0;

	ret = 0;
	if (lineStyl == 1){
		//縦線
		if (sizeRow == 0 && sizeCol != 0 && yrep == 0){
			//縦線指定で大きさ・行に指定がなく、大きさ・桁に指定があり、繰り返し・縦回数に指定がない→表形式と判定
			ret = 1;
		}
	}else if (lineStyl == 2){
		//横線
		if (sizeRow != 0 && sizeCol == 0 && xrep == 0){
			//横線指定で大きさ・行に指定があって、大きさ・桁に指定がなく、繰り返し・横回数に指定がない→表形式と判定
			ret = 1;
		}
	}

	return ret;
}

//【文字列右側の空白文字を削除】
//→char *str：処理対象文字列(I-O)
void RTrim(char *str){
	int len = 0;
	int idx = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"RTrim :");
	gfuncid = 70;
	gtermid = 0;

	//文字列長を取得
	len = strlen(str);

	//文字列末尾から空白以外の文字が現れるまで空白文字をNULL文字に置き換え
	for (idx = len - 1; idx > 0; idx--){
		if (str[idx] != '\0' && str[idx] != ' '){
			break;
		}
		str[idx] = '\0';
	}
}

//【文字列の大文字を小文字に変換】
//→char *str：変換元文字列(I)
//　(返値)(文字列中の大文字が小文字に変換された文字列)
char *strToLower(char *str){
	int pos = 0;
	char *strS = NULL,*strE = NULL;
	
	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"strToLower :");
	gfuncid = 71;
	gtermid = 0;
	strS = str;
	strE = str + strlen(str);
	for (; strS < strE; strS++){
		//大文字のとき数置換で小文字に置き換え
		*strS = tolower(*strS);
	}
#ifdef DEBUG
printf(">> toLower  str   : %s\n" , str);
printf(">> toLower  gstrup : %s\n" , gstrup);
#endif
	
	return str;
}





/*
char *toLower(char *str){
	int pos = 0;

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"toLower :");
	gfuncid = 71;
	gtermid = 0;

	for (pos = 0; pos < strlen(str); pos++){
		if (str[pos] >= 'A' && str[pos] <= 'Z'){
			//大文字のとき数置換で小文字に置き換え
			str[pos] += 0x20;
		}
	}
#ifdef DEBUG
printf(">> toLower  str   : %s\n" , str);
#endif
	
	return str;
}
*/
