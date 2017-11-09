#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include <time.h>      /* localtimeに必要 20150828 */
#include <mysql.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/
#include <sys/types.h>           //ファイルの情報を取得するため
#include <sys/stat.h>            //ファイルの情報を取得するため
#include <iconv.h>               //プリンタへ文字を送る時の文字コード変換
#include <libcob.h>
#include "hpdf.h"
#include "confheader.h"

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN];
int MT_Initialize();
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

static char PR_db_server[64];
static char PR_db_user[34];
static char PR_db_password[64];
static char PR_db_database[64];
static int  PR_db_port;
static char strTime[] = "0000/00/00 00:00:00.000000";


//confから取得するdebug_flg.cob-mysqlで宣言
//static int myConfDebugFlg;

static HPDF_Doc  pdf;
static HPDF_Page PR_page[2048];
static HPDF_PageSizes PR_pagesize;                 //ページのサイズ
static HPDF_PageDirection PR_pagestyle;            //ページの縦横

//pdfの用紙設定(フォントの設定を含む)
static char PR_Lineprint = 0;
static float PR_fontsize = 0;                       //pdfのフォントサイズ
static HPDF_Font PR_definition_font;                //フォント
static float PR_linepitch = 0,PR_charpitch = 0,PR_chartimes = 0;     //ページに対する文字配置を設定
static float PR_topmargin = 0,PR_leftmargin = 0;    //余白の基準(上下,左右は同じ幅とする)
static char PR_fontname[31];                 //pdfのフォント名を格納
static char PR_fontFilePath[256];                   //pdfフォントファイルのパス
const char* PR_font;                         //pdfのフォントを格納
//pdfのオブジェクトのプロパティ
static int PR_currentLine = 0;               //次に書き込みする行番号
static int PR_currentPage = 0;                      //現在書き込み中のページ番号
static int PR_currentObj = 0;                       //現在書き込み中のファイル番号
static int PR_maxPage     = 0;                      //ファイル番号の最大
static int PR_maxObj      = 0;                      //ファイル番号の最大
//char *PR_fileArray[100] = {0};             //ファイルポインタの配列最大ページ数はPR_maxObj*100になる
static char PR_temppath[128] = "";                  //作成されるpdfの名前
static char PR_tempname[128] = "";                  //作成されるpdfの名前
static char PR_bindname[128] = "";                  //結合後のpdfの名前
static char PR_basepath[128] = "";                  //背景となるpdfまでのパス
static char PR_basename[128] = "";                  //背景となるpdfの名前
static char PR_printername[31] = "";
static float divCharSpace = 10;	//CharSpace分割値	//2016/01/05 kawada add
jmp_buf PR_env;    //エラー処理用

//印字サイズ制御コード用変数 upd
static HPDF_REAL PR_lineheight = 0;		//行高
static HPDF_REAL PR_horizontalScale = 0;	//水平割合
//制御コード番号(前回判定用保存)(文字ピッチ用)（1:全角文字=1.5バイト、2:全角文字=2バイト）
static int PR_ctrlCodeNoSave = 0;
//制御コード番号(前回判定用保存)(横倍、縦倍、２倍指定用)
//（3:横2倍文字、4:縦2倍文字、5:2倍文字、6:横方向リセット、7:2倍文字リセット、8:縦方向リセット）
static int PR_ctrl2CodeNoSave = 0;
float PR_charsize = 0;				//半角文字の横幅ポイント値

//confから取得するdebug_flg.他と被らないようにoriginal name
static int prMyConfDebugFlg;

//////////////////Function liet AND prototype Start
//ID=01 帳票設定が複数ある時は変更するときにこの関数を用いる
int PR_Open_Assign(char *formatname);
//ID=02 pdfを作成、書き込み用に
int PR_Open();
//ID=03 現在の対象ファイルを保存し、次のファイルを作成
int PR_ReDim();
//ID=04 改ページ
int PR_NewPage();
//ID=05 グローバル変数よりtempファイルの作成
int PR_makePdfFile();
//ID=6 二つpdfを結合
int PR_Pdf_Join();
//ID=FF ファイルの存在確認 ->使われていない
int IsFileExisting(const char* );
// 設定ファイルの読み込み
int PR_conf_read();
// pdf作成用に設定をxml,dbから取得
int PR_Initialize(char *);
// pdfに行を書き込み
int PR_Write(char *linetext);
// 改行
int PR_LineFeed(char *linecount);
// 現在の行数を返す
int PR_Get_Linage();
//////////////////Function liet AND prototype End


typedef struct drawing{
	float  startXPoint;      //x軸のスタート
	float  endXPoint;        //x軸のエンド
	float  startYPoint;      //y軸のスタート
	float  endYPoint;        //y軸のエンド
	char type;
}DRAW_OBJ;


//エラー処理
#ifdef HPDF_DLL
void  __stdcall
#else
void
#endif
error_handler(HPDF_STATUS error_no,HPDF_STATUS detail_no,void *user_data){
	cob_runtime_error(
		" Error C [%04x]: error_no=%04X, detail_no=%u : %s"
		,mytoolgetErrorId(LIB_ID_PRINT,15,15)
		,(HPDF_UINT)error_no
		,(HPDF_UINT)detail_no
		,map_source_func);
	longjmp(PR_env, 1);
}

static int (*func)(char *errno, const char *errmsg);

//ファイルの存在確認
//return : 1=存在 , 0=存在しない
//date:20160706
//author:koyama
int IsFileExisting(const char* pszFile){
  FILE *pf;

  if ((pf = fopen(pszFile, "r")) == 0) {
    return 0;
  }
  fclose(pf);

  return 1;
}

//設定ファイルの読み込み
int PR_conf_read(){
	int i;
	char strConfPath[1024]; //20150828 add koyama ファイル名を受け取るためのポインタ

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_conf_read :");

	//下で値が設定されなかったらこれがdefault
	prMyConfDebugFlg = 0;

	//ファイルネームを元にリーダポインタを作成   //ファイル名を関数に変更 20150828
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
				if(strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(PR_db_server,node->content);
				}
				//設定ファイルからDBのユーザ名を取得
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(PR_db_user,node->content);
				}
				//設定ファイルからDBのパスワードを取得
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(PR_db_password,node->content);
				}
				//設定ファイルからDB名を取得
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(PR_db_database,node->content);
				}
				//設定ファイルから背景のパスを取得
				if(strcmp(node->parent->name,PR_BASE_PATH) == 0){
					//後で後ろにファイル名をつなぐ
					strcpy(PR_basepath,node->content);
				}
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					PR_db_port = atoi(node->content);
				}
				//設定ファイルからTEMPディレクトリのパスを取得
				if(strcmp(node->parent->name,PR_TEMP_PATH) == 0){
					//後で後ろにファイル名をつなぐ
					strcpy(PR_temppath,node->content);
					strcpy(PR_bindname,node->content);
				}
				//設定ファイルからファイル数の最大を取得
				if(strcmp(node->parent->name,PR_OBJ_MAX) == 0){
					//後で後ろにファイル名をつなぐ
					PR_maxObj = atoi(node->content);
				}
				//設定ファイルからページの最大を取得
				if(strcmp(node->parent->name,PR_PAGE_MAX) == 0){
					//後で後ろにファイル名をつなぐ
					PR_maxPage = atoi(node->content);
				}
				//debugフラグがあるなら取得
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//返還できない文字列は0になるはず
					prMyConfDebugFlg = atoi(node->content);
				}
			} else {
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				cob_runtime_error(" ERROR [%02d] conf format error %s ",99,map_source_func);
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return 1;
			}
		}
	}
	xmlXPathFreeObject(xpobj);
	xmlXPathFreeContext(ctx);
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	if(PR_maxPage == 0){
		cob_runtime_error(" ERROR [%02d] Pagemax value has not been set %s ",99,map_source_func);
		return 1;
	}
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

char *getFontFilePath(char *fontname,char *filePath){
	char strConfPath[1024]=""; //20150828 add koyama ファイル名を受け取るためのポインタ
	int i;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getFontFilePath :font %s filepath %.15s ",fontname,filePath);

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
				if(i == 0){
					//ファイルパスの挿入(テキストノード分ずれる)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
				if(strcmp(node->children->next->children->content,fontname) == 0){
					//ファイルパスの挿入(テキストノード分ずれる)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
			} else {
				xmlXPathFreeObject(xpobj);
				xmlXPathFreeContext(ctx);
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				cob_runtime_error(" ERROR [%02d] conf format error %s ",99,map_source_func);
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return NULL;
			}
		}
	}
	xmlXPathFreeObject(xpobj);
	xmlXPathFreeContext(ctx);
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return filePath;
}

//pdf作成用に設定をxml,dbから取得
int PR_Initialize(char *formatname){
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);	sprintf(map_source_func,"PR_Initialize :format %s ",formatname);

	//後で値が入ったかどうか判断するため初期化
	PR_font = NULL;
	//下で値が設定されなかったらこれがdefault
	prMyConfDebugFlg = 0;

	//設定の読み込み(グローバル変数へ格納)
	PR_conf_read();

	//ファイル名のベースを先に作っておく(設定のパスとファイル名をつなぐ)
	strcpy(PR_tempname,PR_temppath);
	strcat(PR_tempname,PR_TMP_FNAME);
	strcat(PR_bindname,PR_PRN_FNAME);

	PR_setProperty(formatname);

	//通常の横幅(拡大率)
	PR_horizontalScale = HPDF_Page_GetHorizontalScalling(PR_page[PR_currentPage]);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//
//グローバル変数よりtempファイルの作成
//return : 成功 0 失敗 1
//author:koyama
int PR_makePdfFile(){
	int ret=0;
	int tempfp;

	int funcId = 5;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_makePdfFile :");

	//ユニークなファイル名を作る
	//添付ファイルを作っていなければ作る
	if(strstr(PR_tempname,"XXXXXX") != NULL){
		tempfp = mkstemp(PR_tempname);
		if(tempfp == -1){
			cob_runtime_error(" Error C [%02d]: can't create temp file ",21);
			ret = 1;
		}
		if(ret == 0){
			if(chmod(PR_tempname, S_IRWXU | S_IRWXG | S_IRWXO) == -1){
				cob_runtime_error(" Error C [%02d]: can't changing file permission ",22);
				ret = 1;
			}
		}
	}

	//添付ファイルを作っていなければ作る
	if(strstr(PR_bindname,"XXXXXX") != NULL){
		tempfp = mkstemp(PR_bindname);
		if(tempfp == -1){
			cob_runtime_error(" Error C [%02d]: don't create temp file ",21);
			ret = 1;
		}
		if(ret == 0){
			if(chmod(PR_bindname,S_IRWXU | S_IRWXG | S_IRWXO) == -1){
				cob_runtime_error(" Error C [%02d]: don't changeing file permission ",22);
				ret = 1;
			}
		}
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

int PR_setProperty(char *formatname){
	MYSQL PR_conn,*PR_mysql = &PR_conn;
	MYSQL_RES *res;
	MYSQL_ROW row = NULL;
	char query[512] = "";    //初期設定を読み込むSQLの格納
	char temp[512] = "";    //sql用temporary
	char *source_file_name = NULL;
	char *source_user_name = NULL;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_setProperty :format %s ",formatname);

	//DB接続
	if ((PR_mysql = mysql_init(&PR_conn)) == NULL) {
		cob_runtime_error(" Error [%02d]:%s ", 99,mysql_error(PR_mysql));
		return 1;
	}
	if (!mysql_real_connect(&PR_conn, PR_db_server, PR_db_user, PR_db_password, PR_db_database, PR_db_port, NULL, 0)) {
		cob_runtime_error( " Error [%02d]:db_connect server :%s user:%s pass:%s database:%s ,%s ",01, PR_db_server,PR_db_user,PR_db_password,PR_db_database,map_source_func);
		return 1;
	}


	//	//ユーザID取得準備
	if(source_file_name == NULL){
		source_file_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
		if(source_file_name == NULL){
			cob_runtime_error(" Error [%02d]: can't get source_file_name,%s ",02,map_source_func);
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return 1;
		}
		memset(source_file_name, '\0', (PATHNAME_SIZE + 1));

		source_user_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
		if(source_user_name == NULL){
			cob_runtime_error(" Error [%02d]: can't get source_user_name,%s ",02,map_source_func);
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return 1;
		}
		memset(source_user_name, '\0', (PATHNAME_SIZE + 1));
		// ユーザID取得
		getUserAndProcessName(source_file_name, source_user_name);
	}
	

	strcpy(temp," SELECT mf.size, mf.page_style, mf.font ");
	strcat(temp," ,mf.font_size, mf.base_pdf, mf.line_pitch,mf.char_pitch ");
	strcat(temp," , mf.top_margin, mf.left_margin, mp.print_id, mp.print_name ");
	strcat(temp," ,mpr.size, mp2.print_id, mp2.print_name ");
	strcat(temp," FROM (M_FORM mf ");
	strcat(temp," INNER JOIN M_PRINTER mp ");
	strcat(temp," ON mp.print_id = mf.print_id) ");
	strcat(temp," LEFT OUTER JOIN M_PRINTER_RELATION mpr ");
	strcat(temp," ON (mpr.form_id = mf.id ");
	strcat(temp," AND mpr.user_id = '%s') ");
	strcat(temp," LEFT OUTER JOIN M_PRINTER mp2 ");
	strcat(temp," ON mp2.print_id = mpr.print_id ");
	strcat(temp," WHERE mf.id='%s' ");
	sprintf(query, temp, source_user_name, formatname);

	//DB帳票データを抽出
	if (mysql_query(PR_mysql, query)!=0) {
		//エラー内容を出力
		cob_runtime_error(" Error C [%02d]: Query Error %s %s %s ",99,map_source_func,local_server_time(strTime),mysql_error(PR_mysql));
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}

	res = mysql_store_result(PR_mysql);

	//idを指定しているので1行しか取れないはず
	while( row = mysql_fetch_row(res) ) {
		//帳票種類ID
		//ページサイズの取得
		if(strcmp(row[0],"A4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A4;
		}else if(strcmp(row[0],"A5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A5;
		}else if(strcmp(row[0],"B4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B4;
		}else if(strcmp(row[0],"B5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B5;
		}else if(strcmp(row[0],"A3") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A3;
		}

		//ページのスタイルを取得(縦横)
		if (strcmp(row[1],"HPDF_PAGE_LANDSCAPE") == 0){
			//横向き
			PR_pagestyle=HPDF_PAGE_LANDSCAPE;
		}else if(strcmp(row[1],"HPDF_PAGE_PORTRAIT") == 0){
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
		//行の文字数を格納 206 136
		PR_charpitch = atof(row[6]);

		//文字の水平方向倍率
		PR_chartimes = atof(row[6]);

		//上下の余白を格納
		PR_topmargin = atof(row[7]);
		//左右の余白を格納
		PR_leftmargin = atof(row[8]);

		if(row[9][0] == '9'){
			PR_Lineprint = 1;
		}
		//取得していた設定をクリア
		memset(PR_printername,'\0',strlen(PR_printername));
		//プリンタ名を取得
		strcpy(PR_printername,row[10]);
		

		//NULLの場合エラーになるのを回避。
		if(row[11] != '\0'){
			//個人設定を取得した場合上書きする。
			if(strcmp(row[11],"A4") == 0){
				PR_pagesize = HPDF_PAGE_SIZE_A4;
			}else if(strcmp(row[11],"A5") == 0){
				PR_pagesize = HPDF_PAGE_SIZE_A5;
			}else if(strcmp(row[11],"B4") == 0){
				PR_pagesize = HPDF_PAGE_SIZE_B4;
			}else if(strcmp(row[11],"B5") == 0){
				PR_pagesize = HPDF_PAGE_SIZE_B5;
			}else if(strcmp(row[11],"A3") == 0){
				PR_pagesize = HPDF_PAGE_SIZE_A3;
			}
		}

		//NULLの場合エラーになるのを回避。
		if(row[12] != '\0'){
			PR_Lineprint = 0;
		//取得していた設定をクリア
		memset(PR_printername,'\0',strlen(PR_printername));
		//プリンタ名を取得
			strcpy(PR_printername,row[13]);
		
			if(row[12][0] == '9'){
				PR_Lineprint = 1;
			}
		}
		
	}
	mysql_free_result(res);//res開放
	mysql_close(PR_mysql);

	//失敗したらエラーを返す
	if(strlen(PR_basename) == 0){
		cob_runtime_error(" Error C [%02d]: cannot read database for PDF ",11);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}
}

int fontSetting(){
	HPDF_REAL lineheight,charwidth,pageheight,pagewidth;
	HPDF_Font definition_font;          //pdfのフォント
	HPDF_STATUS retCode;                //function return
	const char *fontPath = PR_fontFilePath;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," fontSetting :");

	//TODO FONTファイル名は一度とったら変更は無いはず Openで取得しているはず
	if(strlen(PR_fontFilePath) == 0){
		getFontFilePath(PR_fontname,PR_fontFilePath);
	}
	//ttfont_tagはフォントの埋め込みがしてあれば文字列が入っている
	if(PR_font == NULL || (strlen(PR_font) == 0) || (pdf != NULL && (strlen(pdf->ttfont_tag) == 0))){
		PR_font = HPDF_LoadTTFontFromFile(pdf, fontPath, HPDF_TRUE);
	}

	//フォント設定
	definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
	retCode = HPDF_Page_SetFontAndSize (PR_page[PR_currentPage], definition_font, PR_fontsize);

	//書き込み開始
//	retCode = HPDF_Page_BeginText(PR_page[PR_currentPage]);
	retCode = HPDF_Page_SetFontAndSize (PR_page[PR_currentPage], definition_font, PR_fontsize);

	//一行の高さの設定
	pageheight = HPDF_Page_GetHeight(PR_page[PR_currentPage]);
	//上下のマージンだが、調整のため1.8倍とする
//	lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / PR_linepitch );

	if(PR_Lineprint == 1){
		lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 66 * 1.39 );
		if(PR_pagesize == HPDF_PAGE_SIZE_A3){
			lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 132 * 1.39 );
		}
	}else{
		if(PR_pagesize == HPDF_PAGE_SIZE_A5){
			lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 33.7 );
		}else{
	lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 66 );
		}
	}

	//HPDF_Page_SetTextLeading行間隔を設定する
	retCode = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],(lineheight / 2));

	//2015/10/29 kawada add S
	//半角１文字分の横幅ポイントを取得
	PR_charsize = HPDF_Page_TextWidth(PR_page[PR_currentPage], " ");
	//行高を取得
	PR_lineheight = HPDF_Page_GetTextLeading(PR_page[PR_currentPage]);
	//フォント設定を取得
	PR_definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return retCode;
}

//帳票設定が複数ある時は変更するときにこの関数を用いる
int PR_Open_Assign(char *formatname){
	const char *fontPath = PR_fontFilePath;
	int ret=0;

	int funcId = 1;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Open_Assign :format %s ",formatname);

	PR_setProperty(formatname);

	//プリンタを開き直すときは現在を保存して新規を作成
	PR_ReDim();
	PR_makePdfFile();

	//dotのときはPDFにしない
	//if(PR_Lineprint != 1){
		// pdf = HPDF_New(error_handler, NULL);
		//
		// if(setjmp(PR_env)) {
		// 	HPDF_Free(pdf);
		// 	pdf = NULL;
		// 	cob_runtime_error(" Error C [%04x]: %s pdf free "
		// 		,mytoolgetErrorId(LIB_ID_PRINT,funcId,1),map_source_func);
		// 	//共有変数を元に戻す
		// 	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		// 	return 1;
		// }

		// //日本語エンコードを許可
		// HPDF_UseJPEncodings(pdf);
		// //エンコードを設定
		// HPDF_SetCurrentEncoder(pdf,PR_DEFAULT_ENCODE);
		// //日本語が使えるフォントを追加
		// HPDF_UseJPFonts(pdf);
		//
		// if (!pdf) {
		// 	cob_runtime_error(" Error C [%02d]: cannot create PdfDoc object",32);
		// 	//共有変数を元に戻す
		// 	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		// 	return 1;
		// }

		ret = PR_NewPage();
		if(ret != 0){
			cob_runtime_error(" Error C [%02d]: cannot create PdfDoc object",33);
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
	//}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//結合したpdfを印刷
int PR_Pdf_Print(){
	char cmd[256]="";
	int ret=1;
	struct stat st;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Pdf_Print : ");

	ret=stat(PR_bindname,&st);

	if(ret == 0){
		if(PR_Lineprint == 1){
			sprintf(cmd, "lpr -P %s %s", PR_printername,PR_bindname);
			system(cmd);
		}else{
			//印刷コマンドとその実行
//			sprintf(cmd, "lpr -P %s %s", PR_printername,PR_bindname);
//			system(cmd);

			//印刷したらファイルを削除できるはず
//			sprintf(cmd, "rm %s ", PR_bindname);
//			system(cmd);
		}
		ret=0;
	}
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//pdfを書き込み、保存
//Option1　flg:どこから来たか(PR_Close:0,PR_Redim:1)
//author:koyama
int PR_Save(int flg){
	HPDF_STATUS temp;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Save : flg= %d ",flg);

	if(flg == 0){
		//PR_Closeから来たときは最後のページを閉じる
		// HPDF_Page_EndText(PR_page[PR_currentPage]);
	}
	temp = HPDF_SaveToFile(pdf, PR_tempname);

	//正常に保存できなければ終了
	if(temp != HPDF_OK){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}

	/* clean up */
	// HPDF_FreeDoc(pdf);
	HPDF_Free(pdf);
	pdf = NULL;
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//二つpdfを結合
int PR_Pdf_Join(){
	char cmd[256]="";
	int ret;
	struct stat st;
	FILE *fp;
	char buf[256]="";
	char comp_name[256]="";

	int funcId = 6;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Pdf_Join :");

	//実行
	sprintf(cmd, "pdftk %s background %s output %s 2>&1", PR_tempname,PR_basename,PR_bindname);
	//拡張子の変更
	sprintf(comp_name, "%s.pdf", PR_bindname);
	sprintf(cmd, "%s;mv %s  %s",cmd,PR_bindname,comp_name);
	sprintf(cmd, "%s;rm %s ", cmd,PR_tempname);
	fp=popen(cmd,"r");
	if(fp){
		while (!feof(fp)) {
			fgets(buf, 256, fp);
		}
		pclose(fp);
			return 0;
	}else{
		cob_runtime_error(" Error C [%02d]: %s pdftk Error "
			,mytoolgetErrorId(LIB_ID_PRINT,funcId,1),map_source_func);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}
}

//Closeとpdfの結合印刷を連続させる
int PR_Close(){
	int ret = 0;
	char cmd[256]="";
	struct stat st;
	FILE *fp;
	char buf[256]="";

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Close :");


	if(PR_currentPage == 1 && PR_currentLine == 1){		//2015/08/21 kawada add
		//OpenしてそのままCloseする場合が必要かどうか改めて判断する必要があるか add comment koyama
		if (strcmp(PR_tempname, "") != 0){
			// 何故入れたのか不明 comment koyama 20170601
			// ret = stat(PR_tempname, &st);
			if(ret == 0){
				sprintf(cmd, "rm %s", PR_tempname);
			}
		}
		if (strcmp(PR_bindname, "") != 0){
			// 何故入れたのか不明 comment koyama 20170601
			// ret = stat(PR_bindname, &st);
			if(ret == 0){
				if(strlen(cmd) != 0){
					sprintf(cmd, "%s;rm %s",cmd,PR_bindname);
				}else{
					sprintf(cmd, "rm %s",PR_bindname);
				}
			}
		}
		//上で連結したコマンドを発行
		if(strlen(cmd) != 0){
			fp=popen(cmd, "r");
			if(fp){
				while (!feof(fp)) {
					fgets(buf, 256, fp);
				}
				pclose(fp);
			}
		}
	} else {
		//dotのときはPDFにしない
		//if(PR_Lineprint != 1){
			ret = PR_Save(0);
			if(ret == 1){
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return 1;
			}
			ret = PR_Pdf_Join();
			if(ret == 1){
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return 1;
			}
			//現在のオブジェクトを保存したので++
			PR_currentObj++;
			PR_currentPage = 0;
		//}
	}
	ret = PR_Pdf_Print();
	if(ret == 1){
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}

	//ファイル名のベースをリセットしておく
	strcpy(PR_tempname,PR_temppath);
	strcat(PR_tempname,PR_TMP_FNAME);
	strcat(PR_bindname,PR_PRN_FNAME);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//pdfを作成、書き込み用に
int PR_Open(){
	int ret=0;

	int funcId = 2;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Open :");

	//ファイルの開き直しの時動きがおかしくなる可能性を考えて
	PR_ReDim();

	//dotのときはPDFにしない
	//if(PR_Lineprint != 1){
		// //Redimの時に作られていなければ(いらないかも)
		// if(!pdf){
		// 	pdf = HPDF_New(error_handler, NULL);
		// }
		// if(setjmp(PR_env)) {
		// 	HPDF_Free(pdf);
		// 	pdf = NULL;
		// 	cob_runtime_error(" Error C [%04x]: %s pdf free "
		// 		,mytoolgetErrorId(LIB_ID_PRINT,funcId,1),map_source_func);
		// 	return 1;
		// }

		// //日本語エンコードを許可
		// HPDF_UseJPEncodings(pdf);
		// //エンコードを設定
		// HPDF_SetCurrentEncoder(pdf,PR_DEFAULT_ENCODE);
		// //日本語が使えるフォントを追加
		// HPDF_UseJPFonts(pdf);
		//
		// if (!pdf) {
		// 	cob_runtime_error(" Error C [%04x]: cannot create PdfDoc object "
		// 		,mytoolgetErrorId(LIB_ID_PRINT,funcId,2),map_source_func);
		// 	return 1;
		// }

		ret = PR_NewPage();
		if(ret != 0){
			return ret;
		}
	//}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//現在の対象ファイルを保存し、次のファイルを作成
//author: koyama
int PR_ReDim(){
	int ret=0;
	char *tempPnt;
	char *bindPnt;

	int funcId = 3;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_ReDim :");

	//最初に呼ばれた時は必要ない
	if(strstr(PR_tempname,"XXXXXX") == NULL){
		//一旦ファイルを正常に保存
		//dotのときはPDFにしない
		//if(PR_Lineprint != 1){
			ret = PR_Save(1);
			if(ret == 1){
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				//Save 失敗は2
				return 1;
			}
			ret = PR_Pdf_Join();
			if(ret == 1){
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				//Join 失敗は3
				return 3;
			}
		//}

		//現在のオブジェクトを保存したので++
		PR_currentObj++;
		PR_currentPage = 0;
	}

	//ファイル名を作り直す
//	bindPnt = malloc(PR_bindname[0] * (strlen(PR_bindname) + 1));
	//ファイル名(path付き)をクリア
	memset(PR_tempname,0x00,strlen(PR_tempname));
	memset(PR_bindname,0x00,strlen(PR_bindname));
	//ファイル名にパスを再設定
	strcpy(PR_tempname,PR_temppath);
	strcpy(PR_bindname,PR_temppath);
	//ファイル名(path付きをリセット)
	strcat(PR_tempname,PR_TMP_FNAME);
	strcat(PR_bindname,PR_PRN_FNAME);

	//新ファイル名
	PR_makePdfFile();
//	//mod 100でファイル名を格納しておく
//	PR_fileArray[(PR_currentObj % 100)] = PR_bindname;
	//一旦中身を開放して次を作る
	if(pdf && pdf->cur_page != NULL){
		// HPDF_FreeDoc(pdf);
		HPDF_Free(pdf);
		pdf = NULL;
	}
	pdf = HPDF_New(error_handler, NULL);

	if(setjmp(PR_env)) {
		HPDF_Free(pdf);
		pdf = NULL;
		cob_runtime_error(" Error C [%04x]: %s pdf free ",mytoolgetErrorId(LIB_ID_PRINT,funcId,1),map_source_func);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}

	//日本語エンコードを許可
	HPDF_UseJPEncodings(pdf);
	//エンコードを設定
	if(HPDF_GetCurrentEncoder(pdf) == NULL){
		HPDF_SetCurrentEncoder(pdf,PR_DEFAULT_ENCODE);
	}
	//日本語が使えるフォントを追加
	HPDF_UseJPFonts(pdf);

	if (!pdf) {
		cob_runtime_error(" Error C [%02d]: cannot create PdfDoc object ",34);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//
//改ページ
//author:koyama
int PR_NewPage(){
	HPDF_STATUS retCode;                //function return
	int ret = 0;

	int funcId = 4;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_NewPage :");

	if(PR_currentPage > 0){
		HPDF_Page_EndText(PR_page[PR_currentPage]);
	}
	//枚数が指定を超えるようだったらそこまでを保存して次に
	if(PR_currentPage == PR_maxPage){
		ret = PR_ReDim();
		if(ret != 0){
			cob_runtime_error(" Error C [%04x]: %s PDF File add errror "
				,mytoolgetErrorId(LIB_ID_PRINT,funcId,1),map_source_func);
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
		//ページの最初から
		PR_currentPage = 0;
	}
	//0を飛ばして使う
	PR_currentPage++;
	//pageオブジェクトの作成
	PR_page[PR_currentPage] = HPDF_AddPage(pdf);

	//ページ設定(initializeで取得したもの)
	retCode = HPDF_Page_SetSize(PR_page[PR_currentPage], PR_pagesize, PR_pagestyle);

	//フォント設定だけを分割
	retCode = fontSetting();

	retCode = HPDF_Page_BeginText(PR_page[PR_currentPage]); //テキストオブジェクトを開始する
	//文字間隔を設定
//	if(PR_chartimes != 1){
//		retCode = HPDF_Page_SetCharSpace(PR_page[PR_currentPage],1.1);
//	}
	retCode = HPDF_Page_MoveTextPos(PR_page[PR_currentPage], PR_leftmargin, HPDF_Page_GetHeight(PR_page[PR_currentPage]) - PR_topmargin); //テキストの位置を、指定したオフセットに移動する
	retCode = HPDF_Page_ShowText(PR_page[PR_currentPage], "");  //ページの現在位置にテキストを表示する
//	printf("charwidth:%d = (((int)HPDF_Page_GetWidth(page):%d) - (PR_leftmargin:%d * 2)) / PR_charpitch:%d \n",charwidth,(int)HPDF_Page_GetWidth(PR_page[PR_currentPage]),PR_leftmargin,PR_charpitch);

	//次に書き込む行をセット
	PR_currentLine = 1;
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//2015/10/27 kawada add S

//指定された制御文字コード番号に対応する文字倍率を行に設定
int setcharSizeofHPDF(int ctrlCodeNo, int ctrl2CodeNo){
	int   wctrlCodeNo = 0;
	int   wctrl2CodeNo = 0;
	float cRate = 0;
	float c2Rate = 0;
	HPDF_REAL lnhi = 0;
	int  ret = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"setcharSizeofHPDF :");

	//現在の制御コード番号が指定されていない(=0)ときは、以前の設定を引き継ぐ
	wctrlCodeNo = ctrlCodeNo;
	if(wctrlCodeNo == 0){		//現在の制御コード番号に指定がなければ
		wctrlCodeNo = PR_ctrlCodeNoSave;	//以前の制御コード番号(制御コード番号保存変数から)を使用
	}
	wctrl2CodeNo = ctrl2CodeNo;
	if(wctrl2CodeNo == 0){		//現在の制御コード番号に指定がなければ
		wctrl2CodeNo = PR_ctrl2CodeNoSave;	//以前の制御コード番号(制御コード番号保存変数から)を使用
	}

	//制御文字コード番号(ピッチ用)文字倍率を設定
	switch(wctrlCodeNo){
		case 1:
			//全角文字=1.5バイト
			cRate = 0.75;
			//フォントは標準
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
		case 2:
			//全角文字=2バイト
			cRate = 1;
			//フォントは標準
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
		default:
			//(指定なし→通常とみなす)
			cRate = 1;
			//フォントは標準
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
	}

	//制御文字コード番号(倍字用)文字倍率を設定
	switch(wctrl2CodeNo){
		case 3:
			//横2倍文字
			c2Rate = 2;
			//フォントは標準
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
		case 4:
			//縦2倍文字
			c2Rate = 0.5;		//フォントを２倍すると横幅も２倍となるのでHorizontalScallingを1/2にして対応
			//フォントを２倍
			ret = HPDF_Page_SetFontAndSize (PR_page[PR_currentPage], PR_definition_font, PR_fontsize * 2);
			if(ret != HPDF_OK){
				return ret;
			}
			//行間隔を２倍して縦２倍化
			ret = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],PR_lineheight * 2);
			if(ret != HPDF_OK){
				return ret;
			}
			break;
		case 5:
			//2倍文字
			c2Rate = 1;			//フォントを２倍すると横幅も２倍となるので倍率は１で可
			//フォントを２倍
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize * 2);
			if(ret != HPDF_OK){
				return ret;
			}
			//行間隔を２倍して縦２倍化
			ret = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],PR_lineheight * 2);
			if(ret != HPDF_OK){
				return ret;
			}
			break;
		case 6:
			//横方向リセット
			c2Rate = 1;
			//フォントを標準に戻す
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
		case 7:
			//2倍文字リセット
			c2Rate = 1;
			//フォントを標準に戻す
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			if(ret != HPDF_OK){
				return ret;
			}
			//現在の行間隔を取得してそれを1/2する
			lnhi = HPDF_Page_GetTextLeading(PR_page[PR_currentPage]);
			ret = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],(lnhi / 2));
			if(ret != HPDF_OK){
				ret = 1;
				return ret;
			}
			break;
		case 8:
			//縦方向リセット
			c2Rate = 1;
			//フォントを標準に戻す
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			if(ret != HPDF_OK){
				return ret;
			}
			//現在の行間隔を取得してそれを1/2する
			lnhi = HPDF_Page_GetTextLeading(PR_page[PR_currentPage]);
			ret = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],(lnhi / 2));
			if(ret != HPDF_OK){
				ret = 1;
				return ret;
			}
			break;
		default:
			//(指定なし→通常とみなす)
			c2Rate = 1;
			//フォントは標準
			ret = HPDF_Page_SetFontAndSize(PR_page[PR_currentPage], PR_definition_font, PR_fontsize);
			break;
	}

	//HorizontalScallingを設定
	ret = HPDF_Page_SetHorizontalScalling(PR_page[PR_currentPage], (PR_horizontalScale * cRate * c2Rate * PR_chartimes)); //水平方向の拡大率を取得する
	if(ret != HPDF_OK){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}
	//現在の制御コード番号を制御コード番号保存変数に設定
	//(現在の制御コード番号が指定されていない(=0)のときは制御コード番号保存変数に設定しない～保存されている設定をつぶさないため)
	if(ctrlCodeNo != 0){
		PR_ctrlCodeNoSave = ctrlCodeNo;
	}
	if(ctrl2CodeNo != 0){
		PR_ctrl2CodeNoSave = ctrl2CodeNo;
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//指定された文字倍率から実際の現在位置を求める(罫線用)
void getcurPos(int ctrlCodeNo, int ctrl2CodeNo, int charCnt, float *curPos){
	float cRate = 0;
	float c2Rate = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getcurPos :");

	//制御文字番号(ピッチ用)に該当する文字倍率を求める
	switch(ctrlCodeNo){
		case 1:
			//全角文字=1.5バイト
			cRate = 0.75;
			break;
		case 2:
			//全角文字=2バイト
			cRate = 1;
			break;
		default:
			//(指定なし→通常とみなす)
			cRate = 1;
			break;
	}

	//制御文字番号(倍字用)に該当する文字倍率を求める
	switch(ctrl2CodeNo){
		case 3:
			//横2倍文字
			c2Rate = 2;
			break;
		case 4:
			//縦2倍文字
			c2Rate = 1;
			break;
		case 5:
			//2倍文字
			c2Rate = 2;
			break;
		default:
			//(指定なし→通常とみなす)
			c2Rate = 1;
			break;
	}

	//文字倍率を適用した場合の文字サイズを算出し現在位置に加算
	*curPos += (float)(charCnt * cRate * c2Rate * PR_chartimes) * PR_charsize;
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
}

//制御コード番号により指定の文字ピッチ指定を全角文字のみに反映
int setstrCharRate(int ctrlCodeNo, int ctrl2CodeNo, char *charBuf, float *curPos){
	char chkchr;           //チェック用文字
	int  posStart;         //charBufのチェックの基点となるchar位置
	int  posNow;           //charBufの現在char位置
	int  charStart;        //基点となるchar種類（1:1バイト文字、2:2バイト文字）
	int  charNow;          //現在のchar種類（1:1バイト文字、2:2バイト文字）
	int  fstF;             //初回フラグ(1:初回、2:2回目以降)
	int  ctrlCodeF = 0;    //制御コードでループ終了したフラグ(0:終了は制御コードでない、1:終了は制御コード)
	int  wctrlCodeNo = 0;
	int  wctrl2CodeNo = 0;
	int  ret = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"setstrCharRate :");
	if(prMyConfDebugFlg >= 15){
		cob_runtime_error(" Error [%02d]: %s Info setstrCharRate %s ",66,local_server_time(strTime),charBuf);
	}
	//現在の制御コード番号が指定されていない(=0)ときは、以前の設定を引き継ぐ
	wctrlCodeNo = ctrlCodeNo;
	if(wctrlCodeNo == 0){		//現在の制御コード番号に指定がなければ
		wctrlCodeNo = PR_ctrlCodeNoSave;	//以前の制御コード番号(制御コード番号保存変数から)を使用
	}
	wctrl2CodeNo = ctrl2CodeNo;
	if(wctrl2CodeNo == 0){		//現在の制御コード番号に指定がなければ
		wctrl2CodeNo = PR_ctrl2CodeNoSave;	//以前の制御コード番号(制御コード番号保存変数から)を使用
	}

	//文字倍率が通常比率(全角文字=2バイト)以外の指定は、半角・全角文字別に文字倍率指定して表示(印字)する
	if(wctrlCodeNo == 1 ||      //全角文字=1.5バイト
		wctrl2CodeNo == 3 ||    //横2倍文字
		wctrl2CodeNo == 5){     //2倍文字
		//文字倍率が通常比率以外の処理(全角・半角文字別に文字倍率指定表示(印字))
		//印字対象文字列をchar単位でチェックして1バイト、2バイト別にピッチ指定する
		fstF = 1;
		posNow = 0;
		while(posNow < strlen(charBuf)){
			//次の制御コードが見つかったらExit
			if((unsigned char)charBuf[posNow] == 0x1A){
				ctrlCodeF = 1;
				break;
			}
			//現在位置のcharを取得
			chkchr = charBuf[posNow];

			//半角・全角判定
			if(((unsigned char)chkchr >= 0x00 && (unsigned char)chkchr <= 0x7F ) ||
			   ((unsigned char)chkchr >= 0xA1 && (unsigned char)chkchr <= 0xDF ))
			{
				//半角文字
				charNow = 1;
			}else{
				//全角文字
				charNow = 2;
			}
			if(fstF == 1){
				//初回：現在位置とchar種類を開始位置とchar種類に保存
				posStart = posNow;
				charStart = charNow;
				fstF = 0;		//初回フラグをリセット
			}else{
				//前位置のchar種類と現在位置のchar種類が変わったとき前位置までのcharをその種類にしたがって表示(印刷)
				if(charNow != charStart){
					if(charStart == 1){
						//半角(1倍指定にする)
						ret = setcharSizeofHPDF(-1, -1);	//該当する指定なし→通常とみなされるため -1 を指定
						if(ret != HPDF_OK){
							return ret;
						}
						getcurPos(-1, -1, (posNow - posStart), curPos);		//該当する指定なし→通常とみなされるため -1 を指定
					}else{
						//全角(制御コード番号に該当する倍字指定にする)
						ret = setcharSizeofHPDF(wctrlCodeNo, wctrl2CodeNo);	//制御コード番号を渡す
						if(ret != HPDF_OK){
							return ret;
						}
						getcurPos(wctrlCodeNo, wctrl2CodeNo, (posNow - posStart), curPos);	//制御コード番号を渡す
					}
					//前位置までの文字列を切り出し～表示(印刷)
					char strcut[256]="";
					strncpy(strcut, charBuf + posStart, (posNow - posStart));

					HPDF_Page_ShowText(PR_page[PR_currentPage], strcut);
					//現在位置とchar種類を開始位置とchar種類に保存
					posStart = posNow;
					charStart = charNow;
				}
			}
			//現在位置に加算(char種類により1or2)
			posNow += charNow;
		}
		//最終部分を切り出し～表示(印刷)
		if(posNow > posStart){
			if(charStart == 1){
				//半角(1倍指定にする)
				ret = setcharSizeofHPDF(-1, -1);		//該当する指定なし→通常とみなされるため -1 を指定
				if(ret != HPDF_OK){
					return ret;
				}
				getcurPos(-1, -1, (posNow - posStart), curPos);		//該当する指定なし→通常とみなされるため -1 を指定
			}else{
				//全角(制御コード番号に該当する倍字指定にする)
				ret = setcharSizeofHPDF(wctrlCodeNo, wctrl2CodeNo);	//制御コード番号を渡す
				if(ret != HPDF_OK){
					return ret;
				}
				getcurPos(wctrlCodeNo, wctrl2CodeNo, (posNow - posStart), curPos);	//制御コード番号を渡す
			}
			char strcut[256]="";
			strncpy(strcut, charBuf + posStart, (posNow - posStart));

			HPDF_Page_ShowText(PR_page[PR_currentPage], strcut);
		}
		//制御コード番号に該当する文字サイズに戻しておく
		ret = setcharSizeofHPDF(wctrlCodeNo, wctrl2CodeNo);	//制御コード番号を渡す
		if(ret != HPDF_OK){
			return ret;
		}
	}else{
		//文字倍率が通常比率の処理(対象文字列を全て表示(印字))
		HPDF_Page_ShowText(PR_page[PR_currentPage], charBuf);
		//現在位置に加算
		getcurPos(wctrlCodeNo, wctrl2CodeNo, strlen(charBuf), curPos);
	}
	//現在の制御コード番号を制御コード番号保存変数に設定
	//(現在の制御コード番号が指定されていない(=0)のときは制御コード番号保存変数に設定しない～保存されている設定をつぶさないため)
	if(ctrlCodeNo != 0){
		PR_ctrlCodeNoSave = ctrlCodeNo;
	}
	if(ctrl2CodeNo != 0){
		PR_ctrl2CodeNoSave = ctrl2CodeNo;
	}
	ret = HPDF_OK;
	return ret;
}
//2015/10/27 kawada add E

//設定込の文字の書き込み
int setAttrAndPrint(char *linetext){
	char *attrPos;
	char *endPos;
	char *showText;

	//どの2倍文字を付けたかのフラグ 0x01:横2倍 0x02:縦2倍 0x03縦横2倍
	char twoInfo        = 0x00;

	char onePfive[]        ={0x1A,0x24,0x21,0x20,0x78,0x00};    //1.5バイト文字
	char twoPzero[]        ={0x1A,0x24,0x21,0x24,0x74,0x00};    //2.0バイトの文字
	char twoWidth[]        ={0x1A,0x26,0x21,0x20,0x68,0x22,0x21,0x76,0x00};    //横2倍文字
	char twoHeight[]       ={0x1A,0x26,0x22,0x21,0x66,0x21,0x20,0x78,0x00};    //縦2倍文字
	char twoProduct[]      ={0x1A,0x26,0x22,0x21,0x66,0x22,0x21,0x76,0x00};    //2倍文字のリセット222176
	char twoProductReset[] ={0x1A,0x26,0x21,0x20,0x68,0x21,0x20,0x78,0x00};    //2倍文字のリセット
	char gridHrizonStart[] ={0x1A,0xC0,0x00};                                  //横罫線スタート
	char gridHrizonEnd[]   ={0x1A,0xC1,0x00};                                  //横罫線エンド
	char gridVertStart[]   ={0x1A,0xC2,0x00};                                  //縦罫線
	char gridnone[]        ={0x00,0x00,0x00};                                  //線なし?

	DRAW_OBJ drawArray[32];                                                    //一行に対する罫線
	int      drawCount = 0;
	int      drawCount_start = 0;	//横罫線用、drawArray現在件数カウント保存変数（終点指定編集時に使用） //2015/08/27 kawada add
	int      ctrlCodeNo = 0;		//制御コード番号(文字ピッチ用)（1:全角文字=1.5バイト、2:全角文字=2バイト） //2015/10/29 kawada add
	int      ctrl2CodeNo = 0;		//制御コード番号(横倍、縦倍、２倍指定用)（3:横2倍文字、4:縦2倍文字、5:2倍文字、6:横方向リセット、7:2倍文字リセット、8:縦方向リセット） //2015/10/29 kawada add
	float    curPos = 0;			//現在位置(倍字指定を考慮した実際の文字位置／罫線指定用)	//2015/10/29 kawada add

	int ret=0;
	HPDF_Point basePoint;
	//definition end

	//関数名を共有変数にセット
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"setAttrAndPrint :%.10s",linetext);

	//showTextに受け取る
	showText = linetext;

	//一旦空の行を表示
	HPDF_Page_ShowTextNextLine(PR_page[PR_currentPage],"");
	basePoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);

	//2015/10/29 kawada add S
	//縦倍・2倍文字指定があれば、拡大文字用に空1行追加しておく
	if(strstr(showText, twoHeight) != NULL || strstr(showText, twoProduct) != NULL){
		HPDF_Page_ShowTextNextLine(PR_page[PR_currentPage],"");
		basePoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);
	}
	//初期開始位置を取得して現在位置に設定
	HPDF_Point curPoint;
	curPoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);
	curPos = curPoint.x;

	if(strchr(showText,0x1A) != NULL){							//＜showTextに制御コードが含まれているか？＞
		while(strchr(showText,0x1A) != NULL){					//＜showTextに制御コードが含まれている間ループする＞
			if(strchr(showText,0x1A) == showText){				//＜showTextカレントポインタ位置に制御コードがあるか？＞
				//フォーマットの変更
				if(strncmp(showText,onePfive,strlen(onePfive)) == 0){
					//1.5バイト文字
					ctrlCodeNo = 1;
					//制御コードを除去
					showText = showText + strlen(onePfive);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,twoPzero,strlen(twoPzero)) == 0){
					//2バイト文字
					ctrlCodeNo = 2;
					ret = setcharSizeofHPDF(ctrlCodeNo, ctrl2CodeNo);	//制御コード番号によって文字倍率を設定	//2015/10/29 kawada add
					if(ret != HPDF_OK){
						ret = 1;
						return ret;
					}

					showText = showText + strlen(twoPzero);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,twoWidth,strlen(twoWidth)) == 0){
					//横2倍文字
					ctrl2CodeNo = 3;		//2015/10/29 kawada add
					if(ret != HPDF_OK){
						ret = 1;
						return ret;
					}

					//もとに戻すためのフラグ
					twoInfo = 0x01;
					showText = showText + strlen(twoWidth);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}
				}else if(strncmp(showText,twoHeight,strlen(twoHeight)) == 0){
					//縦2倍文字
					ctrl2CodeNo = 4;		//2015/10/29 kawada add
					//制御コード番号によって文字倍率を設定
					ret = setcharSizeofHPDF(ctrlCodeNo, ctrl2CodeNo);
					if(ret != HPDF_OK){
						ret = 1;
						return ret;
					}

					//もとに戻すためのフラグ
					twoInfo = 0x02;
					showText = showText + strlen(twoHeight);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,twoProduct,strlen(twoProduct)) == 0){
					//2倍文字
					ctrl2CodeNo = 5;
					if(ret != HPDF_OK){
						ret = 1;
						return ret;
					}

					//もとに戻すためのフラグ
					twoInfo = 0x03;
					showText = showText + strlen(twoProduct);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,twoProductReset,strlen(twoProductReset)) == 0){
					if(twoInfo == 0x01){
						//横方向リセット
						ctrl2CodeNo = 6;
						//制御コード番号によって文字倍率を設定
						ret = setcharSizeofHPDF(ctrlCodeNo, ctrl2CodeNo);
						if(ret != HPDF_OK){
							ret = 1;
							return ret;
						}
					}
					if(twoInfo == 0x03){
						//2倍文字リセット
						ctrl2CodeNo = 7;		//2015/10/29 kawada add
						//制御コード番号によって文字倍率を設定
						ret = setcharSizeofHPDF(ctrlCodeNo, ctrl2CodeNo);
						if(ret != HPDF_OK){
							ret = 1;
							return ret;
						}
					}
					if(twoInfo == 0x02 || twoInfo == 0x03){
						//縦方向リセット
						ctrl2CodeNo = 8;
						//制御コード番号によって文字倍率を設定
						ret = setcharSizeofHPDF(ctrlCodeNo, ctrl2CodeNo);
						if(ret != HPDF_OK){
							ret = 1;
							return ret;
						}
					}
					//制御コードを除去
					showText = showText + strlen(twoProductReset);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}
				}else if(strncmp(showText,gridHrizonStart,strlen(gridHrizonStart)) == 0){
					//横罫線スタート
					//definition
					HPDF_Point startPoint;
					int distance = 0;
					HPDF_REAL xpoint;
					HPDF_REAL ypoint;
					HPDF_REAL fontSize;

					//罫線のスタートポイントを取得
					startPoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);
					//終了位置を計算
					fontSize = HPDF_Page_GetCurrentFontSize(PR_page[PR_currentPage]);
					distance = (int)(strstr(showText,gridHrizonEnd) - showText) * fontSize;
					//後で描画するために格納
					drawArray[drawCount].startXPoint = curPos;		//2015/10/29 kawada add
                    drawArray[drawCount].startYPoint = startPoint.y - (PR_lineheight / 2) + 0.2;		//2015/10/29 kawada add
					//2015/08/27 kawada del 終了一編集時に指定 drawArray[drawCount].endYPoint   = startPoint.y;
					drawArray[drawCount].type       = 'H';
					drawCount_start = drawCount;	//2015/08/27 kawada add 終了位置指定時に編集するため、現在のdrawArray件数を保存
					drawCount++;

					//制御コードを除去
					showText = showText + strlen(gridHrizonStart);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,gridHrizonEnd,strlen(gridHrizonEnd)) == 0){
					//横罫線エンド
					//終了位置の編集等はこちらで行う
					HPDF_Point endPoint;

					//罫線のエンドポイントを取得
					endPoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);
					//後で描画するために格納
					drawArray[drawCount_start].endXPoint = curPos;
					drawArray[drawCount_start].endYPoint = endPoint.y - (PR_lineheight / 2) + 0.2;
					//2015/08/27 kawada add E

					//横罫線はスタートとエンドでセット
					showText = showText + strlen(gridHrizonEnd);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else if(strncmp(showText,gridVertStart,strlen(gridVertStart)) == 0){
					//縦罫線スタート
					//definition
					HPDF_Point startPoint;
					HPDF_REAL xpoint;
					HPDF_REAL ypoint;
					HPDF_REAL fontSize;

					//罫線のスタートポイントを取得
					startPoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);

					//終了位置を計算
					fontSize = HPDF_Page_GetCurrentFontSize(PR_page[PR_currentPage]);
					//後で描画するために格納
					drawArray[drawCount].startXPoint = curPos;		//2015/10/29 kawada add
					drawArray[drawCount].endXPoint   = curPos;		//2015/10/29 kawada add
					drawArray[drawCount].startYPoint = (startPoint.y - (PR_lineheight / 2));	//2015/10/29 kawada add
					drawArray[drawCount].endYPoint   = (startPoint.y + fontSize) + 0.2;		//2015/10/29 kawada add

					//2015/10/29 kawada add S
					//倍字指定が2行分使用(縦倍・2倍文字指定)するとき
					//制御コード番号の確認
					int wctrl2CodeNo = 0;
					wctrl2CodeNo = ctrl2CodeNo;
					if(wctrl2CodeNo == 0){		//現在の制御コード番号に指定がなければ
						//以前の制御コード番号(制御コード番号保存変数から)を使用
						wctrl2CodeNo = PR_ctrl2CodeNoSave;
					}
					if(wctrl2CodeNo == 4 || wctrl2CodeNo == 5){		//縦倍・2倍文字指定(制御コード番号=4, 5)
						//縦罫線を1行分上に長く引く
						drawArray[drawCount].endYPoint += PR_lineheight;
					}
					//2015/10/29 kawada add E

					drawArray[drawCount].type        = 'V';
					drawCount++;

					//制御コードを除去
					showText = showText + strlen(gridVertStart);
					//制御コードが連続していたらcontinue;
					if(*(showText + 0) == 0x1A){
						continue;
					}

				}else{
					cob_runtime_error(
						" Error C [%02d]:Print Error %2x:%2x:%2x:%2x:%2x:%2x:%2x:%2x:",51
						,showText[0],showText[1],showText[2],showText[3],showText[4],showText[5],showText[6],showText[7]);
					ctrlCodeNo = 0;		//2015/10/29 kawada add
					return 1;
				}
				//横の最大は136なはず
				char strtmp[256]="";
				if(strchr(showText,0x1A) != NULL){
					strncpy(strtmp,showText,(strchr(showText,0x1A) - showText));
				}else{
					strcpy(strtmp,showText);
				}
//				//添付に入れたテキストを表示
			}else{													//＜If ～ Else(showTextカレントポインタ位置に制御コードがあるときの処理)＞
				//横の最大は136なはず
				char strtmp[256]="";
				if(strchr(showText,0x1A) != NULL){
					strncpy(strtmp,showText,(strchr(showText,0x1A) - showText));
					showText = strchr(showText,0x1A);
				}else{
					//ここはいらない？
					strcpy(strtmp,showText);
				}
				//①制御コード番号により半角・全角文字を考慮して表示(印字)する
				ret = setstrCharRate(ctrlCodeNo, ctrl2CodeNo, strtmp, &curPos);
				if(ret != HPDF_OK){
					ret = 1;
					return ret;
				}
				//2015/10/29 kawada add E
				showText += (strchr(showText,0x1A) - showText);
				//制御コードがなくなったらブレイク(whileの条件的にいらないか)
				if(strchr(showText,0x1A) == NULL){
					break;
				}
			}													//＜End If(showTextカレントポインタ位置に制御コードがあるときの処理)＞
		}														//＜End while(showTextに制御コードが含まれている間のループ)＞
	}else{														//＜showTextに制御コードが含まれていない↓＞
		//		//制御コードが何もないとき
		//		HPDF_Page_ShowText(PR_page[PR_currentPage],showText);
	}

	//②制御コード番号により半角・全角文字を考慮して表示(印字)する
	ret = setstrCharRate(ctrlCodeNo, ctrl2CodeNo, showText, &curPos);
	if(ret != HPDF_OK){
		ret = 1;
		return ret;
	}

	//罫線の描画
	if(drawCount > 0){
		//definition
		int intCurCount = 0;
		HPDF_REAL dblTargDistX,dblTargDistY;
		HPDF_Point currentPoint;

		//いったん描画モードに
		HPDF_Page_EndText (PR_page[PR_currentPage]);
		//初期値に移動
		HPDF_Page_MoveTo(PR_page[PR_currentPage],basePoint.x,basePoint.y);

		//罫線を1本ずつ引く
		for(intCurCount = 0;intCurCount < drawCount;intCurCount++){
			if(drawArray[intCurCount].type == 'H' || drawArray[intCurCount].type == 'V'){
				//横罫線
				//縦罫線 endXPoint,endYPointが違うので区別されているはず
				HPDF_Page_MoveTo(PR_page[PR_currentPage], drawArray[intCurCount].startXPoint, drawArray[intCurCount].startYPoint);
				//罫線を描画
				HPDF_Page_LineTo(PR_page[PR_currentPage],drawArray[intCurCount].endXPoint,drawArray[intCurCount].endYPoint);
				currentPoint = HPDF_Page_GetCurrentPos(PR_page[PR_currentPage]);
			}
		}
		HPDF_Page_Stroke (PR_page[PR_currentPage]);
		//元の位置に戻す
		ret = HPDF_Page_BeginText(PR_page[PR_currentPage]);
		currentPoint = HPDF_Page_GetCurrentTextPos(PR_page[PR_currentPage]);
		ret = HPDF_Page_MoveTextPos(PR_page[PR_currentPage],  basePoint.x, basePoint.y);
	}

	//フォント設定だけを分割
	ret = fontSetting();


	return ret;
}

//pdfに行を書き込み
int PR_Write(char *linetext){
	//プリンタの横の最大は138前後のはず
	char *tempStr;
	char size_e[1];
	int ret=0;
	FILE *fp;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Write :");

	if(prMyConfDebugFlg >= 10){
		cob_runtime_error(" Error [%02d]: %s Info PR_Write S ",66,local_server_time(strTime));
	}
	//改ページ処理のない場合を考慮
	if (PR_currentLine > PR_linepitch){
		//現在行が１ページの行数をこえていたら改ページ処理(PR_NewPage)を呼ぶ
		ret = PR_NewPage();
		if (ret != 0){
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
	}
	
	tempStr = malloc((cob_current_module->cob_procedure_parameters[0]->size * sizeof(size_e[0])) + 1);
	memset(tempStr,0x00,cob_current_module->cob_procedure_parameters[0]->size + 1);
	memcpy(tempStr,linetext,cob_current_module->cob_procedure_parameters[0]->size);
	//ドットプリンタ対応
	//if(PR_Lineprint == 1){
	//	//ドット
	//	char outStr[256]="";
	//	iconv_t ic;
	//	char    *ptr_in  = tempStr;
	//	char    *ptr_out = outStr;
	//	size_t  mybufsz = (size_t) 256;
	//	//文字コード変換
	//	ic = iconv_open("ISO-2022-JP", "SJIS");
	//	iconv(ic,&ptr_in,&mybufsz,&ptr_out,&mybufsz);
	//	iconv_close(ic);

	//	fp = fopen(PR_bindname,"a");  //追加書込み
	//	if(fp != NULL){
	//		fprintf(fp,"%s\n",outStr);
	//		fclose(fp);
	//	}
	//}else{
		ret = setAttrAndPrint(tempStr);
		if(ret == 0){
			//一行書いたら、その時の設定で改行を入れる
			HPDF_REAL lineheight,pageheight;
			pageheight = HPDF_Page_GetHeight(PR_page[PR_currentPage]);
			//上下のマージンだが、調整のため1.8倍とする
//			lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / PR_linepitch );
			
			if(PR_Lineprint == 1){
				lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 66 * 1.39 );
			}

			if(PR_Lineprint != 1 && PR_pagesize == HPDF_PAGE_SIZE_A5){
				lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 33.7 );
			}else if(PR_Lineprint != 1){
			lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / 66 );
			}
			
			
			//半角だから/2
			ret = HPDF_Page_SetTextLeading(PR_page[PR_currentPage],(lineheight / 2));
		}
		HPDF_Page_MoveToNextLine(PR_page[PR_currentPage]);
		//一行書いたらfontとサイズをリセット
		ret = fontSetting();
	//}
	free(tempStr);
	PR_currentLine++;

	if(prMyConfDebugFlg >= 10){
		cob_runtime_error(" Error [%02d]: %s Info PR_Write E ",66,local_server_time(strTime));
	}
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//改行
//
int PR_LineFeed(char *linecount){
	int linenum;
	//8桁以上はないはず
	char strNum[8];
	int ret=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_LineFeed :");

	memset(strNum,0x00,8);
	memcpy(strNum,linecount,cob_current_module->cob_procedure_parameters[0]->size);
	linenum = atoi(strNum);

	for(linenum;linenum > 1;linenum--){
		//改ページ処理のない場合を考慮
		if (PR_currentLine > PR_linepitch){
			//現在行が１ページの行数をこえていたら改ページ処理(PR_NewPage)を呼ぶ
			ret = PR_NewPage();
			if (ret != 0){
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return ret;
			}
		}
		HPDF_Page_ShowTextNextLine(PR_page[PR_currentPage],"  ");
		HPDF_Page_MoveToNextLine(PR_page[PR_currentPage]);
		PR_currentLine++;
	}
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//現在の行数を返す
int PR_Get_Linage(){
	return PR_currentLine;
}
