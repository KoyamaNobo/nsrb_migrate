#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <mysql.h>
#include <time.h>
#include <math.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/

//DBのタグ名
#ifndef CONF_DB_TAG
#define CONF_DB_TAG
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
const char* CONF_DB_PORT = "dbPort";
const char* MANA_TBL_NAME = "dbTMName";
const char* DEBUG_FLGNAME = "debug";
#endif
#ifndef CONF_PATH
#define CONF_PATH
char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";	//20150915ローカルに conf.xml がないときの対応
char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif

#define mysql_failure() __mysql_failure(__func__, __LINE__)

//定数
#define DEBUG_FLG 1
#define WK_INTSTR_LEN 11
const int  NSHIFT     = 4;		// 引数開始文字判定用の移動距離
const int  ARGITM     = 5;		// 引数構造体の要素数
const char* TYPE_CHAR  = "C";    //文字列定数
#define OPER_LEN 4		// 演算子の文字列長
//const int  ITEMLST    = 20;		// SORT対象構造体の要素数
#define ITEMLST 256		// SORT対象構造体の要素数
#define SQLSTR 2048		// SQL文字列の最大長
#define LOGICAL_DEF -1
#define LOGICAL_AND 0
#define LOGICAL_OR 1
#define PATHNAME_SIZE 1024
#define QUERYMAX_SIZE 32768
#define DATA_MAX_LEN 4096

//SORT対象の引数構造体
struct argv_inf {
	int s_point;	// 開始位置
	int length;	// 文字列長
	char *type;		// 出力タイプ
	char *operand;	// 比較対象フィールド
	char *value;	// 出力フィールド構成
	bool  sumflg;	// 集計項目フラグ
	//仕様の誤りのため追加 add koyama
	int   logical;		// 論理演算子 0:AND,1:OR,-1:その他
};
typedef struct argv_inf Argv_inf;
//argv_inf={"0","0","","","",false}

//SORT対象構造体
struct trgt_inf {
//	各引数値の最大文字列長
//	char ide[3];	// 入力対象装置　MSD:磁気ディスク　FDU:フロッピー
//	char ifi[12];	// 入力ファイル名
//	char ofi[12];	// 出力ファイル名
//	char key[56];	// 比較対象フィールド
//	char out[120];	// 出力フィールド構成
//	char sum[120];	// 集計対象フィールド
//	char sel[120];	// where句

	char     *ide;			// 入力対象装置　MSD:磁気ディスク　FDU:フロッピー
	char     *ifi;			// 入力ファイル名
	char     *ofi;			// 出力ファイル名
	Argv_inf key[ITEMLST];	// 比較対象フィールド
	Argv_inf out[ITEMLST];	// 出力フィールド構成
	Argv_inf sum[ITEMLST];	// 集計対象フィールド
	Argv_inf sel[ITEMLST];	// where句
	//仕様の誤りのため削除 add koyama
//	int      logical;		// 論理演算子 0:AND,1:OR,-1:その他
};
typedef struct trgt_inf Trgt_inf;

//文字列構造体
struct string_t{
	size_t size;   // 文字列の長さ (allocateは+1でとること)
	char   *data;  // データの先頭 ->文字列ポインタに変更
};
typedef struct string_t String_t;

//DB設定構造体
struct db_setting {
	char db_server[64];        // host名
	char db_user[64];          // user名
	char db_password[64];      // password
	char db_database[64];      // database名
	int db_port;               // port番号
	char db_manatbl[64];       // 管理table名	//2015/08/28 kawada add 管理テーブル名
}db_setting={"","","","",3306};
typedef struct db_setting DB_SETTING;

//------------------------------------------------------プロトタイプ宣言(極力全てを宣言するように変更)
//--------構造体系--------
void argv_ini(Argv_inf * );
void trgt_ini(Trgt_inf * );
//--------common系--------
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
char *local_server_time(char*);
//プロセス名を返す
int getUserAndProcessName(char *,char *);
//設定ファイルのパスを取得
char *getConfFilename(char *);
//文字列中の指定文字の数をカウント
int searchChr(char * ,char );
//特定の文字を置換する
void chgChar(char *,char ,char ,char );
//特定の文字を除去しながらつなげる
void remTargChar(char *,char );
//特定の文字を除去しながらつなげる
void remCRLF(char *);

//エラーにプロセス名を付けて出力
void mytool_runtime_error(char ,const char *,const char *, ...);
//スプリット関数(上記関数では空文字列に対応しないので作り変え)
int strSplit( char *, char *, char ** );
//スプリット関数(要素分解「)」検索) &[@]<&[)]が前提
int strSplit_ELM( char *, char *[] );
//--------データベース系--------
//設定の読み込みとデータベースへの接続を行う
int db_init(DB_SETTING *);
//グローバル変数から設定を取得し、データベースに接続する
int DB_Open(DB_SETTING *);
//設定の値を使ってデータベースを閉じる
int DB_Close();
//SQL実行エラー時の後処理
int __mysql_failure(const char *funcName,int funcLine);
//SQL発行
int mysql_run(char * iquery);
//テーブル名check
char *check_tablename(char *);
// ユニークID生成
int getUid();
//処理対象ファイルのLOCK処理
int setTableLock(DB_SETTING *, Trgt_inf *,char *);
//処理対象ファイルのUNLOCK処理
int setTableUnlock(DB_SETTING *, Trgt_inf *);
//実際の処理を受け持つ
int mysql_Init(Trgt_inf *,DB_SETTING *);
//------------------------------------------------------global変数の定義
//confから取得するdebug_flg.他と被らないようにoriginal name
static int  myConfDebugFlg;
static char source_file_name[PATHNAME_SIZE];
static char source_user_name[PATHNAME_SIZE];
int         unqId = 0;                  //ユニークID(管理テーブル認証用)		//2015/08/28 kawada add

MYSQL *mysqlConn;


DB_SETTING *background_targSetting;
Trgt_inf   *background_itrgt;

//------------------------------------------------------関数
//--------構造体系--------
//*********************************************************
//SORT対象構造体の初期化
//*********************************************************
void argv_ini(Argv_inf *ioargv ) {
//	"開始位置", "文字列長", "出力ファイル名", "比較対象フィールド", "出力フィールド構成", false
//	Argv_inf wk_argv ={0,0,"","","",false};
	ioargv->s_point = 0;
	ioargv->length  = 0;
	ioargv->type    = 0;          // NULLポインタで初期化
	ioargv->operand = 0;          // NULLポインタで初期化
	ioargv->value   = 0;          // NULLポインタで初期化
	ioargv->sumflg  = false;      // FALSEで初期化
	ioargv->logical = 0;          // 論理演算子 0:AND,1:OR,-1:その他
}


//*********************************************************
//SORT対象構造体の初期化
//*********************************************************
void trgt_ini(Trgt_inf *iotrgt ) {
	int ii = 0;
	//初期化内容
//	char     *wk_ide = "";		// 入力対象装置　MSD:磁気ディスク　FDU:フロッピー
//	char     *wk_ifi = "";		// 入力ファイル名
//	char     *wk_ofi = "";		// 出力ファイル名
//	Argv_inf *wk_key[ITEMLST];	// 比較対象フィールド
//	Argv_inf *wk_out[ITEMLST];	// 出力フィールド構成
//	Argv_inf *wk_sum[ITEMLST];	// 集計対象フィールド
//	Argv_inf *wk_sel[ITEMLST];	// where句
//	int       wk_logical = LOGICAL_AND;	// 論理演算子 0:AND,1:OR,-1:その他
	Trgt_inf wktrgt;

	*iotrgt = wktrgt;

	iotrgt->ide = NULL;    //NULLPOで初期化
	iotrgt->ifi = NULL;    //NULLPOで初期化
	iotrgt->ofi = NULL;    //NULLPOで初期化

	for(ii = 0; ii < ITEMLST ; ii++){
//		iotrgt->key[ii] = malloc(sizeof(Argv_inf));
//		iotrgt->out[ii] = malloc(sizeof(Argv_inf));
//		iotrgt->sum[ii] = malloc(sizeof(Argv_inf));
//		iotrgt->sel[ii] = malloc(sizeof(Argv_inf));
		argv_ini(&iotrgt->key[ii]);
		argv_ini(&iotrgt->out[ii]);
		argv_ini(&iotrgt->sum[ii]);
		argv_ini(&iotrgt->sel[ii]);
	}

//	iotrgt->logical = LOGICAL_AND;

}

//RTrim実装
//(それぞれにRtimがあるのでここでも専用を書き直す)
//date:20160520
//auth: koyama
int isNullOrEmpty(char *targ){
	char *strstart;
	char *strend;
	int num=0;
	strstart = targ;
	//最後の文字から判定対象
	strend = targ + strlen(strstart);
	for(strend--;strend >= strstart;strend--){
		if(*strend == ' '){
			num++;
		}
	}
	//全てがスペースなら
	if(num == strlen(strstart)){
		num = 1;
	}else{
		num = 0;
	}
	return num;
}

//
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
//in/out :retStr 返す対象文字列のポインタ
//author : koyama
char *local_server_time(char *retStr){
	struct timeval timer;
	struct tm *date;

	gettimeofday(&timer, NULL);
	date = localtime(&timer.tv_sec);
	sprintf(retStr,"%02d/%02d/%02d %02d:%02d:%02d.%06d"
		,date->tm_year+1900
		,date->tm_mon+1
		,date->tm_mday
		,date->tm_hour
		,date->tm_min
		,date->tm_sec
		,timer.tv_usec);
	return retStr;
}

//設定ファイルのパスを取得
//date:20150828
//auth: koyama
//カレントdirのconfだけを開くとどこででも実行できないので
char *getConfFilename(char *strConfPath){
	char strTime[] = "0000/00/00 00:00:00.000000";
	FILE *fpFileExist;      //20150828 add koyama

	if((fpFileExist = fopen(CONF_FLIEPATH, "r")) == NULL){
		if((fpFileExist = fopen(CONF_SHREPATH, "r")) == NULL){
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

//文字列中の指定文字の数をカウント
//in :haystack 検索対象の文字列,needle 指定文字
//out: retval文字列中に含まれる指定文字の数
//author : koyama
int searchChr(char *haystack ,char needle){
	int retval=0;
	char *pstr;
	pstr = haystack;
	while(*pstr != '\0'){
		if(*pstr == needle){
			retval += 1;
		}
		pstr++;
	}
	return retval;
}

//特定の文字を置換する
//origText:対象文字列,target:対象文字
//,replacement:置換後文字,splitterこれで囲まれた中だけ判定。指定しないときは0
//return なし
//author:koyama
void chgChar(char *origText,char target,char replacement,char splitter){
	char *origTextEnd;
	char *copyText;
	char elemflg = 0x00;
	char splitchar = 0x00;

	splitchar = splitter;
	if(splitchar == 0x00){
		//splitterに空を指定されたときは飛ばす仕様
		elemflg = 0x01;
	}

	copyText  = origText;
	//\0までをコピーしたい
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;copyText < origTextEnd;copyText++){
		//改行文字を飛ばす
		if(splitchar == *copyText){
			if(elemflg == 0x00){
				elemflg = 0x01;
			}else{
				elemflg = 0x00;
			}
		}
		if(elemflg != 0x00 && *copyText == target){
			//こちらに入ったときは余分に+
			*copyText = replacement;

		}
	}
}

//特定の文字を除去しながらつなげる
//return なし
void remTargChar(char *origText,char targ){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0までをコピーしたい
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//改行文字を飛ばす
		if(*copyText == targ ){
			//こちらに入ったときは余分に+
			copyText++;
		}
		//文字のコピー
		*origText = *copyText;
		copyText++;
	}
}


//特定の文字を除去しながらつなげる
//return なし
void remCRLF(char *origText){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0までをコピーしたい
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//改行文字を飛ばす
		if(*copyText == 0x0A || *copyText == 0x0D){
			//こちらに入ったときは余分に+
			copyText++;
		}
		//文字のコピー
		*origText = *copyText;
		copyText++;
	}
}



//
//設定の読み込みとデータベースへの接続を行う
//IN  ： targSetting->データベース接続情報
//OUT ： なし
//author:koyama
int db_init(DB_SETTING *targSetting){
	int result=0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	if(conf_read(targSetting)){
		mytool_runtime_error('E',source_file_name," Error C [%02d] :can't read configuration file           %s ", 01, local_server_time(strTime));
		result = 1;
	}else{
		if(DB_Open(targSetting)){
			result = 1;
		}
	}
	//エラーになった時ようにコピーをとっておく
	background_targSetting = targSetting;

	return result;
}

//グローバル変数から設定を取得し、データベースに接続する
//IN ：なし
//OUT：0正常
//     1異常
//author:koyama
int DB_Open(DB_SETTING *targSetting){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	char strAutocommit[] = "SET AUTOCOMMIT=0;";
	mysqlConn = mysql_init(NULL);

	if (!mysql_real_connect(mysqlConn, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0)) {
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Connect Error %s                    %s", 02,mysql_error(mysqlConn),local_server_time(strTime));
		//接続不可
//		raise(SIGABRT);
		ret = 1;
	}else{
	    //接続可
	    ret = 0;
	}
	//仕様としてSJISしかダメなので
	if(mysql_options(mysqlConn, MYSQL_SET_CHARSET_NAME, "SJIS") != 0){
		mytool_runtime_error('E',source_file_name, " Error C [%02d]:db init Error %s :%s\n",34, local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}

	//AutoCommit Off
	ret = mysql_query(mysqlConn, strAutocommit);

	unqId = getUid();		//2015/08/28 kawada add 管理テーブルの更新用にユニークIDを取得する

    return ret;
}

//設定の値を使ってデータベースを閉じる
//IN ：なし
//OUT：0正常
//     1異常
//author:koyama
int DB_Close(){
	int ret =0;
	//DB接続の切断

	// 管理テーブル用のコネクションエンド
	setTableUnlock(NULL,NULL);
	//DB_initを通っていることを確認
	if(background_targSetting != NULL){
		//return voidなので必ず0で返す
		mysql_close(mysqlConn);
	}
	return ret;
}
//*********************************************************
//SQL実行エラー時の後処理
// upd koyama function名と行数が出るように修正
//*********************************************************
int __mysql_failure(const char *funcName,int funcLine){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	//エラー内容を出力
	mytool_runtime_error(' ',source_file_name," Error C [%02d]: Query Error %20s %s:%d %s "
		,99,mysql_error(mysqlConn),funcName,funcLine,local_server_time(strTime));

	//ロールバック
	ret = mysql_query(mysqlConn, "ROLLBACK");

	return ret;
}

//*********************************************************
//SQL発行
//*********************************************************
int mysql_run(char * iquery){
	int ret = 0;
	int query_size = 0;

	query_size = strlen(iquery);

//	printf("mysql_run :%s\n",iquery);	//デバッグ用
	ret = mysql_real_query(mysqlConn, iquery,query_size);

	if(ret != 0){
		mysql_failure();
		ret = 1;
	}

	return ret;
}

//
//テーブル名check
//add commment テーブル名の前後に「`」を付加している
char *check_tablename(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="`";

	if(strlen(strTable) == 0){

	}
	if(*(strTable + 0) != 0x60){
		strpTarg  = strTable;
		//一文字目は決まっているので
		strpTemp = (strTemp + 1);
		while(*strpTarg != '\0'){
			if(*strpTarg != ' '){
				*strpTemp = *strpTarg;
				strpTemp++;
			}
			strpTarg++;
		}
		//最後に'`'を追加
		*strpTemp = 0x60;
		*(strpTemp + 1) = '\0';
		strcpy(strTable,strTemp);
	}
	return strTable;
}

//
//テーブル名check
//add commment テーブル名の前後に「`」を付加している
char *convertTablenameToVariable(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="";

	if(strlen(strTable) == 0){

	}
	if(*(strTable + 0) == 0x60){
		//2文字目から
		strpTarg  = strTable + 1;
		strpTemp = (strTemp);
		while(*strpTarg != '\0'){
			if(*strpTarg != ' '){
				*strpTemp = *strpTarg;
				strpTemp++;
			}
			strpTarg++;
		}
		//最後の'`'を除去
		if(*(strpTemp - 1) == 0x60){
			*(strpTemp - 1) = '\0';
		}
		*(strpTemp) = '\0';
		strcpy(strTable,strTemp);
	}
	return strTable;
}


// -----------------------------------------------------Lock,Unlock Start
//##############################################################################
//#####  F I L E  O P E N  L O C K / U N L O C K ###############################
//##############################################################################
//##############################################################################
//##############################################################################
// ユニークID生成
//返値：生成された、一意のID
int getUid(){

	unsigned int now = (unsigned int)time( 0 );
	srand( now );
	// srand関数で、乱数パターンを初期化する
	// プログラム実行ごとに異なるパターンが使える

	return rand();
}
//処理対象ファイルのLOCK処理
//引数：targSetting	DB設定構造体
//引数：itrgt		受け取り引数の情報構造体
//引数:lckTableName TableNameがin,outなので外から複数回読んでもらう
//返値：実行結果
int setTableLock(DB_SETTING *targSetting, Trgt_inf *itrgt,char *lckTableName){
	int intSub = 0, ret =0;
	int nn =0;
	char sqlStr[256]="";
	MYSQL_ROW	res;
	MYSQL_RES	*result;
	MYSQL_ROW	inres;
	MYSQL_RES	*inResult;
	char strUid[16]="";
	char strSS[8]="";
	char strTime[] = "0000/00/00 00:00:00.000000";	//時間の文字列を格納

	//管理テーブル更新用トランザクション
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_query(mysqlConn, sqlStr) != 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:can't run the Transaction %s %s  %s ", 81, targSetting->db_manatbl, mysql_error(mysqlConn), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	// 対象テーブルのロック状態確認(ロック無視)
	//TableNameがin,outなので
	strcat(sqlStr,"SELECT LockMode,Uid FROM `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` WHERE TableName = '");
	strcat(sqlStr,lckTableName);
	strcat(sqlStr,"';");

	if(mysql_query(mysqlConn, sqlStr) != 0){
		mysql_failure();
		ret = 1;
		return ret;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	if(result = mysql_store_result(mysqlConn)){
		int rr=0;

		rr=mysql_num_rows(result);

		// 文字列初期化
		memset(sqlStr,'\0',strlen(sqlStr));

		//UUIDを初期セット
		sprintf(strUid,"%d", unqId);

		//openModeは固定にしておく
		//lckTableNameがifiと一致していたらifi,そうでなければofi
		if(strcmp(lckTableName,itrgt->ifi) == 0){
			sprintf(strSS,"%s", "SI");
		}else{
			sprintf(strSS,"%s", "SE");
		}

		//先行の処理がなければそのまま接続
		if(rr == 0){
			// 管理テーブルの登録
			strcat(sqlStr,"INSERT INTO `");
			strcat(sqlStr,targSetting->db_manatbl);
			strcat(sqlStr,"` ( TableName,Uid,LockMode,pg_name,user_name,LockDateTime) ");
			strcat(sqlStr,"VALUES ( '");
			strcat(sqlStr,lckTableName);
			strcat(sqlStr,"',");
			strcat(sqlStr,strUid);
			strcat(sqlStr,",'");
			strcat(sqlStr,strSS);
			strcat(sqlStr,"','");
			strcat(sqlStr,source_file_name);    //pg_name
			strcat(sqlStr,"','");
			strcat(sqlStr,source_user_name);    //pg_name
			strcat(sqlStr,"', now() );");
		}else{
			for(nn = 0;nn < rr;nn++){
				//
				char strDecision[5]="";
				res=mysql_fetch_row(result);

				//先行の判定用2文字を判定変数にコピー
				if (strcmp(res[0], "") != 0){
					memcpy(strDecision, res[0], 2);
				}else{
					memcpy(strDecision, "  ", 2);
				}
				memcpy((strDecision + 2), strSS, 2);
				//Lockできるか(Lockできない場合は処理不可)チェック
				if (
				    (*strDecision == 'E') ||
				    (
				     (*strDecision == 'P') &&
				     (
				      (strcmp(strDecision,"PIPI") != 0 && strcmp(strDecision,"PISI") != 0 &&
				       strcmp(strDecision,"P-SI") != 0 && strcmp(strDecision,"PESI") != 0)
				     )
				    ) ||
				    (
				     (*strDecision == 'S') &&
				     (
				      (strcmp(strDecision,"SIPI") != 0 && strcmp(strDecision,"SISI") != 0 &&
				       strcmp(strDecision,"SIP-") != 0 && strcmp(strDecision,"SIS-") != 0 &&
				       strcmp(strDecision,"SIPE") != 0 && strcmp(strDecision,"SISE") != 0 &&
				       strcmp(strDecision,"S-SI") != 0 && strcmp(strDecision,"S-S-") != 0 &&
				       strcmp(strDecision,"S-SE") != 0 && strcmp(strDecision,"SESI") != 0 &&
				       strcmp(strDecision,"SES-") != 0 && strcmp(strDecision,"SESE") != 0)
				     )
				    )
				   ){
					//先行が E(XCLUSIVE)ならLockできない
					//P(ROTECT)：(先行→後行)が(PI→PI)(PI→SI)(P-→SI)(PE→SI)以外はLockできない
					//S(HARE)：(先行→後行)が(SI→PI)(SI→SI)(SI→P-)(SI→S-)(SI→PE)(SI→SE)(S-→SI)(S-→S-)(S-→SE)(SE→SI)(SE→S-)(SE→SE)以外はLockできない
						 mytool_runtime_error('E',source_file_name," Error C [%02d]:alredy locked %s:%s %s ", 82, lckTableName, targSetting->db_manatbl, local_server_time(strTime));
					ret = 1;
					break;
				}else{
				//管理テーブルのLockModeの更新が必要かチェック
					if (
					    (*strDecision == ' ') ||
					    (
					     (*strDecision == 'P') &&
					     (
					      (strcmp(strDecision,"PIPI") == 0)
					     )
					    ) ||
					    (
					     (*strDecision == 'S') &&
					     (
					      (strcmp(strDecision,"SIPI") == 0 || strcmp(strDecision,"SISI") == 0 ||
					       strcmp(strDecision,"SIP-") == 0 || strcmp(strDecision,"SIS-") == 0 ||
					       strcmp(strDecision,"SIPE") == 0 || strcmp(strDecision,"SISE") == 0 ||
					                                          strcmp(strDecision,"S-S-") == 0 ||
					       strcmp(strDecision,"SES-") == 0 || strcmp(strDecision,"SESE") == 0)
					     )
					    )
					   ){
					   	//(空白)：空白(Lockなし)はLockMode置き換え
						//P(ROTECT)：(先行→後行)が(PI→PI)はLockMode置き換え
						//S(HARE)：(先行→後行)が(SI→PI)(SI→SI)(SI→P-)(SI→S-)(SI→PE)(SI→SE)(S-→S-)(SE→S-)(SE→SE)はLockMode置き換え
						// 管理テーブルの更新
						strcat(sqlStr,"UPDATE `");
						strcat(sqlStr,targSetting->db_manatbl);
						strcat(sqlStr,"` SET Uid = ");
						strcat(sqlStr,strUid);
						strcat(sqlStr,", LockMode = '");
						strcat(sqlStr,strSS);
						strcat(sqlStr,"', pg_name = '");
						strcat(sqlStr,source_file_name);
						strcat(sqlStr,"', user_name = '");
						strcat(sqlStr,source_user_name);
						strcat(sqlStr,"', LockDateTime = now() ");
						strcat(sqlStr," WHERE TableName = '");
						strcat(sqlStr,lckTableName);
						strcat(sqlStr,"' ");
						strcat(sqlStr,";");
					}
				}
			}
		}
		if(ret != 1 && *sqlStr != '\0'){
			// SQLの実行⇒COMMIT
			if(mysql_query(mysqlConn, sqlStr) != 0){
				mysql_failure();
				ret = 1;
				mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 82, lckTableName);
				return ret;
			}else{
				//使った変数をリセット
				memset(sqlStr,'\0',strlen(sqlStr));
				//コミット
				strcat(sqlStr,"COMMIT; ");
				if(mysql_query(mysqlConn, sqlStr) != 0){
					mysql_failure();
					mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 83, lckTableName);
					ret = 1;
					return ret;
				}
			}
		}
	}else{
		mytool_runtime_error('E',source_file_name," Error C [%02d]:%s", 84, mysql_error(mysqlConn));
		ret = 1;
	}
	mysql_free_result(result);

	return ret;
}

//処理対象ファイルのUNLOCK処理
//引数：targSetting	DB設定構造体
//引数：itrgt		受け取り引数の情報構造体
//返値：実行結果
int setTableUnlock(DB_SETTING *targSetting, Trgt_inf *itrgt){
	int ret =0,intSub = 0,ii = 0;
	char sqlStr[256]="";
	char strUid[16]="";
	MYSQL_RES	*result;
	char strTime[] = "0000/00/00 00:00:00.000000";	//時間の文字列を格納

	if(targSetting == NULL && itrgt == NULL){
		targSetting = background_targSetting;
		itrgt       = background_itrgt;
	}
	//BackGroundにもないときはUnlockする必要なし
	if(targSetting == NULL && itrgt == NULL){
		return 1;
	}

	//管理テーブル更新用トランザクション
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_ping(mysqlConn) && mysql_query(mysqlConn, sqlStr) != 0){
		//unLockでエラーに成った場合エラーのだしようが無い
		//mytool_runtime_error(source_file_name," Error C [%02d]:can't run the Transaction %s %s          %s ", 91, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	sprintf(strUid, "%d", unqId);

	//一度にifi,ofi共にLockが外れるように変更 upd koyama 20161108
	strcat(sqlStr,"UPDATE `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` SET Uid = NULL, LockMode = '");
	strcat(sqlStr, "    ");								//LOCKMODE DEFAULT VALUE
	strcat(sqlStr,"' , LockDateTime = now() ");
	strcat(sqlStr," WHERE Uid = ");
	strcat(sqlStr,strUid);
	strcat(sqlStr,";");

	// SQLの実行
	if(mysql_query(mysqlConn, sqlStr) != 0){
//		mysql_failure();
		ret = 1;
		return ret;
	}else{
		//使った変数をリセット
		memset(sqlStr,'\0',strlen(sqlStr));
		//コミット
		strcat(sqlStr,"COMMIT; ");
		if(mysql_query(mysqlConn, sqlStr) != 0){
			mysql_failure();
			//unLockでエラーに成った場合エラーのだしようが無い
//			mytool_runtime_error(source_file_name," Error C [%02d]:can't lock %s ", 92, itrgt->ifi);
			ret = 1;
			return ret;
		}
	}

	return ret;
}
// -----------------------------------------------------Lock,Unlock END

//--------common系--------
//エラーにプロセス名を付けて出力
//in/out :fmt 出力フォーマット,...フォーマットに対応する変数
//author : koyama
void mytool_runtime_error(char typeChar,const char *sfile,const char *fmt, ...){
	va_list ap;

	/* prefix */
	fprintf(stderr,"%s :",sfile);
	fflush (stderr);

	/* body */
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);
	va_end (ap);

	/* postfix */
	fputs ("\n", stderr);
	fflush (stderr);

	if(typeChar == 'E'){
		//DBUNLOCKを試行
		setTableUnlock(NULL,NULL);
	}
}

//プロセス名を返す
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
//in/out :setName プロセス名を返す文字列ポインタ in/out:setUser ユーザ名を返す文字列ポインタ ::::こちらの関数は文字列をオーバする場合を無視
//author : koyama
int getUserAndProcessName(char *setName,char *setUser){
	FILE  *fp;
	char  buf[1024];   //コマンドの受け取り用
	int   pid=0;
	char  cmdReturn[2048]="";
	char  cmd[1024]="";
	char  *cmdReturnP;
	char  *cmdReturnNexrP;
	pid = getpid();
	sprintf(cmd,"ps -p %d -o cmd= ",pid);

	if ( (fp=popen(cmd,"r")) ==NULL) {
		//コマンドの実行に失敗したら空文字列でreturn
		return 0;
	}
	//結果をsize分だけコピー
	//溢れ定義は必要？
	while(fgets(buf, 1024, fp) != NULL) {
		if((strlen(cmdReturn) + strlen(buf) ) > 2048){
			//コマンドの実行に失敗したら空文字列でreturn
			memset(cmdReturn,'\0',2048);
			return 0;
		}
		strcat(cmdReturn,buf);
	}
	pclose(fp);
	//まずひとつ目のスペースを探す
	cmdReturnP = strchr(cmdReturn,' ');
	if(cmdReturnP != NULL){
		strncpy(setName,cmdReturn,(cmdReturnP - cmdReturn));
		cmdReturnP = cmdReturnP + 1;
		cmdReturnNexrP = strchr(cmdReturnP,' ');
		if(cmdReturnNexrP != NULL){
			//Util専用仕様
			if(strncmp(cmdReturnNexrP,"STN",strlen("STN")) == 0){
				strncpy(setUser,cmdReturnP,(cmdReturnNexrP - cmdReturnP));
			}
		}else{
			strncpy(setUser,cmdReturnP,strlen(cmdReturnP));
			//改行が入る可能性がある ->あれば削除
			if(strchr(setUser, '\n') != NULL){
				*(strchr(setUser, '\n')) = '\0';
			}
		}
	}else{
		strncpy(setName,cmdReturn,strlen(cmdReturn));
	}

	return 1;
}

//*********************************************************
//スプリット関数(一文字対応)
//*********************************************************
int strSplit__( char *iStr, char *iDelim, char *oList[] ) {
	char	*tk;
	int		cnt = 0;

	tk = strtok( iStr, iDelim );
	while( tk != NULL && cnt < 1000 ) {
		oList[cnt++] = tk;
		tk = strtok( NULL, iDelim );
	}
	return cnt;
}

//
//スプリット関数(上記関数では空文字列に対応しないので作り変え)
//in/out :oList 区切った文字列の配列
//in     :対象文字列
//author : koyama
int strSplit( char *iStr, char *iDelim, char **oList ) {
	char	*tk;
	char    *preneedle,*needle,*postneedle;
	int		icnt = 0,ocnt=0;

	//token
	tk = iStr;

	//最初のtoken
	oList[ocnt] = tk;
	ocnt++;

	//最初のtokenで初期化しておく
	preneedle  = tk;
	needle     = tk;
	postneedle = tk;

	for(;*(preneedle) != '\0' && ocnt < 16384;ocnt++){
		//needleがなくなるまで
		for(icnt = 0; *(iDelim + icnt) != '\0';icnt++){
			postneedle = strchr(preneedle,*(iDelim + icnt));
			//検索文字が存在するなら検索文字列の連続を除去
			if(postneedle != NULL){
				while(*postneedle == *(postneedle + 1) ){
					*postneedle = '\0';
					postneedle++;
				}
			}
			if(icnt==0 || postneedle < needle){
				needle = postneedle;
			}
		}
		//使ったあとのままだったら抜ける
		if(needle == NULL){
			oList[ocnt] = NULL;
			break;
		}
		//現在のtokenを格納
		oList[ocnt] = (needle + 1);
		*needle = '\0';
		preneedle  = (needle + 1);
		//使ったあとのneedleをnullに
		needle = NULL;
	}
	return ocnt;
}

//*********************************************************
//スプリット関数(要素分解「)」検索) &[@]<&[)]が前提
//*********************************************************
int strSplit_ELM( char *iStr, char *oList[] ) {
	char	*tk;
	int		cnt = 0;
	int		str_flg = 0;
	char	*str1;            //「@」位置(初期位置は比較文字の先頭ポインタ)
	char	*str2;            //「)」位置(初期位置は比較文字の先頭ポインタ)

	str1 = iStr;
	str2 = strstr(iStr, ")");

  //最初の要素にしか対応しないものは削除 rem koyama
	// //文字列要素「@」囲みを検索
	// if(strstr(iStr, "@") != NULL){
	// 	if(str2 < strstr(iStr, "@")){
	// 		while(str1 > str2){
	// 			if(strstr(strstr(iStr, "@"),"@") != NULL){
	// 				str_flg = 1;
	// 				str1 = strstr(strstr(iStr, "@"),"@");
	// 			}
	// 		}
	// 	}
	// }

  //最初の一つを入れる処理？
	//区切り文字を検索
	if(str_flg == 1){
		while(str1 > str2){
			str2 = strstr(str2, ")");
		}
		tk = str2;
		oList[cnt] = tk;
    cnt++;
		tk = strtok( str2, ")" );
	}else{
		tk = strtok( iStr, ")" );
	}

  //ここで実際の分割
	while( tk != NULL && isNullOrEmpty(tk)==0 && cnt < 1000 ) {
		oList[cnt] = tk;
    cnt++;
		tk = strtok( NULL, ")" );
	}

	return cnt;
}

//*********************************************************
//文字列置換(バイナリデータ非対応)
//*********************************************************
int strChg(char *ioBuf, const char *iStr1, const char *iStr2){
	int		ret = 1;
	char	tmp[1024 + 1];
	char	*p;
	char	s = '\0';

	while ((p = strstr(ioBuf, iStr1)) != NULL) {
		//見つからなくなるまで繰り返す。pは旧文字列の先頭を指す
		*p = s;					// 元の文字列を旧文字列の直前で区切って
		p += strlen(iStr1);		// ポインタを旧文字列の次の文字へ
		strcpy(tmp, p);			// 旧文字列から後を保存
		strcat(ioBuf, iStr2);	// 新文字列をその後につなぎ
		strcat(ioBuf, tmp);		// さらに残りをつなぐ
		ret = 0;				// ヒットした場合に戻り値変更
	}

	return ret;
}
//*********************************************************
// 可変長文字列 *ioData を iStr で初期化する
//*********************************************************
int string_init(String_t *ioData, char const * iStr){
	int ret = 0;

	// size_t size = strlen(iStr);
	size_t size = QUERYMAX_SIZE;

	ioData->data = (char *)malloc(sizeof(char) * (size + 1) );
	/* 確保した領域をNULLクリア */
	ioData->size = 0;
	memset( ioData->data , '\0' , sizeof(char) * (size + 1));

	if(ioData->data != NULL){
		ioData->size = strlen(iStr);
		memcpy(ioData->data , iStr, ioData->size);
	}else{
		ret = 1;
	}

	return ret;
}
//*********************************************************
// 可変長文字列 *ioData を破棄する
//*********************************************************
int string_fin(String_t *ioData){
	memset(ioData->data,'\0',strlen(ioData->data));
	free(ioData->data);
	return 0;
}

//文字列がすべて数字か
//ioTrgt:対象文字列
//return エラー1,正常0
//author:koyama
int isStrDigit(char *strTarg){
	int ret=0;
	char *temp;
	temp = strTarg;

	while(*temp != '\0'){
		if(isdigit(*temp) == 0){
			ret = 1;
			break;
		}
		temp++;
	}

	return ret;
}

//
//対象のオプションの要素数のチェック
//input:対象のオプション名が入った文字列ポインタ,対象のオプションの要素数
//author:koyama
int checkParamNum(char *strTarg,int intNum){
	int ret=0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//KEY
	if(strncmp(strTarg,"KEY",strlen("KEY")) == 0 && (intNum > 4)){
		fprintf(stderr," Error C [%02d]:Too many arguments to %.3s %s \n",21,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}

	//KEY
	if(strncmp(strTarg,"KEY",strlen("KEY")) == 0 && (intNum < 3)){
		fprintf(stderr," Error C [%02d]:Too few arguments to %.3s %s \n",21,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}

	//SEL
	if(strncmp(strTarg,"SEL",strlen("SEL")) == 0 && (intNum > 5)){
		fprintf(stderr," Error C [%02d]:Too many arguments to %.3s %s \n",22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}
	if(strncmp(strTarg,"SEL",strlen("SEL")) == 0 && (intNum < 5)){
		fprintf(stderr," Error C [%02d]:Too few arguments to %.3s %s \n",22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}
	//SUM
	if(strncmp(strTarg,"SUM",strlen("SUM")) == 0 && (intNum < 2)){
		fprintf(stderr," Error C [%02d]:Too few arguments to %.3s %s \n",22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}
	if(strncmp(strTarg,"SUM",strlen("SUM")) == 0 && (intNum > 3)){
		fprintf(stderr," Error C [%02d]:Too many arguments to %.3s %s \n",22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}

	//OUT
	if(strncmp(strTarg,"OUT",strlen("OUT")) == 0 && intNum > 2){
		fprintf(stderr," Error C [%02d]:Too many arguments to %.3s %s \n",23,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}

	return ret;
}

//type判断
//input:対象のtypeが入った文字列ポインタ
//author:koyama
int checkType(char *strOpt ,char *strTarg){
	int ret=0;
	char *pt;
	pt = strTarg;

	//SUMの時
	if(strncmp(strOpt,"SUM",strlen("SUM")) == 0){
		while(*pt != '\0'){
//kawada			if(*pt != 'P' && *pt != 0x20 ){
			if(*pt != 'P' && *pt != 'Q' && *pt != 0x20 ){		//kawada
				ret=1;
			}
			pt++;
		}
	}

	//SELの時
	if(strncmp(strOpt,"SEL",strlen("SEL")) == 0){
		while(*pt != '\0'){
			if(*pt != 'C' && *pt != 'N'
				&&  *pt != 'K'
				&&  *pt != 'S' && *pt != 'P'
				&&  *pt != 'Q' && *pt != 'T'
				&& *pt != 0x20 ){
				ret=1;
			}
			pt++;
		}
	}
	return ret;
}


//
//括弧の対応をcheck
//input:対象文字列,検索キーを文字列にしたものn,n+1番目が組み合わせとなる
//文字列にnが来たら次のnn+1が来れば正,来なければ否
//n+1が来る前にnnが来たら再帰的に次のnn+1を探す
//author:koyama
int checkParentheses(char *haystack,char *needles){
	if(needles == NULL){
		return 1;
	}
	if(haystack == NULL){
		return 1;
	}
	int targStrLen = strlen(haystack);
	int pairSize = strlen(needles) / 2;
	int nn=0,ii=0;
	int ret=0;

	//検索キーの数ループ
	for(nn = 0; nn < pairSize;nn++){
		for(ii=0;ii < targStrLen;ii++){
			char *currS;
			char *currE;
			char *nextS;

			currS = strchr(haystack,needles[nn]);
			//nnが対象に存在するか
			if(currS != NULL){
				//終了の場所を
				currE = strchr((currS + 1),needles[nn + 1]);
				nextS = strchr((currS + 1),needles[nn]);
				if(currE == NULL){
					ret = 1;
				}

				//閉じより先に開きが入ったらnest
				if(currE > nextS){
					ret = checkParentheses((currS + 1),(needles + nn));
				}
			}else{
				break;
			}
		}
	}
	return ret;
}

//*********************************************************
// 連結する
//*********************************************************
int string_concat(String_t * ioData, char *iStr){
	int ret = 0;

	char *old = ioData->data;
	size_t size = strlen(iStr) + ioData->size;

	if(size >= QUERYMAX_SIZE){
		mytool_runtime_error(' ',source_file_name," Error C [%02d]:can not Alloc Query %100.100s", 84, ioData->data);
		return 1;
	}
	//TODO :: このサイズが足りていない？
	// self = (String_t *)malloc( sizeof(String_t) + sizeof(char) * size);
	// ioData->data = (char *)malloc(  sizeof(char) * (size + 1));
	/* 確保した領域をNULLクリア */
	// memset( ioData->data , '\0' , sizeof(char) * (size + 1));

	if(ioData->data != NULL){
		//後ろに文字をそのまま繋ぐのでNULLは繋がない
		// memcpy(ioData->data, old, ioData->size );
		memcpy(ioData->data + ioData->size, iStr, strlen(iStr));
		ioData->size = size;
		// memset(old,'\0',strlen(old));
		// free(old);
	}else{
		ret = 1;
	}

	return ret;
}

//*********************************************************
// 文字列 String を nShift だけ移動する。
// 移動先へのポインタを返す。
//*********************************************************
char *StrShift( char *String, size_t nShift ){
	char *start = String;
	char *stop  = String + strlen( String );
	memmove( start + nShift, start, stop-start+1 );

	return String + nShift;
}

//*********************************************************
// 文字列 String の文字列 From を文字列 To で置換する。
// 置換後の文字列 String のサイズが String の記憶領域を超える場合の動作は未定義。
//*********************************************************
char *StrReplace( char *String, const char *From, const char *To ){
	int   nToLen;   // 置換する文字列の長さ
	int   nFromLen; // 検索する文字列の長さ
	int   nShift;
	char *start;    // 検索を開始する位置
	char *stop;     // 文字列 String の終端
	char *p;

	nToLen   = strlen( To );
	nFromLen = strlen( From );
	nShift   = nToLen - nFromLen;
	start    = String;
	stop     = String + strlen( String );

	// 文字列 String の先頭から文字列 From を検索
	while( NULL != ( p = strstr( start, From ) ) )
	{
		// 文字列 To が複写できるようにする
		start = StrShift( p + nFromLen, nShift );
		stop  = stop + nShift;

		// 文字列 To を複写
		memmove( p, To, nToLen );
	}

	return String;
}//StrReplace


//*********************************************************
//要素分解関数
//*********************************************************
int set_argv(Argv_inf *ioTrgt_arg, char * iArgv){
	int ii,jj, ret = 0;

	char *strOptArr[ITEMLST] = {'\0'};        //オプション別
	char *strArgArr[ARGITM + 1];    //要素別
	int result_ii = 0;
	int result_jj = ARGITM;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//括弧の対応をcheck
	if(checkParentheses(iArgv,"()")){
		fprintf(stderr," Error C [%02d]:Parentheses is not supported %s \n",10,local_server_time(strTime));
		exit(1);
	}

	remTargChar(iArgv,0x5c);
	remCRLF(iArgv);
	result_ii = strSplit_ELM(iArgv,strOptArr);

	if(result_ii >= ITEMLST){
		fprintf(stderr," Error C [%02d]:Too many arguments %s \n",16,local_server_time(strTime));
		exit(1);
	}


	for(ii=0; ii< result_ii; ii++){
		//不要な括弧,A/Oの削除
		if(ii > 0){
			//AND/OR条件の場合(区切り文字の先頭がA/O)
			if(*strOptArr[ii] == 'A'){
				ioTrgt_arg[ii].logical = LOGICAL_AND;
			}else if(*strOptArr[ii] == 'O'){
				ioTrgt_arg[ii].logical = LOGICAL_OR;
			}else{
				if(ii==0 || *strOptArr[ii] == ','){
					ioTrgt_arg[ii].logical = LOGICAL_DEF;
				}else{
					fprintf(stderr," Error C [%02d]:comma can't be found %s \n",12,local_server_time(strTime));
					exit(1);
				}
			}
			if(strchr(strOptArr[ii],'(') != NULL){
				strOptArr[ii] = strchr(strOptArr[ii],'(');
			}
		}

		strChg(strOptArr[ii],"(","");
		strChg(strOptArr[ii],")","");

		result_jj = strSplit(strOptArr[ii],",",strArgArr);

		//最初の要素と元のstringのポインタ位置は同じはず
		if(strArgArr[0] != strOptArr[ii]){
			fprintf(stderr," Error C [%02d]:param parse error %s \n",11,local_server_time(strTime));
			exit(1);
		}

		if((strlen(strOptArr[ii]) > 0 && result_jj == 0) || result_jj > ARGITM){
			fprintf(stderr," Error C [%02d]:Can't determine specific element %s \n",13,local_server_time(strTime));
			exit(1);
		}

		//引数の数を判定
		//エラーの時は関数内でexit
		checkParamNum((strOptArr[0] - 4),result_jj);

		for(jj=0; jj< result_jj; jj++){
			switch(jj){
			case 0:
				//初期化
				ioTrgt_arg[ii].s_point  = 0;		// 開始位置
				ioTrgt_arg[ii].length   = 0;		// 文字列長
				ioTrgt_arg[ii].type     = "";		// 出力タイプ
				ioTrgt_arg[ii].operand  = "";		// 比較対象フィールド
				ioTrgt_arg[ii].value    = "";		// 出力フィールド構成
				ioTrgt_arg[ii].sumflg   = false;	// 集計項目フラグ

				if (result_jj == 1){
					//()内の第一項目がひとつ(,区切りでない)なら定数
					//＠マークの除去
					if(strchr(strArgArr[jj],0x40) != 0){
						strChg(strArgArr[jj],"@","");
						//(0だと初期値扱いで以降の処理できない)
						ioTrgt_arg[ii].s_point = -99999;
						ioTrgt_arg[ii].value   = strArgArr[jj];
					}else{
						fprintf(stderr," Error C [%02d]:param parse error %s \n",11,local_server_time(strTime));
						exit(1);
					}
				}else{
					ioTrgt_arg[ii].s_point = atoi(strArgArr[jj]);
					//ここに入ったのに開始位置が0以上でないことはあり得ない
					if(ioTrgt_arg[ii].s_point <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
						fprintf(stderr," Error C [%02d]:Start point is incorrect %s \n",15,local_server_time(strTime));
						exit(1);
					}
				}
				break;
			case 1:
				ioTrgt_arg[ii].length  = atoi(strArgArr[jj]);
				//ここに入ったのに長さが0以上でないことはあり得ない
				if(ioTrgt_arg[ii].length <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
					fprintf(stderr," Error C [%02d]:Length is incorrect %s \n",14,local_server_time(strTime));
					exit(1);
				}
				break;
			case 2:
				//長さが合わない時
				if(strlen(strArgArr[jj]) <= 0){
					fprintf(stderr," Error C [%02d]:type is incorrect in %.3s %s \n",17,(strOptArr[0] - 4),local_server_time(strTime));
					exit(1);
				}
				//SUMで指定の値ではない時
				if(checkType((strOptArr[0] - 4),strArgArr[jj]) != 0){
					fprintf(stderr," Error C [%02d]:type is incorrect in %.3s %s \n",21,(strOptArr[0] - 4),local_server_time(strTime));
					exit(1);
				}
				ioTrgt_arg[ii].type    = strArgArr[jj];
				break;
			case 3:
				if((strncmp((strOptArr[0] - 4),"SEL",strlen("SEL")) == 0) && (
				strncmp(strArgArr[jj],"GE",strlen("GE")) != 0
				&& strncmp(strArgArr[jj],"EQ",strlen("EQ")) != 0
				&& strncmp(strArgArr[jj],"GT",strlen("GT")) != 0
				&& strncmp(strArgArr[jj],"LE",strlen("LE")) != 0
				&& strncmp(strArgArr[jj],"LT",strlen("LT")) != 0
				&& strncmp(strArgArr[jj],"NE",strlen("NE")) != 0
				)){
					fprintf(stderr," Error C [%02d]:operand is incorrect %s \n",19,local_server_time(strTime));
					exit(1);
				}
				ioTrgt_arg[ii].operand = strArgArr[jj];
				break;
			case 4:
				if(strchr(strArgArr[jj],'@') == 0 || strchr((strchr(strArgArr[jj],'@') + 1),'@') == 0
					|| searchChr(strArgArr[jj],'@') > 2){
					fprintf(stderr," Error C [%02d]:@ Parentheses is not supported %s \n",10,local_server_time(strTime));
					exit(1);
				}
				//＠マークの除去
//				strChg(strArgArr[jj],"@","");
				*(strchr((strchr(strArgArr[jj],'@') + 1),'@')) = '\0';
				ioTrgt_arg[ii].value   = (strchr(strArgArr[jj],'@') + 1);
				break;
			default:
				fprintf(stderr," Error C [%02d]:Too many arguments to %s %s \n",29,(strOptArr[0] - 4),local_server_time(strTime));
				exit(1);
				break;
			}
		}
	}
	//残項目の初期化
	for(ii=ii; ii < ITEMLST; ii++){
		ioTrgt_arg[ii].s_point = 0;
		ioTrgt_arg[ii].value   = "";
	}

	return ret;
}

//KEYとSUMの重複チェック
//ioTrgt:引数オブジェクト
//return エラー1,正常0
//author:koyama
int overlapKeyToSum(Trgt_inf *ioTrgt){
	int ii=0,jj=0;
	int ret=0;

	for(ii=0;ii < ITEMLST;ii++){
		if(ioTrgt->key[ii].s_point == 0){
			//keyのii番目の開始位置が0なら以降は設定されていない
			break;
		}
		for(jj=0;jj < ITEMLST;jj++){
			if(ioTrgt->sum[jj].s_point == 0){
				//sumのjj番目の開始位置が0なら以降は設定されていない
				break;
			}
			//sumの開始位置がkeyの開始位置から終了位置の間に含まれる
			//または、
			if((ioTrgt->key[ii].s_point <= ioTrgt->sum[jj].s_point
				&& (ioTrgt->key[ii].s_point + ioTrgt->key[ii].length) > ioTrgt->sum[jj].s_point)
			|| (ioTrgt->key[ii].s_point <= (ioTrgt->sum[jj].s_point + ioTrgt->sum[jj].length - 1)
				&& (ioTrgt->key[ii].s_point + ioTrgt->key[ii].length) > (ioTrgt->sum[jj].s_point + ioTrgt->sum[jj].length - 1))){
				ret = 1;
			}
		}
	}

	return ret;
}

//必須入力等の引数が正しく入っているか
//ioTrgt:引数オブジェクト
//return エラー1,正常0
//author:koyama
int checkTrgtCorrect(Trgt_inf *ioTrgt){
	char strTime[] = "0000/00/00 00:00:00.000000";

	if(strlen(ioTrgt->ifi) == 0){
		fprintf(stderr," Error C [%02d]:Where is the input source  %s \n",01,local_server_time(strTime));
		return 1;
	}
	if(strlen(ioTrgt->ofi) == 0){
		fprintf(stderr," Error C [%02d]:Where is the output destination %s \n",02,local_server_time(strTime));
		return 1;
	}
	return 0;
}
//*********************************************************
//引数分解関数
//*********************************************************
int set_trgt(Trgt_inf *ioTrgt, char *iArgv){
	int ii,ret = 0, cnt =0;
	char wkstr[(120+1)] = "";
	char *strOptArr[(20+1)] = {'\0'};		//オプション別
	char *strTmpArgv = iArgv;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//定数の間のSPをいったん置換
	chgChar(strTmpArgv,(char)0x20,(char)0x07,(char)0x40);

	//第二引数を「 」でオプション別に区分けする
	int result = strSplit(strTmpArgv," ",strOptArr);

	for(ii=0; ii< result; ii++){
		chgChar(strOptArr[ii],(char)0x07,(char)0x20,(char)0x40);
		//入力装置設定
		if (strstr(strOptArr[ii], "IDE=") != NULL ) {
			ioTrgt->ide = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "IFI=") != NULL ) {
			//入力ファイル名設定
			ioTrgt->ifi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "OFI=") != NULL ) {
			//出力ファイル名設定
			ioTrgt->ofi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "KEY=") != NULL ) {
			//比較対象フィールド設定
			ret = set_argv(ioTrgt->key, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			continue;
		}else if (strstr(strOptArr[ii], "OUT=") != NULL ) {
			//出力フィールド構成設定
			ret = set_argv(ioTrgt->out, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			continue;
		}else if (strstr(strOptArr[ii], "SUM=") != NULL ) {
			//集計対象フィールド設定
			ret = set_argv(ioTrgt->sum, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			continue;
		}else if (strstr(strOptArr[ii], "SEL=") != NULL ) {
//			//対象条件設定
//			if((strstr(strOptArr[ii], ")A(") != NULL ) && (strlen(strOptArr[ii]) > 0)){
//				//Where句の場合(AND条件)
//				ioTrgt->logical = LOGICAL_AND;
//			}else if((strstr(strOptArr[ii], ")O(") != NULL ) && ( strlen(strOptArr[ii]) > 0)){
//				//Where句の場合(OR条件)
//				ioTrgt->logical = LOGICAL_OR;
//			}else{
//				//Where句以外の場合
//				ioTrgt->logical = LOGICAL_DEF;
//			}

			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			continue;
		}
	}

	return ret;
}

//key(argvから取得されるフィールド指定)をバブルソートで昇順に
void boubleSortArgv(Argv_inf *Array[ITEMLST],Argv_inf *key,int endPoint){
	Argv_inf *tmpArgv;
	if(Array[endPoint - 1]->s_point > key->s_point){
		tmpArgv = Array[endPoint - 1];
		Array[endPoint - 1] = key;
		Array[endPoint] = tmpArgv;
		//endPointが0のときはkeyが１番目になるので終了
		if((endPoint-1) > 0){
			boubleSortArgv(Array,key,(endPoint-1));
		}
	}else{
		//keyを現在のendPointの場所に置いて終了
		Array[endPoint] = key;
	}
}
///}
//配列の途中に要素を挿入する
void insertArrayArgv(Argv_inf *Array[ITEMLST],Argv_inf *key,int insPoint,int endPoint){
	Argv_inf *tmpArgv;
	//必要になるかわからないポインタを一旦保存
	tmpArgv = Array[insPoint];
	//挿入すべき箇所に受けたデータを格納
	Array[insPoint] = key;
	//endPointが0のときはkeyが１番目になるので終了
	if(endPoint > insPoint){
		//挿入が間だった場合
		insertArrayArgv(Array,tmpArgv,(insPoint+1),endPoint);
	}
}




//*********************************************************
//int ⇒ char 変換
//*********************************************************
char *itoa( int val, char *str, int radix ){
	char size_e[1];
	char *p = str;
	unsigned int v = val;				// 作業用(変換対象の値)
	int n = 1;							// 変換文字列の桁数記憶用
	while(v >= radix){					// 桁数を求める
		v /= radix;
		n++;
	}
	// str = (char *)malloc((sizeof(size_e) * n));
	if(str == NULL) {
		fprintf(stderr,"can't alloc memory!!\n");
		return str;
	}
	p = str + n;						//最下位の位置から設定する
	v = val;
	*p = '\0';							// 文字列終端の設定
	do {
		--p;
		*p = v % radix + '0';			// 1桁の数値を文字に変換
		if(*p > '9') {					// 変換した文字が10進で表現できない場合
			*p = v % radix - 10 + 'A';	// アルファベットを使う
		}
		v /= radix;
	} while ( p != str);

	return str;
}

//*********************************************************
//設定ファイルの読み込み
//*********************************************************
int conf_read(DB_SETTING *targSetting){
	int ii;
	char strConfPath[1024]; //20150915 add koyama

	//下で値が設定されなかったらこれがdefault
	myConfDebugFlg = 0;

	//ファイルネームを元にリーダポインタを作成 upd koyama 20150915 ファイルパスを可変に getConfFilename(strConfPath)
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
	for (ii = 0; ii < size; ++ii) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, ii);
			if (node->content) {
				//設定ファイルからDBのホスト名を取得
				if(strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(targSetting->db_server,node->content);
				}
				//設定ファイルからDBのユーザ名を取得
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(targSetting->db_user,node->content);
				}
				//設定ファイルからDBのパスワードを取得
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(targSetting->db_password,node->content);
				}
				//設定ファイルからDB名を取得
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(targSetting->db_database,node->content);
				}
				//設定ファイルからポートを取得
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					targSetting->db_port = atoi(node->content);
				}
				//2015/08/28 kawada add S 管理テーブルIDの取得
				//設定ファイルから共有モード用のテーブル名を取得
				if(strcmp(node->parent->name,MANA_TBL_NAME) == 0){
					strcpy(targSetting->db_manatbl,node->content);
				}
				//debugフラグがあるなら取得
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//返還できない文字列は0になるはず
					myConfDebugFlg = atoi(node->content);
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
	return 0;
}

//*********************************************************
//テーブルレコード長取得
//*********************************************************
int getRecLen(char *strRow, const char *chkstr){
	char *str1;			//レコード長の文字列（レコード長文字列を含む）
	char *str2;			//レコード長の文字列（レコード長文字列を含まない）
	char wstr[16]="";
	int len1;			//レコード長の文字列（レコード長文字列を含む）の文字列長
	int len2;			//レコード長の文字列（レコード長文字列を含まない）の文字列長

	//CREATE TABLEの情報文字列からレコード長の前までの文字列をカットして変数に保存＋その文字列長を取得
	str1 = strstr(strRow, chkstr) + strlen(chkstr);
	len1 = strlen(str1);
	//レコード長の後の文字列をカットして変数に保存＋その文字列長を取得
	str2 = strstr(str1,")");
	len2 = strlen(str2);
	if (len1 > len2){
		//str1(先頭に文字列長あり)から文字列長分(len1 - len2)を取得
		strncpy(wstr, str1, len1 - len2);
		return atoi(wstr);
	}else{
		return 0;
	}
}
//*********************************************************
//SORTオプション(引数)のレコード長矛盾指定をチェック
//*********************************************************
int chkRecLenParam(Trgt_inf *itrgt, int recLen, char **errstr, int *errcnt, const int errmax){
	int cii;		//チェック用添字

	*errcnt = 0;

	//[KEY]
	cii = 0;
	while(itrgt->key[cii].s_point != 0){
		//開始位置にフィールド長を加算した結果、レコード長を超えていた → エラー
		if ((itrgt->key[cii].s_point + itrgt->key[cii].length - 1) > recLen){
			*errcnt += 1;
			if (*errcnt >= errmax){
				break;
			}
			errstr[*errcnt - 1] = (char *)malloc(127);
			sprintf(errstr[*errcnt - 1], "record length over Error [KEY]=(%d,%d)", itrgt->key[cii].s_point, itrgt->key[cii].length);
		}
		cii ++;
	}

	//[OUT]
	cii = 0;
	while(itrgt->out[cii].s_point != 0){
		//開始位置にフィールド長を加算した結果、レコード長を超えていた → エラー
		if ((itrgt->out[cii].s_point + itrgt->out[cii].length - 1) > recLen){
			*errcnt += 1;
			if (*errcnt >= errmax){
				break;
			}
			errstr[*errcnt - 1] = (char *)malloc(127);
			sprintf(errstr[*errcnt - 1], "record length over Error [OUT]=(%d,%d)", itrgt->out[cii].s_point, itrgt->out[cii].length);
		}
		cii ++;
	}

	//[SUM]
	cii = 0;
	while(itrgt->sum[cii].s_point != 0){
		//開始位置にフィールド長を加算した結果、レコード長を超えていた → エラー
		if ((itrgt->sum[cii].s_point + itrgt->sum[cii].length - 1) > recLen){
			*errcnt += 1;
			if (*errcnt >= errmax){
				break;
			}
			errstr[*errcnt - 1] = (char *)malloc(127);
			sprintf(errstr[*errcnt - 1], "record length over Error [SUM]=(%d,%d)", itrgt->sum[cii].s_point, itrgt->sum[cii].length);
		}
		cii ++;
	}

	//エラー件数が上限を超えた
	if (*errcnt >= errmax){
		errstr[*errcnt - 1] = (char *)malloc(127);
		sprintf(errstr[*errcnt - 1], "...more Error Exist");
	}

	if (*errcnt > 0){
		return 1;
	}else{
		return 0;
	}
}

//outの構造体配列をsumで調整する
//add koyama 20170920 元の処理を切り出して関数化
int adjustOutWithSum(Argv_inf *parseArgs[],Trgt_inf *itrgt,int *item_cnt_p){
		int argParseCount = 0;
		int sumMaxLength = 0;
		int item_cnt = *item_cnt_p;
		int wkCount = 0;
		int checkArray[ITEMLST] = {};
		int ret = 0;
		char strTime[] = "0000/00/00 00:00:00.000000";

		for(wkCount = 0;wkCount < item_cnt;wkCount++){
			int existFlg = 0;        //同じスタートポイント,長さのものが出来た時に
			for(argParseCount = 0;(itrgt->sum[argParseCount].s_point != 0) && (argParseCount < ITEMLST) ;argParseCount++){
				//前側がなければ0,あれば長さが入る
				int frontExists = 0;
				//前側がなければ0,あれば開始位置が入る
				int frontSPoint = 0;
				if((parseArgs[wkCount]->s_point == itrgt->sum[argParseCount].s_point)
				&& (parseArgs[wkCount]->length == itrgt->sum[argParseCount].length)){
					//開始,長さともに同じものがあればそのまま入れる
					parseArgs[wkCount]->type = itrgt->sum[argParseCount].type;
					parseArgs[wkCount]->sumflg = true;
					existFlg = 1;
				}else{
					//構造体をコピーしてだんだん中身を小さくする仕様に変更 20170920 upd koyama =><失敗
					//outに含まれている(一致してなくて範囲に入る=>前か後ろが違う)
					if(parseArgs[wkCount]->s_point <= itrgt->sum[argParseCount].s_point
					&& (parseArgs[wkCount]->s_point + parseArgs[wkCount]->length) >= (itrgt->sum[argParseCount].s_point + itrgt->sum[argParseCount].length)){
						if(parseArgs[wkCount]->s_point < itrgt->sum[argParseCount].s_point){
							//sumaryより前側があるなら長さを短くする
							//後で長さが必要になる可能性があるので
							frontExists  = parseArgs[wkCount]->length;
							frontSPoint  = parseArgs[wkCount]->s_point;
							parseArgs[wkCount]->length = itrgt->sum[argParseCount].s_point - parseArgs[wkCount]->s_point;
							//sumaryの部分を追加
							wkCount++;                       //前側があるかないかで+する位置が違うため
							insertArrayArgv(parseArgs,&itrgt->sum[argParseCount],wkCount,item_cnt);
							//ここはsumからとったので,sumflg = true
							parseArgs[wkCount]->sumflg = true;
							wkCount++;                       //前側があるかないかで+する位置が違うため
						}else{
							// TODO: 20161208 ここで何をしているのか調査
							// TODO:スタートが一致していて,parseArgsが大きいもののよう
							//sumの開始位置が前回入れた項目より小さい時なので必ず間に挿入になる？20161208 add koyama
							frontExists  = parseArgs[wkCount]->length;
							frontSPoint  = parseArgs[wkCount]->s_point;
							parseArgs[wkCount]->length = itrgt->sum[argParseCount].length;
							insertArrayArgv(parseArgs,&itrgt->sum[argParseCount],wkCount,item_cnt);
							parseArgs[wkCount]->sumflg = true;
							wkCount++;							//
						}
						item_cnt++;
						if((parseArgs[wkCount-1]->s_point + frontExists) > (itrgt->sum[argParseCount].s_point + itrgt->sum[argParseCount].length)){
							//summaryより後ろ側があるならそれを入れる
							//TODO : 後ろが更に分割になる場合を考慮できていない
							if(frontExists > 0){
								//前も後ろもの時は要素を作って挿入する
								Argv_inf *tempArg;
								tempArg = malloc(sizeof(Argv_inf));
								argv_ini(tempArg);
								tempArg->s_point = itrgt->sum[argParseCount].s_point + itrgt->sum[argParseCount].length;
								//frontSPoint,frontExistsに関しては-1の値のときの-2の値の時がある
								tempArg->length = (frontSPoint + frontExists) - tempArg->s_point; //元々の終了位置- 自分のスタート
								tempArg->type = malloc((strlen(TYPE_CHAR) + 1) * sizeof(*TYPE_CHAR));
								strcpy(tempArg->type,TYPE_CHAR);
								tempArg->operand = itrgt->sum[argParseCount].operand;
								tempArg->value = itrgt->sum[argParseCount].value;
								tempArg->sumflg = false;
								tempArg->logical = itrgt->sum[argParseCount].logical;
								insertArrayArgv(parseArgs,tempArg,wkCount,item_cnt);
								//ここを次の対象にするためにitem_cntをインクリメント市内
								// item_cnt++;
							}else{
								//後ろ側だけの時はsummaryの最後を開始,元の長さからsummaryを引いたものを長さとする
								parseArgs[wkCount]->length = itrgt->sum[argParseCount].s_point + itrgt->sum[argParseCount].length;
								parseArgs[wkCount]->length = parseArgs[wkCount]->length - itrgt->sum[argParseCount].length;
							}
						}
						//ここに入ったら一旦はbreakしておく TODO:正しく動くようにするために除去
						// break;
					}
					//sumの最大長を確保しておく
					if(sumMaxLength < argParseCount){
						sumMaxLength = argParseCount;
					}
					existFlg = 1;
				}
			}
		}
		//outに含まれていなければの条件を判定する方法を検討 TODO
		// if(existFlg != 1){
		// 	fprintf(stderr," Error [%02d] %s : Summary is not included in the output(start:%d) \n", 88,local_server_time(strTime),itrgt->sum[argParseCount].s_point);
		// 	//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		// 	ret = 1;
		// }
		*item_cnt_p = item_cnt;
		return ret;
}


//*********************************************************
//DB移行処理
//*********************************************************
int mysql_Init(Trgt_inf *itrgt,DB_SETTING *targSetting){
	String_t query;	      //初期設定を読み込むSQLの格納
	char temp[256] = "";	//SQL用temporary
	char size_e[1];
	void *buffer[100];
	char *strWhereArr[100] = {'\0'};	//where句が複数ある時のために
	char strType[64] = "";
	int ii,jj, ret = 0,item_cnt = 0,keycnt=0;    //item_cntフィールド指定のカウント(0ならすべてをそのまま)
	Argv_inf wkout[ITEMLST * 2];		//表示用フィールド配列
	Argv_inf **parseArgs;                //表示用フィールド配列を作るためのポインタ
	char *wkstr = (char *)malloc((sizeof(size_e) * 100));    //後でポインタ代入するためポインタに修正 koyama
	char *wkstr_p;
	// char *wk_length2 = (char *)malloc((sizeof(size_e) * WK_INTSTR_LEN));    //後でポインタ代入するためポインタに修正 koyama
	char strTime[] = "0000/00/00 00:00:00.000000";
	//2015/08/04 kawada add S テーブル長取得用
	char sqlstr[127] = "";	//SQｌステートメント
	char *errstr[24];		//エラーメッセージ用
	const int errmax = 25;	//エラー格納最大件数
	const char chkstr[] = "`ITEM` varbinary(";		//)は後からつなぐ
	int recLen;				//テーブルのレコード長
	int errcnt;				//エラー件数
	int cii;				//チェック用添字
	MYSQL_RES *mysqlResult;
	MYSQL_ROW row;

	//2015/08/04 kawada add E
	// parseArgs = &parseArgs_entity;

	//初期化
	for(ii = 0; ii < ITEMLST * 2 ; ii++){
		argv_ini(&wkout[ii]);
	}
	// memset(wk_length2,0x00,WK_INTSTR_LEN);

	//keyとsummaryの重複チェック
	//どちらの指定が先かなどの影響をうけるため、ここでチェック
	if(overlapKeyToSum(itrgt)){
		fprintf(stderr, " Error C [%02d]:key overlap sum %s \n",31, local_server_time(strTime));
		return 1;
	}

	//2015/08/04 kawada add S レコード長に反した引数指定のチェック
	//CREATE TABLEの情報を取得
	strcat(sqlstr,"SHOW CREATE TABLE ");
	strcat(sqlstr, check_tablename(itrgt->ifi));
//fprintf(stderr,">>sql=%s\n", sqlstr);		//kawatst
	if(mysql_query(mysqlConn, sqlstr) != 0){
		mysql_failure();
		fprintf(stderr, " Error C [%s]:show create table Error %s :%s\n", "35-1", local_server_time(strTime), mysql_error(mysqlConn));
		return 1;
	}
	mysqlResult = mysql_use_result(mysqlConn);
	recLen = 0;
	while((row = mysql_fetch_row(mysqlResult)) != NULL ){
//printf( "%s : %s\n" , row[0] , row[1] );
		//CREATE TABLEの情報からレコード長を取得
		recLen = getRecLen(row[1], chkstr);
		if (recLen <= 0){
			fprintf(stderr, " Error C [%s]:IFI table record length=0 %s :%s\n", "35-2", local_server_time(strTime), mysql_error(mysqlConn));
			return 1;
		}
		//SORTオプション(引数)でIFI指定のテーブルレコード長と矛盾する指定チェック
		//[KEY][OUT][SUM]の開始位置から長さ指定がレコード長をオーバーしているものはエラーとする([SEL]はチェックしない)
		if (chkRecLenParam(itrgt, recLen, errstr, &errcnt, errmax) != 0){
			for (cii = 0 ; cii < errcnt ; cii++){
				fprintf(stderr, " Error C [%s]:%s %s :%s\n", "35-3", errstr[cii], local_server_time(strTime), mysql_error(mysqlConn));
			}
			return 1;
		}
	}
	//2015/08/04 kawada add E

	//フロー(1)}}
	//引数parse Start
	//parseArgに中身を作り、それをwk_outに後でコピーする
	parseArgs = malloc(ITEMLST * sizeof(Argv_inf *));
	if(parseArgs == NULL){
		fprintf(stderr,"can't alloc memory!!\n");
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	//OUTを判定,クエリ埋め込み情報に変換  // みかん (outが一番情報が少ないので一番前に)
	if((itrgt->out[0].s_point != 0) ){
		//実際にoutをクエリ埋め込み情報に		//入力されたoutを順に判定
		int argParseCount = 0;
		for(argParseCount = 0;(itrgt->out[argParseCount].s_point != 0) && (argParseCount < ITEMLST) ;argParseCount++){
			Argv_inf *tempArg;
			tempArg = malloc(sizeof(Argv_inf));
			argv_ini(tempArg);
			tempArg->s_point = itrgt->out[argParseCount].s_point;
			tempArg->length = itrgt->out[argParseCount].length;
			tempArg->type = itrgt->out[argParseCount].type;
			tempArg->operand = itrgt->out[argParseCount].operand;
			tempArg->value = itrgt->out[argParseCount].value;
			tempArg->sumflg = itrgt->out[argParseCount].sumflg;
			tempArg->logical = itrgt->out[argParseCount].logical;

			//OUTは並び替えない
			*(parseArgs + item_cnt) = tempArg;
			item_cnt++;
		}

	}

	//out がなくsumだけがぞんざいするときはすべてをそのまま出力する形で出力データ生成
	if(item_cnt == 0){
		//中身の領域確保
		parseArgs[0] = malloc(sizeof(Argv_inf));
		argv_ini(parseArgs[0]);
		parseArgs[0]->s_point = 1;
		parseArgs[0]->length = recLen;     //前情報のレコード長を流用
		parseArgs[0]->type = malloc((strlen(TYPE_CHAR) + 1) * sizeof(*TYPE_CHAR));
		strcpy(parseArgs[0]->type,TYPE_CHAR);
		parseArgs[0]->operand = "";
		parseArgs[0]->value = "";
		parseArgs[0]->sumflg = false;
		parseArgs[0]->logical = LOGICAL_DEF;
		item_cnt++;
	}

	//SUMを判定,クエリ埋め込み情報に変換
	if((itrgt->sum[0].s_point != 0)){
		//実際にsumをクエリ埋め込み情報に		//入力されたsumを順に判定
		//sum ループの中にparseArgsループ >> parseArgsループ の中にsum ループに変更
		//+ 関数化
		adjustOutWithSum(parseArgs,itrgt,&item_cnt);
	}

	//中身をすべてwkoutにコピー
	for(ii=0;ii < item_cnt;ii++){
		if(ii == 0 && parseArgs[ii]->s_point == 0){
			//最初の一つの初期位置が0のときは指定なしなので初期化
			wkout[ii].s_point = 1;
		}else{
			wkout[ii].s_point = parseArgs[ii]->s_point;
		}
		wkout[ii].length = parseArgs[ii]->length;
		wkout[ii].type = parseArgs[ii]->type;
		wkout[ii].operand = parseArgs[ii]->operand;
		wkout[ii].value = parseArgs[ii]->value;
		wkout[ii].sumflg = parseArgs[ii]->sumflg;
		wkout[ii].logical = parseArgs[ii]->logical;
		//開放できない場合があるため解放なし20170123
//		free(parseArgs[ii]);
	}
	//引数parse End

	//ここまででこの変数の役割終了 中身を開放してないので、解放なし20170123
//	free(parseArgs);
	//フロー(2)
	//SQL初期化
	string_init(&query,   "INSERT INTO tmp (ITEM) (");
	string_concat(&query, " SELECT ");
	//)開きカッコがじゃまになるため閉じる

	//フロー2-1
	string_concat(&query, " CAST(CONCAT(");
	//))開きカッコがじゃまになるため閉じる

	//出力対象の指定の数が0>とき
	if(item_cnt > 0){
		for(ii = 0; ii < item_cnt ; ii++){
			if (ii != 0) string_concat(&query, " ,");
			string_concat(&query, " t1.item");
			wkstr = itoa(ii+1,wkstr,10);
			string_concat(&query, wkstr);
		}
	}else{
		string_concat(&query, " t1.itemall ");
	}
	string_concat(&query, " ) AS BINARY) AS item ");
	string_concat(&query, " FROM (");
	string_concat(&query, " SELECT ");
	// string_concat(&query, " @ii:=@ii+1 AS rownum ");
	string_concat(&query, " t2.rownum");

	//出力対象の指定の数が0>とき
	if(item_cnt > 0){
		//変換されたものを元に戻す部分
		for(ii = 0; ii < item_cnt ; ii++){
			char wk_length1[WK_INTSTR_LEN]="";
			wkstr = itoa(ii+1,wkstr,10);
			sprintf(wk_length1,"%d",wkout[ii].length);
			if(*(wkout[ii].type) == 'P'){
				//2015/08/04 kawada add S 下桁落ち→上桁落ち　対応
				// ,UNHEX(CASE CAST(t2.item1 AS SIGNED) < 0
				//WHEN TRUE THEN CONCAT(RIGHT(LPAD(ABS(t2.item1 ),((CEIL((4 + 1 + 1) / 2) * 2) - 1) * 2,'0'),((CEIL((4 + 1 + 1) / 2) * 2) - 1)),'D')
				// ELSE CONCAT(RIGHT(LPAD(ABS(t2.item1 ),((CEIL((4 + 1) / 2) * 2) - 1) * 2,'0'),((CEIL((4 + 1) / 2) * 2) - 1)),'C') END) AS item
				string_concat(&query, " ,UNHEX(CASE CAST(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, " AS SIGNED) < 0 ");
				string_concat(&query, " WHEN TRUE THEN CONCAT(RIGHT(LPAD(ABS(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, " ),((");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) - 1) * 2,'0'), ((");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) - 1)),'D') ");
				string_concat(&query, " ELSE CONCAT(RIGHT(LPAD(ABS(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, " ),((");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2) - 1) * 2,'0'), ((");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) - 1)),'C') ");
				string_concat(&query, " END) AS item");
				string_concat(&query, wkstr);
				//2015/08/04 kawada add E
			}else if(*(wkout[ii].type) == 'Q'){
				//フロー2-2-1
				//2015/08/04 kawada add S 下桁落ち→上桁落ち　対応
				// , UNHEX(CONCAT(RIGHT(LPAD(ABS(t2.item1 ),
				//((CEIL((4 + 1) / 2) * 2) - 1) * 2,'0'),'F')),((CEIL((4 + 1) / 2) * 2) - 1)) AS item
				string_concat(&query, " , UNHEX(CONCAT(RIGHT(LPAD(ABS(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, " ),((");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2) - 1) * 2,'0'), ((");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2) - 1)),'F')) ");
				string_concat(&query, " AS item");
				string_concat(&query, wkstr);
				//2015/08/04 kawada add E
			}else if(wkout[ii].sumflg == true){
				//フロー2-2-2
				//Pでなくsumflugが立っているときは数字として処理するので元に戻す
				//CASE t2.item4 < 0
				string_concat(&query, " ,CASE t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, " < 0 ");
				//2015/08/04 kawada add S 下桁落ち→上桁落ち　対応
				//WHEN true THEN UNHEX(CAST(HEX(RIGHT(LPAD(CAST(abs(t2.item4) as char),9 * 2,'0'),9))as UNSIGNED) + 40)
				//BIGINT でオーバフローするため処理順変更 add koyama 20150807
				// WHEN TRUE THEN RIGHT(LPAD(UNHEX(HEX(CAST(abs(t2.item9) as char)) + 40),(10 * 2) ,'0'),10)
				string_concat(&query, " WHEN TRUE THEN RIGHT(LPAD(UNHEX(HEX(CAST(abs(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, ") as char)) +40),(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2),'0'), ");
				string_concat(&query, wk_length1);
				string_concat(&query, ") ");
				//2015/08/04 kawada add E
				//2015/08/04 kawada add S 下桁落ち→上桁落ち　対応
				//ELSE RIGHT(LPAD(CAST(abs(t2.item4) as char),9 * 2,'0'),9) END
				string_concat(&query, " ELSE RIGHT(LPAD(CAST(abs(t2.item");
				string_concat(&query, wkstr);
				string_concat(&query, ") as char), ");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2,'0'),");
				string_concat(&query, wk_length1);
				string_concat(&query, ")");
				string_concat(&query, " END AS item");
				string_concat(&query, wkstr);
				//2015/08/04 kawada add E
			}else{
				string_concat(&query, " ,t2.item");
				string_concat(&query, wkstr);
			}
		}
	}else{
		string_concat(&query, " ,t2.itemall as itemall");
	}
	//フロー2-3
	string_concat(&query, " ");
	string_concat(&query, " FROM (");
	// string_concat(&query, " FROM (SELECT @ii:=0 as num) AS NUMBER ");
	// string_concat(&query, " ,");
	// string_concat(&query, " (");

// TODO:20170214:ここで発番しないと、ソートがうまくかからないので直すときには確認すること
	string_concat(&query, " SELECT");
	string_concat(&query, " @ii:=@ii+1 AS rownum ,");
	//))

	if(item_cnt > 0){
		//SUMMARY or CONSTANT
		for(ii = 0; ii < item_cnt ; ii++){
			char wk_point[WK_INTSTR_LEN]="";
			char wk_length1[WK_INTSTR_LEN]="";
			char wk_length2[WK_INTSTR_LEN]="";
			if(ii != 0){
				string_concat(&query, " ,");
			}

			//条件2-3
			if(wkout[ii].sumflg == true){
				 string_concat(&query, " SUM(");
			}
				//)開きカッコがじゃまになるため閉じる

			wkstr = itoa(ii+1,wkstr,10);
			sprintf(wk_point,"%d",wkout[ii].s_point);
			sprintf(wk_length1,"%d",wkout[ii].length);
			sprintf(wk_length2,"%d",wkout[ii].length * 2);
			//wkoutはN(数),S(符号付数),P(packed),C(Constant)に分類
			//条件2-4
			if(*(wkout[ii].type) == 'P' || *(wkout[ii].type) == 'Q'){
				//packed decimal
				//CASE SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,CEIL((4 + 1) / 2) * 2)
				string_concat(&query, " CASE");
				string_concat(&query, "  SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) ");
				//最後の文字はcastの内部変換で落ちる
				//WHEN 'C' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED)
				string_concat(&query, " WHEN 'C' THEN CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2) as SIGNED) ");
				//WHEN 'D' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED) *(-1) END
				string_concat(&query, " WHEN 'D' THEN CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) as SIGNED) * (-1)");
				//WHEN 'F' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED)
				string_concat(&query, " ELSE CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) as SIGNED) ");
				string_concat(&query, " END ");
				if(wkout[ii].sumflg == true){
					 string_concat(&query, " ) ");
				}
				string_concat(&query, " AS item");
				string_concat(&query, wkstr);
			}else if((atoi(wk_length1) != 0) && (strlen(wkout[ii].value) == 0)){
				//フロー2-3-1
				if(wkout[ii].sumflg == true){
					//符号付
					//CASE MID(HEX(MID(item,15 + (9 - 1),1)),1,1)
					string_concat(&query, " CASE");
					string_concat(&query, " MID(HEX(MID(item,");
					string_concat(&query, wk_point);
					string_concat(&query, " + (");
					string_concat(&query, wk_length1);
					string_concat(&query, " - 1),1)),1,1) ");
					//WHEN '7' THEN (CAST(MID(item, 15 , (9 - 1) ) as SIGNED) * 10
					if(wkout[ii].length != 1){
						string_concat(&query, " WHEN '7' THEN (CAST(MID(item, ");
						string_concat(&query, wk_point);
						string_concat(&query, " , (");
						string_concat(&query, wk_length1);
						string_concat(&query, " - 1) ) as SIGNED) * 10 ");
					}else{
						string_concat(&query, " WHEN '7' THEN (0 ");
					}
					//+ CAST(SUBSTR(HEX(MID(item, 15 + (9 - 1),1 )),2,1 ) as SIGNED) ) *(-1)
					string_concat(&query, " + CAST(SUBSTR(HEX(MID(item,  ");
					string_concat(&query, wk_point);
					string_concat(&query, " + (");
					string_concat(&query, wk_length1);
					//2015/08/04 kawada del string_concat(&query, " - 1),1 )),2,1 ) as SIGNED)  * (-1))");
					string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) * (-1)");		//2015/08/04 kawada add 不具合修正
					//ELSE (CAST(MID(item, 15 , (9 - 1) ) as SIGNED) * 10
					if(wkout[ii].length != 1){
						string_concat(&query, " ELSE (CAST(MID(item, ");
						string_concat(&query, wk_point);
						string_concat(&query, " , (");
						string_concat(&query, wk_length1);
						string_concat(&query, " - 1) ) as SIGNED) * 10 ");
					}else{
						string_concat(&query, " ELSE (0 ");
					}
					//+ CAST(SUBSTR(HEX(MID(item, 15 + (9 - 1),1 )),2,1 ) as SIGNED) ) END
					string_concat(&query, " + CAST(SUBSTR(HEX(MID(item,  ");
					string_concat(&query, wk_point);
					string_concat(&query, " + (");
					string_concat(&query, wk_length1);
					string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) END");
				}else{
					//その他
					string_concat(&query, " MID(item,");
					string_concat(&query, wk_point);
					//開始位置だけあるときは残りすべて
					if(atoi(wk_length1) != 0){
						string_concat(&query, " ,");
						string_concat(&query, wk_length1);
					}
				}
				string_concat(&query, " ) as item");
				string_concat(&query, wkstr);
			}else if(strlen(wkout[ii].value) != 0){
				//定数
				//上は(strlen(wkout[ii].value) == 0これで定数を省いている
				string_concat(&query, " \"");
				string_concat(&query, wkout[ii].value);
				string_concat(&query, "\" item");
				string_concat(&query, wkstr);
			}else{
				//その他
				//どの条件に当てはまらないときも対象カラムを直接出力20150129
				string_concat(&query, " MID(item,");
				string_concat(&query, wk_point);
				//開始位置だけあるときは残りすべて
				if(atoi(wk_length1) != 0){
					string_concat(&query, " ,");
					string_concat(&query, wk_length1);
				}
				string_concat(&query, " ) as item");
				string_concat(&query, wkstr);
			}
		}
	}else{
		string_concat(&query, " item as itemall ");
	}
	//
	string_concat(&query, " ");
	// TODO:20170214:ここで定義しないと、ソートがうまくかからないので直すときには確認すること
	// string_concat(&query, " FROM ");
	string_concat(&query, " FROM (SELECT @ii:=0 as num) AS NUMBER ");
	string_concat(&query, " ,");
	string_concat(&query, check_tablename(itrgt->ifi));
	string_concat(&query, " AS DATA ");

	//頭がFFのものは削除扱いなので、対象にしない add koyama 20170619
	string_concat(&query, " WHERE HEX(MID(ITEM,1,1)) <> 'FF' ");
	//条件文
	//フロー3
	for(ii = 0; ii < ITEMLST ; ii++){
		char wk_point[WK_INTSTR_LEN]="";
		char wk_length1[WK_INTSTR_LEN]="";
		//条件3-1
		//TODO 201 ここにSUmが指定されていたときに底が数字であることを確認
		if((itrgt->sel[ii].s_point != 0) && (itrgt->sel[ii].length != 0)){
			char oper[OPER_LEN + 1] = "";
			sprintf(wk_point,"%d",itrgt->sel[ii].s_point);
			sprintf(wk_length1,"%d",itrgt->sel[ii].length);

			if (ii == 0){
				string_concat(&query, " AND ");
			}else if(itrgt->sel[ii].logical == LOGICAL_AND){
				string_concat(&query, " AND ");
			}else if(itrgt->sel[ii].logical == LOGICAL_OR){
				string_concat(&query, " OR ");
			}

			//比較演算子
			if(strcmp(itrgt->sel[ii].operand,"EQ") == 0){
				memcpy(oper, " =  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"GE") == 0){
				memcpy(oper, " >= ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"GT") == 0){
				memcpy(oper, " >  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"LE") == 0){
				memcpy(oper, " <= ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"LT") == 0){
				memcpy(oper, " <  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"NE") == 0){
				memcpy(oper, " != ",OPER_LEN);
			}else{

			}

			//条件2-6∪
			if(itrgt->sel[ii].type[0] == 'S'){
				//プラス
				//CAST(MID(DATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(DATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED)
				string_concat(&query, " ((MID(HEX(MID(DATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '3' AND ((CAST(MID(DATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(DATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//マイナス
				//CAST(MID(DATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(DATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED) * (-1)
				string_concat(&query, " (MID(HEX(MID(DATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '7' AND (((CAST(MID(DATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(DATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1), 1 )),2,1 ) as SIGNED)) * (-1)) ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) ) )");
			}else if(itrgt->sel[ii].type[0] == 'P' ||itrgt->sel[ii].type[0] == 'Q'){
				//符号なし
				//SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,(CEIL((4 + 1) / 2) * 2)) = 'C'
				//AND
				//(CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,(CEIL((4 + 1) / 2) * 2)) as SIGNED) <= 0)
				string_concat(&query, "( (SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) = 'F' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) as SIGNED) ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//プラス
				string_concat(&query, " (SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) = 'C' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) as SIGNED) ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//マイナス
				string_concat(&query, " (SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) = 'D' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) as SIGNED) * -1 ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) ) )");
			}else if(itrgt->sel[ii].type[0] == 'N'){
				//数値比較の場合
				string_concat(&query, " CAST(MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " ) as UNSIGNED)");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ");

			}else{
				//文字比較の場合
				string_concat(&query, " MID(DATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, "\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\"");
			}
		}else{
			break;
		}
	}

	//testのためKeyのみをgroup by
	if(itrgt->sum[0].s_point != 0){
		bool chk = false;
		for(ii = 0; ii < item_cnt ; ii++){
			//TODO::20170216::定数のものはついでに付加で問題ないか？
			//breakする前についでに定数のものを付加(keyとは関係ないがgroup byに必要)
			if(wkout[ii].s_point < 0){
				char tmp[10]="";
				string_concat(&query, " , item");
				//item[0-9]の[0-9]の部分を文字列に
				sprintf(tmp,"%d",(ii + 1));
				string_concat(&query, tmp);
			}

			char wk_point[WK_INTSTR_LEN]="";
			char wk_length1[WK_INTSTR_LEN]="";
			if(itrgt->key[ii].s_point == 0){
				break;
			}

			if (chk == false){
				string_concat(&query, " GROUP BY");
				chk = true;
			}else{
				string_concat(&query, " ,");
			}

			//nensrt参照
			//出力に必ずキーなどがあるわけではないようなのでキーをGroupの指定キーとする
			//wkoutはN(数),S(符号付数),P(packed),C(Constant)に分類
			sprintf(wk_point,"%d", itrgt->key[ii].s_point);
			sprintf(wk_length1,"%d",itrgt->key[ii].length);

			//定数のときは
			//残りすべての指定の時にはlengthなしで指定
			string_concat(&query, " MID(item,");
			string_concat(&query, wk_point);
			if(atoi(wk_length1) != 0){
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
			}
			string_concat(&query, " ) ");
		}
	}

	//ORDER BY
	for(ii = 0; ii < ITEMLST ; ii++){
		char wk_point[WK_INTSTR_LEN]="";
		char wk_length1[WK_INTSTR_LEN]="";
		char wk_length2[WK_INTSTR_LEN]="";

		if((itrgt->key[ii].s_point != 0) && (itrgt->key[ii].length != 0)){
			if(ii == 0){
				string_concat(&query, " ORDER BY BINARY ");
			}else{
				string_concat(&query, " ,");
			}

			sprintf(wk_point,"%d", itrgt->key[ii].s_point);
			sprintf(wk_length1,"%d",itrgt->key[ii].length);
			sprintf(wk_length2, "%d",itrgt->key[ii].length * 2);
			if(*(itrgt->key[ii].type) == 'S'){
			//符号付
			//CASE MID(HEX(MID(item,15 + (9 - 1),1)),1,1)
				string_concat(&query, " LPAD(CAST(CASE");
				string_concat(&query, " MID(HEX(MID(item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
			//WHEN '7' THEN (CAST(MID(item, 15 , (9 - 1) ) as SIGNED) * 10
				string_concat(&query, " WHEN '7' THEN (CAST(MID(item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
			//+ CAST(SUBSTR(HEX(MID(item, 15 + (9 - 1),1 )),2,1 ) as SIGNED) ) *(-1) END
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(item,  ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) * (-1) ");
			//WHEN '3' THEN (CAST(MID(item, 15 , (9 - 1) ) as SIGNED) * 10
				string_concat(&query, " ELSE (CAST(MID(item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
			//+ CAST(SUBSTR(HEX(MID(item, 15 + (9 - 1),1 )),2,1 ) as SIGNED) )
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(item,  ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) END as char),");
				string_concat(&query, wk_length1);
				string_concat(&query, ", '0') ");
			}else if(*(itrgt->key[ii].type) == 'P' || *(itrgt->key[ii].type) == 'Q'){
				//packed decimal
				//CASE SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,CEIL((4 + 1) / 2) * 2)
				string_concat(&query, " LPAD(CAST(CASE");
				string_concat(&query, "  SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2) ");
				//最後の文字はcastの内部変換で落ちる
				//WHEN 'C' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED)
				string_concat(&query, " WHEN 'C' THEN CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) as SIGNED) ");
				//WHEN 'D' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED) *(-1) END
				string_concat(&query, " WHEN 'D' THEN CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) as SIGNED) * (-1)");
				//WHEN 'F' THEN CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,CEIL((4 + 1) / 2) * 2) as SIGNED)
				string_concat(&query, " ELSE CAST(SUBSTR(HEX(MID(ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2) as SIGNED) ");
				string_concat(&query, " END as char),");
				string_concat(&query, wk_length1);
				string_concat(&query, ", '0') ");
			}else{
				string_concat(&query, " CAST(chCharMap(item,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )as binary) ");
			}
			if(strcmp(itrgt->key[ii].operand,"D") == 0) string_concat(&query, " DESC");
		}else{
			break;
		}
	}

	//SQL閉じ
//	if(item_cnt != 0){
		string_concat(&query, " ) AS t2 ");
//		string_concat(&query, " ORDER BY rownum ");
		string_concat(&query, " ) AS t1 ");
		string_concat(&query, " );");
//	}else{
//		string_concat(&query, " ); ");
//
//	}

	//クエリ作成終了で変数の使用終了
	free(wkstr);
	////////////////////////////////////////////////////
	//LockTableでTrunsactionがかかるので全体のTrunsactionの直前で
	setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ifi));
	setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ofi));
	//トランザクションスタート
	memset(sqlstr,'\0',strlen(sqlstr));
	strcpy(sqlstr,"START TRANSACTION;");
	ret = mysql_run(sqlstr);
	if(ret != 0){
		fprintf(stderr, " Error C [%02d]:START TRANSACTION Error %s \n",36, local_server_time(strTime));
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	//////////////////     sql     /////////////////////

	// temporary作成
	memset(sqlstr,'\0',strlen(sqlstr));
	sprintf(sqlstr,"CREATE TEMPORARY TABLE tmp (ID INT NOT NULL AUTO_INCREMENT PRIMARY KEY, ITEM VARBINARY(%d));", DATA_MAX_LEN );
	//strcpy(sqlstr,"CREATE TEMPORARY TABLE tmp (ID INT NOT NULL AUTO_INCREMENT PRIMARY KEY, ITEM VARBINARY(1024));");
	ret = mysql_run(sqlstr);
	if(ret != 0){
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		fprintf(stderr, "CREATE TEMPORARY Error %s \n", local_server_time(strTime));
		return 1;
	}

	//SQL発行(TEMPORARYにデータ移行)
//	fprintf(stderr,query);
	ret = mysql_real_query(mysqlConn,query.data,query.size);
	if(ret != 0){
		//debug
		mysql_failure();
		string_fin(&query);
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	//使ったら開放
	string_fin(&query);

	//移行先テーブルのクリアの準備
	string_init(&query,   "SELECT ID FROM ");
	string_concat(&query, check_tablename(itrgt->ofi));
	string_concat(&query, "  FOR UPDATE ;");
	//SQL発行(移行先データロック)
	ret = mysql_real_query(mysqlConn,query.data,query.size);
	if(ret != 0){
		mysql_failure();
		string_fin(&query);
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	mysqlResult = mysql_use_result(mysqlConn);
	mysql_free_result(mysqlResult);
	//使ったら開放
	string_fin(&query);

	//移行先テーブルのクリア
	string_init(&query,   "TRUNCATE TABLE ");
	string_concat(&query, check_tablename(itrgt->ofi));
	string_concat(&query, ";");
	//SQL発行
	ret = mysql_real_query(mysqlConn,query.data,query.size);
	if(ret != 0){
		mysql_failure();
		string_fin(&query);
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	//使ったら開放
	string_fin(&query);

	//tmp⇒ソート対象へ
	string_init(&query,   "INSERT INTO ");
	string_concat(&query, check_tablename(itrgt->ofi));
	string_concat(&query, " (ITEM) SELECT ITEM FROM tmp ORDER BY ID;");

	//SQL発行
	ret = mysql_real_query(mysqlConn,query.data,query.size);
	if(ret != 0){
		mysql_failure();
		string_fin(&query);
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}
	//使ったら開放
	string_fin(&query);

	//テンプレートテーブルの削除
	ret = mysql_run("DROP TABLE tmp;");
	if(ret != 0){
		//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)
		return 1;
	}

	//コミット実行
	//コミット(ここまで来ていたらすべて成功している)
	ret = mysql_run("COMMIT");
	//ここまで来たらUnlock
	setTableUnlock(targSetting, itrgt);

	//DB接続の切断(外で接続を繋いだので外で切る upd koyama 20161108)

	return ret;
}

//author:iida
//update:koyama  20140911
int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	DB_SETTING targSetting; //add koyama 20161107 共通化

	Trgt_inf *trgt;
	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //メモリ確保
	//このパラメータ領域使われていない？ comment koyama

	if(db_init(&targSetting)){
		fprintf(stderr," Error [%02d] %s : DB Initialize Error \n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}
	//初期化
	trgt_ini(trgt);

	//プロセス名とユーザ名を取得
	getUserAndProcessName(source_file_name, source_user_name);

	//引数が全くなしはダメ(プログラム名は必ずある)
	if(argc <= 1){
		fprintf(stderr," Error [%02d] %s : Option is nothing\n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}
	//	printf("引数の個数: %d \n",argc);	//デバッグ用
	for(ii=1;ii < argc; ii++){
		//第二引数を「_」でオプション別に区分けする
		if(myConfDebugFlg > 0){
			FILE  *fp;
			char strCmd[1024]="";
			sprintf(strCmd,"logger -i \"%.768s\"",argv[ii]);
			if ( (fp=popen(strCmd,"r")) !=NULL) {
				//コマンドの実行に失敗したら空文字列でreturn
				pclose(fp);
			}
		}
		ret = set_trgt(trgt,argv[ii]);
	}

	if(ret==1){
		fprintf(stderr,"SORT OPTION Error %s : %s\n", local_server_time(strTime),argv[1]);
		//強制終了
		free(trgt);
		exit(ret);
	}else{
		//引数チェックを行う
		if(checkTrgtCorrect(trgt)){
			//エラーの場合関数の中でエラー出力
			//強制終了
			free(trgt);
			exit(1);
		}
	}

	// DB処理
	ret = mysql_Init(trgt,&targSetting);

	//DBのCloseはreturnに関係なくここでする
	DB_Close();
	// メモリ解放
	free(trgt);

	return ret;
}
