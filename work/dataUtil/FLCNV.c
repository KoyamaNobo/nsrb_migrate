#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <mysql.h>
#include <time.h>
#include <signal.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/

// container_of マクロ
#define container_of(ptr, type, member) ({                              \
        const typeof( ((type *)0)->member ) *__mptr = (void *)(ptr);    \
        (type *)( (char *)__mptr - offsetof(type,member) );})

/*
#define container_of(ptr, type, member) ({                              \
		(type *)( (char *)(ptr) - offsetof(type,member) );})
*/

//DBのタグ名
#ifndef CONF_DB_TAG
#define CONF_DB_TAG 1
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
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";            //20150915ローカルに conf.xml がないときの対応
char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif
#define OPER_LEN (4)		// 演算子の文字列長
#define ITEMLST 256		// SORT対象構造体の要素数
#define SQLSTR (2048)		// SQL文字列の最大長
#define MAP_SRC_FUNC_LEN 50
#define PATHNAME_SIZE 1024
#define MODECHLEN 3

#define mysql_failure() __mysql_failure(__func__, __LINE__)

//定数
#define WK_INTSTR_LEN 11
const int  NSHIFT     = 4;            // 引数開始文字判定用の移動距離
const int  ARGITM     = 5;            // 引数構造体の要素数
//const int  ITEMLST    = 20;         // SORT対象構造体の要素数
const char *EXCLUSIVE = "EXCLUSIVE";  //2015/08/28 kawada add オープンモード：排他(EXCLUSIVE)
const char *PROTECT   = "PROTECT";    //2015/08/28 kawada add オープンモード：保護(PROTECT)
const char *SHARE     = "SHARE";      //2015/08/28 kawada add オープンモード：共用(SHARE)

//DB設定構造体
struct db_setting {
	char db_server[64];        // host名
	char db_user[64];          // user名
	char db_password[64];      // password
	char db_database[64];      // database名
	int db_port;               // port番号
	char db_manatbl[64];       // 管理table名    //2015/08/28 kawada add 管理テーブル名
}db_setting={"","","","",3306};
typedef struct db_setting DB_SETTING;

//SORT対象の引数構造体
struct argv_inf {
	int  s_point;          // 開始位置
	int  length;           // 文字列長
	char *type;            // 出力タイプ
	char *operand;         // 比較対象フィールド
	char *value;           // 出力フィールド構成
	int  logical;          // 論理演算子 0:AND,1:OR,-1:その他
//	char *opmod;           // DOS形式のファイルオープンモード Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE（xは変換方法により変わる）	//2015/08/28 kawada add

};
typedef struct argv_inf Argv_inf;

//SORT対象構造体
struct trgt_inf {
//	各引数値の最大文字列長
//	char ifi[12];              // 入力ファイル名
//	char ofi[12];              // 出力ファイル名
//	char mod[03];              // 書き込み方法
//	char out[120];             // 出力フィールド構成
//	char sel[120];             // where句
	char     *ifi;             // 入力ファイル名
	char     *ofi;             // 出力ファイル名
	int      mod;              // 書き込み方法 0:ADD,1:NEW
	char     opmod[MODECHLEN]; // DOS形式のファイルオープンモード Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE（xは変換方法により変わる）
	Argv_inf out[ITEMLST];     // 出力フィールド構成
	Argv_inf sel[ITEMLST];     // where句
//	int      logical;          // 論理演算子 0:AND,1:OR,-1:その他
};
typedef struct trgt_inf Trgt_inf;

//文字列構造体
struct string_t{
	size_t size;            // 文字列の長さ + NIL文字の長さの合計値
	char   *data;           // データの先頭
};
typedef struct string_t String_t;

///////////////////////////////////////////////Function liet AND prototype Start
//SORT対象構造体の初期化
void argv_ini(Argv_inf * );
//SORT対象構造体の初期化
void trgt_ini(Trgt_inf * );
//SORT対象構造体の解放
void trgt_free(Trgt_inf * );
// recieve INTERRUPT signal
void* sigInt( int  );
// recieve SegmentationFalet signal
void* sigSegv( int  );
// recieve Abort signal
void* sigAbrt( int  );
// recieve USER1 signal
void* sigUsr1( int  );
// recieve USER2 signal
void* sigUsr2( void* );
//エラーハンドルを設定
int setErrorHundle();
//RTrim実装
int isNullOrEmpty(char *);
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
char *local_server_time(char*);
//プロセス名を返す
int getUserAndProcessName(char *,char *);
//プロセス名を返す
char *getprocessName(char *,int );
//--------String_t関数系--------
// 可変長文字列 *ioData を iStr で初期化する
int string_init(String_t *ioData, char const * iStr);
//エラーにプロセス名を付けて出力
void mytool_runtime_error(char ,const char *sfile,const char *fmt, ...);
//設定ファイルのパスを取得
char *getConfFilename(char *);
//設定ファイルの読み込み
int conf_read(DB_SETTING *);
//テーブル名の前後の「`」を除去している
char *convertTablenameToVariable(char *);
//--------データベース系--------
// ユニークID生成
int getUid();
//処理対象ファイルのLOCK処理
int setTableLock(DB_SETTING *, Trgt_inf *,char *);
//処理対象ファイルのUNLOCK処理
int setTableUnlock(DB_SETTING*, Trgt_inf*);
//設定の読み込みとデータベースへの接続を行う
int db_init(DB_SETTING *);
//グローバル変数から設定を取得し、データベースに接続する
int DB_Open(DB_SETTING *);
//設定の値を使ってデータベースを閉じる
int DB_Close();
//SQL実行エラー時の後処理
int __mysql_failure(const char *,int );
//SQL発行
int mysql_run(char * );
///////////////////////////////////////////////Function liet AND prototype End

///////////////////////////////////////////////Global Variables Start
int unqId = 0;            //ユニークID(管理テーブル認証用)		//20160506 add koyamaテーブルアクセスの管理に伴い
char *myoptions=0;            //一番最後ののパスを取得(初期設定)

static char source_file_name[PATHNAME_SIZE];
static char source_user_name[PATHNAME_SIZE];

//confから取得するdebug_flg.他と被らないようにoriginal name
static int myConfDebugFlg;

MYSQL *mysqlConn;
MYSQL_RES *res;
MYSQL_ROW row;
char map_source_func[MAP_SRC_FUNC_LEN]="";

DB_SETTING *background_targSetting;
Trgt_inf   *background_itrgt;

///////////////////////////////////////////////Global Variables End

//*********************************************************
//SORT対象構造体の初期化
//*********************************************************
void argv_ini(Argv_inf *ioargv ) {
//	"開始位置", "文字列長", "出力ファイル名", "比較対象フィールド", "出力フィールド構成"
//	Argv_inf wk_argv ={0,0,"","",""};
	ioargv->s_point  =0;
	ioargv->length   =0;
	ioargv->type     =NULL;    //NULLPOで初期化
	ioargv->operand  =NULL;    //NULLPOで初期化
	ioargv->value    =NULL;    //NULLPOで初期化
	ioargv->logical  =-1;
//	ioargv->opmod    =NULL;    //NULLPOで初期化
//	*ioargv = wk_argv;
}


//*********************************************************
//SORT対象構造体の初期化
//*********************************************************
void trgt_ini(Trgt_inf *iotrgt ) {
	int ii = 0;
	//初期化内容
//	char     *wk_ifi = "";            // 入力ファイル名
//	char     *wk_ofi = "";            // 出力ファイル名
//	int       wk_mod = 0;            // 書き込み方法 0:ADD,1:NEW
//	Argv_inf *wk_out[ITEMLST];            // 出力フィールド構成
//	Argv_inf *wk_sel[ITEMLST];            // where句
//	int       wk_logical = 0;            // 論理演算子 0:AND,1:OR,-1:その他

	Trgt_inf wktrgt;

	*iotrgt = wktrgt;

	iotrgt->ifi = 0;    //NULLPOで初期化
	iotrgt->ofi = 0;    //NULLPOで初期化
	iotrgt->mod = 1;
	memset(iotrgt->opmod,'\0',MODECHLEN);
//	iotrgt->opmod = (char *)malloc(sizeof(char) * 2);            //DOS形式のファイルオープンモード	//2015/08/28 kawada add

	for(ii = 0; ii < ITEMLST ; ii++){
		argv_ini(&iotrgt->out[ii]);
		argv_ini(&iotrgt->sel[ii]);
	}

//	iotrgt->logical = 0;

}

// recieve INTERRUPT signal
// Author koyama
void* sigInt( int args ){
	int i = 0;

	//エラー出力をすることでAbortを設定
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve interrupt signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve SegmentationFalet signal
// Author koyama
void* sigSegv( int args ){
	int i = 0;

	//エラー出力をすることでAbortを設定
	if (map_source_func && strlen(map_source_func) > 0) {
		mytool_runtime_error('E',source_file_name, " Error C [99]:recieve Segmentation Fault signal :%s %s ", map_source_func,myoptions);
	}else{
		mytool_runtime_error('E',source_file_name," Error C [99]:recieve Segmentation Fault signal  %s  ",myoptions);
	}
	raise(SIGTERM);

	return NULL;
}
// recieve Abort signal
// Author koyama
void* sigAbrt( int args ){
	int i = 0;
	//エラー出力をすることでAbortを設定
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve Abort signal ");
	raise(SIGTERM);

	return NULL;
}


// recieve USER1 signal
// Author koyama
void* sigUsr1( int args ){
	int i = 0;

	//エラー出力をすることでAbortを設定
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve usr1=kill signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve USER2 signal
// Author koyama
void* sigUsr2( void *args ){
	int i = 0;

	//エラー出力をすることでAbortを設定
	raise(SIGSTOP);

	return NULL;
}

//
//エラーハンドルを設定
//author : koyama
int setErrorHundle(){
	int retval;
	retval = 0;

	//signal
	//
	if (signal(SIGINT, (__sighandler_t)sigInt) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//KILL処理
	if (signal(SIGUSR1, (__sighandler_t)sigUsr1) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//STOP処理
	if (signal(SIGUSR2, (__sighandler_t)sigUsr2) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGSEGV処理
	if (signal(SIGSEGV, (__sighandler_t)sigSegv) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGABRT処理
	if (signal(SIGABRT, (__sighandler_t)sigAbrt) == SIG_ERR){
		retval = 1;
		return retval;
	}
	return retval;

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


//プロセス名を返す
//in/out :setName プロセス名を返す文字列ポインタ in:size プロセス名を返す文字列の長さ
//author : koyama
char *getprocessName(char *setName,int size){
	FILE  *fp;
	char  buf[1024];   //コマンドの受け取り用
	int   pid=0;
	char  cmd[1024]="";
	pid = getpid();
	sprintf(cmd,"ps -p %d -o comm= ",pid);

	memset(setName,'\0',size+1);
	if ( (fp=popen(cmd,"r")) ==NULL) {
		//コマンドの実行に失敗したら空文字列でreturn
		return setName;
	}
	memset(setName,'\0',size+1);
	//結果をsize分だけコピー
	//溢れ定義は必要？
	while(fgets(buf, 1024, fp) != NULL) {
		if((strlen(setName) + strlen(buf) ) > size){
			//コマンドの実行に失敗したら空文字列でreturn
			memset(setName,'\0',size+1);
			return (char *)0;
		}
		strcat(setName,buf);
	}
	pclose(fp);
	//ひとつ目の改行までで切る
	if( strchr(setName, '\n') ){
		*(strchr(setName, '\n')) = '\0';
	}
	return setName;
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

//特定の文字を置換しながらつなげる
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

	for(;preneedle != NULL && ocnt < 4096;ocnt++){
		//needleがなくなるまで
		for(icnt = 0; *(iDelim + icnt) != '\0';icnt++){
			postneedle = strchr(preneedle,*(iDelim + icnt));
			//同じ文字が連続している場合は飛ばす
			for(;postneedle !=NULL && *(postneedle + 1) == *(iDelim + icnt);){
				postneedle = (postneedle + 1);
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

	//文字列要素「@」囲みを検索
	if(strstr(iStr, "@") != NULL){
		if(str2 < strstr(iStr, "@")){
			while(str1 > str2){
				if(strstr(strstr(iStr, "@"),"@") != NULL){
					str_flg = 1;
					str1 = strstr(strstr(iStr, "@"),"@");
				}
			}

		}
	}

	//区切り文字を検索
	if(str_flg == 1){
		while(str1 > str2){
			str2 = strstr(str2, ")");
		}
		tk = str2;
		oList[cnt++] = tk;
		tk = strtok( str2, ")" );
	}else{
		tk = strtok( iStr, ")" );
	}

	while( tk != NULL && isNullOrEmpty(tk)!=0 && cnt < 1000 ) {
		oList[cnt++] = tk;
		tk = strtok( NULL, ")" );
	}

	return cnt;
}

//*********************************************************
//文字列置換(バイナリデータ非対応)
//add comment koyama string.hの関数を使っているため'\0'(NULL)に対応しない
//*********************************************************
int strChg(char *ioBuf, const char *iStr1, const char *iStr2)
{
	int		ret = 1;
	char	tmp[1024 + 1];
	char	*p;
	char	s = '\0';

	while ((p = strstr(ioBuf, iStr1)) != NULL) {
		//見つからなくなるまで繰り返す。pは旧文字列の先頭を指す
		*p = s;            // 元の文字列を旧文字列の直前で区切って
		p += strlen(iStr1);            // ポインタを旧文字列の次の文字へ
		strcpy(tmp, p);            // 旧文字列から後を保存
		strcat(ioBuf, iStr2);            // 新文字列をその後につなぎ
		strcat(ioBuf, tmp);            // さらに残りをつなぐ
		ret = 0;            // ヒットした場合に戻り値変更
	}

	return ret;
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

	//SEL
	if(strncmp(strTarg,"SEL",strlen("SEL")) == 0 && (intNum > 5)){
		fprintf(stderr," %s : Error C [%02d]:Too many arguments to %.3s                %s "
			,source_file_name,22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}
	//OUT
	if(strncmp(strTarg,"OUT",strlen("OUT")) == 0 && intNum > 2){
		fprintf(stderr," %s : Error C [%02d]:Too many arguments to %.3s                %s \n"
			,source_file_name,23,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
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

//--------String_t関数系--------関数Start
//*********************************************************
// 可変長文字列 *ioData を iStr で初期化する
//*********************************************************
int string_init(String_t *ioData, char const * iStr){
	int ret = 0;

	// size_t size = strlen(iStr);
	size_t size = 8192;

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

//*********************************************************
// 可変長文字列の長さを返す
//*********************************************************
size_t string_length(char ** ioData){
	return (container_of(*ioData, String_t, data))->size - 1;
}

//*********************************************************
// 連結する
//*********************************************************
int string_concat(String_t * ioData, char *iStr){
	int ret = 0;

  if(iStr==NULL){
    //つなぐ変数がNULLなら何もしない
    return ret;
  }
	size_t size = strlen(iStr) + ioData->size;

	/* 確保した領域をNULLクリア */
	if(ioData->data != NULL){
		//後ろに文字をそのまま繋ぐのでNULLは繋がない
		memcpy(ioData->data + ioData->size, iStr, strlen(iStr));
		ioData->size = size;
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
		fprintf(stderr," Error C [%02d]:Parentheses is not supported              %s \n",10,local_server_time(strTime));
		exit(1);
	}

	if(isNullOrEmpty(iArgv)){
		return 0;
	}

	remTargChar(iArgv,0x5c);
	remCRLF(iArgv);
	result_ii = strSplit_ELM(iArgv,strOptArr);

	if(result_ii >= ITEMLST){
		fprintf(stderr," Error C [%02d]:Too many arguments                        %s \n",16,local_server_time(strTime));
		exit(1);
	}

	for(ii=0; ii< result_ii; ii++){
		//不要な括弧,A/Oの削除
		if(ii > 0){
			//AND/OR条件の場合(区切り文字の先頭がA/O)
			if(*strOptArr[ii] == 'A'){
				ioTrgt_arg[ii].logical = 0;
			}else if(*strOptArr[ii] == 'O'){
				ioTrgt_arg[ii].logical = 1;
			}else{
				if(ii==0 || *strOptArr[ii] == ','){
					ioTrgt_arg[ii].logical = -1;
				}else{
					fprintf(stderr," Error C [%02d]:comma can't be found                %s \n",12,local_server_time(strTime));
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

		//引数の数を判定
		//エラーの時は関数内でexit
		checkParamNum(strOptArr[0],result_jj);

		//最初の要素と元のstringのポインタ位置は同じはず
		if(strArgArr[0] != strOptArr[ii]){
			fprintf(stderr," Error C [%02d]:param parse error                       %s \n",11,local_server_time(strTime));
			exit(1);
		}

		if((strlen(strOptArr[ii]) > 0 && result_jj == 0) || result_jj > ARGITM){
			fprintf(stderr," Error C [%02d]:Can't determine specific element        %s \n",13,local_server_time(strTime));
			exit(1);
		}

		for(jj=0; jj< result_jj; jj++){
			switch(jj){
			case 0:
				//初期化
				ioTrgt_arg[ii].s_point  = 0;            // 開始位置
				ioTrgt_arg[ii].length   = 0;            // 文字列長
				ioTrgt_arg[ii].type     = "";            // 出力タイプ
				ioTrgt_arg[ii].operand  = "";            // 比較対象フィールド
				ioTrgt_arg[ii].value    = "";            // 出力フィールド構成

				if (result_jj == 1){
					//()内の第一項目がひとつ(,区切りでない)なら定数
					//＠マークの除去
					if(strchr(strArgArr[jj],0x40) != 0){
						strChg(strArgArr[jj],"@","");
						//(0だと初期値扱いで以降の処理できない)
						ioTrgt_arg[ii].s_point = -99999;
						ioTrgt_arg[ii].value   = strArgArr[jj];
					}else{
						fprintf(stderr," Error C [%02d]:param parse error                       %s \n",11,local_server_time(strTime));
						exit(1);
					}
				}else{
					ioTrgt_arg[ii].s_point = atoi(strArgArr[jj]);
					//ここに入ったのに開始位置が0以上でないことはあり得ない
					if(ioTrgt_arg[ii].s_point <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
						fprintf(stderr," Error C [%02d]:Start point is incorrect          %s \n",15,local_server_time(strTime));
						exit(1);
					}
				}
				break;
			case 1:
				ioTrgt_arg[ii].length  = atoi(strArgArr[jj]);
				//ここに入ったのに長さが0以上でないことはあり得ない
				if(ioTrgt_arg[ii].length <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
					fprintf(stderr," Error C [%02d]:Length is incorrect                 %s \n",14,local_server_time(strTime));
					exit(1);
				}
				break;
			case 2:
				ioTrgt_arg[ii].type    = strArgArr[jj];
				break;
			case 3:
				ioTrgt_arg[ii].operand = strArgArr[jj];
				break;
			case 4:
				if(strchr(strArgArr[jj],'@') == 0 || strchr((strchr(strArgArr[jj],'@') + 1),'@') == 0
					|| searchChr(strArgArr[jj],'@') > 2){
					fprintf(stderr," Error C [%02d]:@ Parentheses is not supported      %s \n",10,local_server_time(strTime));
					exit(1);
				}
				//＠マークの除去
//				strChg(strArgArr[jj],"@","");
				*(strchr((strchr(strArgArr[jj],'@') + 1),'@')) = '\0';
				ioTrgt_arg[ii].value   = (strchr(strArgArr[jj],'@') + 1);
				break;
			default:
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

//必須入力等の引数が正しく入っているか
//ioTrgt:引数オブジェクト
//return エラー1,正常0
//author:koyama
int checkTrgtCorrect(Trgt_inf *ioTrgt){
	char strTime[] = "0000/00/00 00:00:00.000000";

	//入力がないときは出力先クリア
//	if(strlen(ioTrgt->ifi) == 0){
//		fprintf(stderr," Error [%02d]:Where is the input source  %s \n",01,local_server_time(strTime));
//		return 1;
//	}
	if(strlen(ioTrgt->ofi) == 0){
		fprintf(stderr," Error C [%02d]:Where is the output destination           %s \n",02,local_server_time(strTime));
		return 1;
	}
	return 0;
}

//*********************************************************
//引数分解関数
//*********************************************************
int set_trgt(Trgt_inf *ioTrgt, char *iArgv){
	int ii,ret = 0, cnt =0;
	char wkstr[120+1] = "";
	char *strOptArr[20+1] = {'\0'};            //オプション別
	char *strTmpArgv = iArgv;

	//定数の間のSPをいったん置換
	chgChar(strTmpArgv,(char)0x20,(char)0x07,(char)0x40);
	//第二引数を「 」でオプション別に区分けする
	int result = strSplit(strTmpArgv," ",strOptArr);

	for(ii=0; ii< result; ii++){
		chgChar(strOptArr[ii],(char)0x07,(char)0x20,(char)0x40);
		if (strstr(strOptArr[ii], "IFI") != NULL ) {
			//入力ファイル名設定
			ioTrgt->ifi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "OFI=") != NULL ) {
			//出力ファイル名設定
			ioTrgt->ofi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "MOD=") != NULL ) {
			//書き込み方法
			if((strstr(strOptArr[ii], "ADD") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//追加書き込みの場合
				ioTrgt->mod = 0;
			}else if(((strstr(strOptArr[ii], "CRE") != NULL ) || (strstr(strOptArr[ii], "CREATE") != NULL )) && ( strlen(strOptArr[ii]) > 0)){
				//新規書き込みの場合
				ioTrgt->mod = 1;
			}else{
				//上記以外
				ioTrgt->mod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "OUT=") != NULL ) {
			//出力フィールド構成設定
			ret = set_argv(ioTrgt->out, (strchr(strOptArr[ii],'=') + 1));            //要素分解
			continue;
		}else if (strstr(strOptArr[ii], "SEL=") != NULL ) {
			//対象条件設定
//			if((strstr(strOptArr[ii], ")A(") != NULL ) && (strlen(strOptArr[ii]) > 0)){
//				//Where句の場合(AND条件)
//				ioTrgt->logical = 0;
//			}else if((strstr(strOptArr[ii], ")O(") != NULL ) && ( strlen(strOptArr[ii]) > 0)){
//				//Where句の場合(OR条件)
//				ioTrgt->logical = 1;
//			}else{
//				//Where句以外の場合
//				ioTrgt->logical = -1;
//			}
			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));            //要素分解
		}else if (strstr(strOptArr[ii], "PB3=") != NULL ) {
			//DOS形式のファイルオープンモード設定
			char strPara[20]="";
			if (strlen(strOptArr[ii]) > strlen("PB3=")){
				strcpy(strPara, (strchr(strOptArr[ii],'=') + 1));            //指定があるときのみ変数へ複写
			}
			if (strcmp(strPara, EXCLUSIVE) == 0) {
					//EXCLUSIVE
					strcpy(ioTrgt->opmod, "E");
			}else if (strcmp(strPara, PROTECT) == 0) {
					//PROTECT
					strcpy(ioTrgt->opmod, "P");
			}else if (strcmp(strPara, SHARE) == 0) {
					//SHARE
					strcpy(ioTrgt->opmod, "S");
			}else if (strPara[0] == '\0') {
					//(指定なし)
					strcpy(ioTrgt->opmod, "S");            //指定がないときはSHAREとする
			}else{
					//指定があって規定以外
					strcpy(ioTrgt->opmod, "_");
			}
			if (strcmp(ioTrgt->opmod, "_") != 0){
				//処理モードによりファイルオープンモードを確定
				if (ioTrgt->mod == 0){
					//AV-X->DOS
					strcat(ioTrgt->opmod, "I");
				}else if (ioTrgt->mod == 1) {
					//DOS->AV-X
					strcpy(ioTrgt->opmod, "SO");            //いずれの指定でもSHARE OUTPUT(SO)に置き換え
				}
			}
			continue;
		}
	}

	return ret;
}


//*********************************************************
//int ⇒ char 変換
//*********************************************************
char *itoa( int val, char *str, int radix )
{
	char *p = str;
	unsigned int v = val;            // 作業用(変換対象の値)
	int n = 1;            // 変換文字列の桁数記憶用
	while(v >= radix){					// 桁数を求める
		v /= radix;
		n++;
	}
	p = str + n;            //最下位の位置から設定する
	v = val;
	*p = '\0';            // 文字列終端の設定
	do {
		--p;
		*p = v % radix + '0';            // 1桁の数値を文字に変換
		if(*p > '9') {					// 変換した文字が10進で表現できない場合
			*p = v % radix - 10 + 'A';            // アルファベットを使う
		}
		v /= radix;
	} while ( p != str);

	return str;
}


//
//テーブル名check
//
char * check_tablename(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="`";

  if(strTable == NULL){
    //NULLの場合はそのまま返す
    return strTable;
  }
	if(*(strTable + 0) != 0x60 && strlen(strTable) > 0){
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

	//DBUNLOCKを試行
	if(typeChar == 'E'){
		//DBUNLOCKを試行
		setTableUnlock(NULL,NULL);
	}
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
			fprintf(stderr, "%s : Error C [%02d]:conf read Error %s \n",source_file_name, local_server_time(strTime));
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

//*********************************************************
//設定ファイルの読み込み
//*********************************************************
int conf_read(DB_SETTING *targSetting){
	int ii;
	//2015/08/28 kawda S ローカルに conf.xml がないときの対応
	char strConfPath[1024]; //20150828 add koyama

	myConfDebugFlg = 0;
	//ファイルネームを元にリーダポインタを作成   //ファイル名を変数に変更 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//2015/08/28 kawda E
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
				//設定ファイルから共有モード用のテーブル名を取得
				if(strcmp(node->parent->name,MANA_TBL_NAME) == 0){
					strcpy(targSetting->db_manatbl,node->content);
				}
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

//テーブル名check
//add commment テーブル名の前後の「`」を除去している
char *convertTablenameToVariable(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="";

	if(strTable==NULL || strlen(strTable) == 0){
    //NULLまたは空文字列のときは処理なし
    return strTable;
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
//--------データベース系--------関数Start
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
	char strTime[] = "0000/00/00 00:00:00.000000";            //時間の文字列を格納

  if(lckTableName==NULL || strlen(lckTableName)==0){
    //テーブル名なし、または、テーブル変数NULLの場合は処理なし
    return 0;
  }
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
	strcat(sqlStr,"SELECT LockMode FROM `");
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

		sprintf(strUid,"%d", unqId);
		//openModeは固定にしておく
		//lckTableNameがofiと一致していたらofi,そうでなければifi
    //ifiは指定なしのケースがあるのでofiで判断
		if(strcmp(lckTableName,itrgt->ofi) == 0){
      sprintf(strSS,"%s", "SE");
		}else{
      sprintf(strSS,"%s", "SI");
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
	char strTime[] = "0000/00/00 00:00:00.000000";

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
	if(mysql_query(mysqlConn, sqlStr) != 0){
		DB_Close();
		// fprintf(stderr, " Error C [%02d]:can't run the Transaction %s %s          %s \n", 91, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	sprintf(strUid, "%d", unqId);

	strcat(sqlStr,"UPDATE `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` SET Uid = NULL, LockMode = '");
	strcat(sqlStr, "    ");            //LOCKMODE DEFAULT VALUE
	strcat(sqlStr,"' , LockDateTime = now() ");
	strcat(sqlStr," WHERE Uid = ");
	strcat(sqlStr,strUid);
	strcat(sqlStr,";");

	// SQLの実行
	if(mysql_query(mysqlConn, sqlStr) != 0){
		// mysql_failure();
		ret = 1;
		return ret;
	}else{
		//使った変数をリセット
		memset(sqlStr,'\0',strlen(sqlStr));
		//コミット
		strcat(sqlStr,"COMMIT; ");
		if(mysql_query(mysqlConn, sqlStr) != 0){
			// mysql_failure();
			DB_Close();
			fprintf(stderr," Error C [%02d]:can't lock %s \n", 92, itrgt->ifi);
			ret = 1;
			return ret;
		}
	}

	return ret;
}
// -----------------------------------------------------Lock,Unlock END

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

	unqId = getUid();            //2015/08/28 kawada add 管理テーブルの更新用にユニークIDを取得する

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

//	printf("mysql_run :%s\n",iquery);            //デバッグ用
	ret = mysql_real_query(mysqlConn, iquery,query_size);

	if(ret != 0){
		mysql_failure();
		ret = 1;
	}

	return ret;
}

//トランザクションのスタートと関連の処理を行う
//return 失敗したら1 正常終了は0
//author:koyama 20170111
int executeStartTrunsaction(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[1024]="";
  char strTime[] = "0000/00/00 00:00:00.000000";

  sprintf(strSql, "START TRANSACTION;");
	if(mysql_query(Connection,strSql) != 0){
    fprintf(stderr, " Error C [%02d]:trunsaction start Error %s %s\n", 31,local_server_time(strTime), mysql_error(mysqlConn));
		setTableUnlock(targSetting, itrgt);
    return 1;
  }
  return 0;
}

//テンポラリーテーブルの作成
//return 失敗したら1 正常終了は0
//author:koyama 20170111
int executeCreateTemporary(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[1024]="";

  sprintf(strSql, "CREATE TEMPORARY TABLE tmp (ID INT NOT NULL AUTO_INCREMENT PRIMARY KEY, ITEM VARBINARY(1024));");
  if(mysql_query(Connection, strSql) != 0){
    setTableUnlock(targSetting, itrgt);
    return 1;
  }
  return 0;
}


//作成したテンポラリテーブルをターゲットの出力テーブルに
//return 失敗したら1 正常終了は0
//author:koyama 20170111
int executeTmpToTargTable(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[2048]="";

  sprintf(strSql,"INSERT INTO %s (ITEM) SELECT ITEM FROM tmp ORDER BY ID;"
  ,check_tablename(itrgt->ofi));

	//infileがないときは中身を削除するだけ add 20141117 koyama
	if(itrgt->ifi != NULL && strlen(itrgt->ifi) > 0){
		//SQL発行
		if(mysql_query(Connection,strSql) != 0){
			setTableUnlock(targSetting, itrgt);
			//エラーのときはmysql_failureで
			mysql_failure();
			DB_Close();
			return 1;
		}
	}
}

//作成したtmpという名前のテンポラリテーブルを削除する
//return 失敗したら1 正常終了は0
//author:koyama 20170111
int dropTemporaryTable(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  if(mysql_query(Connection,"DROP TABLE tmp;") != 0){
		setTableUnlock(targSetting, itrgt);
		//エラーのときはmysql_failureで
		mysql_failure();
		DB_Close();
		return 1;
	}
  return 0;
}

//*********************************************************
//DB移行処理
//*********************************************************
int dbExecute(Trgt_inf *itrgt,DB_SETTING *targSetting){
	String_t query;            //初期設定を読み込むSQLの格納
	int  query_size = 0;            //SQL文字列の長さを保持する(最大値はsizeof(query)まで)
	char temp[256] = "";            //SQL用temporary
	void *buffer[100];
	char *strWhereArr[100] = {'\0'};            //where句が複数ある時のために
	char strType[64] = "";
	int ii,jj, ret = 0,item_cnt = 0;
	Argv_inf wkout[ITEMLST * 2];            //表示用フィールド配列
	char wkstr[100];
	char *wkstr_p;
	char wk_point[WK_INTSTR_LEN];
	char wk_length1[WK_INTSTR_LEN];
	char wk_length2[WK_INTSTR_LEN];
	char strTime[] = "0000/00/00 00:00:00.000000";
	char sqlstr[127] = "";            //SQｌステートメント(短いものはこちらで処理)


	//初期化
	for(ii = 0; ii < ITEMLST * 2 ; ii++){
		argv_ini(&wkout[ii]);
	}

	//設定の読み込み(グローバル変数へ格納)
	if (conf_read(targSetting) != 0){
		fprintf(stderr, "conf read Error %s :%s\n", local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}

	//DB接続
	if ((mysqlConn = mysql_init(mysqlConn)) == NULL) {
		fprintf(stderr, "db init Error %s :%s\n", local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}
	if(mysql_real_connect(mysqlConn, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0) == NULL) {
		fprintf(stderr, "db connect Error %s :%s\n", local_server_time(strTime), mysql_error(mysqlConn));
		return 1;
	}

	unqId = getUid();            //ユニークIDを作成 //add koyama 管理テーブル更新に伴い
  if(checkTrgtCorrect(itrgt) == 1){
    //エラーメッセージは関数内で出力
    return 1;
  }
	//20160817 add koyama LockMode更新 upd 20161109
	if ((setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ifi)) != 0 ) || (setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ofi)) != 0)){
		setTableUnlock(targSetting, itrgt);
		return 1;
	}

	//トランザクションスタート

	//////////////////     sql     /////////////////////
	// temporary作成
  //function化20170111
  if(executeCreateTemporary(mysqlConn,targSetting,itrgt)){
    string_fin(&query);
    return 1;
  }

	for(ii = 0; ii < ITEMLST ; ii++){
		if(itrgt->out[ii].s_point != 0) {
			//表示対象の項目
			wkout[item_cnt].s_point = itrgt->out[ii].s_point;
			wkout[item_cnt].length  = itrgt->out[ii].length;
			wkout[item_cnt].type    = itrgt->out[ii].type;
			wkout[item_cnt].value   = itrgt->out[ii].value;

			item_cnt += 1;
		}else{
			break;
		}
	}

	//SQL初期化
	string_init(&query,   "INSERT INTO tmp (ITEM) (");
	string_concat(&query, " SELECT ");

	//item_cntが0のときはitemを変換していく
	if(item_cnt != 0){
		string_concat(&query, " CAST(CONCAT(");

		for(ii = 0; ii < item_cnt ; ii++){
			if (ii != 0) string_concat(&query, " ,");
			string_concat(&query, " t1.item");
			itoa(ii+1,wkstr,10);
			string_concat(&query, wkstr);
		}
		string_concat(&query, " ) AS BINARY) AS item ");
	}else{
		string_concat(&query, " t1.item ");
	}
	string_concat(&query, " FROM (");
	string_concat(&query, " SELECT");
	string_concat(&query, " t2.rownum");

	//item_cntが0のときはitemを変換していく
	if(item_cnt != 0){
		for(ii = 0; ii < item_cnt ; ii++){
			itoa(ii+1,wkstr,10);
			string_concat(&query, " ,t2.item");
			string_concat(&query, wkstr);
		}
	}else{
		string_concat(&query, " ,t2.item");
	}
	string_concat(&query, " ");
	string_concat(&query, " FROM (");

	string_concat(&query, " SELECT");
	string_concat(&query, " @ii:=@ii+1 AS rownum");

	//item_cntが0のときはitemを変換していく
	if(item_cnt != 0){
		for(ii = 0; ii < item_cnt ; ii++){
			string_concat(&query, " ,");

			itoa(ii+1,wkstr,10);
			sprintf(wk_point,"%d",wkout[ii].s_point);
			sprintf(wk_length1,"%d",wkout[ii].length);
			sprintf(wk_length2,"%d",wkout[ii].length * 2);
			if((strcmp(wk_length1,"0") != 0) && (strlen(wkout[ii].value) == 0)){
				string_concat(&query, " MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " ) AS item");
				string_concat(&query, wkstr);
			}else if(strlen(wkout[ii].value) != 0){
				string_concat(&query, " \"");
				string_concat(&query, wkout[ii].value);
				string_concat(&query, "\" AS item");
				string_concat(&query, wkstr);
			}
		}
	}else{
		string_concat(&query, " ,MDATA.item ");
	}
	string_concat(&query, " ");
	string_concat(&query, " FROM (SELECT @ii:=0) AS NUMBER,");
	string_concat(&query, check_tablename(itrgt->ifi));
	string_concat(&query, " AS MDATA ");

	//条件文
	for(ii = 0; ii < ITEMLST ; ii++){

		if((itrgt->sel[ii].s_point != 0) && (itrgt->sel[ii].length) != 0){
			char oper[OPER_LEN + 1] = "";
			sprintf(wk_point,"%d",itrgt->sel[ii].s_point);
			sprintf(wk_length1,"%d",itrgt->sel[ii].length);

			if (ii == 0){
				string_concat(&query, " WHERE ");
			}else if(itrgt->sel[ii].logical == 0){
				string_concat(&query, " AND ");
			}else if(itrgt->sel[ii].logical == 1){
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
			}


			if(itrgt->sel[ii].type[0] == 'S'){
				//プラス
				//CAST(MID(MDATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(MDATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED)
				string_concat(&query, " ((MID(HEX(MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '3' AND ((CAST(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(MDATA.item, ");
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
				//CAST(MID(MDATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(MDATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED) * (-1)
				string_concat(&query, " (MID(HEX(MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '7' AND (((CAST(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(MDATA.item, ");
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
				string_concat(&query, "( (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) = 'F' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
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
				string_concat(&query, " (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) = 'C' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) as SIGNED) ");
				//比較演算子
				string_concat(&query, oper);
				//値の比較
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//マイナス
				string_concat(&query, " (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) = 'D' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
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
				string_concat(&query, " CAST(MID(MDATA.ITEM,");
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
				string_concat(&query, " MID(MDATA.ITEM,");
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

	string_concat(&query, " ) AS t2 ");
	string_concat(&query, " ORDER BY t2.rownum ");
	string_concat(&query, " ) AS t1 ");
	string_concat(&query, " );");

	////////////////////////////////////////////////////

	//infileがないときは中身を削除するだけ add 20141117 koyama
//	fprintf(stderr," : %s : \n",query);
	if(itrgt->ifi !=NULL && strlen(itrgt->ifi) > 0){
		//SQL発行
		if(mysql_query(mysqlConn,query.data) != 0){
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			//エラーのときはmysql_failureで
			DB_Close();
			string_fin(&query);
			return 1;
		}
	}

	if(itrgt->mod == 1){
    //移行先テーブルのクリア
		string_init(&query,   "TRUNCATE TABLE ");
		string_concat(&query, check_tablename(itrgt->ofi));
		string_concat(&query, ";");
		//SQL発行
		if(mysql_query(mysqlConn,query.data) != 0){
			setTableUnlock(targSetting, itrgt);
			//エラーのときはmysql_failureで
			mysql_failure();
			DB_Close();
			string_fin(&query);
			return 1;
		}
	}

	//tmp⇒ソート対象へ
  //infileがないときは中身を削除するだけ add 20141117 koyama
  //function化 20170111 koyama
  if(executeTmpToTargTable(mysqlConn,targSetting,itrgt)){
    string_fin(&query);
    return 1;
  }

	//テンポラリテーブルの削除
  //function化 20170111 koyama
  if(dropTemporaryTable(mysqlConn,targSetting,itrgt) == 1){
    string_fin(&query);
    return 1;
  }

	//コミット実行
	//コミット upd koyama ここまで来ていたらOKなのでコミット
	ret = mysql_query(mysqlConn,"COMMIT");

	setTableUnlock(targSetting, itrgt);
	string_fin(&query);

	//DB接続の切断
	mysql_close(mysqlConn);

	return ret;
}


//author:koyama
int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	DB_SETTING targSetting;
	Trgt_inf *trgt;
	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //メモリ確保

	// 変数初期化
	memset(source_file_name, '\0', PATHNAME_SIZE);
	//エラーハンドルをセット
	setErrorHundle();
	//DBの接続
	if(db_init(&targSetting)){
		fprintf(stderr," Error [%02d] %s : DB Initialize Error \n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}
	//初期化
	trgt_ini(trgt);

	//引数が全くなしはダメ(プログラム名は必ずある)
	if(argc <= 1){
		fprintf(stderr," Error C [%02d]: Option is nothing                        %s \n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//プロセス名とユーザ名を取得
	getUserAndProcessName(source_file_name, source_user_name);

	myoptions = malloc(sizeof(char) * (strlen(argv[argc-1]) + 1));
	memset(myoptions,'\0',(strlen(argv[argc-1]) + 1));
	strcpy(myoptions,argv[argc-1]);
//	myoptions = argv[argc-1];
//	fprintf(stderr," Error C [%02d]: %s %s \n", 00,local_server_time(strTime),myoptions);

	for(ii=1;ii < argc; ii++){
		//第二引数を「_」でオプション別に区分けする
		ret = set_trgt(trgt,argv[ii]);
	}

	// DB処理
	ret = dbExecute(trgt,&targSetting);

	// メモリ解放
	free(trgt);
	free(myoptions);

	return ret;
}
