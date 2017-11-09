#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>    /*va_listを使うために必要*/
#include <string.h>
#include <stdbool.h>
#include <mysql.h>
#include <time.h>
#include <sys/time.h>
#include <math.h>
#include <signal.h>
#include<unistd.h>
#include <libcob.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/

// container_of マクロ
/*
#define container_of(ptr, type, member) ({                              \
        const typeof( ((type *)0)->member ) *__mptr = (void *)(ptr);    \
        (type *)( (char *)__mptr - offsetof(type,member) );})
*/
#define container_of(ptr, type, member) ({ 								\
		(type *)( (char *)(ptr) - offsetof(type,member) );})

#define ITEMLST 256              // SORT対象構造体の要素数//フィールドの数の上限(64列を発見)
#define LOOP_MAX 10000000       //データを読む最大行数
#define TOLERANCE 20            //データ長の誤差(これを誤差が超えたら強制終了)
#define MAP_SRC_FUNC_LEN 50
#define PATHNAME_SIZE 1024
#define CONVTARGLEN 8192
#define CONVTARGROW 16

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
#ifndef CONF_PATH_
#define CONF_PATH_ 1
char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";	//2015/08/28 kawada add ローカルに conf.xml がないときの対応
char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif
//定数
const int  NSHIFT     = 4;              // 引数開始文字判定用の移動距離
const int  ARGITM     = 5;              // 引数構造体の要素数
//const int  ITEMLST    = 20;           // SORT対象構造体の要素数
const char *EXCLUSIVE = "EXCLUSIVE";    //オープンモード：排他(EXCLUSIVE)
const char *PROTECT   = "PROTECT";      //オープンモード：保護(PROTECT)
const char *SHARE     = "SHARE";        //オープンモード：共用(SHARE)


//型指定構造体
struct argv_inf {
	int s_point;                  // 開始位置
	int length;                   // 文字列長
	char *type;                   // 出力タイプ
	char *operand;                // 比較対象フィールド
	char *value;                  // 出力フィールド構成
};
typedef struct argv_inf Argv_inf;

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


//引数情報造体
struct trgt_inf {
//	各引数値の最大文字列長
//	char ifi[12];	// 入力ファイル名
//	char ofi[12];	// 出力ファイル名
//	char mod[03];	// 書き込み方法
//	char out[120];	// 出力フィールド構成
//	char sel[120];	// where句

	char     *ifi;			// AVXファイル名
	char     *ofi;			// DOSファイル名
	int      mod;			// 変換方法 0:AV-X->DOS,1:DOS->AV-X
	int      wmod;          // 書き込み方法 0:NEW,1:MOD
	Argv_inf out[ITEMLST];	// COBOL側フィールド構成
	Argv_inf sel[ITEMLST];	// CSV側フィールド構成
	int      logical;		// 論理演算子 0:AND,1:OR,-1:その他
	FILE     *fp;           //変換対象or出力対象file pointer
	char     deb;
	char     *opmod;        // DOS形式のファイルオープンモード Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE（xは変換方法により変わる）	//2015/08/28 kawada add
	int      dataLength;    //テーブルのバイト長

};
typedef struct trgt_inf Trgt_inf;

//文字列構造体
struct string_t{
	size_t size;	// 文字列の長さ + NIL文字の長さの合計値
	char   *data;  // データの先頭
};
typedef struct string_t String_t;

///////////////////////////////////////////////Function liet AND prototype Start
//RTrim実装
int isNullOrEmpty(char *targ);
char *local_server_time(char*);
int setTableLock(DB_SETTING*, Trgt_inf*);
int setTableUnlock(DB_SETTING*, Trgt_inf*);
//空白文字を飛ばした文字列ポインタを返す
char *myftrim(char *s);
//エラーにプロセス名を付けて出力
void mytool_runtime_error(char ,const char *sfile,const char *fmt, ...);
///////////////////////////////////////////////Function liet AND prototype End
//

MYSQL conn,*mysql = &conn;
MYSQL_RES *res;
MYSQL_ROW row;
char map_source_func[MAP_SRC_FUNC_LEN]="";

DB_SETTING *background_targSetting;
Trgt_inf   *background_itrgt;

static cob_field *cob_user_parameters[COB_MAX_FIELD_PARAMS];
static struct cob_module module = { NULL, NULL, NULL, NULL, cob_user_parameters, 0, '.', '\\', ',', 1, 1, 1, 0 };

static char source_file_name[PATHNAME_SIZE];
//static const char *source_file_name = NULL;

int unqId = 0;                  //ユニークID(管理テーブル認証用)		//2015/08/28 kawada add
char *myself=0;                 //自分自身のパスを取得(初期設定)

//confから取得するdebug_flg.他と被らないようにoriginal name
static int myConfDebugFlg;

//ローカルに conf.xml がないときの対応
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

//空白文字を飛ばした文字列ポインタを返す
//IN：
//  s：飛ばす文字列のポインタ
//OUT：飛ばした後の文字列ポインタ
//author: koyama
char *myftrim(char *s){
	while(isspace(*s)){
		s++;
	}
	return s;
}

//プロセス名を返す
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
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



//設定ファイルのパスを取得
//date:20150828
//auth: koyama
//カレントdirのconfだけを開くとどこででも実行できないので
char *getConfFilename(char *strConfPath){
	char strTime[] = "0000/00/00 00:00:00.000000";
	FILE *fpFileExist;      //20150828 add koyama

	if((fpFileExist = fopen(CONF_FLIEPATH, "r")) == NULL){
		if((fpFileExist = fopen(CONF_SHREPATH, "r")) == NULL){
			mytool_runtime_error('E',source_file_name,"Error C [%02d]:conf read Error %s ",99, local_server_time(strTime));
			raise(SIGABRT);
		}else{
			strcpy(strConfPath,CONF_SHREPATH);
		}
	}else{
		strcpy(strConfPath,CONF_FLIEPATH);
	}
	fclose(fpFileExist);

	return strConfPath;
}
//2015/08/28 kawada add E

//*********************************************************
//SORT対象構造体の初期化
//auth:iida
//*********************************************************
void argv_ini(Argv_inf *ioargv ) {
//	"開始位置", "文字列長", "出力ファイル名", "比較対象フィールド", "出力フィールド構成"
	Argv_inf wk_argv ={0,0,"","",""};

	*ioargv = wk_argv;
}


//*********************************************************
//SORT対象構造体の初期化
//auth:iida
//*********************************************************
void trgt_ini(Trgt_inf *iotrgt ) {
	int ii = 0;

	Trgt_inf wktrgt;

	*iotrgt = wktrgt;

	iotrgt->ifi = "";      //テーブル名
	iotrgt->ofi = "";      //ファイル名設定
	iotrgt->mod = -1;       //モードAD:0,DA:1
	iotrgt->wmod= 0;       //追記モード 新規:0,追記:1
	iotrgt->opmod = (char *)malloc(3);		//DOS形式のファイルオープンモード	//2015/08/28 kawada add

	for(ii = 0; ii < ITEMLST ; ii++){
		argv_ini(&iotrgt->out[ii]);
		argv_ini(&iotrgt->sel[ii]);
	}

	iotrgt->logical = 0;
	iotrgt->fp = NULL;
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

	DB_Close();
	//エラー出力をすることでAbortを設定
	if (map_source_func && strlen(map_source_func) > 0) {
		mytool_runtime_error('E',source_file_name, " Error C [99]:recieve Segmentation Fault signal :%s ", map_source_func);
	}else{
		mytool_runtime_error('E',source_file_name," Error C [99]:recieve Segmentation Fault signal ");
	}
	raise(SIGTERM);

	return NULL;
}
// recieve Abort signal
// Author koyama
void* sigAbrt( int args ){
	int i = 0;

	DB_Close();
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
//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
//in/out :retStr 返す対象文字列のポインタ
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

//対象テーブルをtruncate(内容をクリア)
//IN ：
//  tablename ： 内容をクリアするテーブル名(追加するテーブル名)
//OUT ： 0 正常
//       1 異常
//author:koyama
int truncateTable(char *tablename){
	int ret =0;
	char sqlstr[2048] = "";
	char tName[256] = "";
	MYSQL_ROW    res;
	MYSQL_RES    *result;

	strcpy(tName,tablename);

	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		return ret;
	}

	strcat(sqlstr,"TRUNCATE TABLE  `");
	strcat(sqlstr,tName);
	strcat(sqlstr,"`");

	if(mysql_query(mysql, sqlstr) != 0){
		ret = 1;
		return ret;
	}

	if(result = mysql_use_result(mysql)){
		res=mysql_fetch_row(result);
		ret = atoi(res[0]);
		mysql_free_result(result);
	}
	return ret;
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

//文字列を長さ基準(NULL文字込み)で結合する
//IN：
//  mainstr：つなげる先の対象文字列
//  mainLength：つなげる先の現在の長さ
//  substr：つなぐ文字列
//  subLength：つなぐ文字列の長さ
//OUT：つなげた後の文字列ポインタ
//author:koyama
char *mystrncat(char *mainstr ,int mainLength ,char * substr,int subLength){
	memcpy((mainstr + (mainLength)),substr,subLength);
	return mainstr;
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
//カンマ区切りに書き換えb
//auth:iida
//upd :koyama
//*********************************************************
int strSplit_ELM( char *iStr, char *oList[] ) {
	char *tk;
	int  cnt = 0;
	int  str_flg = 0;
	char *str1;			//「@」位置(初期位置は比較文字の先頭ポインタ)
	char strDelim[] = ",";
	char chrDelim = ',';

	str1 = iStr;

	if(strchr(str1,chrDelim)){
		tk = strtok( str1, strDelim );
		oList[cnt] = tk;
		//区切り文字[,]を検索
		while(tk != NULL){
			cnt++;
			tk = strtok( NULL, strDelim );
			oList[cnt] = tk;
		}
	}else{
		//区切りがひとつもないときはその対象のみ
		oList[cnt] = iStr;
		cnt++;
	}

	return cnt;
}

//*********************************************************
//文字列置換(バイナリデータ非対応)
//auth:iida
//*********************************************************
int strChg(char *ioBuf, const char *iStr1, const char *iStr2)
{
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
//auth:iida
//*********************************************************
int string_init(char ** ioData, char const * iStr){
	int ret = 0;

	String_t * self;
	size_t size = strlen(iStr) + 1;

	self = (String_t *)malloc( sizeof(String_t) + sizeof(char) * size );

	/* 確保した領域をNULLクリア */
	memset( self , '\0' , sizeof(String_t) + sizeof(char) * size);

	if(self != NULL){
		self->size = size;
		memcpy(self->data, iStr, size);
		*ioData = self->data;
	}else{
		ret = 1;
	}

	return ret;
}
//*********************************************************
// 可変長文字列 *ioData を破棄する
//auth:iida
//*********************************************************
int string_fin(char ** ioData){
	free(container_of(*ioData, String_t, data));
	return 0;
}
//*********************************************************
// 可変長文字列の長さを返す
//auth:iida
//*********************************************************
size_t string_length(char ** ioData){
	return (container_of(*ioData, String_t, data))->size - 1;
}

//*********************************************************
// 連結する
//auth:iida
//*********************************************************
int string_concat(char ** ioData, char *iStr){
	int ret = 0;

	char * old = *ioData;
	String_t * self;
	size_t size = strlen(iStr) + string_length(ioData) + 1;

	self = (String_t *)malloc( sizeof(String_t) + sizeof(char) * size);

	/* 確保した領域をNULLクリア */
	memset( self , '\0' , sizeof(String_t) + sizeof(char) * size);

	if(self != NULL){
		self->size = size;
		memcpy(self->data, *ioData, string_length(ioData) + 1);
		memcpy(self->data + string_length(ioData), iStr, strlen(iStr));
		*ioData = self->data;
		string_fin(&old);
	}else{
		ret = 1;
	}

	return ret;
}

//*********************************************************
// 文字列 String を nShift だけ移動する。
// 移動先へのポインタを返す。
//auth:iida
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
//auth:iida
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

//対象のオプションの要素数のチェック
//input:対象のオプション名が入った文字列ポインタ,対象のオプションの要素数
//author:koyama
int checkParamFormat(char *strTarg,char *targ){
	char *currPnt;
	int retval=0,commaCnt=0;

	currPnt = targ;

	if(strncmp(strTarg,"PA5",strlen("PA5")) == 0){
		//最初の文字は型なので飛ばす
		currPnt++;
		while(*currPnt != '\0'){
			if(isdigit(*currPnt) == 0 && *currPnt != 'V'){
				retval=1;
			}
			if(*currPnt == 'V'){
				if(isdigit(*(currPnt + 1)) == 0 && *(currPnt + 1) != '0'){
					retval=1;
				}
				commaCnt++;
			}
			currPnt++;
		}
		//Vがひとつ以上
		if(commaCnt > 1){
			retval=1;
		}
	}
	return retval;
}


//*********************************************************
//要素分解関数
//auth:iida
//*********************************************************
int set_argv(Argv_inf *ioTrgt_arg, char * iArgv){
	int ii,jj, ret = 0;

	char *strOptArr[ ITEMLST +1] = {'\0'};		//オプション別
	int result_ii = 0;
	int result_jj = ARGITM;
	int countUp_s_point = 1;
	char size_e[1];
	char strBuff[11] = "";              //ストリングの長さを測るダミーのbuffer
	char strTime[] = "0000/00/00 00:00:00.000000";

	//ここは途中で'\'が入ることはないが、shの書き方として入っていることがある
	//改行とともに除去
	remTargChar(iArgv,0x5c);
	remCRLF(iArgv);
	result_ii = strSplit_ELM(iArgv,strOptArr);

	if(result_ii >= ITEMLST){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Too many arguments                      %s ",16,local_server_time(strTime));
		raise(SIGABRT);
	}
	if(isNullOrEmpty(iArgv)){
		return 0;
	}

	for(ii=0; ii< result_ii; ii++){
		int colLength  = 0;
		//初期化
		//開始位置
		ioTrgt_arg[ii].s_point = countUp_s_point;

		if(checkParamFormat((iArgv - 4),strOptArr[ii])){
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
			raise(SIGABRT);
		}
		//長さ
		//2文字目が数字かどうか判定
		if(isdigit(*(strOptArr[ii] + 1))){
			colLength = atoi((strOptArr[ii] + 1));
			if(strchr((strOptArr[ii] + 1),'V') != 0){
				if(isdigit(*(strchr((strOptArr[ii] + 1),'V') + 1))){
					colLength += atoi(strchr((strOptArr[ii] + 1),'V') + 1);
					if(strncmp((iArgv - 4),"PA5",strlen("PA5")) == 0 && atoi(strchr((strOptArr[ii] + 1),'V') + 1) <= 0){
						mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
						raise(SIGABRT);
					}
				}else{
					mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
					raise(SIGABRT);
				}
			}
			if(strncmp((iArgv - 4),"PA5",strlen("PA5")) == 0 && (colLength <= 0 || colLength > 4096) ){
				mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
				raise(SIGABRT);
			}
			countUp_s_point += colLength;
		}else{
			if(strncmp((iArgv - 4),"PA5",strlen("PA5")) == 0){
				mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
				raise(SIGABRT);
			}
		}

		//数字が入っていなければ初期値0が入り続ける
		ioTrgt_arg[ii].length = colLength;

		// 出力タイプ
		ioTrgt_arg[ii].type    = malloc(sizeof(size_e) * (1 + 1));
		memset(ioTrgt_arg[ii].type,'\0',sizeof(size_e) * (1 + 1));
		if(ioTrgt_arg[ii].type == NULL){
			ret = 1;
			return ret;
		}
		ioTrgt_arg[ii].type[0] = *(strOptArr[ii] + 0);
		ioTrgt_arg[ii].value   = strOptArr[ii];
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

	if(ioTrgt->mod == -1){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Mode is Incorrect                       %s ",02,local_server_time(strTime));
		return 1;
	}
	if(strlen(ioTrgt->ifi) == 0){
		if(ioTrgt->mod == 0){
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Where is the input source       : %s %s ",01,ioTrgt->ofi,local_server_time(strTime));
		}else{
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Where is the output destination : %s %s ",02,ioTrgt->ifi,local_server_time(strTime));
		}
		return 1;
	}
	if(strlen(ioTrgt->ofi) == 0){
		if(ioTrgt->mod != 0){
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Where is the input source       : %s %s ",01,ioTrgt->ofi,local_server_time(strTime));
		}else{
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Where is the output destination : %s %s ",02,ioTrgt->ifi,local_server_time(strTime));
		}
		return 1;
	}
	if(ioTrgt->wmod == -1){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Write Mode is Incorrect                 %s ",02,local_server_time(strTime));
		return 1;
	}
	if(ioTrgt->out[0].s_point == 0
		|| (ioTrgt->out[0].s_point == 1 && strlen(ioTrgt->out[0].type) == 0)){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the data is not correct       %s ",02,local_server_time(strTime));
		return 1;
	}
	if(ioTrgt->sel[0].s_point == 0
		|| (ioTrgt->sel[0].s_point == 1 && strlen(ioTrgt->sel[0].type) == 0)){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the data is not correct       %s ",02,local_server_time(strTime));
		return 1;
	}
	//2015/08/28 kawada add S PB3指定のエラー(指定文字列規定外)処理
	if (strcmp(ioTrgt->opmod, "_") == 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:File Open Mode is Incorrect             %s ",02,local_server_time(strTime));
		return 1;
	}

	return 0;
}

//*********************************************************
//引数分解関数
//IN  ： ioTrgt->セットする引数分解構造体
//       iArgv->引数文字列
//OUT ：0正常
//      1異常
//auth: iida
int set_trgt(Trgt_inf *ioTrgt, char *iArgv){
	int ii,ret = 0, cnt =0;
	char wkstr[120+1] = "";
	char *strOptArr[20+1] = {'\0'};		//オプション別の文字列ポインタ
	char *strTmpArgv = iArgv;
	char strPara[20];		//引数の値	//2015/08/28 kawada add

	//定数の間のSPをいったん置換
	chgChar(strTmpArgv,(char)0x20,(char)0x07,(char)0x40);
	//第二引数を「 」でオプション別に区分けする
	int result = strSplit(strTmpArgv," ",strOptArr);


	if(result > (20 + 1)){
		return 1;
	}

	for(ii=0; ii< result; ii++){
		chgChar(strOptArr[ii],(char)0x07,(char)0x20,(char)0x40);
		if (strstr(strOptArr[ii], "PA3=") != NULL ) {
			//テーブル名設定
			ioTrgt->ifi = (strchr(strOptArr[ii],'=') + 1);
			//必須入力チェック
			if(strlen(ioTrgt->ifi) <= 0){
				ret = 1;
				break;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PB1=") != NULL ) {
			//ファイル名設定
			ioTrgt->ofi = (strchr(strOptArr[ii],'=') + 1);
			chgChar(ioTrgt->ofi,(char)0x20,(char)0x00,(char)0x00);
			//必須入力チェック
			if(strlen(ioTrgt->ofi) <= 0){
				ret = 1;
				break;
			}
			continue;
		}else if (strstr(strOptArr[ii], "MN1=") != NULL ) {
			//ファイル名設定
			ioTrgt->deb = *(strchr(strOptArr[ii],'=') + 1);
			//必須入力チェック
			continue;
		}else if (strstr(strOptArr[ii], "MN2=") != NULL ) {
			//変換方法
			if((strstr(strOptArr[ii], "AD") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//AV-X->DOSの場合
				ioTrgt->mod = 0;
			}else if((strstr(strOptArr[ii], "DA") != NULL ) ){
				//DOS->AV-Xの場合
				ioTrgt->mod = 1;
			}else{
				//上記以外
				ioTrgt->mod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PAA=") != NULL ) {
			//書き込み方法
			if((strstr(strOptArr[ii], "NEW") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//新規
				ioTrgt->wmod = 0;
			}else if((strstr(strOptArr[ii], "MOD") != NULL ) ){
				//追加
				ioTrgt->wmod = 1;
			}else{
				//上記以外
				ioTrgt->wmod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PA5=") != NULL ) {
			//AV-Xフィールド構成設定
			ret = set_argv(ioTrgt->out, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			if(ret){
				break;
			}else{
				continue;
			}
		}else if (strstr(strOptArr[ii], "PB7=") != NULL ) {
			//DOSフィールド構成設定
			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			if(ret){
				break;
			}else{
				continue;
			}
		//2015/08/28 kawada add S PB3指定のチェック、値設定
		}else if (strstr(strOptArr[ii], "PB3=") != NULL ) {
			//DOS形式のファイルオープンモード設定
			memset(strPara, '\0', strlen(strPara));
			if (strlen(strOptArr[ii]) > strlen("PB3=")){
				strcpy(strPara, (strchr(strOptArr[ii],'=') + 1));	//指定があるときのみ変数へ複写
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
					strcpy(ioTrgt->opmod, "S");		//指定がないときはSHAREとする
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
					strcpy(ioTrgt->opmod, "SO");	//いずれの指定でもSHARE OUTPUT(SO)に置き換え
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
	unsigned int v = val;				// 作業用(変換対象の値)
	int n = 1;							// 変換文字列の桁数記憶用
	while(v >= radix){					// 桁数を求める
		v /= radix;
		n++;
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

//["]を除去
//IN ：
//  inStr ： 対象文字列(参照として返す)
//OUT ： なし
//author:koyama
char *remDoubleQuote(char *inStr){
	char *strConv;
	char size_e[1];
	//元の文字列と同じ長さか短くなるため
	strConv = malloc(sizeof(size_e) * (strlen(inStr) + 1));

	if(inStr[0] != '"'){
		strcpy(strConv,inStr);
	}else{
		strcpy(strConv,(inStr + 1));
		memset(strchr(strConv,'"'),'\0',1);
	}
	memset(inStr,'\0',strlen(inStr));
	memcpy(inStr,strConv,strlen(strConv));
	free(strConv);
	return inStr;
}


//改行文字(LF,CR)をNULL文字に変える
//IN ：
//  targStr ： 対象文字列
//OUT ： なし
//author:koyama
void strRemCrLf(char *targStr){
	if(strchr(targStr,0x0A)){
		memset(strchr(targStr,0x0A),'\0',1);
	}
	if(strchr(targStr,0x0D)){
		memset(strchr(targStr,0x0D),'\0',1);
	}
	return;
}

//エスケープする文字をエスケープしながらデータをつなぐ
//エスケープの対象はシングルクォート(0x27)と円記号(0x5c)
// クエリ文字列としてINSERTするときに使用
//ＩＮ：
//  targv ： 対象となる長さ付文字列
//  addtext ： つなぐ文字列
//  len ： つなぐ文字列の論理的な長さ
//OUT ： なし
//author:koyama
int lenStr_addStringToSQL(char *targv,int *toLen,char *addtext,int fromLen){
	int result = 0;
	int addlen = 0;
	char size_e[1];
	char *copyText;
	char *copyAddText;
	char *origText;
	char *origTextEnd;

	//copyTextの初期化
	//つなぐのに必要
	copyAddText = (char *)malloc((sizeof(size_e) * (fromLen * 2)));
	copyText    = copyAddText;
	origText    = addtext;
	origTextEnd = (addtext + fromLen);

	//
	for(;origText < origTextEnd;copyText++){
		//シングルクォート(':0x27)円記号(\:0x5c) をエスケープ
		if(*origText == 0x27 || *origText == 0x5c){
			//エスケープ文字は円記号(\:0x5c)
			*copyText = 0x5c;
			addlen++;
			//こちらに入ったときは余分に+
			copyText++;
		}
		*copyText = *origText;
		origText++;
		addlen++;
	}

	//実際のコピー
	memcpy((targv + *toLen),copyAddText,addlen);
	*toLen += addlen;

	free(copyAddText);
	return result;
}

//エスケープする文字をエスケープしながらデータをつなぐ
//エスケープの対象はシングルクォート(0x27)と円記号(0x5c)
// CSV文字列として出力するときに使用
//ＩＮ：
//  targv ： 対象となる長さ付文字列
//  addtext ： つなぐ文字列
//  len ： つなぐ文字列の論理的な長さ
//OUT ： なし
//author:koyama
int lenStr_addStringToCSV(char *targv,int *toLen,char *addtext,int fromLen){
	int result = 0;
	int addlen = 0;
	char size_e[1];
	char *copyText;
	char *copyAddText;
	char *origText;
	char *origTextEnd;

//	if((targv->len + len) > targv->maxlen){
//		return 1;
//	}

	//copyTextの初期化
	//つなぐのに必要
	copyAddText = (char *)malloc((sizeof(size_e) * (fromLen * 2)));
	copyText    = copyAddText;
	origText    = addtext;
	origTextEnd = (addtext + fromLen);

	//
	for(;origText < origTextEnd;copyText++){
		//ダブルクォート(":0x27)をエスケープ
		if(*origText == 0x22 ){
			//エスケープ文字は円記号(\:0x5c)
			*copyText = 0x22;
			addlen++;
			//こちらに入ったときは余分に+
			copyText++;
		}
		*copyText = *origText;
		origText++;
		addlen++;
	}

	//実際のコピー
	memcpy((targv + *toLen),copyAddText,addlen);
	*toLen += addlen;

	free(copyAddText);
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
	mysql = mysql_init(NULL);

	if (!mysql_real_connect(mysql, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0)) {
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Connect Error %s                    %s", 02,mysql_error(mysql),local_server_time(strTime));
		//接続不可
//		raise(SIGABRT);
		ret = 1;
	}else{
	    //接続可
	    ret = 0;
	}
	//AutoCommit Off
	ret = mysql_query(mysql, strAutocommit);

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
	if(background_targSetting != NULL){
		//return voidなので必ず0で返す
		mysql_close(mysql);
	}
	return ret;
}

//*********************************************************
//設定ファイルの読み込み
//*********************************************************
int conf_read(DB_SETTING *targSetting){
	int ii;
	char strConfPath[1024]; //20150828 add koyama

	//下で値が設定されなかったらこれがdefault
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

//設定の読み込みとデータベースへの接続を行う
//IN  ： targSetting->データベース接続情報
//OUT ： なし
//author:koyama
int db_init(DB_SETTING *targSetting){
	int result=0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	cob_init(0, NULL);
	//エラーハンドルをセット(上でcob_initをしているのでそれを上書きする)
	setErrorHundle();
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

void catWhere(char *strcat,char **ppStrArr){
	int ii;

	ii = 0;
	while(*(ppStrArr + ii) != 0){

		ii++;
	}
}

//*********************************************************
//SQL実行エラー時の後処理
//*********************************************************
int mysql_failure(){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//エラー内容を出力
	mytool_runtime_error('E',source_file_name," Error C [%02d]:Query Error %s [%s] %s ", 32,mysql_error(mysql),myself,local_server_time(strTime));

	//ロールバック
	ret = mysql_query(mysql, "ROLLBACK; ");
	//DB接続の切断

//	mysql_close(mysql);

	return ret;
}

//テキストファイルの行orDBのデータが存在するか
//author:koyama
int existRowData(Trgt_inf *trgt){
	int ret = 0;

	if(trgt->mod == 1){
		//ファイル2DB

	}else if(trgt->mod == 0){
		//DB2ファイル

	}

	return ret;
}

//出力
//IN  ： itrgt->引数情報
//       ,strTo->変換後文字列のポインタ
//       ,intLength->文字列の長さ
//OUT ： 0->成功, 1->失敗
//author:koyama
int outputConvData(Trgt_inf *itrgt,char *strTo,int intLength){
	int ret = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : outputConvData :%.30s",strTo);

	if(itrgt->mod == 1){
		//ファイル2DB
		//SQLの作成
		char sqlStr[2048]="";     //SQL格納用
		int sqlstrlen=0;          //SQL文字列の現在の長さ


		strcat(sqlStr,"INSERT INTO `");
		strcat(sqlStr,itrgt->ifi);
		strcat(sqlStr,"`\n (ITEM) \n VALUES('");
		sqlstrlen = strlen(sqlStr);
		mystrncat(sqlStr ,sqlstrlen ,strTo,intLength);
		sqlstrlen += intLength;
		mystrncat(sqlStr ,sqlstrlen ,"') ",strlen("') "));
		sqlstrlen += strlen("') ");
		//データの追加
		if(mysql_real_query(mysql, sqlStr,sqlstrlen) != 0){
			ret = 1;
		}
	}else{
		//DB2ファイル
		//fputsは失敗のときEOF(-1)を返す
		if(fputs(strTo,itrgt->fp) == EOF){
			ret = 1;
		}
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//データ変換(一行単位)
//IN  ： itrgt->引数情報
//       ,strTo->変換後文字列のポインタ
//       ,strFrom->変換後文字列のポインタ
//OUT ： 0->成功, 1->失敗 (ref)変換後文字列
//author:koyama
int cnvTargToChange(Trgt_inf *itrgt,char *strTo,char *strFrom,int *intLength){
	int ret = 0;
	int cnt = 0;
	char *strLocalFrom = strFrom;


	//copy用のcobol
	cob_field_attr a_From = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field       f0;
	cob_field       f1;

	//0で初期化しておき、最後にintLengthにadd
	int intLocalLength = 0;
	int intLocalFromLength = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : cnvTargToChange :%.30s",strFrom);

	//定義体(初期化)
	cob_current_module = &module;

	//モードの切り替え
	if(itrgt->mod == 1){
		char *pComma;        //CSVなので,の位置ポインタを準備

		//ファイル2DB
		while(cnt < ITEMLST){
			int fromLength = 0,toLength=0;      //現在のdbの値に対応するデータの長さ
			char strTempFrom[4096]="";              //都度初期化するためループの中
			char strTempTo[4096]="";                //都度初期化するためループの中

			//1カラム分の文字列をtemporaryにコピー
			if ((pComma=strchr(strLocalFrom,',')) != NULL){
				//次の,の位置で文字列を切る
				*pComma=0x00;
				strcat(strTempFrom,strLocalFrom);
			}else{
				strcat(strTempFrom,strLocalFrom);
			}


			//\rが残る可能性があるため削除
			remCRLF(strTempFrom);

			//"囲みがあるなら除去
			remDoubleQuote(strTempFrom);
			//代入するデータの長さを定義
			if(itrgt->out[cnt].type[0] == 'J'){
				//2バイトコードのときは*2
				fromLength = strlen(strTempFrom);
				toLength = itrgt->out[cnt].length * 2;
			}else{
				fromLength = strlen(strTempFrom);
				toLength = itrgt->out[cnt].length;
			}


			//変換処理
			switch(itrgt->out[cnt].type[0]){
			case 'C':
				//文字
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_From.digits = fromLength;
				//カンマの位置
				a_From.scale  = 0;
				a_From.flags  = 0;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'J':
				//
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_NATIONAL;
				a_From.digits = fromLength;
				//カンマの位置
				a_From.scale  = 0;
				a_From.flags  = 0;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_NATIONAL;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'N':
				//数字
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//桁数は合計のはず
				a_From.digits = fromLength;
				a_From.scale = 0;
				a_From.flags  = 0;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_NUMERIC;
				a_to.digits  = toLength;
				a_to.flags = 0;
				a_to.pic   = NULL;
				//カンマの位置
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				break;
			case 'S':
				//符号月数字
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//桁数は合計のはず
				a_From.digits = fromLength;


				//変換先cob_field_attrの設定
				a_to.type = (unsigned char)COB_TYPE_NUMERIC;
				//桁数は合計のはず
				a_to.digits = toLength;

				//カンマの位置
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				//SIGNのときはフラグ
				a_to.flags = COB_FLAG_HAVE_SIGN;

				break;

				//picはeditingフォーマットなだけ
			case 'P':
			case 'Q':
				//パック10進数
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//桁数は合計のはず
				a_From.digits = fromLength;


				//変換先cob_field_attrの設定
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_PACKED;
				//桁数はそのままサイズはバイト長((データ長+符号) / 2の整数)
				a_to.digits = toLength;
				toLength = (int)ceil((toLength + 1) / 2.0);

				//カンマの位置
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				//SIGNのときはフラグ
				if(itrgt->out[cnt].type[0] == 'P'){
					a_to.flags = COB_FLAG_HAVE_SIGN;
				}
				break;

				//picはeditingフォーマットなだけ
			default:
				break;
			}

			//from
			f0.size = fromLength;
			f0.data = strTempFrom;
			f0.attr = &a_From;
			//to
			//送り先の長さは元の長さに合わせておく
			f1.size = toLength;
			f1.data = strTempTo;
			f1.attr = &a_to;
			cob_move(&f0,&f1);

			//値のコピー
			//intLocalLengthにSQLにもっていくときの長さが帰ってくる
			lenStr_addStringToSQL(strTo,&intLocalLength,strTempTo,toLength);
//			fprintf(stdout,"FROM:%s",strTempFrom);
//			fprintf(stdout,"TO  :%s",strTempTo);
//			fprintf(stdout,"type:%s",itrgt->out[cnt].type);
//			fprintf(stdout,"type:%s",itrgt->sel[cnt].type);

			//次のため
			cnt++;
			if(itrgt->out[0].s_point == 0){
				break;
			}

			/*'､'の位置をずらして次の位置へ */
			if(pComma == NULL){
				break;
			}
			pComma++;
			strLocalFrom=pComma;
		}
	}else{
		//データの分割ごとに変換
		//DB2ファイル
		while(cnt < ITEMLST){
			//カラム一つのデータを保持
			//元の仕様では4096の長さがCの最大らしい
			char strTempFrom[4096]="";              //都度初期化するためループの中
			char strTempTo[4096]="";                //都度初期化するためループの中
			int fromLength = itrgt->out[cnt].length;    //つなぐ文字の長さ(1カラムの長さ)
			int toLength = fromLength;

			//fromの長さが違うケースがあるものはここで指定
			switch(itrgt->out[cnt].type[0]){
			case 'J':
				fromLength *= 2;
				toLength   *= 2;
				break;
			case 'P':
			case 'Q':
				fromLength = (int)ceil((fromLength + 1) / 2.0);
				break;
			default:
				break;
			}
			//1カラム分の文字列をtemporaryにコピー
			memcpy(strTempFrom,strLocalFrom,fromLength);

			//つなぎの[,]を入れておく
			if(cnt != 0){
				strTo[intLocalLength] = ',';
				intLocalLength += 1;
			}

			//picを準備
			// [定数],[文字数],[謎],[謎],[謎]
			// '-','0x01','0x00','0x00','0x00','9','0x[長さ]','0x00','0x00','0x00'
			char a_topic[48] = "";
			//modeをつけるべき?            check
			//文字なら["]で囲む
			switch(itrgt->sel[cnt].type[0]){
			case 'C':
			case 'J':
//				strTo[intLocalLength] = '"';
//				intLocalLength += 1;
				break;
			case 'S':
			default:
				break;
			}

			//変換処理
			switch(itrgt->out[cnt].type[0]){
			case 'C':
				//from
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_From.digits = fromLength;
				//カンマの位置
				a_From.scale  = 0;
				a_From.flags  = 0;
				a_to.pic   = NULL;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'J':
				//2バイトコード
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_NATIONAL;
				a_From.digits = fromLength;
				//カンマの位置
				a_From.scale  = 0;
				a_From.flags  = 0;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'N':
				//数字
				//Z(n-1)9(1)
				memcpy(a_topic,"Z\000\000\000\0009\001\000\000\000",10);
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC;
				//桁数は合計のはず
				a_From.digits = fromLength;
				//カンマの位置
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//小数点の分ずらす
					toLength++;
				}
				a_to.flags  = 0;
				//picはeditingフォーマットなだけ

				//to
				//初期化
				a_to.type    = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits  = toLength;
				a_to.scale = a_From.scale;
				a_to.flags = 0;
				//Z(n-1)9(1)なので-1
				a_topic[1] = ((char)atoi(itrgt->out[cnt].value + 1) - 1);
				a_to.pic   = a_topic;
				break;
			case 'S':
				//符号月数字
				//-(n)9(1)
				memcpy(a_topic,"-\001\000\000\0009\001\000\000\000",10);
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC;
				//符号文字文ずらすので+1
				toLength = toLength + 1;
				//桁数は合計のはず
				a_From.digits = fromLength;

				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//小数点の分ずらす
					toLength++;
				}
				a_From.flags  = COB_FLAG_HAVE_SIGN;

				//変換先cob_field_attrの設定
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits = a_From.digits;
				a_to.scale = a_From.scale;
				a_to.flags = a_From.flags;

				//フォーマット側に長さ
				a_topic[1] = (char)atoi(itrgt->out[cnt].value + 1);
				a_to.pic = a_topic;

				break;
				//picはeditingフォーマットなだけ
			case 'P':
			case 'Q':
				//パック10進数
				//変換元cob_field_attrの設定
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC_PACKED;
				//桁数はそのままサイズはバイト長((データ長+符号) / 2の整数)
				//toLengthを符号に合わせてずらす可能性があるのでその前に
				a_From.digits = toLength;

				//違うパターン
				if(itrgt->out[cnt].type[0] == 'P'){
					memcpy(a_topic,"-\001\000\000\0009\001\000\000\000",10);
					//符号文字文ずらすので+1
					toLength = toLength + 1;
					a_From.flags  = COB_FLAG_HAVE_SIGN;
					//フォーマット側に長さ
					a_topic[1] = (char)atoi(itrgt->out[cnt].value + 1);
				}else{
					memcpy(a_topic,"Z\000\000\000\0009\001\000\000\000",10);
					//Z(n-1)9(1)なので-1
					a_topic[1] = ((char)atoi(itrgt->out[cnt].value + 1) - 1);
				}

				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//小数点の分ずらす
					toLength++;
				}

				//変換先cob_field_attrの設定
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits = a_From.digits;
				a_to.scale = a_From.scale;
				a_to.flags = a_From.flags;

				a_to.pic = a_topic;

				break;
				//picはeditingフォーマットなだけ
			default:
				break;
			}

			//from
			f0.size = fromLength;
			f0.data = strTempFrom;
			f0.attr = &a_From;
			//to
			//送り先の長さは元の長さに合わせておく
			f1.size = toLength;
			f1.data = strTempTo;
			f1.attr = &a_to;
			cob_move(&f0,&f1);

			//長さが取得してきたデータ長を超えていたら、正しい値がコピーできないとして
			//コピーせずに次の処理へ
			intLocalFromLength += fromLength;
			if(intLocalFromLength > itrgt->dataLength){
				break;
			}

			//値のコピー
			lenStr_addStringToCSV(strTo,&intLocalLength,strTempTo,toLength);

			//文字なら["]で囲む
			switch(itrgt->sel[cnt].type[0]){
			case 'C':
			case 'J':
//				strTo[intLocalLength] = '"';
//				intLocalLength += 1;
				break;
			case 'S':
			default:
				break;
			}

			//次のリードはポインタをずらす
			strLocalFrom = (strLocalFrom + fromLength);
			cnt++;
			if(itrgt->out[cnt].s_point == 0){
				break;
			}
		}
		//最後に改行コードを追加
		strTo[intLocalLength] = '\r';
		intLocalLength += 1;
		//最後に改行コードを追加
		strTo[intLocalLength] = '\n';
		intLocalLength += 1;
	}
	//足しこんだ長さを返す
	*intLength = intLocalLength;

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);

	return ret;
}


//変換対象文字列の取得
//IN  ： itrgt->引数情報,strRetTarget->対象文字列のポインタ
//OUT ： 0->成功, 1->失敗 (ref)変換対象文字列
//author:koyama
int getTargetString(Trgt_inf *itrgt,char *strRetTarget,int *dataLength){
	int ret = 0;
	char tmp[4096]="";			//fileから取得した1行の文字列

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : getTargetString :%.30s",strRetTarget);

	if(itrgt->mod == 1){
		//ファイル2DB
		if(fgets(tmp,sizeof(tmp),itrgt->fp)){
			char *replaceChar;
			//先にCRを検索
			if(strchr(tmp,0x0A) != 0){
				replaceChar = strchr(tmp,0x0A);
				replaceChar[0] = '\0';
			}else{
				//次にLFを検索
				if(strchr(tmp,0x0D) != 0){
					replaceChar = strchr(tmp,0x0D);
					replaceChar[0] = '\0';
				}
			}
			strcpy(strRetTarget,tmp);
			*dataLength = strlen(strRetTarget);
		}else{
			ret = 1;
		}
	}else if(itrgt->mod == 0){
		//DB2ファイル
		unsigned long *f_length;     //フィールドの長さ(配列として受け取るのでポインタ宣言)
		//行を持っていなければ終わり(取れなければ)
		if((row = mysql_fetch_row(res)) == NULL){
			return 1;
		}
		// if(res->row_count == 0){
		// 	//行を1行も持っていなければ終わり
		// 	return 1;
		// }
		// row = mysql_fetch_row(res);
		if(row != NULL){
			f_length =  mysql_fetch_lengths(res);
			//フィールド指定がITEMのみなので0番目のみ
			memcpy(strRetTarget,row[0],f_length[0]);
			//終端記号追加
			strRetTarget[f_length[0]] = '\0';
			*dataLength = f_length[0];
			//長さを定義したものを返す
			itrgt->dataLength = f_length[0];
		}else{
			//終了でもエラーコードを返す
			ret = 1;
		}
	}else{
		ret = 1;
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
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
//返値：実行結果
int setTableLock(DB_SETTING *targSetting, Trgt_inf *itrgt){
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
	if(mysql_query(mysql, sqlStr) != 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:can't run the Transaction %s %s  %s ", 81, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	// 対象テーブルのロック状態確認(ロック無視)
	//TableNameが現行のPKeyなので一行しか取れない20150217
	strcat(sqlStr,"SELECT LockMode FROM `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` WHERE TableName = '");
	strcat(sqlStr,itrgt->ifi);
	strcat(sqlStr,"';");

	if(mysql_query(mysql, sqlStr) != 0){
		mysql_failure();
		ret = 1;
		return ret;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	if(result = mysql_store_result(mysql)){
		int rr=0;

		rr=mysql_num_rows(result);

		// 文字列初期化
		memset(sqlStr,'\0',strlen(sqlStr));

		sprintf(strUid,"%d", unqId);
		sprintf(strSS,"%s", itrgt->opmod);

		//先行の処理がなければそのまま接続
		if(rr == 0){
			// 管理テーブルの登録
			strcat(sqlStr,"INSERT INTO `");
			strcat(sqlStr,targSetting->db_manatbl);
			strcat(sqlStr,"` ( TableName,Uid,LockMode,pg_name,LockDateTime) ");
			strcat(sqlStr,"VALUES ( '");
			strcat(sqlStr,itrgt->ifi);
			strcat(sqlStr,"',");
			strcat(sqlStr,strUid);
			strcat(sqlStr,",'");
			strcat(sqlStr,strSS);
			strcat(sqlStr,"','");
			strcat(sqlStr,source_file_name);    //pg_name
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
				memcpy((strDecision + 2), itrgt->opmod, 2);
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
						 mytool_runtime_error('E',source_file_name," Error C [%02d]:alredy locked %s:%s [%s] %s ", 82, itrgt->ifi, targSetting->db_manatbl,myself, local_server_time(strTime));
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
						strcat(sqlStr,"', LockDateTime = now() ");
						strcat(sqlStr," WHERE TableName = '");
						strcat(sqlStr,itrgt->ifi);
						strcat(sqlStr,"' ");
						strcat(sqlStr,";");
					}
				}
			}
		}
		if(ret != 1 && *sqlStr != '\0'){
			// SQLの実行⇒COMMIT
			if(mysql_query(mysql, sqlStr) != 0){
				mysql_failure();
				ret = 1;
				mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 82, itrgt->ifi);
				return ret;
			}else{
				//使った変数をリセット
				memset(sqlStr,'\0',strlen(sqlStr));
				//コミット
				strcat(sqlStr,"COMMIT; ");
				if(mysql_query(mysql, sqlStr) != 0){
					mysql_failure();
					mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 83, itrgt->ifi);
					ret = 1;
					return ret;
				}
			}
		}
	}else{
		mytool_runtime_error('E',source_file_name," Error C [%02d]:%s", 84, mysql_error(mysql));
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
	if(mysql_ping(mysql) && mysql_query(mysql, sqlStr) != 0){
		//unLockでエラーに成った場合エラーのだしようが無い
		//mytool_runtime_error(source_file_name," Error C [%02d]:can't run the Transaction %s %s          %s ", 91, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));

	sprintf(strUid, "%d", unqId);

	strcat(sqlStr,"UPDATE `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` SET Uid = NULL, LockMode = '");
	strcat(sqlStr, "    ");								//LOCKMODE DEFAULT VALUE
	strcat(sqlStr,"' , LockDateTime = now() ");
	strcat(sqlStr," WHERE TableName = '");
	strcat(sqlStr,itrgt->ifi);
	strcat(sqlStr,"' AND Uid = ");
	strcat(sqlStr,strUid);
	strcat(sqlStr,";");

	// SQLの実行
	if(mysql_query(mysql, sqlStr) != 0){
//		mysql_failure();
		ret = 1;
		return ret;
	}else{
		//使った変数をリセット
		memset(sqlStr,'\0',strlen(sqlStr));
		//コミット
		strcat(sqlStr,"COMMIT; ");
		if(mysql_query(mysql, sqlStr) != 0){
			mysql_failure();
			//unLockでエラーに成った場合エラーのだしようが無い
//			mytool_runtime_error(source_file_name," Error C [%02d]:can't lock %s ", 92, itrgt->ifi);
			DB_Close();
			ret = 1;
			return ret;
		}
	}

	return ret;
}
// -----------------------------------------------------Lock,Unlock END


//データ変換、入出力
//IN  ： なし
//OUT ： なし
//author:koyama
//2015/08/28 kawada 管理テーブルIDが必要なため、引数に追加
//2015/08/28 kawada del int dataconvert(Trgt_inf *itrgt){
int dataconvert(DB_SETTING *targSetting, Trgt_inf *itrgt){		//2015/08/28 kawada add
	int ret = 0;
	FILE *fp;
	int count = 0,dataRowCount = 0,dataLength = 0;
	char sqlStr[2048] = "";
	int intDefDataLen = 0;
	char strTime[] = "0000/00/00 00:00:00.000000";	//時間の文字列を格納

	//エラーになった時ようにコピーをとっておく
	background_targSetting = targSetting;
	background_itrgt       = itrgt;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"%d : dataconvert",source_file_name);

	//メイン処理スタート
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name," Error [%02d]: Info dataconvert Start %s "
		,99,local_server_time(strTime));
	}
	//2015/08/28 kawada add S 管理テーブルのLock情報(LockMode)をLockに更新
	if (setTableLock(targSetting, itrgt) != 0){
		return 1;
	}
	//2015/08/28 kawada add E

	//最初の第一歩
	//まずはトランザクション
	strcat(sqlStr,"START TRANSACTION; ");
	if(mysql_query(mysql, sqlStr) != 0){
		//エラーになったらとりあえず、unLock試行 add koyama 20160313
		mysql_failure();
		setTableUnlock(targSetting, itrgt);
		mytool_runtime_error('E',source_file_name
			," Error C [%02d]:can't run the Transaction %s %s          %s "
			, 10,itrgt->ifi,mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//使った変数をリセット
	memset(sqlStr,'\0',strlen(sqlStr));
	if(itrgt->mod == 1){
		//ファイル2DB
		fp = fopen(itrgt->ofi,"r");
		//ファイルオープンに失敗したときは強制終了
		if(fp == NULL){
			//エラーになったらとりあえず、unLock試行 add koyama 20160313
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name," Error C [%02d]:can't open file %s                  %s ", 11,itrgt->ofi,local_server_time(strTime));
			DB_Close();
			return 1;
		}else{
			itrgt->fp = fp;
		}
	}else if(itrgt->mod == 0){
		//DB2ファイル
		fp = fopen(itrgt->ofi,"w");
		strcat(sqlStr,"SELECT ITEM FROM `");
		strcat(sqlStr, itrgt->ifi);
		strcat(sqlStr,"` ORDER BY ID; ");
		//ファイルオープンに失敗したときは強制終了
		if(fp == NULL){
			//エラーになったらとりあえず、unLock試行 add koyama 20160313
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name
				," Error C [%02d]:can't open output file  %s [%s] %s "
				, 12,itrgt->ofi,myself,local_server_time(strTime));
			DB_Close();
			return 1;
		}else{
			itrgt->fp = fp;
		}
		//データを取得
		if(mysql_query(mysql, sqlStr) != 0){
			//エラーになったらとりあえず、unLock試行 add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name
				," Error C [%02d]:property get error %s %s  [%s] %s "
				, 13,itrgt->ifi,mysql_error(mysql),myself,local_server_time(strTime));
			DB_Close();
			return 1;
		}
		//グローバルな位置のresponse変数に使用するデータを格納
		res = mysql_use_result(mysql);
	}else{
		//エラーになったらとりあえず、unLock試行 add koyama 20160313
		setTableUnlock(targSetting, itrgt);
		DB_Close();
		return 1;
	}

	//モードDAのとき追加かどうかを判断
	//wmod 0のときは新規(defaultも0なので指定がなければ0)
	if(itrgt->mod == 1 && itrgt->wmod == 0){
		//テーブルのtruncate
		truncateTable(itrgt->ifi);
	}

	//データ処理処理スタート
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name
			," Error [%02d]: Info data processing Start %s ",99,local_server_time(strTime));
	}

	//データをレコード数ループ
	for(count = 0; 1 ;count++){
		char strConvTarg[CONVTARGLEN]="";
		char strConvChanged[CONVTARGLEN]="";
		int rowLength = 0;

		//datareadスタート
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info data_read Start %s ",99,local_server_time(strTime));
		}
		//読み込み(モードによって分岐)
		//strConvTargのポインタに結果が格納されて帰ってくる
		if(getTargetString(itrgt,strConvTarg,&dataLength)){
			break;
		}
		//datareadスタート
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info data_read End %s ",99,local_server_time(strTime));
		}

		//対象文字列の変換(モードによって分岐)
		//行単位 (return が1になることはなさそう)
		if(cnvTargToChange(itrgt,strConvChanged,strConvTarg,&rowLength)){;
			//エラーになったらとりあえず、unLock試行 add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			DB_Close();
			fclose(fp);
			return 1;
		}
		if(count == 0){
			//最初の行はデータ長を保存
			intDefDataLen = rowLength;
		}else{
			//誤差が大きい行が来たら強制終了
			if(rowLength > (intDefDataLen + TOLERANCE) ||rowLength < (intDefDataLen - TOLERANCE)){
				break;
			}
			if(strchr(strConvTarg,0x1A) != NULL){
				break;
			}
		}
		//datawriteスタート
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info outputConvData Start %s ",99,local_server_time(strTime));
		}
		//出力(モードによって分岐)
		if(outputConvData(itrgt,strConvChanged,rowLength)){
			//エラーになったらとりあえず、unLock試行 add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			DB_Close();
			fclose(fp);
			return 1;
		}
		//datawriteスタート
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info outputConvData End %s ",99,local_server_time(strTime));
		}

		if(itrgt->deb == 'D'){
			mytool_runtime_error('I',source_file_name
				," Debug [%2d]:Write %8d %s                             "
				, 99,(count + 1),local_server_time(strTime));
//			fprintf(stderr
//				," %s : Debug [%2d]:Write %8d %s                             \n"
//				,source_file_name, 99,(count + 1),local_server_time(strTime));
		}

		//無限ループなので終了を設けておく
		if(count > LOOP_MAX){
			break;
		}
	}

	//readように開いた時は
	if(itrgt->mod == 0){
		//開放しておかないとダメ
		mysql_free_result(res);
	}

	//終了処理
	//ここまで正常に来たらcommit
	memset(sqlStr,'\0',strlen(sqlStr));
	strcat(sqlStr,"COMMIT; ");
	if(mysql_query(mysql, sqlStr) != 0){
		//エラーになったらとりあえず、unLock試行 add koyama 20160313
		setTableUnlock(targSetting, itrgt);
		//ここまで来てエラーが出るのはおかしいのでmysql
		mytool_runtime_error('E',source_file_name," Error C [%2d]:commit error %s %s                       %s ", 19,itrgt->ifi,mysql_error(mysql),local_server_time(strTime));
		fclose(fp);
		return 1;
	}
	//メイン処理エンド
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name," Error [%02d]: Info dataconvert End %s ",99,local_server_time(strTime));
	}

	fclose(fp);

	//2015/08/28 kawada add S 管理テーブルのLock情報(LockMode)をUnlockに更新
	if (setTableUnlock(targSetting, itrgt) != 0){
		return 1;
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}


//Main
//IN  ： なし
//OUT ： なし
//author:koyama
int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";	//時間の文字列を格納
	Trgt_inf *trgt;                         //対象テーブル,対象ファイル,入出力モードなどを保持
	DB_SETTING targSetting;

	// 変数初期化
    memset(source_file_name, '\0', PATHNAME_SIZE);
    // カレントディレクトリ取得
//    getcwd(source_file_name, PATHNAME_SIZE);
	getprocessName(source_file_name, PATHNAME_SIZE);

	if(db_init(&targSetting)){
		//エラーはそろぞれの関数で出力
		//上では強制終了のみ
		raise(SIGABRT);
	}

	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //メモリ確保

	//引数が全くなしはダメ(プログラム名は必ずある)
	if(argc <= 1){
		fprintf(stderr," %s : Error C [%02d]: Option is nothing                      %s \n",source_file_name, 00,local_server_time(strTime));
		raise(SIGABRT);
	}


	//初期化
	trgt_ini(trgt);

	//自分自身の名前を取得 add koyama 20160506
	if(strrchr(argv[0],'/')){
		myself = strrchr(argv[0],'/') + 1;
	}else{
		myself = argv[0];
	}

	for(ii=1;ii < argc; ii++){
		ret = set_trgt(trgt,argv[ii]);
	}


	// DB処理
	ret = dataconvert(&targSetting, trgt);      //2015/08/28 kawada upd

	// 終了処理メモリ解放
	free(trgt);
	if(ret==1){
		//エラー出力はここまでに出力されているはず
//		fprintf(stderr," Error C [%02d]: %s                                      %s ", 9,argv[1], local_server_time(strTime));
	}else{
		//エラーが出たらその時点でコネクションが閉じられているので
		DB_Close();
	}

	return ret;
}
