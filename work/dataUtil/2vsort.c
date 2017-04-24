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
const char* DEBUG_FLGNAME = "debug";
#endif
#ifndef CONF_PATH
#define CONF_PATH
char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";	//20150915ローカルに conf.xml がないときの対応
char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif

/////////////////////////////////////////////////////////////////////CONSTANT
#define DEBUG_FLG 1
#define ITEMLST 256		// SORT対象構造体の要素数
const int  ARGITM     = 5;		// 引数構造体の要素数
#define LOGICAL_DEF -1
#define LOGICAL_AND 0
#define LOGICAL_OR 1

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

////////////////////////////////////////////////////////////////DB設定構造体
struct db_setting {
	char db_server[64];        // host名
	char db_user[64];          // user名
	char db_password[64];      // password
	char db_database[64];      // database名
	int db_port;               // port番号
	char db_manatbl[64];       // 管理table名	//2015/08/28 kawada add 管理テーブル名
}db_setting={"","","","",3306};
typedef struct db_setting DB_SETTING;

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
	size_t size;	// 文字列の長さ (allocateは+1でとること)
	char   *data;  // データの先頭 ->文字列ポインタに変更
};
typedef struct string_t String_t;

////////////////////////////////////////////////////////////////プロトタイプ宣言
//////////////////////////////共通関数系//////////////////////////////
char *local_server_time(char *);
char *getConfFilename(char *);
int conf_read(char *,char *,char *,char *,int *);
void chgChar(char *,char ,char ,char );
int strSplit( char *, char *, char ** );
//////////////////////////////mysql系関数//////////////////////////////
int mysql_failure();

//////////////////////////////unique系関数//////////////////////////////
int set_argv(Argv_inf *, char * );
int set_trgt(Trgt_inf *, char *);

////////////////////////////////////////////////////////////////global変数の定義
//confから取得するdebug_flg.他と被らないようにoriginal name
static int myConfDebugFlg;

MYSQL conn,*mysql = &conn;
MYSQL_RES *res;
MYSQL_ROW row;

////////////////////////////////////////////////////////////////実体
//////////////////////////////共通関数系//////////////////////////////
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

//設定ファイルの読み込み
//auth: koyama
//カレントdirのconfだけを開くとどこででも実行できないので
int conf_read(char *db_server,char *db_user,char *db_password,char *db_database,int *db_port){
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
					strcpy(db_server,node->content);
				}
				//設定ファイルからDBのユーザ名を取得
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(db_user,node->content);
				}
				//設定ファイルからDBのパスワードを取得
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(db_password,node->content);
				}
				//設定ファイルからDB名を取得
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(db_database,node->content);
				}
				//設定ファイルからポートを取得
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					*db_port = atoi(node->content);
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
//////////////////////////////mysql系関数//////////////////////////////
//*********************************************************
//SQL実行エラー時の後処理
//*********************************************************
int mysql_failure(){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	//エラー内容を出力
	fprintf(stderr,"Query Error %s: %s\n",local_server_time(strTime),mysql_error(mysql));

	//ロールバック
	ret = mysql_query(&conn, "ROLLBACK");

	//DB接続の切断
	mysql_close(&conn);

	return ret;
}

//////////////////////////////unique系関数//////////////////////////////
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
			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));	//要素分解
			continue;
		}
	}

	return ret;
}

//*********************************************************
//DB移行処理
//*********************************************************
int sortExecute(Trgt_inf *itrgt){
	int count = 0,dataRowCount = 0,dataLength = 0;
	char sqlStr[2048] = "";
	int intDefDataLen = 0;
	char strTime[] = "0000/00/00 00:00:00.000000";	//時間の文字列を格納

	String_t query;	      //初期設定を読み込むSQLの格納
}

int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	Trgt_inf *trgt;
	DB_SETTING targSetting;
	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //メモリ確保

	//初期化
	trgt_ini(trgt);

	for(ii=1;ii < argc; ii++){
		//第二引数を「_」でオプション別に区分けする
		ret = set_trgt(trgt,argv[ii]);
		if(ret==1){
			fprintf(stderr,"SORT OPTION Error %s : %s\n", local_server_time(strTime),argv[1]);
			//強制終了
			free(trgt);
			exit(ret);
		}
	}
	// DB処理
	ret = sortExecute(trgt);

	// メモリ解放
	free(trgt);
	return ret;
}
