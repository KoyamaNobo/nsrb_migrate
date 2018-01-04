/* COBOL MultiThread Signal Reciept And Output chenged Argment Version 0.1 */
/* Create 20150408  Author koyama */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>    /*va_listを使うために必要*/
#include <signal.h>
#include <time.h>
#include <sys/time.h>      /* localtimeに必要 20160916 */
#include <libcob.h>
#include <pthread.h>   //スレッド操作の実装
#include "confheader.h"	//同一ディレクトリにあるとき

//画面変化構造体
typedef struct screen_param{
	int statusFlg;     //値の変化のフラグ (初期値 : 0 , 文字列ポインタ設定 : 1 ,値一回以上変化後 : 2)
	char *param;       //対象文字列
} SD_SCREEN_PARAM;

//DBへpingする間隔
#define DB_PING_INTERVAL 30

static int counter_out=0;
static int counter_sig=0;
static char strTime[] = "0000/00/00 00:00:00.000000";
static struct timeval mt_db_access_time;
static struct timeval mt_m_db_access_time;
//////////////////Function liet AND prototype Start
//値の変化の判断(NULLの時は初期化)
int checkScreenValue(SD_SCREEN_PARAM *chrCache,cob_field *cobTarg);
//////////////////Function liet AND prototype End

//void SigHandler(int SignalName);

//値の変化の判断(NULLの時は初期化)
//変化があるとき0を変化なしまたは
// Author koyama
int checkScreenValue(SD_SCREEN_PARAM *chrCache,cob_field *cobTarg){

	return 0;
}

// 定期的にDBアクセスをする
// Author koyama
void* outThread( void* args ){
	int i = 0,lcnt=0;
	struct timeval mt_db_now_time;
	SD_SCREEN_PARAM curentScreen[SCR_OBJ_MAX];

	//最初の実行時間を取得
	gettimeofday(&mt_db_access_time, NULL);
	gettimeofday(&mt_m_db_access_time, NULL);
	while(1){
		//最初の実行時間を取得
		gettimeofday(&mt_db_now_time, NULL);
		//時間が一定以上経っていたら->DB_PING -> 成功したらその時間から再スタート
		if((mt_m_db_access_time.tv_sec + (DB_PING_INTERVAL * 60) < mt_db_now_time.tv_sec) ){
			//管理接続
			if(getDBMReadWriteFlg() == 0){
				if(DB_M_PING()==0){
					mt_m_db_access_time = mt_db_now_time;
				}
			}else{
				// DBのアクセスがあったということなので、アクセスしたことにする
				mt_m_db_access_time = mt_db_now_time;
			}
		}
		if((mt_db_access_time.tv_sec + (DB_PING_INTERVAL * 60) < mt_db_now_time.tv_sec) ){
			//通常接続
			if(strncmp(map_source_func,"DB_",3) != 0 && getDBReadWriteFlg() == 0){
				if(DB_PING()==0){
					mt_db_access_time = mt_db_now_time;
				}
			}else{
				// DBのアクセスがあったということなので、アクセスしたことにする
				mt_db_access_time = mt_db_now_time;
			}
		}
		sleep( 10 ); // スレッド 10 秒停止
	}

	return NULL;
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
			strncpy(setUser,cmdReturnP,(cmdReturnNexrP - cmdReturnP));
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

//エラーにプロセス名を付けて出力
//in/out :fmt 出力フォーマット,...フォーマットに対応する変数
//author : koyama
void mytool_runtime_error(const char *sfile,const char *fmt, ...){
	va_list ap;

	if(strlen(sfile) > 0){
		/* prefix */
		fprintf(stderr,"%s :",sfile);
		fflush (stderr);
	}
	/* body */
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);
	va_end (ap);

	/* postfix */
	fputs ("\n", stderr);
	fflush (stderr);
}

//文字列を大文字に
//author:n.koyama
//date  :20140530
//sは文字列配列じゃないとだめ
//pをsにコピーしながら小文字に変換
char *StrToLowerCpy(char *s,char *p){
	int i = 0;

	while(p[i] != '\0' ){
		s[i] = (char)tolower(p[i]);  /* pの指す中身を大文字に変換 */
		i++;
	}

	return (s);                  /* 文字列の先頭アドレスを返す */
}

//関数名を共有変数にセット(共通関数の実体)
//in/out :format 出力フォーマット,...フォーマットに対応する変数
//author : koyama
void setCommonFunctionName(char *tempStack,char *setStrFunc,char *format,...){
	va_list ap;

	//元々入っていた文字列をswap
	memcpy(tempStack,setStrFunc,MAP_SRC_FUNC_LEN);
	memset(setStrFunc,'\0',MAP_SRC_FUNC_LEN + 1);
	//
	va_start (ap, format);
	vsprintf (setStrFunc, format, ap);
	va_end (ap);
	if(myConfDebugFlg >= 5){
		cob_runtime_error(" Error [%04d]: %s Info MT   setCommonFunctionName : %s : %s ",__LINE__,local_server_time(strTime),tempStack,setStrFunc);
	}
	return;
}

void unsetCommonFunctionName(char *tempStack,char *setStrFunc){
	//共有変数を元に戻す
	if(myConfDebugFlg >= 5){
		cob_runtime_error(" Error [%04d]: %s Info MT unsetCommonFunctionName : %s : %s ",__LINE__,local_server_time(strTime),tempStack,setStrFunc);
	}
	memcpy(tempStack,setStrFunc,MAP_SRC_FUNC_LEN);
	return;
}

//エラーID出力
//in : libId = ライブラリ番号,funcId = 関数番号,seqId = 関数内での出力番号
//author : koyama
int mytoolgetErrorId(int libId,int funcId,int seqId){
	return ((16 * 16 * 16) * libId) +  (16 * funcId) + seqId;
}

// recieve INTERRUPT signal
// Author koyama
void* sigInt( int args ){
	int i = 0;

	//エラー出力をすることでAbortを設定
	cob_runtime_error(" Error C [99]:recieve interrupt signal ");
	raise(SIGABRT);

	return NULL;
}

// recieve SegmentationFalet signal
// Author koyama
void* sigSegv( int args ){
	int i = 0;

	//DBは閉じておかないと困るので、無理やり落としてみる
	DB_Close();
	//エラー出力をすることでAbortを設定
	if (map_source_func && strlen(map_source_func) > 0) {
		cob_runtime_error(" Error C [99]:recieve Segmentation Fault signal :%s ", map_source_func);
	}else{
		cob_runtime_error(" Error C [99]:recieve Segmentation Fault signal ");
	}
	raise(SIGTERM);

	return NULL;
}
// recieve Abort signal
// Author koyama
void* sigAbrt( int args ){
	int i = 0;

	//DBは閉じておかないと困るので、無理やり落としてみる
	DB_Close();
	//エラー出力をすることでAbortを設定
	if (map_source_func && strlen(map_source_func) > 0) {
		cob_runtime_error(" Error C [99]:recieve Abort signal :%s ", map_source_func);
	}else{
		cob_runtime_error(" Error C [99]:recieve Abort signal ");
	}
	raise(SIGTERM);

	return NULL;
}


// recieve USER1 signal
// Author koyama
void* sigUsr1( int args ){
	int i = 0;

	//DBは閉じておかないと困るので、無理やり落としてみる
	DB_Close();
	//エラー出力をすることでAbortを設定
	if (map_source_func && strlen(map_source_func) > 0) {
		cob_runtime_error(" Error C [99]:recieve usr1=kill signal :%s ", map_source_func);
	}else{
		cob_runtime_error(" Error C [99]:recieve usr1=kill signal ");
	}
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




int MT_Initialize(){
	int retval;
	pthread_t th_out;
//	pthread_t th_sig;
	//表示のバッファをしない設定
	setvbuf( stdout, NULL, _IOLBF, 0 );

	retval = 0;
	//output
	pthread_create( &th_out, NULL, outThread, (void *)NULL );

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
