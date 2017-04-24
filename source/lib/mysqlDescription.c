/* COBOL MultiThread Signal Reciept And Output chenged Argment Version 0.1 */
/* Create 20140401  Author iida -> koyama */
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <mysql.h>
#include <libcob.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>      /* localtimeに必要 20150828 */
#include <libxml/xmlreader.h>	/*confファイルのxmlを読むため*/
#include <libxml/xpath.h>		/*confファイルのxmlを読むため*/
#include "confheader.h"	 /*DB設定ファイル*/
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) < (b) ? (b) : (a))
/*
#define MYSQL_SERVER "localhost"
#define MYSQL_USERNAME "USER"
#define MYSQL_PASSWORD "PASSWORD"
#define MYSQL_DATABASE "mysql"
#define MYSQL_PORT 3306
*/
#define MYSQL_SOCKET NULL
#define MYSQL_OPT 0
#define WHERE_LENGTH 200
#define TNAME_LENGTH 31
#define LONGLONG_LENGTH 21
#define FIELD_MAX_NUM 100
#define READDATA_MAX_LEN 2048
#define STAT_LENGTH 4
//データの分類(素数で指定し %のあまりがあるかどうかで判定)
#define DB_WHERE_DEF 1
#define DB_WHERE_INX 2
#define DB_WHERE_RDB 3
const char* DATA_READ_NUM   = " 300";
#define DATA_UPDATE_NUM 50

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
const char* DEBUG_FLGNAME = "debug";
#endif

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif

//confから取得するdebug_flg.ここで代表宣言
int myConfDebugFlg;

//DBの設定(長さ制限の倍まで取れるようにしておく(mysqlの仕様を基準とする))
static char DB_db_server[128];
static char DB_db_user[34];
static char DB_db_password[130];
static char DB_db_database[130];
static int  DB_db_port;
static char DB_db_item_tbl[64];
static char DB_db_mana_tbl[64];
static char strTime[] = "0000/00/00 00:00:00.000000";

//文字列構造体
typedef struct dbWhere{
	size_t   data_size;                 // data変数の文字列の長さ
	char     *data;                     // データの指定
	size_t   where_size;                // where変数の文字列の長さ
	char     *where;                    // Whereの指定
	char     *tableName;                // Where句がかかる要因で作られるTableName
	unsigned long long int rdbReaded;   //rdbでどこまで読んだか
	int    flg;                         // なんのwhere文字列なのか 1 初期値 *2:key *3:RDB
} DBWhere;

//読み込みデータ構造体
typedef struct dbReaded{
	int                    no;                       //番号(登録時点での先頭からno番目)
	unsigned long long int dbId;                     //データのId
	unsigned long long int dbOriginalId;             //データのId
	char                   dbData[READDATA_MAX_LEN];     // 読んだデータの本体
	int                    dbDataLen;                // 読んだデータの本体の長さ
	struct dbReaded        *nextPoint;               //構造をはっきり決めるまでは双方向連結リスト (次のデータ)
	struct dbReaded        *prevPoint;               //構造をはっきり決めるまでは双方向連結リスト (前のデータ)
} DBReaded;

//DBのフィールド情報を一度とったら保存しておき、以降それを使う
typedef struct db_field_object{
	char fieldName[TNAME_LENGTH];      // 対象のフィールド名
	int  length;             //対象フィールドの長さ
	char midString[TNAME_LENGTH];      //MID(?,?)を格納
	char itemString[TNAME_LENGTH];     //ITEM,?,?を格納
	int isComp;              //COMPがField内に含まれているか
	char chCharMapString[TNAME_LENGTH]; //chCharMap付きの文字列
} DB_FIELD_OBJ;

//fromでつながる変数がどのような形をしているか
//OCCURSで定義されていることがあるので
typedef struct db_object{
	char tableName[TNAME_LENGTH];           // 対象のテーブル名
	char tablePName[TNAME_LENGTH];          // 対象のテーブル名(COBOL)
	char tableLName[TNAME_LENGTH];          // 対象の識別子
	char fileStatus[STAT_LENGTH];           // Openする際にSTATUSがあれば格納[0]:ORGANIZATION,[1]:ACCESS MODE
	char accessStatus[STAT_LENGTH];         // 最後に実行した処理のステータスを格納([0]実行した関数の頭文字 [1]成否 成'0' 否'1')
	char sharedStatus[STAT_LENGTH];         // 共有排他等の設定([0]O:Opened,C:Closed;[1] :初期値,E:排他,S:共有,P:ReadOnly;[2]I:INPUT,-:I-O,O:OUTPUT,E:EXTEND)
	char readStatus[1];                     // 直前のread指定([0]A:At End,I:Invalid;N:Next At End;)
	int  prevPoint;				 // 前回読んだID(ファイルポインタ相当)
	int  nextPoint;				 // 次の読んだID(ファイルポインタ相当)
	char *key1;					 // key1のポインタ
	char key1Name[TNAME_LENGTH];			  // key1の変数名
	int  key1Len;				   // key1の長さ
	int  key1Use;				   // key1の使う 1:true
	char *key2;					 // key2のポインタ*/
	char key2Name[TNAME_LENGTH];			  // key2の変数名
	int  key2Len;				   // key2の長さ
	int  key2Use;				   // key2の使う 1:true
	char *key3;					 // key3のポインタ*/
	char key3Name[TNAME_LENGTH];              // key3の変数名
	int  key3Len;                             // key3の長さ
	int  key3Use;                             // key2の使う 1:true
	DBWhere *strWhere;                        //Where句を格納 初期値nullポインタ 20141002
	DB_FIELD_OBJ fieldObj[FIELD_MAX_NUM];     //フィールドが一度以上使われたら内容を保存しておく
	DBReaded *dataReadedFirst;                //先読みしたデータの先頭
	DBReaded *dataReadedCur;                  //先読みした現在のデータ
	DBReaded *dataReadedIns;                  //先読みした直前に挿入したデータ
	DBReaded *dataInsertFirst;                  //Insert対象の先頭を保存するポインタ
	DBReaded *dataInsertCur;                  //Insert対象を保存するポインタ
	DBReaded *dataUpdateCur;                  //update対象を保存するポインタ
	DBReaded *dataDeleteCur;                  //Delete対象を保存するポインタ
} DB_TABLE_OBJ;

//#ifndef CONF_PATH
//#define CONF_PATH 1
//const char* CONF_FLIEPATH = "./conf.xml";
//const char* CONF_DEFPATH  = "//conf/*/text()";	//トップレベルから一直線を予定
//#endif

static int (*func)(char *errno, const char *errmsg);

//                                                        function list (prottype) Start
//COBOLから呼ばれるもの
int DB_Read(char *,char *,char *,char *,...);
int DB_Select(char *,...);
int DB_Update(char *,char *,char *);
int DB_F_Close(char *,char *);
int DB_Open();
int DB_Close();
int DB_Scratch(char *);
int DB_Trunc(char *);
int DB_Start(char*,char*,char*,char*);
//それ以外                                                        function list (prottype) End
char *mystrncat(char * ,int  ,const char *,int );
int M_UNLOCK(char *,const char*,int);
int __M_CLOSE(const char*,int);
char* getFieldSpecifiedElement(char *,char *,char *);
char* getFieldSpecified(char *,char *,char *);
void getAndSetFileStatus(char *,char *);
DBReaded *initDBReaded(DBReaded *);
int removeDBReaded(DBReaded *);
int destroyDBReaded(DB_TABLE_OBJ *);
int destroyDBInsert(DB_TABLE_OBJ *);
int destroyDBUpdate(DB_TABLE_OBJ *);
int destroyDBDelete(DB_TABLE_OBJ *);
int pushDBReaded(DB_TABLE_OBJ *,DBReaded *);
int pushDBInsert(DB_TABLE_OBJ *,DBReaded *);
int pushDBUpdate(DB_TABLE_OBJ *,DBReaded *);
int popDBReaded(DB_TABLE_OBJ *,char *);
int getDataDBReaded(DB_TABLE_OBJ *,char *);
int setReadedRecordFromDBResult(DB_TABLE_OBJ *,MYSQL_RES *);
int setInsertRecord(DB_TABLE_OBJ *,char *,int );
int setUpdateRecord(DB_TABLE_OBJ *,char *,int );
int setDeleteRecord(DB_TABLE_OBJ *);
int insertRecordFromBuffer(DB_TABLE_OBJ *);
int updateRecordFromBuffer(DB_TABLE_OBJ *);
int deleteRecordFromBuffer(DB_TABLE_OBJ *);
void initFieldObj(DB_FIELD_OBJ *);
int InitDB_Table(DB_TABLE_OBJ *);
//未使用？
int recordLength(char *);    ////テーブルに対する最大サイズ(レコードサイズ)を取得


//関数名を取得するために関数を書き換え
#define M_CLOSE() __M_CLOSE(__func__, __LINE__)
#define m_mysql_failure() __m_mysql_fail(__func__, __LINE__)
#define mysql_failure() __mysql_failure(__func__, __LINE__)

MYSQL	*mysqlConn;
static int	   errout;
DB_TABLE_OBJ  DB_table[128]={{"","","","","","","",0,0,NULL,"",0,0,NULL,"",0,0,NULL,"",0,0}};				  //Tableのオブジェクト
int DB_Table_num = 0;							   //Tableの有効数(次に作るオブジェクトの添え字番号)
//絶対に変わらない固定的なものはグローバルに置いておいたほうが良いのか？
//char DB_strAutocommit[] = "SET AUTOCOMMIT=0;";
char DB_intAutocommit = 0;
char *DB_filestat;
char *source_file_name;
char *source_user_name;
/* ----  アクセスプログラム共通関数  --------  */


/* ----  管理テーブル用関数 追加開始  --------  */
MYSQL	*m_mysqlConn;
int Uid = 0;						 //ユニークID(管理テーブル認証用)
int intCurTableNum = 0;						 //intCurTableNum(直前にどこのテーブルにアクセスしたのか保存しておく)


/* MYSQL型(mysql or m_mysql)を引数として共通化してください */
//m_mysqlエラー処理
int __m_mysql_fail(const char *funcName,int funcLine){
	int ret =0;
	//エラー内容を出力
	mytool_runtime_error(source_file_name," Error C [%02d]: M Query Error %20s %.40s %.20s:%04d %s "
		,99,mysql_error(m_mysqlConn),map_source_func,funcName,funcLine,local_server_time(strTime));

	//ロールバック
	ret = mysql_query(m_mysqlConn, "ROLLBACK");

	return ret;
}
//author:koyama 20170227
//[COBOLより]DBへの接続等の初期化を行う
int DB_Initialize(char *esp){
	int nn;
	char strConfPath[1024]; //20150828 add koyama
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Initialize ");

	//下で値が設定されなかったらこれがdefault
	myConfDebugFlg = 0;

	// 変数初期化
	if(source_file_name == NULL){
		for(nn =0;source_file_name == NULL  && nn < 10;nn++){
			source_file_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
		}
		memset(source_file_name, '\0', (PATHNAME_SIZE + 1));
		for(nn =0;source_user_name == NULL  && nn < 10;nn++){
			source_user_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
		}
		memset(source_user_name, '\0', (PATHNAME_SIZE + 1));
		// 実行コマンド名取得
//		getprocessName(source_file_name, PATHNAME_SIZE);
		getUserAndProcessName(source_file_name, source_user_name);
	}

	//file status pointer set error status pointer
	if(cob_call_params != 0){
		DB_filestat = esp;
	}else{
		DB_filestat = (char *)malloc(sizeof(char) * (STAT_LENGTH));
		memset(DB_filestat,0x00,(STAT_LENGTH));
	}
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';

}

//author:koyama 20170227
//[COBOLより]DBのReadを行う
//in:
//	iMode   : 読んでくる方法の指定
//	iPName  :
//	lockMode:UNLOCKの指定があるかどうかsource_file_name
int DB_Read(char *iMode,char *iPName,char *oItem,char *lockMode,...){

}
