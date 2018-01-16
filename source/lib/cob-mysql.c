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
const char* CONF_DB_HOST  = "dbHost";
const char* CONF_DB_USER  = "dbUser";
const char* CONF_DB_PASS  = "dbPass";
const char* CONF_DB_NAME  = "dbName";
const char* CONF_DB_PORT  = "dbPort";
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
	char fileStatus[STAT_LENGTH];           // Openする際にSTATUSがあれば格納[0]:ORGANIZATION(ファイル編成),[1]:ACCESS MODE(Dynamic,Random,Sequential),[2]:OPEN MODE(INPUT,OUTPUT,I-O,EXTEND)
	char accessStatus[STAT_LENGTH];         // 最後に実行した処理のステータスを格納([0]実行した関数の頭文字 [1]成否 成'0' 否'1' [2])
	char sharedStatus[STAT_LENGTH];         // 共有排他等の設定([0]O:Opened,C:Closed;[1] :初期値,E:排他,S:共有,P:ReadOnly;[2]I:INPUT,-:I-O,O:OUTPUT,E:EXTEND)
	char readStatus[STAT_LENGTH];           // 直前のread指定([0]A:At End,I:Invalid;N:Next At End;)
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
	DBReaded *dataInsertFirst;                //Insert対象の先頭を保存するポインタ
	DBReaded *dataInsertCur;                  //Insert対象を保存するポインタ
	DBReaded *dataUpdateCur;                  //update対象を保存するポインタ
	DBReaded *dataUpdateFirst;                //update対象の先頭を保存するポインタ
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
//  設定ファイルの読み込みや初期設定
int DB_Initialize(char *esp);
// DBへのSELECT (条件指定と一時テーブルの作成)
int DB_Select(char *,...);
//DBへのリード
int DB_Read(char *,char *,char *,char *,...);
// 変更関数
int DB_Update(char *,char *,char *);
//  追加関数
int DB_Insert(char *iPName,char *iLName,char *iItem);
//  削除関数
int DB_Delete(char *iPName);
// ファイルオープン
int DB_F_Open(char *,char *,char *,char *,char *,...);
// ファイルクローズ
int DB_F_Close(char *,char *);
// コネクション開始
int DB_Open();
// データベースのクローズ
int DB_Close();
// RDBファイルの条件指定を削除
int DB_Scratch(char *);
//ファイルの中身削除
int DB_Trunc(char *);
//READの位置決め
int DB_Start(char*,char*,char*,char*);
//それ以外                                                        function list (prottype) End
char *mystrncat(char * ,int  ,const char *,int );
int M_UNLOCK(char *,const char*,int);
int __M_CLOSE(const char*,int);
char *getAssignPName(char **,char *);                //assign用のenv問い合わせ
char* getFieldSpecifiedElement(char *,char *,char *);
char* getFieldSpecified(char *,char *,char *);
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
//insertの際にkeyかぶりのcheckを行う
int existsDupulicateKey(DB_TABLE_OBJ *,char *);
//Updateから呼ばれる専用Read
int DB_Read_for_update(char *,char *,char *,DBReaded *);
int matchTableStatus(DB_TABLE_OBJ *,char *);
void setTableStatus(DB_TABLE_OBJ *,char ,int );
char getTableStatusWithIndex(DB_TABLE_OBJ *,int );
//ファイルのオープンモードを文字列で取得
char *getFileStatus(DB_TABLE_OBJ *);
//ファイルのオープンモードのファイル編成を文字で取得
char getFileStatusOrgnization(DB_TABLE_OBJ *);
//ファイルのオープンモードのアクセスモードを文字で取得
char getFileStatusAccess(DB_TABLE_OBJ *);
//ファイルのアクセスモードやオープンモードをセット
void setFileStatus(char *,DB_TABLE_OBJ *);
//調整したテーブル名を追加 句を作成
void *setStrOrigPrePoint(char *strId ,DB_TABLE_OBJ *targetTable);
//DB_ReadのINVALIDKey際のクエリをつなぐ関数 key3
int set_DB_Read_query_has_invalid_key3(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen);
//DB_ReadのINVALIDKey際のクエリをつなぐ関数 key2
int set_DB_Read_query_has_invalid_key2(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen);
//DB_ReadのINVALIDKey際のクエリをつなぐ関数 key1
int set_DB_Read_query_has_invalid_key1(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen);
//DB_ReadのINVALIDKey際のクエリをつなぐ関数(ラッパー)
int set_DB_Read_query_has_invalid_key(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen,int keyNum);
//初期値のときのテーブルの論理名、物理名を取得(物理名の後ろをNULL埋めし,論理名を返す)
char *setLogicalAndPhysicalTableName(char *strPhysicalName);
//DBへのトランザクションの開始 (COBOLから呼ばれる仕様だったが隠蔽した)
int DB_Transaction(MYSQL *);
//DBへのロールバック (COBOLから呼ばれる仕様だったが隠蔽した)
int DB_Rollback(MYSQL *);
//DBへのコミット (COBOLから呼ばれる仕様だったが隠蔽した)
int DB_Commit(MYSQL *);
//ソートしていないIDをソートしたIDに振替
int getIntConvertOrigPtoOrderP(DB_TABLE_OBJ *targetTable);
//未使用？
////テーブルに対する最大サイズ(レコードサイズ)を取得
int recordLength(char *);

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
char *DB_filestat;                   //COBOLから渡されたステータス変数
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

/*トランザクション*/
int M_Transaction(){
	int ret =0;
	//test いったんtransactionを止める
//	ret = mysql_query(m_mysqlConn, "START TRANSACTION");
	return ret;
}
//assignがある時のために,環境変数にテーブル名があるか確認
//in:iPName:確認するテーブル名
//out:問い合わせた結果のテーブル名,なかったら元のiPName
char *getAssignPName(char **determinePName,char *iPName){
	*determinePName = getenv(iPName);
	//getenvが取れない,または中身が空文字列の時
	if(*determinePName == NULL || strlen(*determinePName) == 0){
		*determinePName = iPName;
	}
	return *determinePName;
}

//テーブルオブジェクト をphysicalな名前で取得
int getTargTable(DB_TABLE_OBJ *arrTable,char *iPName,int intTableMax){
	int i = 0;
	char *determinePName = NULL;
	//assignしてある時の対応
	getAssignPName(&determinePName,iPName);
	//ターゲットとなるテーブルオブジェクトの検索
	for(i=0;i < intTableMax;i++){
		if(strcmp(arrTable[i].tablePName,determinePName) == 0){
			return i;
		}
	}
	//ターゲットが見つからなかったら-1
	return -1;
}

//テーブルオブジェクト をlogicalな名前で取得
int getTargTableLogic(DB_TABLE_OBJ *arrTable,char *iLName,int intTableMax){
	int i = 0;
	//ターゲットとなるテーブルオブジェクトの検索
	for(i=0;i < intTableMax;i++){
		if(strcmp(arrTable[i].tableLName,iLName) == 0){
			return i;
		}
	}
	//ターゲットが見つからなかったら-1
	return -1;
}
//DBWhereを初期化
void initDBWhere(DBWhere *targetWhere){
	targetWhere->data       = NULL;        //文字列はNULL初期化
	targetWhere->data_size  = 0;
	targetWhere->where      = NULL;        //文字列はNULL初期化
	targetWhere->where_size = 0;
	targetWhere->tableName  = NULL;        //文字列はNULL初期化
	targetWhere->rdbReaded  = 0;
	targetWhere->flg  = DB_WHERE_DEF;
	return ;
}

//DBReaded(先読みデータ)に読み込むデータ構造を初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20161213
DBReaded *initDBReaded(DBReaded *item){
	if(item == NULL){
		item = malloc(sizeof(DBReaded));
	}
	item->dbId         = 0;
	item->dbOriginalId = 0;
	memset(item->dbData,'\0',READDATA_MAX_LEN);
	item->dbDataLen    = 0;
	item->nextPoint    = NULL;        //NULLPO
	item->prevPoint    = NULL;        //NULLPO
	return item;
}

//DBReaded(先読みデータ)に読み込むデータ構造をループで全て初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20161213
int removeDBReaded(DBReaded *delItem){
	int incorrect = 0;
	delItem->dbId         = 0;
	delItem->dbOriginalId = 0;
	memset(delItem->dbData,'\0',READDATA_MAX_LEN);
	delItem->dbDataLen    = 0;
	delItem->nextPoint    = NULL;        //NULLPO
	delItem->prevPoint    = NULL;        //NULLPO
	free(delItem);
	return incorrect;
}

//DBReaded(先読みデータ)に読み込むデータ構造をループで全て初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20161213
int destroyDBReaded(DB_TABLE_OBJ *targCluster){
	int incorrect = 0;
	DBReaded *curItem = NULL;
	curItem = targCluster->dataReadedFirst;
	while(curItem != NULL){
		DBReaded *delItem = curItem;
		curItem = curItem->nextPoint;
		removeDBReaded(delItem);
	}
	targCluster->dataReadedFirst = NULL;
	targCluster->dataReadedCur = NULL;
	return incorrect;
}

//DBInsert(insert)に読み込むデータ構造をループで全て初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int destroyDBInsert(DB_TABLE_OBJ *targCluster){
	int incorrect = 0;
	DBReaded *curItem = NULL;
	curItem = targCluster->dataInsertCur;
	while(curItem != NULL){
		DBReaded *delItem = curItem;
		curItem = curItem->prevPoint;
		removeDBReaded(delItem);
	}
	targCluster->dataInsertCur = NULL;
	return incorrect;
}

//DBUpdate(update,insert)に読み込むデータ構造をループで全て初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int destroyDBUpdate(DB_TABLE_OBJ *targCluster){
	int incorrect = 0;
	DBReaded *curItem = NULL;
	curItem = targCluster->dataUpdateCur;
	while(curItem != NULL){
		DBReaded *delItem = curItem;
		curItem = curItem->prevPoint;
		removeDBReaded(delItem);
	}
	targCluster->dataUpdateCur = NULL;
	return incorrect;
}

//DBInsert(insert)に読み込むデータ構造をループで全て初期化
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int destroyDBDelete(DB_TABLE_OBJ *targCluster){
	int incorrect = 0;
	DBReaded *curItem = NULL;
	curItem = targCluster->dataDeleteCur;
	while(curItem != NULL){
		DBReaded *delItem = curItem;
		curItem = curItem->prevPoint;
		removeDBReaded(delItem);
	}
	targCluster->dataDeleteCur = NULL;
	return incorrect;
}

//DBReaded(先読みデータ)に読み込んだデータを１行push
//in:targCluster 読み込んだデータを挿入しておく塊
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20161213
int pushDBReaded(DB_TABLE_OBJ *targCluster,DBReaded *item){
	int incorrect = 0;
	if(targCluster->dataReadedFirst == NULL){
		item->no = 1;
		targCluster->dataReadedFirst = item;
		targCluster->dataReadedCur   = item;
		targCluster->dataReadedIns   = item;
	}else{
		item->no = (targCluster->dataReadedIns->no + 1);
		//直前に入れたデータが今のデータの前のデータ
		item->prevPoint = targCluster->dataReadedIns;
		//前のデータの次に今回のデータを入れる
		targCluster->dataReadedIns->nextPoint =item;
		//次に入れる場所を定義
		targCluster->dataReadedIns = item;
	}

	return incorrect;
}

//DBInsert(insert用)のデータを１行push
//in:targCluster 読み込んだデータを挿入しておく塊
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int pushDBInsert(DB_TABLE_OBJ *targCluster,DBReaded *item){
	int incorrect = 0;
	if(targCluster->dataInsertCur == NULL){
		item->no = 1;
		targCluster->dataInsertCur   = item;
		//最初を保存しておく
		targCluster->dataInsertFirst = targCluster->dataInsertCur;
	}else{
		item->no = (targCluster->dataInsertCur->no + 1);
		//直前に入れたデータが今のデータの前のデータ
		item->prevPoint = targCluster->dataInsertCur;
		//前のデータの次に今回のデータを入れる
		targCluster->dataInsertCur->nextPoint =item;
		//次に入れる場所を定義
		targCluster->dataInsertCur = item;
	}

	return incorrect;
}

//DBUpdate(update,insert用)のデータを１行push
//in:targCluster 読み込んだデータを挿入しておく塊
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int pushDBUpdate(DB_TABLE_OBJ *targCluster,DBReaded *item){
	int incorrect = 0;
	if(targCluster->dataUpdateCur == NULL){
		item->no = 1;
		targCluster->dataUpdateCur   = item;
		//最初を保存しておく
		targCluster->dataUpdateFirst = targCluster->dataUpdateCur;
	}else{
		item->no = (targCluster->dataUpdateCur->no + 1);
		//直前に入れたデータが今のデータの前のデータ
		item->prevPoint = targCluster->dataUpdateCur;
		//前のデータの次に今回のデータを入れる
		targCluster->dataUpdateCur->nextPoint =item;
		//次に入れる場所を定義
		targCluster->dataUpdateCur = item;
	}

	return incorrect;
}

//DBUpdate(update,insert用)のデータを１行push
//in:targCluster 読み込んだデータを挿入しておく塊
//in:item        読み込んだデータの構造体
//out:実行が成功したかどうか エラーなら1
//author:koyama 20170117
int pushDBDelete(DB_TABLE_OBJ *targCluster,DBReaded *item){
	int incorrect = 0;
	if(targCluster->dataDeleteCur == NULL){
		item->no = 1;
		targCluster->dataDeleteCur   = item;
	}else{
		item->no = (targCluster->dataDeleteCur->no + 1);
		//直前に入れたデータが今のデータの前のデータ
		item->prevPoint = targCluster->dataDeleteCur;
		//前のデータの次に今回のデータを入れる
		targCluster->dataDeleteCur->nextPoint =item;
		//次に入れる場所を定義
		targCluster->dataDeleteCur = item;
	}

	return incorrect;
}

//DBReaded(先読みデータ)に読み込んだデータを１行取得し、取得したものをリストから削除
//in:targCluster 読み込んだデータを挿入しておく塊
//in:dataItem    データを返すポインタ
//out:実行が成功したかどうか エラーなら1
//author:koyama 20161213
int popDBReaded(DB_TABLE_OBJ *targCluster,char *dataItem){
	int incorrect = 0;
	DBReaded *delItem=NULL;

	//現在行のIDをセット
	targCluster->prevPoint = targCluster->dataReadedCur->dbId;
	//現在行のデータを取得
	memcpy(dataItem,targCluster->dataReadedCur->dbData, targCluster->dataReadedCur->dbDataLen);

	delItem = targCluster->dataReadedCur;
	targCluster->dataReadedCur = targCluster->dataReadedCur->nextPoint;
	if(targCluster->dataReadedCur != NULL){
		targCluster->dataReadedCur->prevPoint = NULL;
	}
	removeDBReaded(delItem);

	//最初のところに現在を入れ直す(事前に一緒にしてるはずなので、いらないかも)
	targCluster->dataReadedFirst = targCluster->dataReadedCur;

	return incorrect;
}

int getDataDBReaded(DB_TABLE_OBJ *targCluster,char *dataItem){
	//予め格納したデータ構造の最初の一行を取得
	targCluster->prevPoint = targCluster->dataReadedCur->dbId;
	memcpy(dataItem,targCluster->dataReadedCur->dbData, targCluster->dataReadedCur->dbDataLen);
	;
	return 0;
}

//DBからReadした結果を構造体に格納
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
int setReadedRecordFromDBResult(DB_TABLE_OBJ *TableObj,MYSQL_RES *result){
	MYSQL_ROW res;
	int otherRownum =0;
	int maxcols=0;			 //フィールドの数とそのループ変数
	unsigned long *f_length;	 //フィールドの長さ(配列として受け取るのでポインタ宣言)

	while(res = mysql_fetch_row(result)){
		if(otherRownum == 0){
			f_length =  mysql_fetch_lengths(result);
			maxcols = min(cob_call_params, mysql_num_fields(result));
		}
		//次のidだけでいいので0だけ
		if(otherRownum == 0){
			TableObj->nextPoint = atoi(res[0]);
		}
		DBReaded *otherRow=NULL;
		otherRow = initDBReaded(NULL);
		otherRow->dbId         = atoi(res[0]);
		otherRow->dbOriginalId = atoi(res[2]);              //一応元のIDを記憶しておく
		memset(otherRow->dbData,'\0',READDATA_MAX_LEN);
		memcpy(otherRow->dbData,res[1],f_length[1]);
		otherRow->dbDataLen = f_length[1];
		pushDBReaded(TableObj,otherRow);
		otherRownum++;
	}
	return otherRownum;
}

//DBにINSERT予定のデータを構造体に格納
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170117
int setInsertRecord(DB_TABLE_OBJ *TableObj,char *result,int f_length){
	MYSQL_ROW res;
	int otherRownum =0;
	int maxcols=0;			 //フィールドの数とそのループ変数

	DBReaded *otherRow=NULL;
	otherRow = initDBReaded(NULL);
	//ID,originalIDは前回読んだもので更新する
	otherRow->dbId         = 0;     //InsertはIDが無いので
	otherRow->dbOriginalId = 0;
	memset(otherRow->dbData,'\0',READDATA_MAX_LEN);
	memcpy(otherRow->dbData,result,f_length);
	otherRow->dbDataLen = f_length;
	pushDBInsert(TableObj,otherRow);
	return otherRownum;
}

//DBにUPDATE予定のデータを構造体に格納
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数,エラーの時-1
//Author:koyama 20170117
int setUpdateRecord(DB_TABLE_OBJ *TableObj,char *result,int f_length){
	MYSQL_ROW res;
	int otherRownum =0;
	int maxcols=0;			 //フィールドの数とそのループ変数

	DBReaded *otherRow=NULL;
	otherRow = initDBReaded(NULL);
	//INVALID KEYのときはKEYで更新をかけないとダメ -> INVALIDが使えるのはRANDAMで開いた時
	if(getFileStatusAccess(TableObj) == 'R'){
		DBReaded *dataReadedUpdateCur=NULL;                  //現在のデータ(Update用)
		char dataTemp[READDATA_MAX_LEN]="";
		int readResult = 0;
		dataReadedUpdateCur = initDBReaded(NULL);
		readResult = DB_Read_for_update("INVALID",TableObj->tablePName,dataTemp,dataReadedUpdateCur);
		if(readResult != 0){
			// TODO 結果をどう返すかを考える必要がある
			return -1;
		}
		//ID,originalIDは前回読んだもので更新する
		otherRow->dbId         = dataReadedUpdateCur->dbId;     //一応IDを記憶しておくがメインは下
		otherRow->dbOriginalId = dataReadedUpdateCur->dbOriginalId;
		//違う指定で取得したので一旦削除
		destroyDBReaded(TableObj);
	}else{
		//ID,originalIDは前回読んだもので更新する
		otherRow->dbId         = TableObj->dataReadedCur->dbId;     //一応IDを記憶しておくがメインは下
		otherRow->dbOriginalId = TableObj->dataReadedCur->dbOriginalId;
	}
	memset(otherRow->dbData,'\0',READDATA_MAX_LEN);
	memcpy(otherRow->dbData,result,f_length);
	otherRow->dbDataLen = f_length;
	pushDBUpdate(TableObj,otherRow);
	return otherRownum;
}

//DBにUPDATE予定のデータを構造体に格納
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170117
int setDeleteRecord(DB_TABLE_OBJ *TableObj){
	MYSQL_ROW res;
	int otherRownum =0;
	int maxcols=0;			 //フィールドの数とそのループ変数

	DBReaded *otherRow=NULL;
	otherRow = initDBReaded(NULL);
	//ID,originalIDは前回読んだもので更新する
	otherRow->dbId         = TableObj->dataReadedCur->dbId;     //一応IDを記憶しておくがメインは下
	otherRow->dbOriginalId = TableObj->dataReadedCur->dbOriginalId;
	memset(otherRow->dbData,'\0',READDATA_MAX_LEN);
	otherRow->dbDataLen = 0;
	pushDBDelete(TableObj,otherRow);
	return otherRownum;
}

//INSERT予定のデータをInsert分に変換し発行
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170117
int insertRecordFromBuffer(DB_TABLE_OBJ *targCluster){
	int ret = 0,sqlstrLen = 0,itemLen=0;
	char strSql[(DATA_UPDATE_NUM * READDATA_MAX_LEN) + (READDATA_MAX_LEN * 2)] = "";
	char *tmpItem=NULL;
	DBReaded *curItem=NULL;

	sprintf(strSql,"/*  pg_name %s : insertRecordFromBuffer %s */ INSERT INTO ",source_file_name,local_server_time(strTime));

	//テーブル名指定
	sqlstrLen += strlen(strSql);
	mystrncat( strSql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	mystrncat( strSql,sqlstrLen,targCluster->tableName, strlen(targCluster->tableName) );
	sqlstrLen += strlen(targCluster->tableName);
	mystrncat( strSql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");

	//更新項目指定
	mystrncat( strSql, sqlstrLen," (ITEM) VALUES",strlen(" (ITEM) VALUES"));
	sqlstrLen += strlen(" (ITEM) VALUES");
	curItem = targCluster->dataInsertFirst;
	while(curItem != NULL){
		//データの中身の作成 (Insertは順番を正確に)
		if(curItem->prevPoint == NULL){
			//全操作するときのcurItem->noとDATA_UPDATE_NUMは仕組み上一致するように
			mystrncat( strSql, sqlstrLen," ( \"",strlen(" ( \""));
			sqlstrLen += strlen(" ( \"");
		}else{
			mystrncat( strSql, sqlstrLen," ,( \"",strlen(" ,( \""));
			sqlstrLen += strlen(" ,( \"");
		}
		tmpItem = malloc(READDATA_MAX_LEN * sizeof(char));
		memset(tmpItem,'\0',READDATA_MAX_LEN);
		//SQLで意味のある文字をエスケープ
		itemLen = dataEscapeCopy(curItem->dbData,tmpItem,curItem->dbDataLen);
		//\0に置き換えている
		mystrncat( strSql, sqlstrLen, tmpItem,itemLen);
		sqlstrLen += itemLen;

		mystrncat( strSql, sqlstrLen, "\")\n",strlen("\")\n"));
		sqlstrLen += strlen("\")\n");
		free(tmpItem);

		DBReaded *delItem = curItem;
		curItem = curItem->nextPoint;
	}
	mystrncat( strSql, sqlstrLen, "; ",strlen("; "));
	sqlstrLen += strlen("; ");
	//SQL実行
	//クエリ発行の直前でtrunsaction
	DB_Transaction(mysqlConn);
	if(mysql_real_query(mysqlConn, strSql, sqlstrLen) != 0){
	    mysql_failure();
	    DB_Rollback(mysqlConn);
	    ret = 1;
	}else{
	    //AutoCommit Offなので
	    DB_Commit(mysqlConn);
	}
	return ret;
}

//UPDATE予定のデータをUpdate文に変換し発行
//INSERTは一度に発行できるが、
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170117
int updateRecordFromBuffer(DB_TABLE_OBJ *targCluster){
	int ret = 0,sqlstrLen = 0,partSqlStrLen=0,itemLen=0;
	char partSqlStr[READDATA_MAX_LEN]="";
	char strSql[(DATA_UPDATE_NUM * READDATA_MAX_LEN) + (READDATA_MAX_LEN * 2)] = "";
	char *tmpItem=NULL;
	DBReaded *curItem=NULL;

	sprintf(strSql,"/*  pg_name %s : updateRecordFromBuffer %s */ ",source_file_name,local_server_time(strTime));

	//部分だけを作ってあとでつなげる
	mystrncat( partSqlStr, partSqlStrLen,"UPDATE `",strlen("UPDATE `"));
	partSqlStrLen += strlen("UPDATE `");
	mystrncat( partSqlStr,partSqlStrLen,targCluster->tableName, strlen(targCluster->tableName) );
	partSqlStrLen += strlen(targCluster->tableName);
	mystrncat( partSqlStr, partSqlStrLen,"` SET ITEM= \"",strlen("` SET ITEM= \""));
	partSqlStrLen += strlen("` SET ITEM= \"");

	//クエリ発行の直前でtrunsaction
	DB_Transaction(mysqlConn);
	curItem = targCluster->dataUpdateFirst;
	while(curItem != NULL){
		sqlstrLen=0;
		char strKeyTemp[LONGLONG_LENGTH]="";
		//データの中身の作成
		memset(strSql,'\0',strlen(strSql));
		//頭をつなぐ
		mystrncat( strSql, sqlstrLen, partSqlStr ,partSqlStrLen);
		sqlstrLen += partSqlStrLen;

		tmpItem = malloc(READDATA_MAX_LEN * sizeof(char));
		memset(tmpItem,'\0',READDATA_MAX_LEN);
		//SQLで意味のある文字をエスケープ
		itemLen = dataEscapeCopy(curItem->dbData,tmpItem,curItem->dbDataLen);
		//\0に置き換えている
		mystrncat( strSql, sqlstrLen, tmpItem,itemLen);
		sqlstrLen += itemLen;

		mystrncat( strSql, sqlstrLen, "\" ",strlen("\" "));
		sqlstrLen += strlen("\" ");

		//WHEREをつなぐ 20170117 koyama
		memset(strKeyTemp,'\0',LONGLONG_LENGTH);
		sprintf(strKeyTemp,"%d",curItem->dbOriginalId);
		mystrncat( strSql, sqlstrLen, " WHERE ID = ",strlen(" WHERE ID = "));
		sqlstrLen += strlen(" WHERE ID = ");
		mystrncat( strSql, sqlstrLen, strKeyTemp,strlen(strKeyTemp));
		sqlstrLen += strlen(strKeyTemp);
		mystrncat( strSql, sqlstrLen, "; ",strlen("; "));
		sqlstrLen += strlen("; ");
		free(tmpItem);

		if(mysql_real_query(mysqlConn, strSql, sqlstrLen) != 0){
			mysql_failure();
			ret = 1;
			break;
		}

		DBReaded *delItem = curItem;
		curItem = curItem->nextPoint;
	}

	//SQL実行
	if(ret == 1){
			//failureを通ったらダメ
	    DB_Rollback(mysqlConn);
	}else{
	    //AutoCommit Offなので
	    DB_Commit(mysqlConn);
	}
	return ret;
}

//DELETE予定のデータをDELETE文に変換し発行
//INSERTは一度に発行できるが、
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170118
int deleteRecordFromBuffer(DB_TABLE_OBJ *targCluster){
	int ret = 0,itemLen=0;
	char strSql[(DATA_UPDATE_NUM * READDATA_MAX_LEN) + (READDATA_MAX_LEN * 2)] = "";
	char *strKeyTemp=NULL;
	DBReaded *curItem=NULL;

	sprintf(strSql,"/*  pg_name %s : DB_Delete %s */ DELETE FROM ",source_file_name,local_server_time(strTime));
	//テーブル名指定
	strcat( strSql, "`");
	strcat( strSql,targCluster->tableName);
	strcat( strSql, "`");

	strcat( strSql, " WHERE ID IN (");

	curItem = targCluster->dataDeleteCur;
	while(curItem != NULL){
		//データの中身の作成
		if(curItem->nextPoint == NULL){
			//nextが無いのは最初だけ,prevPointには入っている
			strcat( strSql, " ");
		}else{
			strcat( strSql, ",");
		}
		//IDをつなげる
		strKeyTemp = malloc(READDATA_MAX_LEN * sizeof(char));
		memset(strKeyTemp,'\0',LONGLONG_LENGTH);
		sprintf(strKeyTemp,"%d",curItem->dbOriginalId);
		strcat( strSql, strKeyTemp);
		free(strKeyTemp);

		DBReaded *delItem = curItem;
		curItem = curItem->prevPoint;
	}
	strcat( strSql, "); ");
	//クエリ発行の直前でtrunsaction
	DB_Transaction(mysqlConn);
	//SQL実行
	if(mysql_real_query(mysqlConn, strSql, strlen(strSql)) != 0){
	    mysql_failure();
	    DB_Rollback(mysqlConn);
	    ret = 1;
	}else{
	    //AutoCommit Offなので
	    DB_Commit(mysqlConn);
	}
	return ret;
}

//Insert予定のデータがBufferに溜まっているか確認し、溜まっているなら処理
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170119
int checkInsertBuffer(DB_TABLE_OBJ *targCluster){
	int retVal = 0;
	if(targCluster->dataInsertCur != NULL){
	    //全ての更新を行う
	    retVal = insertRecordFromBuffer(targCluster);
	    //全ての削除を行う
	    destroyDBInsert(targCluster);
	}
	return retVal;
}

//UPDATE予定のデータがBufferに溜まっているか確認し、溜まっているなら処理
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170119
int checkUpdateBuffer(DB_TABLE_OBJ *targCluster){
	int retVal = 0;
	if(targCluster->dataUpdateCur != NULL){
	    //全ての更新を行う
	    retVal = updateRecordFromBuffer(targCluster);
	    //全ての削除を行う
	    destroyDBUpdate(targCluster);
	}
	return retVal;
}

//DELETE予定のデータがBufferに溜まっているか確認し、溜まっているなら処理
//in TableObj:対象の構造体 result:取得したデータ
//return otherRownum:読んだ行数
//Author:koyama 20170119
int checkDeleteBuffer(DB_TABLE_OBJ *targCluster){
	int retVal = 0;
	if(targCluster->dataDeleteCur != NULL){
	    //全ての更新を行う
	    retVal = deleteRecordFromBuffer(targCluster);
	    //全ての削除を行う
	    destroyDBDelete(targCluster);
	}
	return retVal;
}

//フィールドオブジェクトを初期化
void initFieldObj(DB_FIELD_OBJ *fieldObj){
	memset(fieldObj->fieldName,0x00,TNAME_LENGTH);
	memset(fieldObj->midString,0x00,TNAME_LENGTH);
	memset(fieldObj->itemString,0x00,TNAME_LENGTH);
	memset(fieldObj->chCharMapString,0x00,TNAME_LENGTH);
	fieldObj->isComp = -1;
	fieldObj->length = -1;
	return ;
}

//ここで作るDB_Tableオブジェクトを初期化
int InitDB_Table(DB_TABLE_OBJ *TableObj){
	TableObj->prevPoint = 0;
	//キー値の初期化
	memset(TableObj->key1Name,'\0',TNAME_LENGTH); //名前はから文字列
	memset(TableObj->key2Name,'\0',TNAME_LENGTH);
	memset(TableObj->key3Name,'\0',TNAME_LENGTH);
	TableObj->key1    = 0;   //ポインタはNULLポインタ
	TableObj->key2    = 0;
	TableObj->key3    = 0;
	TableObj->key1Len = 0;   //変数の長さは0で初期化
	TableObj->key2Len = 0;
	TableObj->key3Len = 0;
	TableObj->key1Use = 0;
	TableObj->key2Use = 0;
	TableObj->key3Use = 0;
	TableObj->dataReadedFirst  = NULL;     //NULLPO
	TableObj->dataReadedCur    = NULL;     //NULLPO
	TableObj->dataReadedIns    = NULL;     //NULLPO
	TableObj->dataUpdateCur    = NULL;     //NULLPO
	TableObj->dataInsertFirst  = NULL;     //NULLPO
	TableObj->dataInsertCur    = NULL;     //NULLPO
	memset(TableObj->fileStatus,'\0',STAT_LENGTH);
	memset(TableObj->accessStatus,'\0',STAT_LENGTH);
	memset(TableObj->sharedStatus,'\0',STAT_LENGTH);
}

//テーブルオブジェクトから次の空のフィールドオブジェクトを(同じものがあれば同じものを)
int getNextFieldObj(DB_TABLE_OBJ *TableObj,char *iLName){
	int i = 0;
	//ターゲットとなるテーブルオブジェクトの検索
	for(i=0;i < FIELD_MAX_NUM;i++){
		//同じものがあれば同じものを
		if(strcmp(TableObj->fieldObj[i].fieldName,iLName) == 0){
			return i;
		}
		//なければ最初にくるカラのものを
		if(strlen(TableObj->fieldObj[i].fieldName) == 0){
			//空だったものはintのものを-1で初期化
			initFieldObj(&TableObj->fieldObj[i]);
			return i;
		}
	}
	//ターゲットが見つからなかったら-1
	return -1;
}

//別スレッド等から管理用DBの状態をチェック
//author:n.koyama
//date  :20161025
int getDBMReadWriteFlg(){
	if(m_mysqlConn->net.reading_or_writing == 0){
		return 0;
	}else{
		return 1;
	}
}

//別スレッド等からDBの状態をチェック
//author:n.koyama
//date  :20161025
int getDBReadWriteFlg(){
	if(mysqlConn->net.reading_or_writing == 0){
		return 0;
	}else{
		return 1;
	}
}

// Lock用コネクションに接続確認
// これでConnectionがLostしないように
int DB_M_PING(){
	int ret =0;

	// 管理接続がつながってなければ無視
	if(m_mysqlConn->thread_id == 0 ){
		return ret;
	}
	//読み書き中にpingを発行するとエラーになるので
	//こちらのほうがアクセスが少ないので優先順位が高い
	if(m_mysqlConn->net.reading_or_writing == 0){
		ret=mysql_ping(m_mysqlConn);
	}

	return ret;
}

// 通常用コネクションに接続確認
// これでConnectionがLostしないように
int DB_PING(){
	int ret =0;

	// つながってなければ無視
	if(mysqlConn->thread_id == 0){
		return ret;
	}
	//読み書き中にpingを発行するとエラーになるので
	if(mysqlConn->net.reading_or_writing == 0){
		ret=mysql_ping(mysqlConn);
	}
	return ret;
}


/*ロールバック*/
int M_Rollback(){
	int ret =0;
	ret = mysql_query(m_mysqlConn, "ROLLBACK");
	if(!ret){
		ret = M_Transaction();
	}
	return ret;
}

/*コミット*/
int M_Commit(){
	int ret =0;
	char strSql[1024]="";

	sprintf(strSql," /* pg_name %s user_name %s : M_COMMIT %02s %s */ COMMIT; ",source_file_name,source_user_name,DB_table[intCurTableNum].accessStatus,local_server_time(strTime));
	ret = mysql_query(m_mysqlConn, strSql);
	if(!ret){
		ret = M_Transaction();
	}
	return ret;
}
/* MYSQL型(mysql or m_mysqlConn)を引数として共通化してください */

// Lock用コネクション開始
int M_OPEN(){
	int ret =0;

	m_mysqlConn = mysql_init(NULL);

	if (!mysql_real_connect(m_mysqlConn, DB_db_server, DB_db_user, DB_db_password, DB_db_database, DB_db_port, NULL, 0)) {
		cob_runtime_error(" Error [%02d]:db_connect server :%s user:%s pass:%s database:%s ",01, DB_db_server,DB_db_user,DB_db_password,DB_db_database);
		//接続不可
		exit(1);
		ret = 1;
	}else{
		//AutoCommit Off これを行ロックすると同時に幾つかの画面を開こうとする妨げになる可能性あり
//		ret = mysql_query(m_mysqlConn, DB_strAutocommit);
		//接続可 上のretは無視
		ret = 0;
		ret = M_Transaction();
	}

	return ret;
}

// テーブルロック(管理テーブルレコード更新)
int M_LOCK(char *iPName){
	int ret =0;
	int nn =0;
	char strSql[256]="";
	MYSQL_ROW	res;
	MYSQL_RES	*result;
	MYSQL_ROW	inres;
	MYSQL_RES	*inResult;
	int maxcols;			 //フィールドの数とそのループ変数
	char strUid[16]="";
	char strSS[8]="";
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"M_LOCK :%.30s ",iPName);

	//対象テーブルの検索
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);
	//テーブル名がなければ-RDBをつけて再検索
	if(intCurTableNum == -1){
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	if(m_mysqlConn == NULL){
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//pg_nameの確認 or 取得
	if(source_file_name == NULL){
		nn =0;
		while(source_file_name == NULL  && nn < 10){
			source_file_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
			nn++;
		}
		memset(source_file_name, '\0', (PATHNAME_SIZE + 1));
		nn =0;
		while(source_user_name == NULL  && nn < 10){
			source_user_name = (char *)malloc(sizeof(char) * (PATHNAME_SIZE + 1));
			nn++;
		}
		memset(source_user_name, '\0', (PATHNAME_SIZE + 1));
	// 実行コマンド名取得
//		getprocessName(source_file_name, PATHNAME_SIZE);
		getUserAndProcessName(source_file_name, source_user_name);
  }

	// 対象テーブルのロック状態確認(ロック無視)
	//TableNameが現行のPKeyなので一行しか取れない20150217
	strcat(strSql,"SELECT LockMode FROM `");
	strcat(strSql,DB_db_mana_tbl);
	strcat(strSql,"` WHERE TableName = '");
	strcat(strSql,DB_table[intCurTableNum].tableName);
	strcat(strSql,"';");

	if(mysql_query(m_mysqlConn, strSql) != 0){
		m_mysql_failure();
		ret = 1;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	if(result = mysql_use_result(m_mysqlConn)){
		int rr=0;
		char strDecision[5]="";

		res = mysql_fetch_row(result);
		if(res != NULL){
			maxcols = mysql_num_fields(result);
			//LOOPは1回しか無いはず
			for(nn=0; nn<maxcols; nn++){
				//先行の判定用2文字を判定変数にコピー
				memcpy(strDecision,res[0],2);
				memcpy((strDecision + 2),(DB_table[intCurTableNum].sharedStatus + 1) ,2);
				//move_to_cob(va_arg(args, char *), res[ii]);
			}
			rr = nn;
		}
		mysql_free_result(result);

		// 文字列初期化
		memset(strSql,'\0',strlen(strSql));
		sprintf(strUid,"%d",Uid);
		sprintf(strSS,"%s",(DB_table[intCurTableNum].sharedStatus + 1));
		//先行の処理がなければそのまま接続
		//TODO:プログラム名 ユーザ名などを入れる comp
		if(rr == 0){
			// 管理テーブルの登録
			strcat(strSql,"INSERT INTO `");
			strcat(strSql,DB_db_mana_tbl);
			strcat(strSql,"` ( TableName,Uid,LockMode,pg_name,user_name,LockDateTime) ");
			strcat(strSql,"VALUES ( '");
			strcat(strSql,DB_table[intCurTableNum].tableName);
			strcat(strSql,"',");
			strcat(strSql,strUid);    //Uid
			strcat(strSql,",'");
			strcat(strSql,strSS);    //LockMode
			strcat(strSql,"','");
			strcat(strSql,source_file_name);    //pg_name
			strcat(strSql,"','");
			strcat(strSql,source_user_name);    //pg_name
			strcat(strSql,"', now() );");
		}else{
			if( *strDecision == 0x20 || *strDecision == 0x00
				|| (*strDecision != 'S' && *strDecision != 'P' && *strDecision != 'E' )
				|| strcmp(strDecision,"SISI") == 0 || strcmp(strDecision,"SIPI") == 0
				|| strcmp(strDecision,"PIPI") == 0
				|| strcmp(strDecision,"SIS-") == 0 || strcmp(strDecision,"SIP-") == 0
				|| strcmp(strDecision,"SISE") == 0 || strcmp(strDecision,"SIPE") == 0
				|| strcmp(strDecision,"S-SI") == 0 || strcmp(strDecision,"P-SI") == 0
				|| strcmp(strDecision,"S-S-") == 0 || strcmp(strDecision,"P-SE") == 0
				|| strcmp(strDecision,"SESI") == 0 || strcmp(strDecision,"PESI") == 0
				|| strcmp(strDecision,"SES-") == 0 || strcmp(strDecision,"PESE") == 0
			){
				//先行がなし(１文字目がスペース) || (１文字目がNULL)
				//先行がなし(１文字目が指定のどれでもない)
				//先行がInput,Shared;後行がInput,Shared
				//先行がInput,Shared;後行がInput,Protected
				//先行がInput,Protected;後行がInput,Protected
				//先行がInput,Shared;後行がI-O,Shared
				//先行がInput,Shared;後行がI-O,Protected
				//先行がInput,Shared;後行がExtended,Shared
				//先行がInput,Shared;後行がExtended,Protected
				// 管理テーブルの更新
				strcat(strSql,"UPDATE `");
				strcat(strSql,DB_db_mana_tbl);
				strcat(strSql,"` SET Uid = ");
				strcat(strSql,strUid);
				strcat(strSql,", LockMode = '");
				strcat(strSql,strSS);
				strcat(strSql,"', pg_name = '");
				strcat(strSql,source_file_name);
				strcat(strSql,"', user_name = '");
				strcat(strSql,source_user_name);
				strcat(strSql,"', LockDateTime = now() ");
				strcat(strSql," WHERE TableName = '");
				strcat(strSql,DB_table[intCurTableNum].tableName);
				strcat(strSql,"' ");
				strcat(strSql,";");
			}else if(strcmp(strDecision,"PISI") == 0 ){
				//権限が先行のほうが強い場合何もせず
				//先行がInput,Protected;後行がInput,Shared
//				continue;
			}else if( strncmp((strDecision + 2),"E",strlen("E")) == 0
			|| strncmp(strDecision,"E",strlen("E")) == 0
			|| strncmp(strDecision,"PI",2) == 0 || strncmp(strDecision,"P-",2) == 0
			|| strncmp((strDecision + 1),"O",strlen("O")) == 0
			|| (*(strDecision + 3)=='O' && (*(strDecision + 1) == 'O' || *(strDecision + 1) == 'E' || *(strDecision + 1) == '-'|| *(strDecision + 1) == 'I'))
			|| strncmp(strDecision,"S-P",strlen("S-P")) == 0 || strncmp(strDecision,"SEP",strlen("SEP")) == 0){
				//後行がExclusive
				//先行がExclusive
				//先行がInput,Protected;!(後行がInput,Protected or 後行がInput,Shared)
				//先行がI-O,Protected;!(後行がInput,Shared)
				//先行がOutput
				//後行がOutput(DBの値が壊れてたときように開かれていることが明示的なときの表現追加20150807)
				// ここはエラーでは無いので,出力なし
				// cob_runtime_error(" Error C [%02d]:locked %s status %s:%s: ",81,iPName,*res,DB_table[intCurTableNum].sharedStatus);
				ret = 1;
//				break;
			}else{
				// 管理テーブルの更新
				strcat(strSql,"UPDATE `");
				strcat(strSql,DB_db_mana_tbl);
				strcat(strSql,"` SET Uid = ");
				strcat(strSql,strUid);
				strcat(strSql,", LockMode = '");
				strcat(strSql,strSS);
				strcat(strSql,"', pg_name = '");
				strcat(strSql,source_file_name);
				strcat(strSql,"', user_name = '");
				strcat(strSql,source_user_name);
				strcat(strSql,"', LockDateTime = now() ");
				strcat(strSql," WHERE TableName = '");
				strcat(strSql,DB_table[intCurTableNum].tableName);
				strcat(strSql,"' ");
				strcat(strSql,";");
			}
		}
		if(ret != 1){
			// SQLの実行⇒COMMIT⇒トランザクション再作成
			if(mysql_query(m_mysqlConn, strSql) != 0){
				m_mysql_failure();
				ret = 1;
				cob_runtime_error(" Error C [%02d]:can't lock %s ",80,DB_table[intCurTableNum].tableName);
				//共有変数を元に戻す
				unsetCommonFunctionName(map_source_func,strStack);
				return ret;
			}else if(M_Commit() != 0){
				m_mysql_failure();
				cob_runtime_error(" Error C [%02d]:can't lock %s ",80,DB_table[intCurTableNum].tableName);
				ret = 1;
				//共有変数を元に戻す
				unsetCommonFunctionName(map_source_func,strStack);
				return ret;
			}else{
				ret = M_Transaction();
			}
			//SELECT で結果を取得しないとエラーになる
			result = mysql_use_result(m_mysqlConn);
			mysql_free_result(result);
			// //AutoCommit Offなので
			// M_Commit();
		}
	}else{
		m_mysql_failure();
		cob_runtime_error(" Error C [%02d]:%s ", mysql_error(m_mysqlConn));
		ret = 1;
	}

	if( DB_table[intCurTableNum].sharedStatus[2] == 'I'
	&& ( DB_table[intCurTableNum].sharedStatus[1] == 'P' ||  DB_table[intCurTableNum].sharedStatus[1] == 'S')){
		//LOCKがかからない場合は行ロックを外しておく
		//排他モード(入力モードのPROTECTED or SHARED)
		M_UNLOCK(DB_table[intCurTableNum].tablePName,__func__,__LINE__);
	}

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

// テーブルアンロック
int M_UNLOCK(char *iPName,const char *funcName,int funcLine){
	int ret =0,ii = 0;
	int storedTargetNum = 0;
	char strSql[256]="";
	char UidStr[16]="";
	//----------------------------------------------------------------諦めるための変数
	MYSQL_ROW res;
	MYSQL_RES *result;
	int maxcols;			 //フィールドの数とそのループ変数

	//対象テーブルの検索
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);
	//テーブル名がなければ-RDBをつけて再検索
	if(intCurTableNum == -1){
		return 1;
	}

	//既にUNLOCKされていれば再実行しない
	sprintf(UidStr,"%d",Uid);
	sprintf(strSql,"/* pg_name %s user_name %s func_name %s : M_UNLOCK %s */ SELECT count(Uid) FROM `",source_file_name,source_user_name,funcName,local_server_time(strTime));
	strcat(strSql,DB_db_mana_tbl);
	strcat(strSql,"`");
	strcat(strSql," WHERE TableName = '");
	strcat(strSql,DB_table[intCurTableNum].tableName);
	strcat(strSql,"' AND Uid = ");
	strcat(strSql,UidStr);
	strcat(strSql,";");

	// 対象が存在するか確認(UNLOCKしていればUidが消える)
	if(mysql_query(m_mysqlConn, strSql) != 0){
		m_mysql_failure();
		ret = 1;
		return ret;
	}
	if((result = mysql_use_result(m_mysqlConn)) == NULL){
		m_mysql_failure();
		ret = 1;
		return ret;
	}
	if((res = mysql_fetch_row(result)) != NULL){
		maxcols = mysql_num_fields(result);
		for(ii=0; ii<maxcols; ii++){
			//データの取得
			storedTargetNum = atoi(res[ii]);
			//move_to_cob(va_arg(args, char *), res[ii]);
		}
	}
	mysql_free_result(result);
	//既に対象がなくなっていれば以降の処理なし
	if(storedTargetNum == 0){
		return 0;
	}
//	// 管理テーブルの更新
//	strcat(strSql,"UNLOCK TABLES; ");
	// mysql_queryのタイミングを同時にしとけば後ろのUPDATEに割り込まれる可能性は低いはず

//	//割り込まれない実行計画を立てるべき
//	if(mysql_query(m_mysqlConn, sql) != 0){
//		m_mysql_failure();
//		ret = 1;
//	}
	memset(strSql,0x00,strlen(strSql));

	//M_LOCKからの呼び出しの場合はUidと状態はそのまま
	if(strcmp(funcName,"M_LOCK") != 0){
		sprintf(UidStr,"%d",Uid);
		sprintf(strSql,"/* pg_name %s user_name %s func_name %s : M_UNLOCK %s */ UPDATE `",source_file_name,source_user_name,funcName,local_server_time(strTime));
		strcat(strSql,DB_db_mana_tbl);
		strcat(strSql,"` SET Uid = NULL, LockMode = '");
		strcat(strSql, "    ");								//LOCKMODE DEFAULT VALUE
		strcat(strSql,"' , LockDateTime = now() ");
		strcat(strSql," WHERE TableName = '");
		strcat(strSql,DB_table[intCurTableNum].tableName);
		strcat(strSql,"' AND Uid = ");
		strcat(strSql,UidStr);
		strcat(strSql,";");

		// SQLの実行＆トランザクション再作成
		if(mysql_query(m_mysqlConn, strSql) != 0){
			m_mysql_failure();
			ret = 1;
			return ret;
		}else if(M_Commit() != 0){
			m_mysql_failure();
			ret = 1;
			return ret;
		}else{
			DB_table[intCurTableNum].sharedStatus[0]='C';
			ret = M_Transaction();
		}
	}

	// 同一Uidの再ロック
	char s_tablelist[1024]="";	// SHARED
	char u_tablelist[1024]="";	// UPDATE
	// 開いたファイルの状態を取得
	for(ii = 0; ii < DB_Table_num ;ii++ ){
		if(DB_table[ii].sharedStatus[0] == 'O'){
			if(strlen(s_tablelist) > 0){
				strcat(s_tablelist,",");
			}
			if(strlen(u_tablelist) > 0){
				strcat(u_tablelist,",");
			}

			if( DB_table[intCurTableNum].sharedStatus[2] == 'I'
			&& ( DB_table[intCurTableNum].sharedStatus[1] == 'P' ||  DB_table[intCurTableNum].sharedStatus[1] == 'S')){
				//排他モード(入力モードのPROTECTED or SHARED)
				strcat(s_tablelist,"'");
				strcat(s_tablelist,DB_table[ii].tableName);
				strcat(s_tablelist,"'");
			}else{
				strcat(u_tablelist,"'");
				strcat(u_tablelist,DB_table[ii].tableName);
				strcat(u_tablelist,"'");
			}
		}
	}

	return ret;
}

// Lock用コネクション終了
int __M_CLOSE(const char *funcName,int funcLine){
	int ret =0;
	char strSql[256]="";
	char UidStr[16]="";

	sprintf(UidStr,"%d",Uid);
	sprintf(strSql,"/* pg_name %s : M_CLOSE %s */ UPDATE `",source_file_name,local_server_time(strTime));
	strcat(strSql,DB_db_mana_tbl);
	strcat(strSql,"` SET Uid = NULL, LockMode = '  ' , LockDateTime = now() ");
	strcat(strSql," WHERE Uid = ");
	strcat(strSql,UidStr);
	strcat(strSql,";");

	if(m_mysqlConn != NULL){
		//Lockを全て解除
		ret = mysql_query(m_mysqlConn, strSql);
		//DB接続の切断
		mysql_close(m_mysqlConn);
	}
	return ret;
}

// ユニークID生成
int M_UID(){
	unsigned int now = (unsigned int)time( 0 );
	srand( now );
	// srand関数で、乱数パターンを初期化する
	// プログラム実行ごとに異なるパターンが使える

	return rand();
}
/* ----  管理テーブル用関数 追加終了  --------  */


/* 出口処理 */
void err_exit(int rc)
{
/*   正常終了 rc = 0  ではそのまま復帰する									   */
/*   異常終了 rc != 0 では初期設定(MySQL_init)で設定されたエラー出口にしたがって */
/*   処理を分ける:															   */
/*	 errout = 1:  エラーメッセージを stderr に出力して終了する				 */
/*	 errout = 2:  エラーは呼び出し元で処理されるものとしてそのまま復帰する　　 */
/*	 errout = 3:  エラーはユーザ作成のプログラムで処理されるものとして、	   */
/*　　　　　　　　　errno, errmsgをパラメタとして指定されたプログラムを呼ぶ。	*/
	char errno[10];

	if( !rc ) return;
	switch(errout){
		case 1:
			 cob_runtime_error(" Error C [%02d]:%s ", 88,mysql_error(mysqlConn));
			 exit(1);
		case 2:
			 break;
		case 3:
			 sprintf(errno,"%d", mysql_errno(mysqlConn));
			 func(errno, mysql_error(mysqlConn));
	}
	return;
}
//   strncatの代わりにmainの後ろにsubstrをつなぐ
//   strcatだと\0に対応できないので長さ分をmemcpy
//   mainstr   :つなげる元の文字列ポインタ
//   mainLength:元の文字列の最大長
//   substr	:つなげる文字列のポインタ
char *mystrncat(char *mainstr ,int mainLength ,const char *substr,int subLength){
	memcpy((mainstr + (mainLength)),substr,subLength);
	return mainstr;
}

//////-----------------------------------------------------------------------------------
//////------------------------------------------------------------M_* テーブル管理関連End
//////-----------------------------------------------------------------------------------
//mysqlエラー処理
int __mysql_failure(const char *funcName,int funcLine){
	int ret =0;
	//エラー内容を出力
	mytool_runtime_error(source_file_name," Error C [%02d]: Query Error %20s %.40s %.20s:%04d %s ",99,mysql_error(mysqlConn),map_source_func,funcName,funcLine,local_server_time(strTime));

	// //ロールバック
	// ret = mysql_query(mysqlConn, "ROLLBACK");

	//DB接続の切断
	//途中で接続を切ったらダメかも
//	mysql_close(mysqlConn);

	return ret;
}

/*
 スペースを探す
返り値:スペースが出たところの文字位置[aa a]なら3
*/
int searchSpace(char *haystack){
	int i = 0;
	int res = 0;
	for(i=0;haystack[i] != ' ';i++){
			res++;
	}
	return res;
}

/* 文字列をcobolに転写する関数 */
void move_to_cob(char *cob_dat, const char *dat)
{
	int len = strlen(cob_dat);					  // data length in cob
	memset(cob_dat, ' ', min(len, strlen(dat)));					  // clear with spaces
	memcpy(cob_dat, dat, min(len, strlen(dat)));	// data copy
	return;
}

/* 文字列をcobolに転写する関数(binary) */
void move_to_cob_for_bin(char *cob_dat, const char *dat,long len){
	memset(cob_dat, ' ', len);					  // clear with spaces
	memcpy(cob_dat, dat, len);	// data copy
	return;
}

/* 領域開放 */
void DB_Free_Result(MYSQL_RES **result){
	mysql_free_result(*result);
	return;
}

//値が入れられていない時に_(ワイルドカード)に置き換える
char *changeWildcard(char *mainstr ,int mainLength ,char *substr,int subLength){
	char *ptnCurrentStr;
	char *ptnEndStr;
	char *ptnCpyStr;
	char strFuncTemp[512]="";
	short int otherExistFlg=0;

	// 20170314 全ての文字列がワイルドカードだと問題がある可能性が -> 全てかどうかを判定
	ptnCpyStr	 = substr;
	// ptnCurrentStr = (mainstr + (mainLength));
	// ptnEndStr	 = (mainstr + (mainLength + subLength));
	ptnCurrentStr = strFuncTemp;
	ptnEndStr	 = (strFuncTemp + subLength);
//	if(subLength < 512){
//		memset(strFuncTemp,'0',subLength);
//		if(strncmp(strFuncTemp,substr,subLength) == 0){
//			//全て0なら全てをスペースに置き換えて下でワイルドカードに
//			memset(strFuncTemp,0x20,subLength);
//			ptnCpyStr = strFuncTemp;
//		}
//	}
	for(;ptnCurrentStr <= ptnEndStr;ptnCurrentStr++){
		if(*ptnCpyStr == 0x20 || *ptnCpyStr == 0x00 ){
			*ptnCurrentStr = 0x5f;        //[_]
		}else{
			*ptnCurrentStr = *ptnCpyStr;
			otherExistFlg = 1;
		}
		ptnCpyStr++;
	}
	if(otherExistFlg != 0){
		memcpy((mainstr + (mainLength)),strFuncTemp,subLength);
	}else{
		memcpy((mainstr + (mainLength)),substr,subLength);
	}

	return mainstr;
//	memcpy((mainstr + (mainLength)),substr,subLength);
}


//datacopy
//return itemlengthの変わったもの
int dataEscapeCopy(char *origText,char *copyText,int itemLength){
	char *origTextEnd;
	int addlen = 0;

	origTextEnd = origText + itemLength;
	//
	for(;origText < origTextEnd;copyText++){
		//シングルクォート(':0x27)ダブルクォート(":0x22)円記号(\:0x5c) をエスケープ
		if(*origText == 0x27 || *origText == 0x5c || *origText == 0x22){
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
	return addlen;
}

/* スプリット関数 */
int Split( char *str[], const char *delim, char *outlist[] ) {
	char	*tk;
	int	 cnt = 0;

	tk = strtok( *str, delim );
	while( tk != NULL && cnt < 1000 ) {
		outlist[cnt++] = tk;
		tk = strtok( NULL, delim );
	}
	return cnt;
}

/* 文末スペースを削除する関数 */
int Trim(char *iostr) {
	int ii = 0;

	/* 空ポインタか? */
	if ( iostr == NULL ) { /* yes */
		return 1;
	}

	/* 末尾から順に空白でない位置を探す */
	for( ii = strlen(iostr)-1; ii >= 0 && isspace( iostr[ii] ); ii-- ) ;

	/* 終端ナル文字を付加する */
	iostr[ii+1] = '\0';

	return 0;
}

//---------------------------------------------------------------------------
//-----------------------  DB_TABLE_OBJ 関連関数 []
//DBのTableNameを格納
//テーブル名の-RDBを除去
//入力:iPName:assign後のPNameを渡す
void setTableName(DB_TABLE_OBJ *targObj,char *iPName){
	char *tmp;
	tmp = iPName;
	strcpy(targObj->tableName,iPName);

	do{
		if(strncmp(tmp,"-RDB",strlen("-RDB")) == 0){
			strncpy(targObj->tableName,iPName,(tmp - iPName) );
			memset(targObj->tableName + (tmp - iPName),'\0',1); //終端記号を
		}
		tmp = (char *)strchr((tmp + 1),'-');
	}while(tmp != NULL);
}


int matchTableStatus(DB_TABLE_OBJ *targTable,char *compStr){
	int matchCorrect = 1;
	int compStrLen = strlen(compStr);

	//一致していなければ
	if(*(targTable->accessStatus + 0) != *(compStr + 0)){
		matchCorrect = 0;
	}
	if(compStrLen == 1){
		return matchCorrect;
	}
	//一致していなければ
	if(*(targTable->accessStatus + 1) != *(compStr + 1)){
		matchCorrect = 0;
	}
	if(compStrLen == 2){
		return matchCorrect;
	}
	//一致していなければ
	if(*(targTable->accessStatus + 2) != *(compStr + 2)){
		matchCorrect = 0;
	}
	return matchCorrect;
}

char getTableStatusWithIndex(DB_TABLE_OBJ *targTable,int index){
	//ステータスの取得
	return *(targTable->accessStatus + index);
}

void setTableStatusWithIndex(DB_TABLE_OBJ *targTable,char status,int index){
	//ステータスの格納
	*(targTable->accessStatus + index) = status;
}

void setTableStatus(DB_TABLE_OBJ *targTable,char funcIni,int status){
	//ステータスの格納
	*(targTable->accessStatus + 0) = funcIni;
	if(status == 1){
		//失敗のとき
		*(targTable->accessStatus + 1) = '1';
	}else{
		//成功のとき
		*(targTable->accessStatus + 1) = '0';
	}
}
//-----------------------------------------------------------------------------
//cobolから関数が呼ばれた時のvarNum番目の変数のサイズを取得
//引数:引数のvarNum番目
int varLength(int varNum){
	int ret =0;

	//n番目 => 添え字(n-1)
	ret = cob_current_module->cob_procedure_parameters[(varNum - 1)]->size;

	return ret;
}

//cobolから関数が呼ばれた時のvarNum番目の変数がpackedか判定
//引数:引数のvarNum番目
//返り値:0=packed以外,1=packed
int isPackedOnCobField(int varNum){
	int ret =0;

	//n番目 => 添え字(n-1)の属性のtypeがPACKED NUMERICなら1
	if(cob_current_module->cob_procedure_parameters[(varNum - 1)]->attr->type == COB_TYPE_NUMERIC_PACKED){
		ret = 1;
	}

	return ret;
}

//
//対象フィールド名のフィールド番号を返す
//in:
//  mysqlDBStruct  : DB接続オブジェクト
//  targetFieldName: 何番目か知りたいフィールド名
//out:
//  returnfieldNum
//    対象フィールド名のフィールド番号(0番目がhitした時も見つからなかったときも0を返す)
//author:
//  koyama 20161020
int searchFieldNumber(MYSQL *mysqlDBStruct,MYSQL_RES *queryResult,char *targetFieldName){
	MYSQL_FIELD  *fields;
	int returnFieldNum = 0,field_max_num=0;
	char targetFieldNameLower[1024]="";
	char tempStr[1024]="";
	//複数行取れても1行目を正とする
	field_max_num = mysql_field_count(mysqlDBStruct);
	fields      = mysql_fetch_field(queryResult);
	returnFieldNum = field_max_num;

	//フィールド名は小文字で比較する
	StrToLowerCpy(targetFieldNameLower,targetFieldName);
	do{
		returnFieldNum--;
		//フィールド名は小文字で比較する
		StrToLowerCpy(tempStr,(fields + returnFieldNum)->name);
	}while((strcmp(tempStr,targetFieldName) != 0) &&  (returnFieldNum > 0));

	return returnFieldNum;

}

//
//指定のテーブル名がデータベース上に存在するか調べる(ViewかTableかのステータスを返す)
//in:
//  mysqlDBStruct: DB接続オブジェクト
//  strTableName : テーブル名
//out:
//  chRetTableStatus
//  v:Viewが存在する,t:テーブルが存在する,0x00:存在しない
//author:
//  koyama 20161020
char confirmTableExistsByName(MYSQL *mysqlDBStruct,char *strTableName){
	char strCheckTableExistsQuery[2048]="";
	char chRetTableStatus=0x00;
	MYSQL_ROW    res;
	MYSQL_RES    *result;


	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"confirmTableExistsByName :%.30s  ",strTableName);

	strcat(strCheckTableExistsQuery," SHOW TABLE STATUS ");
	strcat(strCheckTableExistsQuery," WHERE ");
	strcat(strCheckTableExistsQuery," name like '");
	strcat(strCheckTableExistsQuery,strTableName);
	strcat(strCheckTableExistsQuery,"'");
	if(mysql_query(mysqlDBStruct, strCheckTableExistsQuery) != 0){
		mysql_failure();
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return chRetTableStatus;
	}

	if(result = mysql_store_result(mysqlDBStruct)){
		int target_field_num=0;
		//fieldnameがcommentのfield番号を探す
		target_field_num = searchFieldNumber(mysqlDBStruct,result,"comment");
		res=mysql_fetch_row(result);
		if(res != 0){
			char ctrComparison[1024]="";
			if(strstr(StrToLowerCpy(ctrComparison,res[target_field_num]),"view")){
				chRetTableStatus = 'v';
			}else{
				chRetTableStatus = 't';
			}
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return chRetTableStatus;

}

//
//対象フィールドの長さを返す
//in:
//  tableName : テーブルの論理名
//  fieldName : フィールド名
//out:
//
int getFieldLength(char *tableName,char *fieldName){
	int ret =0;
	int intFieldSub=0;
	char strSql[1024] = "";
	char tName[256] = "";
	char fName[256] = "";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"getFieldLength :%.30s , %.30s ",tableName,fieldName);

	strcpy(tName,tableName);
	strcpy(fName,fieldName);

	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	//保存しておいたものを探す仕組みを 20160824 add koyama
	//対象テーブルの検索
	intCurTableNum = getTargTableLogic(DB_table,tName,DB_Table_num);
	intFieldSub = getNextFieldObj(&DB_table[intCurTableNum],fName);
	if(intFieldSub < 0){
		mytool_runtime_error(source_file_name," Error [%02d]:can not set Field Name %s ",99,local_server_time(strTime));
		ret = 0;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}
	//Fieldの長さが0はありえないので,初期化は-1なので
	if(intFieldSub >= 0 && DB_table[intCurTableNum].fieldObj[intFieldSub].length  >= 0){
		ret = DB_table[intCurTableNum].fieldObj[intFieldSub].length;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	strcat(strSql,"SELECT max(Size) FROM ");
	strcat(strSql,DB_db_item_tbl);
	strcat(strSql," WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND Label = \"");
	strcat(strSql,fName);
	strcat(strSql,"\"; ");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		ret = 0;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		if(res != 0){
			ret = atoi(res[0]);
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}
	//一度とったものを保存しておく仕組みを 20160824 add koyama
	strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].fieldName,fName);
	DB_table[intCurTableNum].fieldObj[intFieldSub].length = ret;
	//check:koyama
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

//第3引数にMID(*,*)を与え必要に応じてchCharMapをつなぐ
//in :
//  tName	 : 対象のテーブル名
//  fName	 : 対象のフィールド名
//  retStr	: MID(?,?)を入れるポインタ20以上の長さがないとエラーになるかも
char *setFieldSpecChrConvNecessary(char *tName,char *fName,char *retStr){
	int intCurTableNum=0,intFieldSub=0;
	char strSql[500] = "",strTemp[512]="",strStart[20]="",strLength[20]="";;
	long int res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setFieldSpecChrConvNecessary :%.30s , %.30s ",tName,fName);

//	//strTempにいったんすべてを移す
//	strcpy(strTemp,retStr);
	*retStr = '\0';
	getFieldSpecified(tName,fName,strTemp);
	if(strlen(strTemp) == 0){
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status ",99,map_source_func);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		//何もせずに返す
		return retStr;
	}

	//MID(ITEM,*,*)の開きカッコをから,までを取得
	strncat(strStart,(strchr(strTemp,',') + 1),(int)(strrchr(strTemp,',') - (strchr(strTemp,',') + 1)));
	//MID(ITEM,*,*)の,から閉じカッコまでを取得
	strncat(strLength,(strrchr(strTemp,',') + 1),(int)(strchr(strTemp,')') - (strrchr(strTemp,',') + 1)));

	intCurTableNum = getTargTableLogic(DB_table,tName,DB_Table_num);
	intFieldSub =getNextFieldObj(&DB_table[intCurTableNum],fName);
	if(intFieldSub < 0){
		mytool_runtime_error(source_file_name," Error [%02d]:can not set Field Name %s ",99,local_server_time(strTime));
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		//TODO:ERRORになった場合そのまま返しても問題ないか？
		return retStr;
	}
	//初めて通る場合は名前,長さを入れておく
	if(strlen(DB_table[intCurTableNum].fieldObj[intFieldSub].fieldName) == 0){
		strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].fieldName,fName);
		DB_table[intCurTableNum].fieldObj[intFieldSub].length = atoi(strLength);
		DB_table[intCurTableNum].fieldObj[intFieldSub].isComp =-1;
	}else{
		//初期化は-1なので、初期値でなければここを通っている
		if(DB_table[intCurTableNum].fieldObj[intFieldSub].isComp >= 0){
			//0の時charMapになっている
			if(DB_table[intCurTableNum].fieldObj[intFieldSub].isComp > 0){
				//compなら
				strcat(retStr,strTemp);
			}else{
				//compでないなら
				strcat(retStr,DB_table[intCurTableNum].fieldObj[intFieldSub].chCharMapString);
			}
			//共有変数を元に戻す
			unsetCommonFunctionName(map_source_func,strStack);
			return retStr;
		}
	}

	//COMPがあるかどうかの検査
	strcat(strSql,"SELECT S_Point,size FROM ");
	strcat(strSql,DB_db_item_tbl);
	strcat(strSql," WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND DataType Like '%C%' ");
	//同じ指定で開始位置が対象フィールドに含まれるかどうか
	strcat(strSql," AND S_Point BETWEEN (");
	strcpy(strSql + strlen(strSql),strStart);
	strcat(strSql,") AND ((");
	strcpy(strSql + strlen(strSql),strStart);
	strcat(strSql," + ");
	strcpy(strSql + strlen(strSql),strLength);
	strcat(strSql,") - 1)");
	//かつ、終了位置が含まれるかどうか
	strcat(strSql," AND (S_Point + Size) BETWEEN (");
	strcpy(strSql + strlen(strSql),strStart);
	strcat(strSql,") AND ((");
	strcpy(strSql + strlen(strSql),strStart);
	strcat(strSql," + ");
	strcpy(strSql + strlen(strSql),strLength);
	strcat(strSql,"))");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return retStr;
	}
	//行を持っていれば
	if(result = mysql_store_result(mysqlConn)){
		res=mysql_num_rows(result);
		DB_table[intCurTableNum].fieldObj[intFieldSub].isComp = res;
		if(res == 0){
			getFieldSpecifiedElement(tName,fName,strTemp);
			strcpy(retStr,"chCharMap(");
			strcat(retStr,strTemp);
			strcat(retStr,")");
			strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].chCharMapString,retStr);
		}else{
			//対象にCOMPがふくまれるなら
			strcat(retStr,strTemp);
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return retStr;
}

//第3引数に ITEM,?,?を返す
//in :
//  tName	 : 対象のテーブル名
//  fName	 : 対象のフィールド名
//  retStr	: ITEM,?,?を入れるポインタ20以上の長さがないとエラーになるかも
char* getFieldSpecifiedElement(char *tName,char *fName,char *retStr){
	int intFieldSub=0;
	char strSql[500] = "";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"getFieldSpecifiedElement :%.30s , %.30s ",tName,fName);

	//保存しておいたものを探す仕組みを 20160824 add koyama
	//対象テーブルの検索
	intCurTableNum = getTargTableLogic(DB_table,tName,DB_Table_num);
	intFieldSub = getNextFieldObj(&DB_table[intCurTableNum],fName);
	if(intFieldSub < 0){
		mytool_runtime_error(source_file_name," Error [%02d]:can not set Field Name %s ",99,local_server_time(strTime));
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return retStr;
	}
	if(intFieldSub >= 0 && strlen(DB_table[intCurTableNum].fieldObj[intFieldSub].itemString ) != 0){
		strcpy(retStr,DB_table[intCurTableNum].fieldObj[intFieldSub].itemString);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return retStr;
	}

	strcat(strSql,"SELECT S_Point,size FROM ");
	strcat(strSql,DB_db_item_tbl);
	strcat(strSql," WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND Label = \"");
	strcat(strSql,fName);
	strcat(strSql,"\" ");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return retStr;
	}

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		if(res != 0){
			strcpy(retStr,"ITEM,");
			strcpy((retStr + strlen(retStr)),res[0]);
			strcpy((retStr + strlen(retStr)),",");
			strcpy((retStr + strlen(retStr)),res[1]);
			strcpy((retStr + strlen(retStr)),"");
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	if(strlen(retStr) == 0){
		//ステータスが取れていないければその旨のエラーを出しておく
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status ",99,map_source_func);
	}
	//一度とったものを保存しておく仕組みを 20160824 add koyama
	strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].fieldName,fName);
	strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].itemString,retStr);

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return retStr;
}

//第3引数にMID(?,?)を返す
//in :
//  tName	 : 対象のテーブル名
//  fName	 : 対象のフィールド名
//  retStr	: MID(?,?)を入れるポインタ20以上の長さがないとエラーになるかも
char* getFieldSpecified(char *tName,char *fName,char *retStr){
	int intFieldSub=0;
	char strSql[500] = "";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"getFieldSpecified :%.30s , %.30s ",tName,fName);

	//保存しておいたものを探す仕組みを 20160824 add koyama
	//対象テーブルの検索
	intCurTableNum = getTargTableLogic(DB_table,tName,DB_Table_num);
	intFieldSub = getNextFieldObj(&DB_table[intCurTableNum],fName);
	if(intFieldSub < 0){
		mytool_runtime_error(source_file_name," Error [%02d]:can not set Field Name %s ",99,local_server_time(strTime));
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return retStr;
	}
	if(intFieldSub >= 0 && strlen(DB_table[intCurTableNum].fieldObj[intFieldSub].midString ) != 0){
		strcpy(retStr,DB_table[intCurTableNum].fieldObj[intFieldSub].midString);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return retStr;
	}

	strcat(strSql,"SELECT S_Point,size FROM ");
	strcat(strSql,DB_db_item_tbl);
	strcat(strSql," WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND Label = \"");
	strcat(strSql,fName);
	strcat(strSql,"\" ");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return retStr;
	}

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		if(res != 0){
			strcpy(retStr,"MID(ITEM,");
			strcpy((retStr + strlen(retStr)),res[0]);
			strcpy((retStr + strlen(retStr)),",");
			strcpy((retStr + strlen(retStr)),res[1]);
			strcpy((retStr + strlen(retStr)),")");
			//一度とったものを保存しておく仕組みを 20160824 add koyama
			DB_table[intCurTableNum].fieldObj[intFieldSub].length=atoi(res[1]);
			strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].fieldName,fName);
			strcpy(DB_table[intCurTableNum].fieldObj[intFieldSub].midString,retStr);
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	if(strlen(retStr) == 0){
		//ステータスが取れていないければその旨のエラーを出しておく
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status ",99,map_source_func);
	}

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return retStr;
}

//ファイルのオープンモードを文字列で取得
//last update 20171016 koyama
char *getFileStatus(DB_TABLE_OBJ *targetTable){
	return targetTable->fileStatus;
}

//ファイルのオープンモードのファイル編成を文字で取得
//last update 20171016 koyama
char getFileStatusOrgnization(DB_TABLE_OBJ *targetTable){
	return *(targetTable->fileStatus + 0);
}

//ファイルのオープンモードのアクセスモードを文字で取得
//last update 20171016 koyama
char getFileStatusAccess(DB_TABLE_OBJ *targetTable){
	return *(targetTable->fileStatus + 1);
}

//ファイルのオープンモードをセット
//ファイルのオープンモードをセットなので、
//last update 20171114 koyama
void setFileStatus(char *charTarget,DB_TABLE_OBJ *targetTable){
	char *charSetting;
	//対象は後ろから見て最初に見つけたスペースから後ろ
	//無いときはそのもの
	if((charSetting = strrchr(charTarget,' ')) !=NULL){
		charSetting = charSetting + 1;
	}else{
		charSetting = charTarget;
	}
	// +0 ORGANIZATION, +1 ACCESS MODE
	if( strncmp(charSetting,"INDEXED",strlen("INDEXED"))==0){
		*(targetTable->fileStatus + 0) = 'I';
		//空ならRandomを入れておく
		if(*(targetTable->fileStatus + 1) == '\0'){
			*(targetTable->fileStatus + 1) = 'R';
		}
	}else if( strncmp(charSetting,"RELATIVE",strlen("RELATIVE"))==0){
		*(targetTable->fileStatus + 0) = 'R';
		//空ならRandomを入れておく
		if(*(targetTable->fileStatus + 1) == '\0'){
			*(targetTable->fileStatus + 1) = 'R';
		}
	}else if( strncmp(charSetting,"SEQUENTIAL",strlen("SEQUENTIAL"))==0){
		//ORGANIZATION SEQUENTIALは省略するのでACCESS MODE
		if(*(targetTable->fileStatus + 0) == '\0'){
			//SEQUENTIALとだけ書いて有る場合はINDEXED扱い
			//SEQUENTIALのSEQUENTIALもINDEXEDの動きで読めるはず
			*(targetTable->fileStatus + 0) = 'I';
		}
		*(targetTable->fileStatus + 1) = 'S';
	}else if( strncmp(charSetting,"RANDOM",strlen("RANDOM"))==0){
		*(targetTable->fileStatus + 1) = 'R';
		//空ならSEQUENTIALを入れておく
		if(*(targetTable->fileStatus + 0) == '\0'){
			*(targetTable->fileStatus + 0) = 'S';
		}
	}else if( strncmp(charSetting,"DYNAMIC",strlen("DYNAMIC"))==0){
		*(targetTable->fileStatus + 1) = 'D';
		//空ならSEQUENTIALを入れておく
		if(*(targetTable->fileStatus + 0) == '\0'){
			*(targetTable->fileStatus + 0) = 'S';
		}
	}else if( strncmp(charSetting,"INPUT",strlen("INPUT"))==0){
		//OPEN-MODE
		*(targetTable->sharedStatus + 2) = charSetting[0];
		*(targetTable->fileStatus   + 2) = charSetting[0];
	}else if( strncmp(charSetting,"OUTPUT",strlen("OUTPUT"))==0){
		//OPEN-MODE
		*(targetTable->sharedStatus + 2) = charSetting[0];
		*(targetTable->fileStatus   + 2) = charSetting[0];
	}else if( strncmp(charSetting,"EXTEND",strlen("EXTEND"))==0){
		//OPEN-MODE
		*(targetTable->sharedStatus + 2) = charSetting[0];
		*(targetTable->fileStatus   + 2) = charSetting[0];
	}else if( strncmp(charSetting,"I-O",strlen("I-O"))==0){
		//OPEN-MODE(I-Oは-で初期化)
		*(targetTable->sharedStatus + 2) = charSetting[1];
		*(targetTable->fileStatus   + 2) = charSetting[1];
	}
	//取得したら切る
	if(charSetting != charTarget){
		charSetting = (charSetting - 1);
		*(charSetting) = '\0';
	}

	return ;
}

//ORDER 句を作成
//author:n.koyama
//date  :20150812
int setOrderSql(DB_TABLE_OBJ *targetTable){
	int ret=0,orderLen=0,intKeycount=0;  //intKeyCount:キーを追加した数
	char strKeyMid[64] = "";
	char *tempOrder;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setOrderSql :%.30s",targetTable->tableLName);

	if((targetTable->strWhere->flg % DB_WHERE_INX) != 0){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 0;
	}

	//ORDER BYを掛けるよう
	tempOrder = (char *)malloc(sizeof(*strKeyMid) * (1024 + 1) );
	memset(tempOrder,'\0',(1024 + 1));

	mystrncat( tempOrder, 0," ORDER BY BINARY ", strlen(" ORDER BY BINARY "));
	orderLen += strlen(" ORDER BY BINARY ");
	//key1を使うか
	if(targetTable->key1Len > 0 && (targetTable->key2Use == 0 && targetTable->key3Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(setFieldSpecChrConvNecessary(targetTable->tableLName,targetTable->key1Name,strTemp)) == 0){
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key1Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
		mystrncat( tempOrder, orderLen," CAST(",strlen(" CAST("));
		orderLen += strlen(" CAST(");
		//MID(ITEM,*,*)かchCharMap(ITEM,*,*)つなぐ
		mystrncat( tempOrder, orderLen, strTemp,strlen(strTemp));
		orderLen += strlen(strTemp);

		mystrncat( tempOrder, orderLen," as binary) ",strlen(" as binary) "));
		orderLen += strlen(" as binary) ");
		intKeycount++;

	}
	//key2を使うか
	if(targetTable->key2Len > 0 && (targetTable->key1Use == 0 && targetTable->key3Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(setFieldSpecChrConvNecessary(targetTable->tableLName,targetTable->key2Name,strTemp)) == 0){
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key2Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}

		if(intKeycount > 0){
			mystrncat( tempOrder, orderLen," ,",strlen(" ,"));
			orderLen += strlen(" ,");
		}
		mystrncat( tempOrder, orderLen," CAST(",strlen(" CAST("));
		orderLen += strlen(" CAST(");
		//MID(ITEM,*,*)かchCharMap(ITEM,*,*)つなぐ
		mystrncat( tempOrder, orderLen, strTemp,strlen(strTemp));
		orderLen += strlen(strTemp);

		mystrncat( tempOrder, orderLen," as binary) ",strlen(" as binary) "));
		orderLen += strlen(" as binary) ");
		intKeycount++;
	}
	//key3を使うか
	if( targetTable->key3Len > 0 && (targetTable->key1Use == 0 && targetTable->key2Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(setFieldSpecChrConvNecessary(targetTable->tableLName,targetTable->key3Name,strTemp)) == 0){
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key3Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}

		if(intKeycount > 0){
			mystrncat( tempOrder, orderLen," ,",strlen(" ,"));
				orderLen += strlen(" ,");
		}
		mystrncat( tempOrder, orderLen," CAST(",strlen(" CAST("));
		orderLen += strlen(" CAST(");
		//MID(ITEM,*,*)かchCharMap(ITEM,*,*)つなぐ
		mystrncat( tempOrder, orderLen, strTemp,strlen(strTemp));
		orderLen += strlen(strTemp);

		mystrncat( tempOrder, orderLen," as binary) ",strlen(" as binary) "));
		orderLen += strlen(" as binary) ");
		intKeycount++;
	}
	//strWhereDataが入れ直しの場合一度解放
	if(targetTable->strWhere->data != NULL){
		free(targetTable->strWhere->data);
	}
	//ORDERに関するWhereなどのdata を格納))))))
	targetTable->strWhere->data = (char *)malloc(sizeof(*strKeyMid) * (orderLen + 1) );
	memset(targetTable->strWhere->data,0x00,(orderLen + 1));
	memcpy(targetTable->strWhere->data, tempOrder, (orderLen));
	targetTable->strWhere->data_size = orderLen;
	//memcopyなので開放
	free(tempOrder);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//
//view作成,viewの名前を返す
//in:
//  targetTable : SQL用の文字列ポインタ
//  targetTable : テーブルのオブジェクト
//out:
//  繋いだ文字列のポインタ
//auth : koyama
//date : 20150911
//summary : viewが存在しないならviewを作成
int setViewSort(char* strSql,DB_TABLE_OBJ *targetTable){
	char Query[2048]="";
	char strTmpTableName[128]="";    //31+ a(keyの位置によってサフィックスをつけるため)
	int ret=0,intKeyExists=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setViewSort :%.30s",targetTable->tableName);

	//view名を作成
	strcpy(strTmpTableName,targetTable->tableName);
	//ここからキーの位置と長さを基準にサフィックスを作成
	if(targetTable->key1Len > 0 && (targetTable->key2Use == 0 && targetTable->key3Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(getFieldSpecifiedElement(targetTable->tableLName,targetTable->key1Name,strTemp)) == 0){
			mysql_failure();
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key1Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
		strcat(strTmpTableName,"_");
		//MID()の開きカッコをから,までを取得
		strncat(strTmpTableName,(strchr(strTemp,',') + 1),(int)(strrchr(strTemp,',') - (strchr(strTemp,',') + 1)));
		//スタートと長さのつなぎ
		strcat(strTmpTableName,"a");
		//MID()の,をから閉じカッコまでを取得
		strncat(strTmpTableName,(strrchr(strTemp,',') + 1),(int)(strchr(strTemp,')') - (strrchr(strTemp,',') + 1)));
		//keyがあればフラグを
		intKeyExists += 1;
	}
	if(targetTable->key2Len > 0 && (targetTable->key1Use == 0 && targetTable->key3Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(getFieldSpecifiedElement(targetTable->tableLName,targetTable->key2Name,strTemp)) == 0){
			mysql_failure();
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key2Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
		strcat(strTmpTableName,"_");
		//MID()の開きカッコをから,までを取得
		strncat(strTmpTableName,(strchr(strTemp,',') + 1),(int)(strrchr(strTemp,',') - (strchr(strTemp,',') + 1)));
		//スタートと長さのつなぎ
		strcat(strTmpTableName,"a");
		//MID()の,をから閉じカッコまでを取得
		strncat(strTmpTableName,(strrchr(strTemp,',') + 1),(int)(strchr(strTemp,')') - (strrchr(strTemp,',') + 1)));
		//keyがあればフラグを
		intKeyExists += 1;
	}
	if( targetTable->key3Len > 0 && (targetTable->key1Use == 0 && targetTable->key2Use == 0)){
		char strTemp[30]="";
		//変数の名前から長さを取得
		if(strlen(getFieldSpecifiedElement(targetTable->tableLName,targetTable->key3Name,strTemp)) == 0){
			mysql_failure();
			cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key3Name);
			ret = 1;
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			return ret;
		}
		strcat(strTmpTableName,"_");
		//MID()の開きカッコをから,までを取得
		strncat(strTmpTableName,(strchr(strTemp,',') + 1),(int)(strrchr(strTemp,',') - (strchr(strTemp,',') + 1)));
		//スタートと長さのつなぎ
		strcat(strTmpTableName,"a");
		//MID()の,をから閉じカッコまでを取得
		strncat(strTmpTableName,(strrchr(strTemp,',') + 1),(int)(strchr(strTemp,')') - (strrchr(strTemp,',') + 1)));
		//keyがあればフラグを
		intKeyExists += 1;
	}
	if(intKeyExists >= 1){
		//最初の文字列をセットして
		sprintf(Query,"/* pg_name %s :setViewSort %s */",source_file_name,local_server_time(strTime));
		strcat(Query,"CREATE OR REPLACE  ");
		strcat(Query,"ALGORITHM = MERGE ");
		strcat(Query,"VIEW `");
		strcat(Query,strTmpTableName);
		strcat(Query,"`");
		strcat(Query," AS SELECT id as orig_id,ITEM  ");
		strcat(Query," FROM `");
		strcat(Query,targetTable->tableName);
		strcat(Query,"` ");
		if((targetTable->strWhere->flg % DB_WHERE_INX) == 0){
		//Invalid keyの時はの時はOrder byをつける
			if((targetTable->key1Use + targetTable->key2Use + targetTable->key3Use) > 0){
				//条件が変わったら検索する条件を作りなおす
				setOrderSql(targetTable);
			}
			strcat( Query,targetTable->strWhere->data);
		}
		// strcat( Query," CHARACTER SET sjis;");
		//テーブル
		targetTable->key1Use = 1;
		targetTable->key2Use = 1;
		targetTable->key3Use = 1;
		//一度テーブル名を一度設定すると使い回すため、その時のテーブル名を保存
		//一度入れている場合(NULLでない)場合
		if(targetTable->strWhere->tableName != NULL){
			free(targetTable->strWhere->tableName);
		}
		targetTable->strWhere->tableName = (char *)malloc(sizeof(strTmpTableName[0]) * (strlen(strTmpTableName) + 1));
		memset(targetTable->strWhere->tableName,'\0',(strlen(strTmpTableName) + 1));
		strcpy(targetTable->strWhere->tableName,strTmpTableName);
		//TODO:CREATE OR REPLACEを発行するときにREADしたことのあるスレッドがあるとLOCKされる？
		if(confirmTableExistsByName(mysqlConn,strTmpTableName) != 'v'){
			if(mysql_query(mysqlConn, Query) != 0){
				mysql_failure();
				ret = 1;
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return ret;
			}
		}
	}else{
		if(targetTable->strWhere->tableName == NULL){
			//テーブル名が確保されていなければtableNameで
			targetTable->strWhere->tableName = (char *)malloc(sizeof(strTmpTableName[0]) * (strlen(targetTable->tableName) + 1));
			memset(targetTable->strWhere->tableName,'\0',(strlen(targetTable->tableName) + 1));
			strcpy(targetTable->strWhere->tableName,targetTable->tableName);
		}
		//テーブルの再設定をしない場合前回のを使い回す
		strcpy(strTmpTableName,targetTable->strWhere->tableName);
	}
	strcat(strSql,strTmpTableName);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//調整したテーブル名を追加 句を作成
//返り値はつなげた文字列の長さ
//author:n.koyama
//date  :20150812
//INVALIDの時はテーブルの並び替えをしない date  :20150825
int setTableNameAfterStr(char *strSql ,DB_TABLE_OBJ *targetTable,char chrKindFlg){
	char strJoinTableSql[512]="";
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setTableNameAfterStr :%.30s",targetTable->tableName);

	//Iでなくても前がinValidでないならSORTされたIDは入っていない
	if(chrKindFlg == 'I'){
		//orig_idを共通化する
		strcat( strJoinTableSql," (SELECT inTbl.ID,inTbl.ITEM,inTbl.ID as orig_id ");
		strcat( strJoinTableSql," FROM `");
		strcat( strJoinTableSql,targetTable->tableName);
		strcat( strJoinTableSql,"` as inTbl ) as outTbl ");
	}else{
		if((targetTable->strWhere->flg % DB_WHERE_INX) == 0){
			//クエリの続きを追加
			strcpy( strJoinTableSql,"(SELECT @ii:=@ii+1 as id,orig_id as orig_id,ITEM ");
			strcat(strJoinTableSql,"FROM (SELECT orig_id as orig_id,ITEM FROM `");  //)固定的な決まり文句
			//view作成
			setViewSort(strJoinTableSql,targetTable);
			strcat( strJoinTableSql,"` as i ,(SELECT @ii:=0 as num) AS NUMBER ");
			strcat( strJoinTableSql,") as d  ) as o ");
		}else{
			strcat( strJoinTableSql," (SELECT inTbl.ID,inTbl.ITEM,inTbl.ID as orig_id ");
			strcat( strJoinTableSql," FROM `");
			strcat( strJoinTableSql,targetTable->tableName);
			strcat( strJoinTableSql,"` as inTbl ) as outTbl ");
		}
	}
	//ここまでで追加用のクエリ終了
	strcat(strSql,strJoinTableSql);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return strlen(strJoinTableSql);
}

//調整したテーブル名を追加 句を作成
//author:n.koyama
//date  :20150812
void *setStrOrigPrePoint(char *strId ,DB_TABLE_OBJ *targetTable){
	char strSql[512]="",strKeyTemp[64]="";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setStrOrigPrePoint :%.30s",targetTable->tableName);

	//先読みしたデータで値を返す
	if(targetTable->dataReadedCur != NULL && targetTable->dataReadedCur->dbOriginalId != 0){
		sprintf(strId,"%d",targetTable->dataReadedCur->dbOriginalId);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ;
	}

	//最初の文字列をセットして
	sprintf(strSql,"/* pg_name %s :setStrOrigPrePoint %s */",source_file_name,local_server_time(strTime));
	strcat( strSql,"SELECT orig_id ");
	strcat( strSql,"FROM(SELECT *,@ii:=@ii+1 AS id FROM(SELECT orig_id as orig_id,ITEM  FROM `");  //)固定的な決まり文句
	//viewテーブル名取得
	setViewSort(strSql,targetTable);
	strcat( strSql,"` as i ,(SELECT @ii:=0 as num) AS NUMBER ");

	strcat( strSql,") as d ) as o ");
	//ここまでで追加用のクエリ終了
	sprintf(strKeyTemp,"%d",targetTable->prevPoint);
	strcat( strSql,"WHERE id = ");
	strcat( strSql, strKeyTemp);
	strcat( strSql," ; ");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return;
	}

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		//データが取れないときNULLが帰ってくるので
		if(res == NULL){
			cob_runtime_error(" Error C [%02d]:Can not get target id :%d: %s ",42, targetTable->prevPoint,targetTable->tableName);
			//共有変数を元に戻す
			memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
			exit(1);
		}

		strcpy(strId,res[0]);
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
}

/* ----  アクセスプログラムCOBOL呼び出し関数  --------  */

//  設定ファイルの読み込みや初期設定
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

	//ファイルネームを元にリーダポインタを作成   //ファイル名を変数に変更 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//リーダをリードできる状態に
	xmlTextReaderRead(reader);
	//現在のノードのポインタをセット？
	xmlTextReaderExpand(reader);
	//現在のノードからDOMを取り出している?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}
	//ドキュメントからコンテキスト()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
	if (!ctx){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}
	//xpathで指定したノードリストを取得
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
	if (!xpobj){
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return 1;
	}
	//ノードリストをノードの配列のようなものに
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//ノード数の取得(取得できないなら0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//ノードリストから値を表示
	for (nn = 0; nn < size; ++nn) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, nn);
			if (node->content) {
				//設定ファイルからDBのホスト名を取得
				if(strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(DB_db_server,node->content);
				}
				//設定ファイルからDBのユーザ名を取得
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(DB_db_user,node->content);
				}
				//設定ファイルからDBのパスワードを取得
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(DB_db_password,node->content);
				}
				//設定ファイルからDB名を取得
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(DB_db_database,node->content);
				}
				//設定ファイルからポートを取得
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					DB_db_port = atoi(node->content);
				}
				//設定ファイルから共有モード用のテーブル名を取得
				if(strcmp(node->parent->name,MANA_TBL_NAME) == 0){
					strcpy(DB_db_mana_tbl,node->content);
				}
				//設定ファイルからITEMのテーブル名を取得
				if(strcmp(node->parent->name,ITEM_TBL_NAME) == 0){
					strcpy(DB_db_item_tbl,node->content);
				}
				//debugフラグがあるなら取得
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//返還できない文字列は0になるはず
					myConfDebugFlg = atoi(node->content);
				}
			} else {
				xmlXPathFreeObject(xpobj);
				xmlXPathFreeContext(ctx);
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				xmlCleanupParser();
				//共有変数を元に戻す
				memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
				return 1;
			}
		}
	}
	if(strlen(DB_db_item_tbl) <= 0){
		mytool_runtime_error(source_file_name," Error [%02d]:Configure Status Table is not found %s ",99,local_server_time(strTime));
		exit(1);
	}
	if(strlen(DB_db_mana_tbl) <= 0){
		mytool_runtime_error(source_file_name," Error [%02d]:Configure Management Table is not found %s ",99,local_server_time(strTime));
		exit(1);
	}
	xmlXPathFreeObject(xpobj);
	xmlXPathFreeContext(ctx);
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	xmlCleanupParser();
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//
//DBへのSELECT
//元ソースRDBへのSELECTへの対応
//in:
//	iPName :
//	可変変数: 条件
int DB_Select(char *iPName,...){
	int ret =0;
	int this_call_param=0,i = 0;		 //this_call_param:処理する可変変数の数
	int keyFlg=0,orderLen=0,whereLen=0;   //
	int orderCount=0,whereCount=0;		//Order Whereの箇所の引数の数
	char fieldName[48];				   //ループ3回ごとにフィールド名を
	char *tempSql;
	char *tempOrder;
	char *tempWhere;
	va_list list;

	if(myConfDebugFlg >= 20){
		cob_runtime_error(" Error [%04d]: %s Info DB_Select %s ",__LINE__,local_server_time(strTime),iPName);
	}
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Select :%.30s",iPName);

	tempSql = (char *)malloc(sizeof(char) * (2048 + 1));
	tempOrder = (char *)malloc(sizeof(char) * (1024 + 1));
	tempWhere = (char *)malloc(sizeof(char) * (1024 + 1));
	memset(tempSql,'\0',(2048 + 1));
	memset(tempOrder,'\0',(1024 + 1));
	memset(tempWhere,'\0',(1024 + 1));

	//対象テーブルの検索
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);
	//テーブル名がなければ-RDBをつけて再検索
	if(intCurTableNum == -1){
		char tempTableName[31];
		strcpy(tempTableName,iPName);
		strcat(tempTableName,"-RDB");
		intCurTableNum = getTargTable(DB_table,tempTableName,DB_Table_num);
	}
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Not Found Table Name ",99,map_source_func);
		ret = 1;
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}
	//関数でのDB操作のはじめでtrunsaction
	DB_Transaction(mysqlConn);
	//処理する可変変数の数の取得
	this_call_param = cob_call_params - 1;
	va_start(list,iPName);
	for(i=0;i<this_call_param;i++){
		char *tmpKey = va_arg( list , char * );
		if(strncmp(tmpKey,"WHERE",strlen("WHERE")) == 0){
			keyFlg=0;										//WHEREのときはKeyではない
			strcpy(tempWhere,"(");				 //全体を()で囲む,初期化の代わり
			whereLen += strlen("(");
		}else if(strncmp(tmpKey,"ORDER",strlen("ORDER")) == 0 ||
			strncmp(tmpKey,"ORDER BY",strlen("ORDER BY")) == 0){
			//括弧の対応除け
			keyFlg=1;
			strcpy(tempOrder,"ORDER BY ");
			orderLen += strlen("ORDER BY ");
		}else{
			//
			char strTemp[30]="";
			//キーの情報(ソート条件)
			if(keyFlg==1){
				//Order句
				if(strcmp(tmpKey,"KEY")!=0){
					if(orderCount != 0){
						strcpy((tempOrder + strlen(tempOrder)),",");
						orderLen += strlen(",");
					}
					//ソートの時は文字変換を
					if(strlen(setFieldSpecChrConvNecessary(DB_table[intCurTableNum].tableLName,tmpKey,strTemp)) == 0){
						mysql_failure();
						cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,tmpKey);
						ret = 1;
						//共有変数を元に戻す
						memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
						return ret;
					}
					//chCharMap(ITEM,*,*)をつなぐ TODO:要チェック?何を?
					strcpy((tempOrder + strlen(tempOrder)),strTemp);
					orderLen += strlen(strTemp);
						orderCount++;
					}
			}else{
				//Where句
				int tmpfieldLength = 0;
				char *data;
				switch (whereCount % 4){
				case 0:
					//0のときはフィールド指定
					if(strlen(tmpKey) >= 48){
						cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,tmpKey);
						break;
					}
					//条件のためにいったん保存しておく
					strcpy(fieldName,tmpKey);
					//条件を一致させるときは文字変換をしない
					if(strlen(getFieldSpecified(DB_table[intCurTableNum].tableLName,tmpKey,strTemp)) == 0){
						mysql_failure();
						cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,tmpKey);
						ret = 1;
						//共有変数を元に戻す
						memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
						return ret;
					}
					//MID(ITEM,*,*)をつなぐ
					mystrncat( tempWhere, whereLen,strTemp,strlen(strTemp));
					whereLen += strlen(strTemp);
					break;
				case 1:
					//1のときは条件
					if(strcmp(tmpKey,"<") == 0 ){
						mystrncat( tempWhere, whereLen," < ",strlen(" < "));
						whereLen += strlen(" < ");

					}else if(strcmp(tmpKey,">") == 0){
						mystrncat( tempWhere, whereLen," > ",strlen(" > "));
						whereLen += strlen(" > ");

					}else if(strcmp(tmpKey,"=") == 0){
						mystrncat( tempWhere, whereLen," = ",strlen(" = "));
						whereLen += strlen(" = ");

					}else if(strcmp(tmpKey,">=") == 0){
						mystrncat( tempWhere, whereLen," >= ",strlen(" >= "));
						whereLen += strlen(" >= ");

					}else if(strcmp(tmpKey,"<=") == 0){
						mystrncat( tempWhere, whereLen," <= ",strlen(" <= "));
						whereLen += strlen(" <= ");

					}else if(strcmp(tmpKey,"NOT =") == 0){
						mystrncat( tempWhere, whereLen," != ",strlen(" != "));
						whereLen += strlen(" != ");

					}else if(strcmp(tmpKey,"NOT <") == 0){
						mystrncat( tempWhere, whereLen," >= ",strlen(" >= "));
						whereLen += strlen(" >= ");

					}else if(strcmp(tmpKey,"NOT LESS") == 0){
						mystrncat( tempWhere, whereLen," >= ",strlen(" >= "));
						whereLen += strlen(" >= ");
					}else if(strcmp(tmpKey,"NOT >") == 0){
						mystrncat( tempWhere, whereLen," <= ",strlen(" <= "));
						whereLen += strlen(" <= ");
					}else{
						cob_runtime_error(" Error C [%02d]: %s That specifier can not be used [%s] ",99,map_source_func,tmpKey);
						ret = 1;
						//共有変数を元に戻す
						memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
						return ret;
					}
					break;
				case 2:
					//2のときはデータ
					tmpfieldLength = getFieldLength(DB_table[intCurTableNum].tableLName,fieldName);
					data = (char *)malloc(sizeof(char) * (tmpfieldLength + 1) );
					memset(data,0x00,(tmpfieldLength + 1));
					memcpy(data,tmpKey,tmpfieldLength);
					mystrncat( tempWhere, whereLen,"\"",strlen("\""));
					whereLen += strlen("\"");
					if(strcmp(data,"SPACE")==0 || strcmp(data,"SPACE")==0){
						memset(data,' ',tmpfieldLength);
						mystrncat( tempWhere, whereLen,data,tmpfieldLength);
						//ここだけ必要な変数の解放
					}else{
						mystrncat( tempWhere, whereLen,tmpKey,tmpfieldLength);
					}
					whereLen += getFieldLength(DB_table[intCurTableNum].tableLName,fieldName);
					mystrncat( tempWhere, whereLen,"\"",strlen("\""));
					whereLen += strlen("\"");
					free(data);
					data =NULL;
					break;
				case 3:
					if(strcmp(tmpKey,"AND") == 0){
						mystrncat( tempWhere, whereLen," AND ",strlen(" AND "));
						whereLen += strlen(" AND ");
					}else if(strcmp(tmpKey,"OR") == 0){
						mystrncat( tempWhere, whereLen," OR ",strlen(" OR "));
						whereLen += strlen(" OR ");
					}else{
						cob_runtime_error(" Error C [%02d]: %s That specifier can not be used [%s] ",99,map_source_func,tmpKey);
						ret = 1;
						//共有変数を元に戻す
						memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
						return ret;
					}
					break;
				}
				whereCount++;
			}
		}
	}
	mystrncat( tempWhere, whereLen,") ",strlen(") "));
	whereLen += strlen(") ");
	//括弧の対応がおかしいので
	//ここまで正常に終了していたらCOMMIT
	DB_Commit(mysqlConn);

	mystrncat( tempSql, 0,tempWhere,whereLen);
	mystrncat( tempSql, whereLen,tempOrder,orderLen);

	//SELECT指定のときのwhereを格納
	DB_table[intCurTableNum].strWhere->where =  (char *)malloc(sizeof(char) * (whereLen + orderLen + 1) );
	memset(DB_table[intCurTableNum].strWhere->where,0x00,(whereLen + orderLen + 1));
	memcpy(DB_table[intCurTableNum].strWhere->where, tempSql, (whereLen +orderLen));
	DB_table[intCurTableNum].strWhere->where_size = (whereLen +orderLen);
	//WHEREの検索をつけるので、RDB扱いにする
	if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0){
		DB_table[intCurTableNum].strWhere->flg *= DB_WHERE_RDB;
	}
	//Selectすると並び替わるので0にしておく
	DB_table[intCurTableNum].strWhere->rdbReaded = 0;
	free(tempSql);
	tempSql = NULL;
	free(tempOrder);
	tempOrder = NULL;
	free(tempWhere);
	tempWhere = NULL;
	//先読みのデータが有れば消しておく
	destroyDBReaded(&DB_table[intCurTableNum]);
	if(myConfDebugFlg >= 20){
		cob_runtime_error(" Error [%04d]: %s Info DB_Select %s ",__LINE__,local_server_time(strTime),DB_table[intCurTableNum].tablePName);
	}
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

//
//DBへのリード
//in:
//	iMode   : 読んでくる方法の指定
//	iPName  :
//	lockMode:UNLOCKの指定があるかどうかsource_file_name
int DB_Read(char *iMode,char *iPName,char *oItem,char *lockMode,...){
	int ret =0;
	char *iLName;
	int i = 0,sqlstrLen=0;
	int const_call_param = 4;
	int readNextFlg = 0;
	int bufferedFlg = 0;               //同じテーブルのbufferに何固まっていたら処理しなければ、不具合が出る
	char strSql[1024] = "";
	char strPrevKey[30] = "";					 //IDを格納するために一時的に使用
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Read :%.30s",iPName);

	if(myConfDebugFlg >= 20){
		cob_runtime_error(" Error [%04d]: %s Info DB_Read S %s ",__LINE__,local_server_time(strTime),iPName);
	}

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//テーブルの名前が取れないのはコーディング上のエラーということにする
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Table is not Open [%.20s] ",99,map_source_func,iPName);
		ret = 1;
		return ret;
	}

	//OPENに失敗していたら
	//前処理はOpenだけでは無いがOpenが失敗している場合ははじく
	if(matchTableStatus(&DB_table[intCurTableNum],"O1") != 0){
		//statusを変更せずに終了
		ret = 1;
		return ret;
	}

	//lockModeの確認
	if( lockMode[0] != ' ' && lockMode[0] != 'U' ){
		mysql_failure();
		ret = 1;
		cob_runtime_error(" Error C [%02d]: Lock mode failed %s,table status:%s ",21,DB_table[intCurTableNum].tablePName,DB_table[intCurTableNum].accessStatus);
		return ret;
	}

	if(strlen(DB_table[intCurTableNum].tableName) == 0 ){
		mysql_failure();
		ret = 1;
		cob_runtime_error(" Error C [%02d]: not found table name. status %s ",22,DB_table[intCurTableNum].accessStatus);
		return ret;
	}
	//最初の文字列をセットして
	sprintf(strSql,"/* pg_name %s user_name %s :DB_Read %s */ SELECT ID,ITEM,orig_id FROM ",source_file_name,source_user_name,local_server_time(strTime));
	sqlstrLen += strlen(strSql);

	//テーブル名の追加 & Readステータス処理
	if(strchr(iMode,'I') != NULL && strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0 ){
		sqlstrLen += setTableNameAfterStr(strSql,&DB_table[intCurTableNum],'I');
		//INVALID KEYのとき(RUNDOM ACCESSになるのでポインタを消去)
		DB_table[intCurTableNum].prevPoint = 0;
		DB_table[intCurTableNum].nextPoint = 0;
		//IDを文字列化
		sprintf(strPrevKey,"%d",DB_table[intCurTableNum].prevPoint);
	}else{
		sqlstrLen += setTableNameAfterStr(strSql,&DB_table[intCurTableNum],'S');
		//前がinvalidの時はIDの撮り直しが必要か?
		if(getTableStatusWithIndex(&DB_table[intCurTableNum],2) == 'I'){
			//IDを文字列化
			sprintf(strPrevKey,"%d",getIntConvertOrigPtoOrderP(&DB_table[intCurTableNum]));
		}else{
			//IDを文字列化
			sprintf(strPrevKey,"%d",DB_table[intCurTableNum].prevPoint);
		}
		//TODO:RDBのときにLIMIT用の変数をstrPrevKeyで作る(入れ替える)
		if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) == 0){
			//IDを文字列化
			sprintf(strPrevKey,"%llu",DB_table[intCurTableNum].strWhere->rdbReaded);
			//文字列変数に書き出したら+1しておく
			DB_table[intCurTableNum].strWhere->rdbReaded += 1;
		}
	}

	//WHERE 句の指定
	mystrncat( strSql, sqlstrLen," WHERE ",strlen(" WHERE "));
	sqlstrLen += strlen(" WHERE ");

	//RDBのときはID検索ではなくLIMITで
	if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0){
		mystrncat( strSql, sqlstrLen," ID > ",strlen(" ID > "));
		sqlstrLen += strlen(" ID > ");
		//IDを結合
		mystrncat( strSql, sqlstrLen,strPrevKey,strlen(strPrevKey));
		sqlstrLen += strlen(strPrevKey);
		mystrncat( strSql, sqlstrLen," ",strlen(" "));
		sqlstrLen += strlen(" ");
	}else if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_INX) == 0){
		//indexファイルの
	}
	//モードによるWHERE句の指定(INVALID KEYのときはWHERE でキー値を指定)
	//INVALIDのとき条件追加
	if(strchr(iMode,'I') != NULL && strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0  &&(DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0 ){
		//------------------------------------------------------------------------------KEY値指定のときがあるため
		//通常関数を呼ぶときの変数の数がconst_call_param
		int this_call_param = cob_call_params - const_call_param;
		int roopInvalid = 0;
		va_list list;
		//それぞれのkeyを使うかどうかのフラグ
		int key1exist = 0;
		int key2exist = 0;
		int key3exist = 0;

		//----------------------------------------------ここの変数スコープでの変数
		//key値が指定されたらそのキー値のみを使う
		if(this_call_param > 0){
			//key値が入っているparamを取得
			va_start(list,lockMode);
			for(roopInvalid = 0;roopInvalid < this_call_param;roopInvalid++){
				char *tmpKey = va_arg( list , char * );
				if(DB_table[intCurTableNum].key1 == tmpKey){
					key1exist = 1;
				}
				if(DB_table[intCurTableNum].key2 == tmpKey){
					key2exist = 1;
				}
				if(DB_table[intCurTableNum].key3 == tmpKey){
					key3exist = 1;
				}
			}
			va_end( list );
		}else{
			//keyの指定がなければ最近変わったものをkeyとして使う必要あり？
			//key1を使うか
			if(DB_table[intCurTableNum].key1Len > 0){
				key1exist = 1;
			}
			//key2を使うか
			if(key1exist != 0 && DB_table[intCurTableNum].key2Len > 0){
				key2exist = 1;
			}
			//key3を使うか
			if((key1exist != 0 && key2exist != 0  ) && DB_table[intCurTableNum].key3Len > 0){
				key3exist = 1;
			}
		}

		//KEYがあるときは()で囲む
		if(key1exist != 0 || key2exist != 0 || key3exist != 0){
			mystrncat( strSql, sqlstrLen," AND ( ",strlen(" AND ( "));
			sqlstrLen += strlen(" AND ( ");
		}
		//ANDでキーの判定
		//キーの値の1番目の長さが0以上なら
		if(key1exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,1);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}

		//キーの値の2番目の長さが0以上なら
		if(key2exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,2);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}

		//キーの値の3番目の長さが0以上なら
		if(key3exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,3);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}
		//KEYがあるときは()で囲む
		if(key1exist != 0 || key2exist != 0 || key3exist != 0){
			mystrncat( strSql, sqlstrLen," ) ",strlen(" ) "));
			sqlstrLen += strlen(" ) ");
		}
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, " ; ", strlen(" ; "));
		sqlstrLen += strlen(" ; ");
		// //ここでも同じ仕組みでとれるのか？
		// if(matchTableStatus(&DB_table[intCurTableNum],"R0I")){
		// 	readNextFlg = 1;
		// }
		//今回InVALIDだったことをセット
		setTableStatusWithIndex(&DB_table[intCurTableNum],'I',2);
	}else if(DB_table[intCurTableNum].strWhere->where_size > 0 && (DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) == 0 ){
		//ここの条件にマッチするものは元のRDB
		//ID指定を上でしないでLimitの一番上を使うのでANDなし
		//なければ0文字つなぐので問題なし
		mystrncat( strSql, sqlstrLen,DB_table[intCurTableNum].strWhere->where,DB_table[intCurTableNum].strWhere->where_size);
		sqlstrLen += DB_table[intCurTableNum].strWhere->where_size;

		//IDを結合
		//必要なのは2行だけなので絞込
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");

		mystrncat( strSql, sqlstrLen,strPrevKey,strlen(strPrevKey));
		sqlstrLen += strlen(strPrevKey);

		//必要なのはN行だけなので絞込
		mystrncat( strSql, sqlstrLen, ", ", strlen(", "));
		sqlstrLen += strlen(", ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, "; ", strlen("; "));
		sqlstrLen += strlen("; ");
		//RDBはDB_Scratch,DB_Selectで中身がリセットされる仕組み
		readNextFlg = 1;
		//今回RDBだったことをセット
		setTableStatusWithIndex(&DB_table[intCurTableNum],'R',2);
	}else{
		//---------------------------------------------------------------------------------NEXT AT END? 20161222
		readNextFlg = 1;
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, " ; ", strlen(" ; "));
		sqlstrLen += strlen(" ; ");
		//今回NEXT AT ENDだったことをセット
		setTableStatusWithIndex(&DB_table[intCurTableNum],iMode[0],2);
	}
	//Bufferで処理ができなかった時Bufferに残っているものを一掃 20170814 add koyama
	bufferedFlg += checkInsertBuffer(&DB_table[intCurTableNum]);
	bufferedFlg += checkUpdateBuffer(&DB_table[intCurTableNum]);
	bufferedFlg += checkDeleteBuffer(&DB_table[intCurTableNum]);

	//既に読んでいるものがある場合をそれを取得して関数を抜ける
	if(readNextFlg != 0 && bufferedFlg == 0 && DB_table[intCurTableNum].dataReadedCur != NULL && DB_table[intCurTableNum].dataReadedCur->nextPoint != NULL){

		//前回読んだ部分を削除,その次のデータを読み込む
		popDBReaded(&DB_table[intCurTableNum],oItem);
		getDataDBReaded(&DB_table[intCurTableNum],oItem);
		//ステータスの格納
		setTableStatus(&DB_table[intCurTableNum],'R',ret);

		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}

	//クエリ発行の直前でtrunsaction
	DB_Transaction(mysqlConn);
	//queryの実行
	if((ret = mysql_real_query(mysqlConn, strSql, sqlstrLen)) != 0){
		mysql_failure();
		ret = 1;
		//エラーコードで返す。(受け側で処理してないから意味ないけど。。。)
		return ret;
	}
	//----------------------------------------------------------------------------ここからデータ取得
	//ここから動きが違うので変数の追加
	MYSQL_ROW res;
	MYSQL_RES *result;

	result = mysql_use_result(mysqlConn);
	if(result == 0){
		mysql_failure();
		//mysqlのエラーを標準エラーに(なぜ最初4文字が\0か不明)
		mytool_runtime_error(source_file_name," Error C [%02d]: Database store Error %20s %s %s:%d %s ",99,(mysqlConn->net.last_error + 4),map_source_func,__func__,__LINE__,local_server_time(strTime));
		return 1;
	}

	int otherRownum = 0;
	destroyDBReaded(&DB_table[intCurTableNum]);
	//いったんすべてをデータ構造として格納
	otherRownum = setReadedRecordFromDBResult(&DB_table[intCurTableNum],result);

	if(otherRownum != 0){
		mysql_free_result(result);
		//AutoCommit Offなので
		DB_Commit(mysqlConn);
		//予め格納したデータ構造の最初の一行を取得
		DB_table[intCurTableNum].prevPoint = DB_table[intCurTableNum].dataReadedCur->dbId;
		memcpy(oItem,DB_table[intCurTableNum].dataReadedCur->dbData, DB_table[intCurTableNum].dataReadedCur->dbDataLen);
		ret = 0;
	}else{
		mysql_free_result(result);
		//AutoCommit OffなのでSELECTでは何も戻らないが暗黙的トランザクションを切る
		DB_Rollback(mysqlConn);
		//ファイル状態キーの変更
		switch(getTableStatusWithIndex(&DB_table[intCurTableNum],2)){
			case 'I':
				//READ INVALID KEYの時(KEY未発見)
				*(DB_filestat + 0) = '2';
				*(DB_filestat + 1) = '3';
				break;
			case 'R':
				*(DB_filestat + 0) = '1';
				*(DB_filestat + 1) = '0';
				break;
			default :
				//READ AT ENDの時(読み切ったことを示す)
				*(DB_filestat + 0) = '1';
				*(DB_filestat + 1) = '0';
				break;
		}
		ret = 1;
	}

	//ここまで成功して、SharedModeがunlockならLockを解除
	if(ret == 0){
		if(lockMode[0] == 'U'){
			ret = M_UNLOCK(DB_table[intCurTableNum].tablePName,__func__,__LINE__);
		}
	}
	//ステータスの格納
	setTableStatus(&DB_table[intCurTableNum],'R',ret);

	//debugFlgがオンの時時間などを出力
	if(myConfDebugFlg >= 20){
		cob_runtime_error(" Error [%04d]: %s Info DB_Read %s ",__LINE__,local_server_time(strTime),iPName);
		if(myConfDebugFlg >= 25){
			cob_runtime_error(" Error [%04d]: %s Debug DB_Read %s %s ",__LINE__,local_server_time(strTime),iPName,strSql);
		}
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//DB_ReadのINVALIDKey際のクエリをつなぐ関数
//keyの3番目
//return 問題なし 0  問題あり 1
//author:koyama
//add-date: 20171121
int set_DB_Read_query_has_invalid_key3(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen){
	char strFeildSpec[64] = "";
	char strKeyVal[1024]="";
	int  keyLen=0;
	int  retVal=0;

	mystrncat( strSql, *sqlstrLen," OR ",strlen(" OR "));
	*sqlstrLen += strlen(" OR ");

	keyLen = dataEscapeCopy(targetTable->key3,strKeyVal,targetTable->key3Len);
	//フィールド名からMID(?,?)をとってくる
	if(strlen(getFieldSpecified(targetTable->tableLName,targetTable->key3Name,strFeildSpec)) == 0){
		mysql_failure();
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key1Name);
		retVal = 1;
		return retVal;
	}
	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ( ",strlen(" ( "));
	*sqlstrLen += strlen(" ( ");
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," '",strlen(" '"));
	*sqlstrLen += strlen(" '");
	//値を埋める ここはWildCardなし
	mystrncat( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' LIKE BINARY REPLACE(",strlen("' LIKE BINARY REPLACE("));
	*sqlstrLen += strlen("' LIKE BINARY REPLACE(");
	//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	mystrncat( strSql, *sqlstrLen," ,' ','_') OR ",strlen(" ,' ','_') OR "));
	*sqlstrLen += strlen(" ,' ','_') OR ");

		//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," LIKE BINARY '",strlen(" LIKE BINARY '"));
	*sqlstrLen += strlen(" LIKE BINARY '");
	//値を埋める
	changeWildcard( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' ",strlen("' "));
	*sqlstrLen += strlen("' ");

	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ) ",strlen(" ) "));
	*sqlstrLen += strlen(" ) ");

	return retVal;
}

//DB_ReadのINVALIDKey際のクエリをつなぐ関数
//keyの2番目
//return 問題なし 0  問題あり 1
//author:koyama
//add-date: 20171121
int set_DB_Read_query_has_invalid_key2(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen){
	char strFeildSpec[64] = "";
	char strKeyVal[1024]="";
	int  keyLen=0;
	int  retVal=0;

	mystrncat( strSql, *sqlstrLen," OR ",strlen(" OR "));
	*sqlstrLen += strlen(" OR ");

	keyLen = dataEscapeCopy(targetTable->key2,strKeyVal,targetTable->key2Len);
	//フィールド名からMID(?,?)をとってくる
	if(strlen(getFieldSpecified(targetTable->tableLName,targetTable->key2Name,strFeildSpec)) == 0){
		mysql_failure();
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key1Name);
		retVal = 1;
		return retVal;
	}
	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ( ",strlen(" ( "));
	*sqlstrLen += strlen(" ( ");
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," '",strlen(" '"));
	*sqlstrLen += strlen(" '");
	//値を埋める ここはWildCardなし
	mystrncat( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' LIKE BINARY REPLACE(",strlen("' LIKE BINARY REPLACE("));
	*sqlstrLen += strlen("' LIKE BINARY REPLACE(");
	//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	mystrncat( strSql, *sqlstrLen," ,' ','_') OR ",strlen(" ,' ','_') OR "));
	*sqlstrLen += strlen(" ,' ','_') OR ");

		//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," LIKE BINARY '",strlen(" LIKE BINARY '"));
	*sqlstrLen += strlen(" LIKE BINARY '");
	//値を埋める
	changeWildcard( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' ",strlen("' "));
	*sqlstrLen += strlen("' ");

	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ) ",strlen(" ) "));
	*sqlstrLen += strlen(" ) ");

	return retVal;
}


//DB_ReadのINVALIDKey際のクエリをつなぐ関数
//keyの1番目
//return 問題なし 0  問題あり 1
//author:koyama
//add-date: 20171121
int set_DB_Read_query_has_invalid_key1(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen){
	char strFeildSpec[64] = "";
	char strKeyVal[1024]="";
	int  keyLen=0;
	int  retVal=0;

	// mystrncat( strSql, *sqlstrLen," OR ",strlen(" OR "));
	// *sqlstrLen += strlen(" OR ");

	keyLen = dataEscapeCopy(targetTable->key1,strKeyVal,targetTable->key1Len);
	//フィールド名からMID(?,?)をとってくる
	if(strlen(getFieldSpecified(targetTable->tableLName,targetTable->key1Name,strFeildSpec)) == 0){
		mysql_failure();
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,targetTable->key1Name);
		retVal = 1;
		return retVal;
	}
	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ( ",strlen(" ( "));
	*sqlstrLen += strlen(" ( ");
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," '",strlen(" '"));
	*sqlstrLen += strlen(" '");
	//値を埋める ここはWildCardなし
	mystrncat( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' LIKE BINARY REPLACE(",strlen("' LIKE BINARY REPLACE("));
	*sqlstrLen += strlen("' LIKE BINARY REPLACE(");
	//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	mystrncat( strSql, *sqlstrLen," ,' ','_') OR ",strlen(" ,' ','_') OR "));
	*sqlstrLen += strlen(" ,' ','_') OR ");

		//MID(*,*)をつなぐ
	mystrncat( strSql, *sqlstrLen,strFeildSpec,strlen(strFeildSpec));
	*sqlstrLen += strlen(strFeildSpec);
	//ここをpacked対応しなければ 20160112 -> ここは例外を作らないとWildCardとぶつかる
	mystrncat( strSql, *sqlstrLen," LIKE BINARY '",strlen(" LIKE BINARY '"));
	*sqlstrLen += strlen(" LIKE BINARY '");
	//値を埋める
	changeWildcard( strSql, *sqlstrLen,strKeyVal,keyLen);
	*sqlstrLen += keyLen;
	mystrncat( strSql, *sqlstrLen,"' ",strlen("' "));
	*sqlstrLen += strlen("' ");

	//ORで逆向きワイルドカードを追加 add koyama 20160811
	mystrncat( strSql, *sqlstrLen," ) ",strlen(" ) "));
	*sqlstrLen += strlen(" ) ");

	return retVal;
}

//DB_ReadのINVALIDKey際のクエリをつなぐ関数(ラッパー)
//keyの何番目かによって関数を再呼び出し
//author:koyama
//add-date: 20171121
int set_DB_Read_query_has_invalid_key(DB_TABLE_OBJ *targetTable,char *strSql,int *sqlstrLen,int keyNum){
	int retVal=0;
	switch (keyNum){
		case 1 :
			retVal = set_DB_Read_query_has_invalid_key1(targetTable,strSql,sqlstrLen);
			break;
		case 2 :
			retVal = set_DB_Read_query_has_invalid_key2(targetTable,strSql,sqlstrLen);
			break;
		case 3 :
			retVal = set_DB_Read_query_has_invalid_key3(targetTable,strSql,sqlstrLen);
			break;
		default :
			break;
	}
	return retVal;
}

//DB_Update 専用のDB_Read
//INVALIDの時に対象を取得するためだけの機能
//author:koyama
//add-date: 20171121
//UpdateではLockmodeが変わらないので、Lockmodeの代わりに変更対象のデータを設定
int DB_Read_for_update(char *iMode,char *iPName,char *oItem,DBReaded *ReadedCur){
	int ret =0;
	char *iLName;
	int i = 0,sqlstrLen=0;
	int const_call_param = 4;
	int readNextFlg = 0;
	int bufferedFlg = 0;               //同じテーブルのbufferに何固まっていたら処理しなければ、不具合が出る
	char strSql[1024] = "";
	char strPrevKey[30] = "";					 //IDを格納するために一時的に使用
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Read_for_update :%.30s",iPName);
	if(myConfDebugFlg >= 20){
		cob_runtime_error(" Error [%04d]: %s Info DB_Read_for_update S %s ",__LINE__,local_server_time(strTime),iPName);
	}

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//テーブルの名前が取れないのはコーディング上のエラーということにする
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Table is not Open [%.20s] ",99,map_source_func,iPName);
		ret = 1;
		return ret;
	}

	//OPENに失敗していたら
	//前処理はOpenだけでは無いがOpenが失敗している場合ははじく
	if(matchTableStatus(&DB_table[intCurTableNum],"O1") != 0){
		//statusを変更せずに終了
		ret = 1;
		return ret;
	}

	if(strlen(DB_table[intCurTableNum].tableName) == 0 ){
		mysql_failure();
		ret = 1;
		cob_runtime_error(" Error C [%02d]: not found table name. status %s ",22,DB_table[intCurTableNum].accessStatus);
		return ret;
	}
	//最初の文字列をセットして
	sprintf(strSql,"/* pg_name %s user_name %s :DB_Read_for_update %s */ SELECT ID,ITEM,orig_id FROM ",source_file_name,source_user_name,local_server_time(strTime));
	sqlstrLen += strlen(strSql);

	//テーブル名の追加 & Readステータス処理
	if(strchr(iMode,'I') != NULL && strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0 ){
		//UPDATEのときなので必ず通る
		sqlstrLen += setTableNameAfterStr(strSql,&DB_table[intCurTableNum],'I');
		//INVALID KEYのとき(RUNDOM ACCESSになるのでポインタを消去)
		//IDを文字列化
		//updateのときは全件から探索
		sprintf(strPrevKey,"%d",0);
	}

	//WHERE 句の指定
	mystrncat( strSql, sqlstrLen," WHERE ",strlen(" WHERE "));
	sqlstrLen += strlen(" WHERE ");

	//RDBのときはID検索ではなくLIMITで
	if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0){
		mystrncat( strSql, sqlstrLen," ID > ",strlen(" ID > "));
		sqlstrLen += strlen(" ID > ");
		//IDを結合
		mystrncat( strSql, sqlstrLen,strPrevKey,strlen(strPrevKey));
		sqlstrLen += strlen(strPrevKey);
		mystrncat( strSql, sqlstrLen," ",strlen(" "));
		sqlstrLen += strlen(" ");
	}else if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_INX) == 0){
		//indexファイルの
	}
	//モードによるWHERE句の指定(INVALID KEYのときはWHERE でキー値を指定)
	//INVALIDのとき条件追加
	if(strchr(iMode,'I') != NULL && strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0  &&(DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0 ){
		//それぞれのkeyを使うかどうかのフラグ
		int key1exist = 0;
		int key2exist = 0;
		int key3exist = 0;
		//key1を使うか
		if(DB_table[intCurTableNum].key1Len > 0){
			key1exist = 1;
		}
		//key2を使うか
		if(key1exist != 0 && DB_table[intCurTableNum].key2Len > 0){
			key2exist = 1;
		}
		//key3を使うか
		if((key1exist != 0 && key2exist != 0  ) && DB_table[intCurTableNum].key3Len > 0){
			key3exist = 1;
		}
		//KEYがあるときは()で囲む
		if(key1exist != 0 || key2exist != 0 || key3exist != 0){
			mystrncat( strSql, sqlstrLen," AND ( ",strlen(" AND ( "));
			sqlstrLen += strlen(" AND ( ");
		}
		//ANDでキーの判定
		//キーの値の1番目の長さが0以上なら
		if(key1exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,1);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}

		//キーの値の2番目の長さが0以上なら
		if(key2exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,2);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}

		//キーの値の3番目の長さが0以上なら
		if(key3exist > 0){
			int falseKeyset = 0;
			falseKeyset = set_DB_Read_query_has_invalid_key(&DB_table[intCurTableNum],strSql,&sqlstrLen,3);
			if(falseKeyset != 0){
				ret = 1;
				return ret;
			}
		}

		//KEYがあるときは()で囲む
		if(key1exist != 0 || key2exist != 0 || key3exist != 0){
			mystrncat( strSql, sqlstrLen," ) ",strlen(" ) "));
			sqlstrLen += strlen(" ) ");
		}
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, " ; ", strlen(" ; "));
		sqlstrLen += strlen(" ; ");
		//ここでも同じ仕組みでとれるのか？
		if(matchTableStatus(&DB_table[intCurTableNum],"R0I")){
			readNextFlg = 1;
		}
		//今回InVALIDだったことをセット
		// setTableStatusWithIndex(&DB_table[intCurTableNum],'I',2);
	}else if(DB_table[intCurTableNum].strWhere->where_size > 0 && (DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) == 0 ){
		//ここの条件にマッチするものは元のRDB
		//ID指定を上でしないでLimitの一番上を使うのでANDなし
		//なければ0文字つなぐので問題なし
		mystrncat( strSql, sqlstrLen,DB_table[intCurTableNum].strWhere->where,DB_table[intCurTableNum].strWhere->where_size);
		sqlstrLen += DB_table[intCurTableNum].strWhere->where_size;

		//IDを結合
		//必要なのは2行だけなので絞込
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");

		mystrncat( strSql, sqlstrLen,strPrevKey,strlen(strPrevKey));
		sqlstrLen += strlen(strPrevKey);

		//必要なのはN行だけなので絞込
		mystrncat( strSql, sqlstrLen, ", ", strlen(", "));
		sqlstrLen += strlen(", ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, "; ", strlen("; "));
		sqlstrLen += strlen("; ");
		//RDBはDB_Scratch,DB_Selectで中身がリセットされる仕組み
		readNextFlg = 1;
		//今回RDBだったことをセット
		// setTableStatusWithIndex(&DB_table[intCurTableNum],'R',2);
	}else{
		//---------------------------------------------------------------------------------NEXT AT END? 20161222
		readNextFlg = 1;
		mystrncat( strSql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");
		mystrncat( strSql, sqlstrLen, DATA_READ_NUM, strlen(DATA_READ_NUM));
		sqlstrLen += strlen(DATA_READ_NUM);
		mystrncat( strSql, sqlstrLen, " ; ", strlen(" ; "));
		sqlstrLen += strlen(" ; ");
		//今回NEXT AT ENDだったことをセット
		// setTableStatusWithIndex(&DB_table[intCurTableNum],iMode[0],2);
	}

	//クエリ発行の直前でtrunsaction
	DB_Transaction(mysqlConn);
	//queryの実行
	if((ret = mysql_real_query(mysqlConn, strSql, sqlstrLen)) != 0){
		mysql_failure();
		ret = 1;
		//エラーコードで返す。(受け側で処理してないから意味ないけど。。。)
		return ret;
	}
	//----------------------------------------------------------------------------ここからデータ取得
	//ここから動きが違うので変数の追加
	MYSQL_ROW res;
	MYSQL_RES *result;
	result = mysql_use_result(mysqlConn);
	if(result == 0){
		mysql_failure();
		//mysqlのエラーを標準エラーに(なぜ最初4文字が\0か不明)
		mytool_runtime_error(source_file_name," Error C [%02d]: Database store Error %20s %s %s:%d %s ",99,(mysqlConn->net.last_error + 4),map_source_func,__func__,__LINE__,local_server_time(strTime));
		mysql_free_result(result);
		//AutoCommit OffなのでSELECTでは何も戻らないが暗黙的トランザクションを切る
		DB_Rollback(mysqlConn);
		//ファイル状態キーの変更は不要
		ret = 1;
		//ステータスの格納
		setTableStatus(&DB_table[intCurTableNum],'R',ret);

		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}
	int maxcols=0;              //フィールドの数とそのループ変数
	unsigned long *f_length;    //フィールドの長さ(配列として受け取るのでポインタ宣言)
	res      = mysql_fetch_row(result);
	f_length = mysql_fetch_lengths(result);
	maxcols  = mysql_num_fields(result);
	if(res != NULL){
		//AutoCommit Offなので
		ReadedCur->dbId          = atoi(res[0]);
		strncpy(ReadedCur->dbData,res[1],f_length[1]);
		ReadedCur->dbOriginalId  = atoi(res[2]);
	}
	mysql_free_result(result);
	//AutoCommit Offなので
	DB_Commit(mysqlConn);
	//予め格納したデータ構造の最初の一行を取得
	ret = 0;

	//ステータスの格納
	setTableStatus(&DB_table[intCurTableNum],'R',ret);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

// 変更関数
int DB_Update(char *iPName,char *iLName,char *iItem){
	int ret = 0;
	int ii = 0, cnt = 0,loopCnt = 0;
	char strSql[4096] = "";
	char strKeyTemp[30] = "";
	int sqlstrLen = 0;
	char *tmpItem;
	int itemLen=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Update :%.30s",iPName);

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//テーブルの名前が取れないのはコーディング上のエラーということにする
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Table is not Open [%.20s] ",99,map_source_func,iPName);
		ret = 1;
		return ret;
	}

	//前の処理でReadが成功していないときはエラー
	//INVALIDで開かれたときは無視する
	if(getFileStatusOrgnization(&DB_table[intCurTableNum]) != 'I'
	&& matchTableStatus(&DB_table[intCurTableNum],"R0") != 1){
		ret = 1;
		//ステータスの格納
		setTableStatus(&DB_table[intCurTableNum],'U',ret);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}
	//Bufferに残っているものを一掃
	checkInsertBuffer(&DB_table[intCurTableNum]);
	checkDeleteBuffer(&DB_table[intCurTableNum]);

	//Updateを一度に行うためのデータを格納しておく
	if(DB_table[intCurTableNum].dataUpdateCur == NULL || DB_table[intCurTableNum].dataUpdateCur->no < DATA_UPDATE_NUM){
		//保存上限より小さいときはセットだけ
		ret = setUpdateRecord(&DB_table[intCurTableNum],iItem,varLength(3));
		if(ret == -1){
			ret=1;
		}
		//ステータスの格納
		setTableStatus(&DB_table[intCurTableNum],'U',ret);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}else{
		int setreturn=0;
		//保存上限になったらセット->全更新->削除の流れ
		setreturn = setUpdateRecord(&DB_table[intCurTableNum],iItem,varLength(3));
		//全ての更新を行う
		ret = updateRecordFromBuffer(&DB_table[intCurTableNum]);
		if(setreturn == -1){
			ret=1;
		}
		//全ての削除を行う
		destroyDBUpdate(&DB_table[intCurTableNum]);
		DB_table[intCurTableNum].dataUpdateCur = NULL;
		//ステータスの格納
		setTableStatus(&DB_table[intCurTableNum],'U',ret);
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return ret;
	}
}

//insertの際にkeyかぶりのcheckを行う
//author:koyama 2018/01/11
int existsDupulicateKey(DB_TABLE_OBJ *TableObj,char *iItem){
	int isRecordExist = 0;
	if(getFileStatusOrgnization(TableObj) == 'I'){
		DBReaded *dataReadedUpdateCur=NULL;
		char dataTemp[READDATA_MAX_LEN]="";
		int readResult = 0;
		dataReadedUpdateCur = initDBReaded(NULL);
		readResult = DB_Read_for_update("INVALID",TableObj->tablePName,dataTemp,dataReadedUpdateCur);
		if(readResult != 0){
			// データが取れれば0ではないので1になる
			isRecordExist = 1;
		}
	}
	return isRecordExist;
}


//  追加関数
int DB_Insert(char *iPName,char *iLName,char *iItem){
	int ret = 0;
	int ii = 0, cnt = 0,sqlstrLen = 0,loopCnt = 0;
	char strSql[4096] = "";
	char *tmpItem;
	int itemLen=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Insert :%.30s",iPName);

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//テーブルの名前が取れないのはコーディング上のエラーということにする
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Table is not Open [%.20s] ",99,map_source_func,iPName);
		ret = 1;
		return ret;
	}

	//OPENに失敗していたら
	if(matchTableStatus(&DB_table[intCurTableNum],"O1") != 0){
		//statusを変更せずに終了
		ret = 1;
		return ret;
	}
	//Bufferに残っているものを一掃
	checkUpdateBuffer(&DB_table[intCurTableNum]);
	checkDeleteBuffer(&DB_table[intCurTableNum]);
	//被ったキーが存在するならエラー
	if(existsDupulicateKey(&DB_table[intCurTableNum],iItem) != 0){
		//statusを変更せずに終了
		ret = 1;
		return ret;
	}
	if(DB_table[intCurTableNum].dataInsertCur == NULL || DB_table[intCurTableNum].dataInsertCur->no < DATA_UPDATE_NUM){
	    //保存上限より小さいときはセットだけ
	    setInsertRecord(&DB_table[intCurTableNum],iItem,varLength(3));
	}else{
	    //保存上限になったらセット->全更新->削除の流れ
	    setInsertRecord(&DB_table[intCurTableNum],iItem,varLength(3));
	    //全ての挿入を行う
			ret = insertRecordFromBuffer(&DB_table[intCurTableNum]);
	    //全ての削除を行う
	    destroyDBInsert(&DB_table[intCurTableNum]);
			DB_table[intCurTableNum].dataInsertCur = NULL;
			return ret;
	}
	//ステータスの格納
	setTableStatus(&DB_table[intCurTableNum],'I',ret);

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//  削除関数
int DB_Delete(char *iPName){
	int ret = 0;
	int ii = 0, cnt = 0;
	char strKeyTemp[30] = "";
	char strSql[1024] = "";

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Delete :%.30s",iPName);

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//テーブルの名前が取れないのはコーディング上のエラーということにする
	if(intCurTableNum == -1){
		cob_runtime_error(" Error C [%02d]: %s Table is not Open [%.20s] ",99,map_source_func,iPName);
		ret = 1;
		return ret;
	}

	//前の処理でReadが成功していないときはエラー
	if(matchTableStatus(&DB_table[intCurTableNum],"R1") != 0){
		ret = 1;
		//ファイル状態キーの変更
		*(DB_filestat + 0) = '4';
		*(DB_filestat + 1) = '3';
		return ret;
	}

	//Bufferに残っているものを一掃
	checkInsertBuffer(&DB_table[intCurTableNum]);
	checkUpdateBuffer(&DB_table[intCurTableNum]);

	//一度に削除する処理実装予定 20170118
	if(DB_table[intCurTableNum].dataDeleteCur == NULL || DB_table[intCurTableNum].dataDeleteCur->no < DATA_UPDATE_NUM){
		//保存上限より小さいときはセットだけ
		//手実装でReadedをクローンしてくればいい？
		setDeleteRecord(&DB_table[intCurTableNum]);
		return ret;
	}else{
		//保存上限になったらセット->全更新->削除の流れ
		setDeleteRecord(&DB_table[intCurTableNum]);
		//全ての更新を行う
		ret = deleteRecordFromBuffer(&DB_table[intCurTableNum]);
		//全ての削除を行う
		destroyDBDelete(&DB_table[intCurTableNum]);
		DB_table[intCurTableNum].dataDeleteCur = NULL;
		return ret;
	}
	//関数のはじめでtrunsaction
	DB_Transaction(mysqlConn);

	sprintf(strSql,"/*  pg_name %s : DB_Delete %s */ DELETE FROM ",source_file_name,local_server_time(strTime));
	//テーブル名指定
	strcat( strSql, "`" );
	strcat( strSql, DB_table[intCurTableNum].tableName );
	strcat( strSql, "`" );

	//検索key設定
	//Readした行に対して行えばいいのでこれでいい
	//IDを文字列化前回のReadがInvalidでなくkeyがあるファイルの時
	if(DB_table[intCurTableNum].readStatus[0] != 'I' && (DB_table[intCurTableNum].strWhere->flg % DB_WHERE_INX) == 0){
		setStrOrigPrePoint(strKeyTemp,&DB_table[intCurTableNum]);
	}else{
		sprintf(strKeyTemp,"%d",DB_table[intCurTableNum].prevPoint);
	}
	if(strlen(strKeyTemp) >= 1){

		strcat( strSql, " WHERE ID = " );
		strcat( strSql, strKeyTemp);

		//SQL実行
		if(mysql_query(mysqlConn, strSql) != 0){
			mysql_failure();
			DB_Rollback(mysqlConn);
			ret = 1;
		}
	}else{
		//IDが取れなければエラー終了
		ret = 1;
	}

	if(ret != 1){
		//ここまで正常に終了していたらCOMMIT
		DB_Commit(mysqlConn);
	}else{
		DB_Rollback(mysqlConn);
	}

	//ステータスの格納
	setTableStatus(&DB_table[intCurTableNum],'D',ret);

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

//初期値のときのテーブルの論理名、物理名を取得(物理名の後ろをNULL埋めし,論理名を返す)
//var   :inputName=入力変数,語尾のスペースを\0うめ,outputNameLnameになるはず
//date  :20161031
//author:koyama
char *setLogicalAndPhysicalTableName(char *strPhysicalName){
	int loopCnt=0,loopDiff=0;
	char *strLogicalName=NULL;
	for(loopCnt=0;loopCnt < strlen(strPhysicalName) && strPhysicalName[loopCnt] != '\0';loopCnt++){
		if(strPhysicalName[loopCnt] == ' '){
			strPhysicalName[loopCnt] = '\0';
			loopCnt++;
			while(*(strPhysicalName + loopCnt) == ' '){
				//スペースの連続を飛ばして次に来るのがLName
				strPhysicalName[loopCnt] = '\0';
				loopCnt++;
			}
			strLogicalName = (strPhysicalName + loopCnt);
			break;
		}
	}

	//変数がつながってしまうためスプリット 20140715 koyama
	for(loopCnt=0;loopCnt < strlen(strLogicalName) && strLogicalName[loopCnt] != '\0';loopCnt++){
		if(strLogicalName[loopCnt] == ' '){
			strLogicalName[loopCnt] = '\0';
			loopDiff=1;
			while(strLogicalName[loopCnt + loopDiff] == ' '){
				loopDiff++;
				if(loopDiff > 5){
					break;
				}
			}
			//1なら変わっていない,スペースが5文字以内なら論理名と物理名のつながりの範囲だとする
			if(loopDiff > 5){
				break;
			}
			strLogicalName = (strLogicalName + (loopCnt + loopDiff));
			loopCnt = 0;
		}
	}

	return strLogicalName;
}

// ファイルオープン
int DB_F_Open(char *strMode,char *iPName,char *lockMode,char *ioidlist,char *cArgc,...){
	char strSql[256]="";
	int ret=0,loopCnt=0;
	char *iLName;
	int targ_Table_num = DB_Table_num;
	int i=0;
	char strInnerMode[512]="";
	char *determinePName = NULL;
	int constVar=5;
	va_list list;
	int iArgc = atoi(cArgc);	//入力は必ず数字になっているはず

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_F_Open :%.30s",iPName);

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';

	//DBの初期設定
	if(getTargTable(DB_table,iPName,targ_Table_num) == -1){
		//初期化するときだけ配列の最後を＋
		DB_Table_num++;

		//変数がつながってしまうためスプリット 20140715 koyama
		//関数に変更 koyama 20161031
		iLName = setLogicalAndPhysicalTableName(iPName);
		//assignしてある時の対応
		getAssignPName(&determinePName,iPName);
		//DBオブジェクト変数の初期化(念のため)
		strcpy(DB_table[targ_Table_num].tablePName,determinePName);
		strcpy(DB_table[targ_Table_num].tableLName,iLName);
		//ここはassign後の名前を渡す
		setTableName(&DB_table[targ_Table_num],determinePName);
	}else{
		//既存で値が入っていたらそちらを使う
		targ_Table_num = getTargTable(DB_table,iPName,DB_Table_num);
		iLName = DB_table[targ_Table_num].tableLName;
	}
	//ここで作るDB_Tableオブジェクトを初期化
	InitDB_Table(&DB_table[targ_Table_num]);

	strcpy(strInnerMode,strMode);
	DB_table[targ_Table_num].sharedStatus[1] = lockMode[0];
	//何も入れられてなければSharedでセットしたことにする(上でスペースが入る時)
	if(DB_table[targ_Table_num].sharedStatus[1] == 0x20){
		DB_table[targ_Table_num].sharedStatus[1] = 'S';
	}
	//後ろから見た最初のスペースに何か指定があれば
	setFileStatus(strInnerMode,&DB_table[targ_Table_num]);
	//スペースで2つめが指定してあることもあるので
	setFileStatus(strInnerMode,&DB_table[targ_Table_num]);
	//最大ORGANIZATION,ACCESS-MODE,OPEN-MODE
	setFileStatus(strInnerMode,&DB_table[targ_Table_num]);

	if( strncmp(strInnerMode,"OUTPUT",strlen("OUTPUT"))==0){
		//INPUTとかぶるので二文字目で初期化
		DB_Trunc(DB_table[targ_Table_num].tablePName);
	}

	//SELECT 用変数の初期化
	DB_table[targ_Table_num].strWhere = malloc(sizeof(DBWhere));
	initDBWhere(DB_table[targ_Table_num].strWhere);

	//argPointの内容が配列
	va_start( list, cArgc );
	if(iArgc > 0 && cob_call_params > 3){
		//va_listを受け取るよう変数
		char *subscript;

		//ここに入るときはkeyがあるファイル
		DB_table[targ_Table_num].strWhere->flg *= DB_WHERE_INX;
		if(getFileStatusOrgnization(&DB_table[targ_Table_num]) == '\0'){
			//ファイル情報のステータスが入れられていないことがあるので
			strcpy(strInnerMode,"INDEXED");
			if(strstr(strMode,"DYNAMIC") == NULL && strstr(strMode,"SEQUENTIAL")){
				strcat(strInnerMode," RANDOM");
				//RANDOMを先に切り取ってもらう
				setFileStatus(strInnerMode,&DB_table[targ_Table_num]);
			}
			setFileStatus(strInnerMode,&DB_table[targ_Table_num]);
		}

		for(i = 0;i < (iArgc * 2); i++){
			subscript = va_arg( list, char* );
			if((i % 2) == 0){
				//奇数のとき
				switch((int)(i / 2)){
					case 0:
						strcpy(DB_table[targ_Table_num].key1Name,subscript);
						break;
					case 1:
						strcpy(DB_table[targ_Table_num].key2Name,subscript);
						break;
					case 2:
						strcpy(DB_table[targ_Table_num].key3Name,subscript);
						break;
				}
			}else{
				//偶数のとき
				switch((int)(i / 2)){
					case 0:
						DB_table[targ_Table_num].key1 = subscript;
						// TODO:Keyを一度どこかで初期化しないと正常に動かせない
						DB_table[targ_Table_num].key1Len = varLength(constVar + (i+1));
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key1,' ',DB_table[targ_Table_num].key1Len);
						break;
					case 1:
						DB_table[targ_Table_num].key2 = subscript;
						DB_table[targ_Table_num].key2Len = varLength(constVar + (i+1));
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key2,' ',DB_table[targ_Table_num].key2Len);
						break;
					case 2:
						DB_table[targ_Table_num].key3 = subscript;
						DB_table[targ_Table_num].key3Len = varLength(constVar + (i+1));
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key3,' ',DB_table[targ_Table_num].key3Len);
						break;
				}
			}
		}
		va_end( list );

		//開いた時にOrderの設定をしておく
		if(setOrderSql(&DB_table[targ_Table_num])){
			cob_runtime_error(" Error C [%02d]: %s Coudn't open table [%s] ",55,map_source_func,DB_table[targ_Table_num].tableLName);
			mysql_failure();
			ret = 1;
			return ret;
		}
	}

	//取得ポインタ用文字列を初期化(COBOLからくる文字列なので文字数初期化しておけばOK)
	memset(( void * )ioidlist , ( int )'\0', sizeof(char) * strlen(ioidlist));
	//ポインタ位置を0にしておく
	move_to_cob_for_bin(ioidlist,"0",strlen("0"));

	//lockModeの確認
	if( lockMode[0] != ' ' && lockMode[0] != 'E' && lockMode[0] != 'P'  && lockMode[0] != 'S'){
		ret = 1;
		cob_runtime_error(" Error lockMode Nothing : %s,table status:%s",DB_table[targ_Table_num].tablePName,DB_table[targ_Table_num].accessStatus);
		return ret;
	}


	// 管理テーブルの更新(対象レコードのロック)
	ret = M_LOCK(DB_table[targ_Table_num].tablePName);

	//ステータスの格納
	setTableStatus(&DB_table[targ_Table_num],'O',ret);

	//ひとつテーブルを開くごとに+1
	targ_Table_num++;


	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

// ファイルクローズ
int DB_F_Close(char *ioidlist,char *iPName){
	int intCurTableNum = 0;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_F_Close ");

	if(cob_call_params != 2){
		cob_runtime_error(" Error [%02d]: DB_F_Close not match %s ",31,local_server_time(strTime));
		exit(1);
	}

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	memset(( void * )ioidlist , ( int )'\0', strlen( ioidlist ));

	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);
	if(intCurTableNum < 0){
		// FileをOPENせずに開くようなケースも存在,
		// その時は内部的に開いた時に作る変数が無いので処理なし(一応エラーリターン) comment koyama 20170808
		// cob_runtime_error(" Error C [%02d]: %s Table is not found [%.20s] ",99,map_source_func,iPName);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}
	//Bufferに残っているものを一掃
	checkInsertBuffer(&DB_table[intCurTableNum]);
	checkUpdateBuffer(&DB_table[intCurTableNum]);
	checkDeleteBuffer(&DB_table[intCurTableNum]);
	//中身を使わなくなるので初期化
	destroyDBReaded(&DB_table[intCurTableNum]);

	// 管理テーブルの更新(対象レコードのアンロック)
	M_UNLOCK(DB_table[intCurTableNum].tablePName,__func__,__LINE__);
	memset(DB_table[intCurTableNum].sharedStatus,'\0',STAT_LENGTH);
	DB_table[intCurTableNum].sharedStatus[0] = 'C';

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

// コネクション開始
int DB_Open(){
	//MySql接続用
	MYSQL_ROW row;
	int ret =0;
	char strSql[256];

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Open ");

	mysqlConn = mysql_init(NULL);

	if (!mysql_real_connect(mysqlConn, DB_db_server, DB_db_user, DB_db_password, DB_db_database, DB_db_port, NULL, 0)) {
		cob_runtime_error(" Error [%02d]:db_connect server :%s user:%s pass:%s database:%s ",01, DB_db_server,DB_db_user,DB_db_password,DB_db_database);
		//接続不可
		//ファイル状態キーの初期化
		*(DB_filestat + 0) = '4';
		*(DB_filestat + 1) = '1';
		ret = 1;
	}else{
		//AutoCommit Off
		ret = mysql_autocommit(mysqlConn, DB_intAutocommit);
		//接続可
		ret = 0;
	}
	Uid = M_UID();
	// 管理テーブル用のコネクションスタート
	ret = M_OPEN();

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

// コネクションクローズ
int DB_Close(){
	int ret =0;
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Close ");

	// 管理テーブル用のコネクションエンド
	M_CLOSE();

	if(mysqlConn != NULL){
		//DB接続の切断
		mysql_close(mysqlConn);
	}
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

// RDBファイルの条件指定を削除
int DB_Scratch(char *iPName){
	int rc=0,loopCnt=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Scratch :%.30s",iPName);

	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//RDBであるかの判断
	if((DB_table[intCurTableNum].strWhere->flg % DB_WHERE_RDB) != 0){
		return 1;
	}

	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';

	//先読みのデータが有れば消しておく
	destroyDBReaded(&DB_table[intCurTableNum]);
	//Where句を削除
	memset(DB_table[intCurTableNum].strWhere->where,'\0',DB_table[intCurTableNum].strWhere->where_size);
	DB_table[intCurTableNum].strWhere->where_size = 0;
	DB_table[intCurTableNum].strWhere->rdbReaded=0;
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//ファイルの中身削除
int DB_Trunc(char *iPName){
	char strSql[256] = "";
	int rc=0,loopCnt=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Trunc :%.30s",iPName);

	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);

	//関数でのDB操作のはじめでtrunsaction
	DB_Transaction(mysqlConn);
	sprintf(strSql,"/*  pg_name %s : DB_Delete %s */ TRUNCATE TABLE  ",source_file_name,local_server_time(strTime));
	strcat(strSql,"`");
	strcat(strSql,DB_table[intCurTableNum].tableName);
	strcat(strSql,"`;");
	rc = mysql_query(mysqlConn, strSql);

	if(rc){
		mysql_failure();
		return 1;
	}
	//ここまで正常に終了していたらCOMMIT
	DB_Commit(mysqlConn);

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//検索Key(Start対応)設定
int DB_Start(char *iPName,char *iLabel,char *iMark,char *iValue){
	int ret =1;
	int sqlstrLen = 0,fLen = 0,dataLen=0,labelIsPacked=0;
	char *iLName;
	char strKeyMid[64] = "";
	char strData[1024] = "";
	char strSql[2048] = "";
	char cpyIValue[1024]="";
	int bufferedFlg = 0;               //同じテーブルのbufferに残っている時処理
	MYSQL_ROW	res;
	MYSQL_RES	 *result;
	//tempの変数
	char strTemp[20]="";
	int intTemp = 0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Start :%.30s",iPName);

	intCurTableNum = getTargTable(DB_table,iPName,DB_Table_num);
	iLName = DB_table[intCurTableNum].tableLName;

	//関数のはじめでtrunsaction
	DB_Transaction(mysqlConn);
	if(strlen(getFieldSpecified(iLName,iLabel,strKeyMid)) == 0){
		mysql_failure();
		cob_runtime_error(" Error C [%02d]: %s Coudn't get Field Status [%s] ",99,map_source_func,iLabel);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	//transaction後に現在の同じ対象のbufferの中身を一掃 add koyama 20170829
	bufferedFlg += checkInsertBuffer(&DB_table[intCurTableNum]);
	bufferedFlg += checkUpdateBuffer(&DB_table[intCurTableNum]);
	bufferedFlg += checkDeleteBuffer(&DB_table[intCurTableNum]);

	//key1の名前が一緒だったら以降それを使う
	if(strcmp(iLabel,DB_table[intCurTableNum].key1Name)==0){
		DB_table[intCurTableNum].key1Use =1;
		DB_table[intCurTableNum].key2Use =0;
		DB_table[intCurTableNum].key3Use =0;
		//条件が変わるのでOrderの設定をしておく
		if(setOrderSql(&DB_table[intCurTableNum])){
			cob_runtime_error(" Error C [%02d]: %s Coudn't open table [%s] ",55,map_source_func,DB_table[intCurTableNum].tableLName);
			mysql_failure();
			//共有変数を元に戻す
			unsetCommonFunctionName(map_source_func,strStack);
			return ret;
		}
	}
	//key2の名前が一緒だったら以降それを使う
	if(strcmp(iLabel,DB_table[intCurTableNum].key2Name)==0){
		DB_table[intCurTableNum].key1Use =0;
		DB_table[intCurTableNum].key2Use =1;
		DB_table[intCurTableNum].key3Use =0;
		//条件が変わるのでOrderの設定をしておく
		if(setOrderSql(&DB_table[intCurTableNum])){
			cob_runtime_error(" Error C [%02d]: %s Coudn't open table [%s] ",55,map_source_func,DB_table[intCurTableNum].tableLName);
			mysql_failure();
			//共有変数を元に戻す
			unsetCommonFunctionName(map_source_func,strStack);
			return ret;
		}
	}
	//key3の名前が一緒だったら以降それを使う
	if(strcmp(iLabel,DB_table[intCurTableNum].key3Name)==0){
		DB_table[intCurTableNum].key1Use =0;
		DB_table[intCurTableNum].key2Use =0;
		DB_table[intCurTableNum].key3Use =1;
		//条件が変わるのでOrderの設定をしておく
		if(setOrderSql(&DB_table[intCurTableNum])){
			cob_runtime_error(" Error C [%02d]: %s Coudn't open table [%s] ",55,map_source_func,DB_table[intCurTableNum].tableLName);
			mysql_failure();
			//共有変数を元に戻す
			unsetCommonFunctionName(map_source_func,strStack);
			return ret;
		}
	}

	//最初の文字列をセットして
	sprintf(strSql,"/* pg_name %s :DB_Start %s */ SELECT ID FROM ",source_file_name,local_server_time(strTime));
	sqlstrLen += strlen(strSql);
	//テーブル名の追加
	sqlstrLen += setTableNameAfterStr(strSql,&DB_table[intCurTableNum],'S');

	mystrncat( strSql, sqlstrLen," WHERE CAST(",strlen(" WHERE CAST("));
	sqlstrLen += strlen(" WHERE CAST(");
	//packedを判定 ->この判定結果は後でも使うので一度判定して使いまわし
	//MID -> chCharMapだと、文字数が増えているので
	intTemp = strlen(strKeyMid);
	if(strlen(setFieldSpecChrConvNecessary(DB_table[intCurTableNum].tableLName,iLabel,strKeyMid)) > intTemp){
		labelIsPacked=1;
	}
	if(labelIsPacked == 0){
		//MID(*,*)をつなぐ
		mystrncat( strSql, sqlstrLen," ",strlen(" "));
		sqlstrLen += strlen(" ");
		mystrncat( strSql, sqlstrLen,strKeyMid,strlen(strKeyMid));
		sqlstrLen += strlen(strKeyMid);
		mystrncat( strSql, sqlstrLen," ",strlen(" "));
		sqlstrLen += strlen(" ");
	}else{
		//chCharMap(*,*)をつなぐ
		mystrncat( strSql, sqlstrLen,strKeyMid,strlen(strKeyMid));
		sqlstrLen += strlen(strKeyMid);
	}
	mystrncat( strSql, sqlstrLen,"as binary)  ",strlen("as binary)  "));
	sqlstrLen += strlen("as binary)  ");

	//<
	if(strchr(iMark,'<') != 0 && strchr(iMark,'N') == 0
		&& strchr(iMark,'=') == 0
		&& strncmp(strchr(iMark,'<'),"<",strlen("<")) == 0){
		mystrncat( strSql, sqlstrLen," < ",strlen(" < "));
		sqlstrLen += strlen(" < ");
	}

	//>
	if(strchr(iMark,'>') != 0 && strchr(iMark,'N') == 0
		&& strchr(iMark,'=') == 0
		&& strncmp(strchr(iMark,'>'),">",strlen(">")) == 0){
		mystrncat( strSql, sqlstrLen," > ",strlen(" > "));
		sqlstrLen += strlen(" > ");
	}

	//>=
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N')	,"NOT <",strlen("NOT <")) == 0){
		mystrncat( strSql, sqlstrLen," >= ",strlen(" >= "));
		sqlstrLen += strlen(" >= ");
	}
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N'),"NOT LESS",strlen("NOT LESS")) == 0){
		mystrncat( strSql, sqlstrLen," >= ",strlen(" >= "));
		sqlstrLen += strlen(" >= ");
	}
	//>=
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N'),"NOT >",strlen("NOT >")) == 0){
		mystrncat( strSql, sqlstrLen," <= ",strlen(" <= "));
		sqlstrLen += strlen(" <= ");
	}

	//MID(ITEM,*,*)の,から閉じカッコまでを取得
	strncat(strTemp,(strrchr(strKeyMid,',') + 1),(int)(strchr(strKeyMid,')') - (strrchr(strKeyMid,',') + 1)));
	fLen = atoi(strTemp);
	//初期化
	*strData = 0x00;
	dataLen = 0;
	//シングルクォーテーションなどをエスケープする
	dataEscapeCopy(iValue,cpyIValue,fLen);
	//packedを判定
	if(labelIsPacked == 0){
		strncpy(strData," '",strlen(" '"));
		dataLen += strlen(strData);
		//文字列が含まれていてもwildCardにしなくていいのか
		mystrncat( strData, dataLen,cpyIValue,fLen);
		dataLen += fLen;
		mystrncat( strData, dataLen,"' ",strlen("' "));
		dataLen += strlen("' ");
	}else{
		//ここはデータを直接埋めるのでchCharMap
		strncpy(strData,"chCharMap('",strlen("chCharMap('"));
		dataLen += strlen(strData);
		//wildCardで埋めるのが正しいか疑問？ 対象より下を指すのでWildCard無し upd koyama 20160815
		mystrncat( strData, dataLen,cpyIValue,fLen);
		dataLen += fLen;
		mystrncat( strData, dataLen,"') ",strlen("') "));
		dataLen += strlen("') ");
	}

	mystrncat( strSql, sqlstrLen," CAST(",strlen(" CAST("));
	sqlstrLen += strlen(" CAST(");

	mystrncat( strSql, sqlstrLen,strData, dataLen);
	sqlstrLen += dataLen;

	mystrncat( strSql, sqlstrLen," as binary); ",strlen(" as binary); "));
	sqlstrLen += strlen(" as binary); ");


	if(mysql_real_query(mysqlConn, strSql, sqlstrLen) != 0){
		mysql_failure();
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	//取得できなかった時のため初期化しなおす
	DB_table[intCurTableNum].prevPoint = 0;
	//先読みのものをリセットする
	destroyDBReaded(&DB_table[intCurTableNum]);

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		if(res != NULL){
			//Readするときに ID > prevPointになる
			//STARTの後にReadを書くと次の行を読むことになるので-1
			DB_table[intCurTableNum].prevPoint = atoi(res[0]) - 1;

			//正しく読めたらreturnを0にセット
			ret = 0;
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	//ここまで正常に終了していたらCOMMIT
	DB_Commit(mysqlConn);
	//ステータスの格納
	setTableStatus(&DB_table[intCurTableNum],'S',ret);

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

//トランザクション開始
int DB_Transaction(MYSQL *mysqlDBStruct){
	int ret =0;
	char strSql[1024]="";

	//COBOLから実行されたCOMMITは一旦無視
	if(cob_call_params == 0){
		mysqlDBStruct = mysqlConn;
	}else if(cob_call_params > 1){
		return 1;
	}

	sprintf(strSql," /* pg_name %s user_name %s : DB_Transaction %s */ START TRANSACTION; ",source_file_name,source_user_name,local_server_time(strTime));
	//失敗しても気にしない
	ret = mysql_query(mysqlDBStruct,strSql );
	return ret;
}

// ロールバック
int DB_Rollback(MYSQL *mysqlDBStruct){
	int ret =0;
	char strSql[1024]="";

	//COBOLから実行されたCOMMITは一旦無視
	if(cob_call_params == 0){
		mysqlDBStruct = mysqlConn;
	}else if(cob_call_params > 1){
		return 1;
	}

	ret =mysql_rollback(mysqlDBStruct);

	return ret;
}

//コミット
int DB_Commit(MYSQL *mysqlDBStruct){
	int ret =0;
	char strSql[1024]="";


	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"DB_Commit :%.30s",mysqlDBStruct);

	//COBOLから実行されたCOMMITは一旦無視 COBOLからのCommitはパラメータなし,Read等にはあるため
	if(cob_call_params == 0){
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	ret = mysql_commit(mysqlDBStruct);
	if(ret != 0){
		mysql_failure();
	}
	//ステータスの格納 Commitでsutatusを入れるとどこから来たのかわからなくなる
	// setTableStatus(&DB_table[intCurTableNum],'C',ret);
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

///////////////////////////////////////////////////////////
////////////////// 未使用
///////////////////////////////////////////////////////////
////テーブルに対する最大サイズ(レコードサイズ)を取得
////引数:テーブル名
int recordLength(char *tableName){
	int ret =0;
	char strSql[2048] = "";
	char tName[256] = "";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"recordLength :%.30s",tableName);

	strcpy(tName,tableName);
	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	strcat(strSql,"SELECT max(Size) FROM ");
	strcat(strSql,DB_db_item_tbl);
	strcat(strSql," WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND S_Point = 1");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		ret = 1;
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ret;
	}

	if(result = mysql_store_result(mysqlConn)){
		res=mysql_fetch_row(result);
		//データが取れないときNULLが帰ってくるので
		if(res[0] == NULL){
			cob_runtime_error(" Error C [%02d]:not found table status  %s ",41, tName);
			exit(1);
		}
		ret = atoi(res[0]);
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return ret;
}

//ソートしていないIDをソートしたIDに振替
//author:n.koyama
//date  :20150825
int getIntConvertOrigPtoOrderP(DB_TABLE_OBJ *targetTable){
	int retVal = 0;
	char tableName[512]="";
	char strId[64]="";
	char strSql[2048]="";
	MYSQL_ROW	res;
	MYSQL_RES	 *result;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getIntConvertOrigPtoOrderP :%.30s ",targetTable->tableName);

	sprintf(strId,"%d",targetTable->prevPoint);
	strcat(strSql,"SELECT ");
	strcat(strSql,"id");
	strcat(strSql," FROM ");
	//結果の長さはいらないので捨てる
	setTableNameAfterStr(strSql, targetTable,'S');
	strcat(strSql," WHERE orig_id = \"");
	strcat(strSql,strId);
	strcat(strSql,"\" ; ");

	if(mysql_query(mysqlConn, strSql) != 0){
		mysql_failure();
		//共有変数を元に戻す
		memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
		return retVal;
	}

	if(result = mysql_use_result(mysqlConn)){
		res=mysql_fetch_row(result);
		if(res != 0){
			retVal = atoi(res[0]);
		}
		mysql_free_result(result);
	}else{
		mysql_failure();
	}

	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return retVal;
}
