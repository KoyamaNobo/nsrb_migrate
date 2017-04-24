#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <mysql/mysql.h>
#include <libcob.h>
#include <stdlib.h>
#include <time.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/

#include "user/confheader.h"     /*DB設定ファイル*/

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
#define STAT_LENGTH 2

// MYSQL *connect;         //MySqlConnectin
//DBのタグ名
#ifndef CONF_DB_TAG
#define CONF_DB_TAG
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
#endif
 

//DBの設定(長さ制限の倍まで取れるようにしておく(mysqlの仕様を基準とする))
char DB_db_server[128];
char DB_db_user[34];
char DB_db_password[130];
char DB_db_database[130];

//fromでつながる変数がどのような形をしているか
//OCCURSで定義されていることがあるので
typedef struct db_object{
	char tableName[31];             // 対象のテーブル名
	char tablePName[31];            // 対象のテーブル名(COBOL)
	char tableLName[31];            // 対象の識別子
	char accessStatus[2];           // 最後に実行した処理のステータスを格納([0]実行した関数の頭文字 [1]成否 成'0' 否'1')
	char sharedStatus[2];           // 共有排他等の設定(O:Opened,C:Closed;\0:初期値,E:排他,S:共有,P:ReadOnly)
	int  prevPoint;                 // 前回読んだID(ファイルポインタ相当)
	char *key1;                     // key1のポインタ
	char key1Name[TNAME_LENGTH];              // key1の変数名
	int  key1Len;                   // key1の長さ
	char *key2;                     // key2のポインタ*/
	char key2Name[TNAME_LENGTH];              // key2の変数名
	int  key2Len;                   // key2の長さ
	char *key3;                     // key3のポインタ*/
	char key3Name[TNAME_LENGTH];              // key3の変数名
	int  key3Len;                   // key3の長さ
	char *strWhere;                 //Where句を格納 初期値nullポインタ 20141002
	int  strWhereLen;               //Where句の文字列長
} DB_TABLE_OBJ;

//#ifndef CONF_PATH 
//#define CONF_PATH
//const char* CONF_FLIEPATH = "./conf.xml";
//const char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
//#endif

static int (*func)(char *errno, const char *errmsg); 

MYSQL            sql, *mysql=&sql;
static int       errout;
DB_TABLE_OBJ  DB_table[64]={{"","","","","",0,NULL,"",0,NULL,"",0,NULL,"",0}};                  //Tableのオブジェクト
int DB_Table_num = 0;                               //Tableの有効数(次に作るオブジェクトの添え字番号)
char *DB_filestat;
/* ----  アクセスプログラム共通関数  --------  */

/* 出口処理 */
void err_exit(int rc)
{
/*   正常終了 rc = 0  ではそのまま復帰する                                       */
/*   異常終了 rc != 0 では初期設定(MySQL_init)で設定されたエラー出口にしたがって */
/*   処理を分ける:                                                               */
/*     errout = 1:  エラーメッセージを stderr に出力して終了する                 */
/*     errout = 2:  エラーは呼び出し元で処理されるものとしてそのまま復帰する　　 */
/*     errout = 3:  エラーはユーザ作成のプログラムで処理されるものとして、       */
/*　　　　　　　　　errno, errmsgをパラメタとして指定されたプログラムを呼ぶ。    */

    char errno[10];

    if( !rc ) return;

    switch(errout){
        case 1: 
             fprintf(stderr,"Error :%s\n", mysql_error(mysql));
             exit(1);
        case 2:
             break;
        case 3:
             sprintf(errno,"%d", mysql_errno(mysql));
             func(errno, mysql_error(mysql));
    }
    return;
}

/*
   strncatの代わりにmainの後ろにsubstrをつなぐ 
   strcatだと\0に対応できないので長さ分をmemcpy
   mainstr   :つなげる元の文字列ポインタ
   mainLength:元の文字列の最大長
   substr    :つなげる文字列のポインタ
*/
char *mystrncat(char *mainstr ,int mainLength ,char * substr,int subLength){
//	int i;
//	char *endPoint;
//	char ec = '\0';/*終端文字*/
//	endPoint = strchr(mainstr,ec);
	memcpy((mainstr + (mainLength)),substr,subLength);
}

//現在時間をフォーマット(YY/MM/dd hh:mm:ss)で返す
//in/out :retStr 返す対象文字列のポインタ
//author : koyama
char *local_server_time(char *retStr){
	time_t timer;
	struct tm *date;
	
	timer = time(NULL);
	date = localtime(&timer);
	strftime(retStr,strlen("00/00/00 00:00:00"),"%y/%m/%d %H:%M:%S",date);
	return retStr;
}

//mysqlエラー処理
int mysql_failure(){
	int ret =0;
	char strTime[] = "00/00/00 00:00:00";
	//エラー内容を出力
	fprintf(stderr,"Query Error %s: %s\n",local_server_time(strTime),mysql_error(mysql));
	
	//ロールバック
	ret = mysql_query(mysql, "ROLLBACK"); 
	
	//DB接続の切断
	//途中で接続を切ったらダメかも
//	mysql_close(mysql);
	
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
    int len = strlen(cob_dat);                      // data length in cob
    memset(cob_dat, ' ', min(len, strlen(dat)));                      // clear with spaces
    memcpy(cob_dat, dat, min(len, strlen(dat)));    // data copy
    return;
}

/* 文字列をcobolに転写する関数(binary) */
void move_to_cob_for_bin(char *cob_dat, const char *dat,long len){
    memset(cob_dat, ' ', len);                      // clear with spaces
    memcpy(cob_dat, dat, len);    // data copy
    return;
}

/* エラー番号 */
void MySQL_errno(char *errno){
    char buf[10];
    sprintf(buf,"%d", mysql_errno(mysql));
    move_to_cob(errno, buf);
    return;
}

/* エラーメッセージ*/
void MySQL_error(char *errmsg){
    move_to_cob(errmsg, mysql_error(mysql));
    return;
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
    char    *tk;
    int     cnt = 0;

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

//テーブルオブジェクト
int getTargTable(DB_TABLE_OBJ *arrTable,char *iPName,int intTableMax){
	int i = 0;
	//ターゲットとなるテーブルオブジェクトの検索
	for(i=0;i < intTableMax;i++){
		if(strcmp(arrTable[i].tablePName,iPName) == 0){
			return i;
		}
	}
	//ターゲットが見つからなかったら-1
	return -1;
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

//テーブルに対する最大サイズ(レコードサイズ)を取得
//引数:テーブル名
int record_Length(char *tableName){
	int ret =0;
	char sqlstr[2048] = "";
	char tName[256] = "";
	MYSQL_ROW	res;
	MYSQL_RES 	*result;
	
	strcpy(tName,tableName);
	
	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		return ret;
	}
	
	strcat(sqlstr,"SELECT max(Size) FROM M_ITEMELEMENT WHERE TableName = \"");
	strcat(sqlstr,tName);
	strcat(sqlstr,"\" AND S_Point = 1");
	
	if(mysql_query(mysql, sqlstr) != 0){
		mysql_failure();
		ret = 1;
		return ret;
	}
	
	if(result = mysql_store_result(mysql)){
		res=mysql_fetch_row(result);
		//データが取れないときNULLが帰ってくるので
		if(res[0] == NULL){
			fprintf(stderr,"get Record Error : not found table status %s \n",tName);
			exit(1);
		}
		
		ret = atoi(res[0]);
	}
	mysql_free_result(result);
	return ret;
}


//
//
//in:
//  tableName : テーブルの論理名
//  fieldName : フィールド名
//out:
//  
int getFieldLength(char *tableName,char *fieldName){
	int ret =0;
	char sqlstr[1024] = "";
	char tName[256] = "";
	char fName[256] = "";
	MYSQL_ROW	res;
	MYSQL_RES 	*result;
	
	strcpy(tName,tableName);
	strcpy(fName,fieldName);
	
	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		return ret;
	}
	
	strcat(sqlstr,"SELECT max(Size) FROM M_ITEMELEMENT WHERE TableName = \"");
	strcat(sqlstr,tName);
	strcat(sqlstr,"\" AND Label = \"");
	strcat(sqlstr,fName);
	strcat(sqlstr,"\"; ");
	
//	if(mysql_real_query(mysql, sqlstr,strlen(sqlstr)) != 0){
	if(mysql_query(mysql, sqlstr) != 0){
		mysql_failure();
		ret = 0;
		return ret;
	}
	
	if(result = mysql_store_result(mysql)){
		res=mysql_fetch_row(result);
		if(*res != 0){
			ret = atoi(res[0]);
		}
	}
	mysql_free_result(result);
	return ret;
}

//第3引数にMID(?,?)を返す
//in : 
//  tName     : 対象のテーブル名
//  fName     : 対象のフィールド名
//  retStr    : MID(?,?)を入れるポインタ20以上の長さがないとエラーになるかも
char* getFieldSpecified(char *tName,char *fName,char *retStr){
	char strSql[500] = "";
	MYSQL_ROW	res;
	MYSQL_RES 	*result;
	
	strcat(strSql,"SELECT S_Point,size FROM M_ITEMELEMENT WHERE TableName = \"");
	strcat(strSql,tName);
	strcat(strSql,"\" AND Label = \"");
	strcat(strSql,fName);
	strcat(strSql,"\" ");
	
	if(mysql_query(mysql, strSql) != 0){
		mysql_failure();
		return retStr;
	}
	
	if(result = mysql_store_result(mysql)){
		strcpy(retStr,"MID(ITEM,");
		res=mysql_fetch_row(result);
		if(res != 0){
			strcpy((retStr + strlen(retStr)),res[0]);
			strcpy((retStr + strlen(retStr)),",");
			strcpy((retStr + strlen(retStr)),res[1]);
			strcpy((retStr + strlen(retStr)),")");
		}
			
	}
	
	mysql_free_result(result);
	
	return retStr;
}


/* ----  アクセスプログラムCOBOL呼び出し関数  --------  */

/* 設定ファイルの読み込み */
int DB_Initialize(char *esp){
	int ii;
	char size_e[1];
	
	//file status pointer set error status pointer 
	if(cob_call_params != 0){
		DB_filestat = esp;
	}else{
		DB_filestat = (char *)malloc(sizeof(size_e) * STAT_LENGTH);
		memset(DB_filestat,0x00,STAT_LENGTH);
	}
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	
	//ファイルネームを元にリーダポインタを作成
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(CONF_FLIEPATH);
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
			} else {
				return 1;
			}
		}
	}
	
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
	char size_e[1];
	int this_call_param=0,i = 0,intSub = 0;         //this_call_param:処理する可変変数の数
	int keyFlg=0,orderLen=0,whereLen=0;   //
	int orderCount=0,whereCount=0;        //Order Whereの箇所の引数の数
	char fieldName[48];                   //ループ3回ごとにフィールド名を
	char *tempSql;
	char *tempOrder;
	char *tempWhere;
	va_list list;
	
	tempSql = (char *)malloc(sizeof(size_e) * 2048);
	tempOrder = (char *)malloc(sizeof(size_e) * 1024);
	tempWhere = (char *)malloc(sizeof(size_e) * 1024);
	memset(tempSql,'\0',2048);
	memset(tempOrder,'\0',1024);
	memset(tempWhere,'\0',1024);
	
	//対象テーブルの検索
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	//テーブル名がなければ-RDBをつけて再検索
	if(intSub == -1){
		char tempTableName[31];
		strcpy(tempTableName,iPName);
		strcat(tempTableName,"-RDB");
		intSub = getTargTable(DB_table,tempTableName,DB_Table_num);
	}
	
	
	//処理する可変変数の数の取得
	this_call_param = cob_call_params - 1;
	va_start(list,iPName);
	for(i=0;i<this_call_param;i++){
		char *tmpKey = va_arg( list , char * );
		if(strncmp(tmpKey,"WHERE",strlen("WHERE")) == 0){
			keyFlg=0;                                        //WHEREのときはKeyではない
			strcpy(tempWhere,"(");                 //全体を()で囲む,初期化の代わり
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
					getFieldSpecified(DB_table[intSub].tableLName,tmpKey,strTemp);
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
					//条件のためにいったん保存しておく
					strcpy(fieldName,tmpKey);
					getFieldSpecified(DB_table[intSub].tableLName,tmpKey,strTemp);
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
						
					}
					break;
				case 2:
					//2のときはデータ
					tmpfieldLength = getFieldLength(DB_table[intSub].tableLName,fieldName);
					data = (char *)malloc(sizeof(size_e) * (tmpfieldLength + 1) );
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
					whereLen += getFieldLength(DB_table[intSub].tableLName,fieldName);
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
	
	mystrncat( tempSql, 0,tempWhere,whereLen);
	mystrncat( tempSql, whereLen,tempOrder,orderLen);
	
	DB_table[intSub].strWhere =  (char *)malloc(sizeof(size_e) * (whereLen + orderLen + 1) );
	memset(DB_table[intSub].strWhere,0x00,(whereLen + orderLen + 1));
	memcpy(DB_table[intSub].strWhere, tempSql, (whereLen +orderLen));
	DB_table[intSub].strWhereLen = (whereLen +orderLen);
	free(tempSql);
	tempSql = NULL;
	free(tempOrder);
	tempOrder = NULL;
	free(tempWhere);
	tempWhere = NULL;
	
	return ret;
}

//
//DBへのリード
//in:
//	iMode   : 読んでくる方法の指定
//	iPName  :
//	lockMode:UNLOCKの指定があるかどうか
int DB_Read(char *iMode,char *iPName,char *oItem,char *lockMode,...){
	int ret =0;
	char *iLName;
	int intSub = 0,i = 0,sqlstrLen=0;
	int const_call_param = 4;
	char Sql[1024] = "SELECT ID,ITEM FROM ";
	char strTemp[30] = "";                     //IDを格納するために一時的に使用
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	
	//lockModeの確認
	if( lockMode[0] != ' ' && lockMode[0] != 'U' ){
		mysql_failure();
		ret = 1;
		fprintf(stderr,"Read Error : Lock mode failed %s,table status:%s \n",DB_table[intSub].tablePName,DB_table[intSub].accessStatus);
		return ret;
	}
	
	if(strlen(DB_table[intSub].tableName) == 0 ){
		mysql_failure();
		ret = 1;
		fprintf(stderr,"Read Error : not found table name. status %s  \n",DB_table[intSub].accessStatus);
		return ret;
	}
	
	//テーブル名の追加
	sqlstrLen += strlen(Sql);
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//
	mystrncat( Sql,sqlstrLen,DB_table[intSub].tableName, strlen(DB_table[intSub].tableName));
	sqlstrLen += strlen(DB_table[intSub].tableName);
	
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//INVALID KEYのとき(RUNDOM ACCESSになるのでポインタを消去)
	if(strchr(iMode,'I') != 0){
		if(strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0){
			DB_table[intSub].prevPoint = 0;
		}
	}
	
	//WHERE 句の指定
	mystrncat( Sql, sqlstrLen," WHERE ",strlen(" WHERE "));
	sqlstrLen += strlen(" WHERE ");
	
	//RDBのときはID検索は行わない
	if(DB_table[intSub].strWhereLen == 0){
		mystrncat( Sql, sqlstrLen," ID > ",strlen(" ID > "));
		sqlstrLen += strlen(" ID > ");
		
		//IDを文字列化
		sprintf(strTemp,"%d",DB_table[intSub].prevPoint);
		mystrncat( Sql, sqlstrLen,strTemp,strlen(strTemp));
		sqlstrLen += strlen(strTemp);
		
		mystrncat( Sql, sqlstrLen," ",strlen(" "));
		sqlstrLen += strlen(" ");
	}
	
	//モードによるWHERE句の指定(INVALID KEYのときはWHERE でキー値を指定)
	if(strchr(iMode,'I') != 0){
		
		//INVALIDのとき条件追加
		if(strncmp(strchr(iMode,'I'),"INVALID",strlen("INVALID")) == 0){
			//KEY値指定のときがあるため
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
					if(DB_table[intSub].key1 == tmpKey){
						key1exist = 1;
					}
					if(DB_table[intSub].key2 == tmpKey){
						key2exist = 1;
					}
					if(DB_table[intSub].key3 == tmpKey){
						key3exist = 1;
					}
				}
				va_end( list );
			}else{
				//key1を使うか
				if(DB_table[intSub].key1Len > 0){
					key1exist = 1;
				}
				//key2を使うか
				if(key1exist == 0 && DB_table[intSub].key2Len > 0){
					key2exist = 1;
				}
				//key3を使うか
				if((key1exist == 0 && key2exist == 0  ) && DB_table[intSub].key3Len > 0){
					key3exist = 1;
				}
			}
			
			//ANDでキーの判定
			//キーの値の1番目の長さが0以上なら
			if(key1exist > 0){
				char strFeildSpec[64] = "";
				mystrncat( Sql, sqlstrLen," AND ",strlen(" AND "));
				sqlstrLen += strlen(" AND ");
				
				//フィールド名からMID(?,?)をとってくる
				getFieldSpecified(DB_table[intSub].tableLName,DB_table[intSub].key1Name,strFeildSpec);
				mystrncat( Sql, sqlstrLen,strFeildSpec,strlen(strFeildSpec));
				sqlstrLen += strlen(strFeildSpec);
				
				mystrncat( Sql, sqlstrLen," = '",strlen(" = '"));
				sqlstrLen += strlen(" = '");

				//値を埋める
				mystrncat( Sql, sqlstrLen,DB_table[intSub].key1,DB_table[intSub].key1Len);
				sqlstrLen += DB_table[intSub].key1Len;
				
				mystrncat( Sql, sqlstrLen,"' ",strlen("' "));
				sqlstrLen += strlen("' ");
			}
			
			//キーの値の2番目の長さが0以上なら
			if(key2exist > 0){
				char strFeildSpec[64] = "";
				mystrncat( Sql, sqlstrLen," AND ",strlen(" AND "));
				sqlstrLen += strlen(" AND ");
				
				getFieldSpecified(DB_table[intSub].tableLName,DB_table[intSub].key2Name,strFeildSpec);
				mystrncat( Sql, sqlstrLen, strFeildSpec,strlen(strFeildSpec));
				sqlstrLen += strlen(strFeildSpec);
				
				mystrncat( Sql, sqlstrLen," = '",strlen(" = '"));
				sqlstrLen += strlen(" = '");

				//値を埋める
				mystrncat( Sql, sqlstrLen,DB_table[intSub].key2,DB_table[intSub].key2Len);
				sqlstrLen += DB_table[intSub].key2Len;
				
				mystrncat( Sql, sqlstrLen,"' ",strlen("' "));
				sqlstrLen += strlen("' ");
			}
			
			//キーの値の3番目の長さが0以上なら
			if(key3exist > 0){
				char strFeildSpec[64] = "";
				mystrncat( Sql, sqlstrLen," AND ",strlen(" AND "));
				sqlstrLen += strlen(" AND ");
				
				getFieldSpecified(DB_table[intSub].tableLName,DB_table[intSub].key3Name,strFeildSpec);
				mystrncat( Sql, sqlstrLen, strFeildSpec,strlen(strFeildSpec));
				sqlstrLen += strlen(strFeildSpec);
				
				mystrncat( Sql, sqlstrLen," = '",strlen(" = '"));
				sqlstrLen += strlen(" = '");

				//値を埋める
				mystrncat( Sql, sqlstrLen,DB_table[intSub].key3,DB_table[intSub].key3Len);
				sqlstrLen += DB_table[intSub].key3Len;
				
				mystrncat( Sql, sqlstrLen,"' ",strlen("' "));
				sqlstrLen += strlen("' ");
			}
		}
		mystrncat( Sql, sqlstrLen, " LIMIT 1", strlen(" LIMIT 1"));
		sqlstrLen += strlen(" LIMIT 1");
	}else if(DB_table[intSub].strWhereLen > 0){
		//ここの条件にマッチするものは元のRDB
		//なければ0文字つなぐので問題なし
		mystrncat( Sql, sqlstrLen,DB_table[intSub].strWhere,DB_table[intSub].strWhereLen);
		sqlstrLen += DB_table[intSub].strWhereLen;
		mystrncat( Sql, sqlstrLen, " LIMIT ", strlen(" LIMIT "));
		sqlstrLen += strlen(" LIMIT ");
		//IDを文字列化
		sprintf(strTemp,"%d",DB_table[intSub].prevPoint);
		mystrncat( Sql, sqlstrLen,strTemp,strlen(strTemp));
		sqlstrLen += strlen(strTemp);
		mystrncat( Sql, sqlstrLen, ",1; ", strlen(",1; "));
		sqlstrLen += strlen(",1; ");
	}else{
		mystrncat( Sql, sqlstrLen, " LIMIT 1;", strlen(" LIMIT 1;"));
		sqlstrLen += strlen(" LIMIT 1;");
	}
	
	//queryの実行
	if(mysql_real_query(mysql, Sql, sqlstrLen) != 0){
		mysql_failure();
		ret = 1;
		//エラーコードで返す。(受け側で処理してないから意味ないけど。。。)
		return ret;
	}
	
	//----------------------------------------------------------------------------ここからデータ取得
	//ここから動きが違うので変数の追加
	MYSQL_ROW res;
	MYSQL_RES *result;
	int jj, maxcols;             //フィールドの数とそのループ変数
	unsigned long *f_length;     //フィールドの長さ(配列として受け取るのでポインタ宣言)
	
	result = mysql_store_result(mysql);
//	ret = result != NULL ? 0 : 1;
	if(result == 0){
		//mysqlのエラーを標準エラーに(なぜ最初4文字が\0か不明)
		fprintf(stderr,"Error :%s:\n",(mysql->net.last_error + 4));
		return 1;
	}
	
	res = mysql_fetch_row(result);
	//行がとれなければinvalid or at end
	if(res != NULL){
		f_length =  mysql_fetch_lengths(result);
		maxcols = min(cob_call_params, mysql_num_fields(result));
		for(jj=0; jj<maxcols; jj++){
			switch(jj){
				case 0:
					DB_table[intSub].prevPoint = atoi(res[jj]);
					break;
				case 1:
				//データの取得
					move_to_cob_for_bin(oItem, res[jj],f_length[1]);
					break;
				default:
					break;
			}
			//move_to_cob(va_arg(args, char *), res[jj]);
		}
		ret = 0;
	}else{
		//ファイル状態キーの変更
		*(DB_filestat + 0) = '1';
		*(DB_filestat + 1) = '0';
		ret = 1;
	}
	mysql_free_result(result);
	
	//ステータスの格納
	setTableStatus(&DB_table[intSub],'R',ret);
	
	return ret;
}



/* 変更関数 */
int DB_Update(char *iPName,char *iLName,char *iItem){
	int ret = 0;
    int ii = 0, cnt = 0,loopCnt = 0,intSub = 0;
	char Sql[2048] = "UPDATE ";
	char strKeyTemp[30] = "";
	int sqlstrLen = 0;
	char size_e[1];
	char *tmpItem;
	int itemLen=0;
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	
	//前の処理でReadが成功していないときはエラー
	if(strcmp(DB_table[intSub].accessStatus,"R1") == 0){
		ret = 1;
		fprintf(stderr,"update Error : %s,table status:%s",DB_table[intSub].tablePName,DB_table[intSub].accessStatus);
		return ret;
	}
	
	//文字をエスケープしながらコピー
	//メモリ確保はエスケープを考えて大きめに
	tmpItem = (char *)malloc(sizeof(size_e) * (record_Length(iLName) * 2));
	memset(tmpItem,'\0',(record_Length(iLName) * 2));
	if(tmpItem == NULL){
		ret = 1;
		fprintf(stderr,"update Error : memory not alloc ");
		return ret;
	}
	itemLen = dataEscapeCopy(iItem,tmpItem,record_Length(iLName));
	
	//テーブル名指定
	sqlstrLen += strlen(Sql);
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//\0に置き換えている
	mystrncat( Sql,sqlstrLen,DB_table[intSub].tableName, strlen(DB_table[intSub].tableName) );
	sqlstrLen += strlen(DB_table[intSub].tableName);
	
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//更新項目指定
	mystrncat( Sql, sqlstrLen," SET ITEM = \"",strlen(" SET ITEM = \""));
	sqlstrLen += strlen(" SET ITEM = \"");
	
	//\0に置き換えている
	mystrncat( Sql, sqlstrLen, tmpItem,itemLen);
	sqlstrLen += itemLen;
	
	mystrncat( Sql, sqlstrLen, "\"",strlen("\""));
	sqlstrLen += strlen("\"");
	
	//検索key設定
	
	//Readした行に対して行えばいいのでこれでいい
	//IDを文字列化
	sprintf(strKeyTemp,"%d",DB_table[intSub].prevPoint);
	mystrncat( Sql, sqlstrLen, " WHERE ID = ",strlen(" WHERE ID = "));
	sqlstrLen += strlen(" WHERE ID = ");
	mystrncat( Sql, sqlstrLen, strKeyTemp,strlen(strKeyTemp));
	sqlstrLen += strlen(strKeyTemp);
	
	//SQL実行
	//binaryを含むのでこちらに書き換えmysql_real_query() 20140718 koyama
	if(mysql_real_query(mysql, Sql, sqlstrLen) != 0){
		mysql_failure();
		ret = 1;
	}
	
	//終了処理
	free(tmpItem);
	tmpItem = NULL;
	//ステータスの格納
	setTableStatus(&DB_table[intSub],'U',ret);
	
	return ret;
}

/* 追加関数 */
int DB_Insert(char *iPName,char *iLName,char *iItem){
	int ret = 0,intSub = 0;
    int ii = 0, cnt = 0,sqlstrLen = 0,loopCnt = 0;
	char Sql[2048] = "INSERT INTO ";
	char size_e[1];
	char *tmpItem;
	int itemLen=0;
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	
	//文字をエスケープしながらコピー
	tmpItem = (char *)malloc(sizeof(size_e) * (record_Length(iLName) * 2));
	itemLen = dataEscapeCopy(iItem,tmpItem,record_Length(iLName));
	
	//テーブル名指定
	sqlstrLen += strlen(Sql);
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//\0に置き換えている
	mystrncat( Sql,sqlstrLen,DB_table[intSub].tableName, strlen(DB_table[intSub].tableName) );
	sqlstrLen += strlen(DB_table[intSub].tableName);
	
	mystrncat( Sql, sqlstrLen,"`",strlen("`"));
	sqlstrLen += strlen("`");
	
	//更新項目指定
	mystrncat( Sql, sqlstrLen," (ITEM) VALUES( \"",strlen(" (ITEM) VALUES( \""));
	sqlstrLen += strlen(" (ITEM) VALUES( \"");
	
	//\0に置き換えている
	mystrncat( Sql, sqlstrLen, tmpItem,itemLen);
	sqlstrLen += itemLen;
	
	mystrncat( Sql, sqlstrLen, "\")",strlen("\")"));
	sqlstrLen += strlen("\")");
	
	//SQL実行
	if(mysql_real_query(mysql, Sql, sqlstrLen) != 0){
		mysql_failure();
		ret = 1;
	}
	
	//終了処理
	free(tmpItem);
	tmpItem = NULL;
	//ステータスの格納
	setTableStatus(&DB_table[intSub],'I',ret);
	
	return ret;
}

/* 削除関数 */
int DB_Delete(char *iPName){
	int ret = 0;
	int ii = 0, cnt = 0,intSub = 0;
	char strKeyTemp[30] = "";
	char Sql[100] = "DELETE FROM ";
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	
	//前の処理でReadが成功していないときはエラー
	if(strcmp(DB_table[intSub].accessStatus,"R1") == 0){
		ret = 1;
		//ファイル状態キーの変更
		*(DB_filestat + 0) = '4';
		*(DB_filestat + 1) = '3';
//		fprintf(stderr,"delete Error : %s,table status:%s",DB_table[intSub].tablePName,DB_table[intSub].accessStatus);
		return ret;
	}
	
	
	//テーブル名指定
	strcat( Sql, "`" );
	strcat( Sql, DB_table[intSub].tableName );
	strcat( Sql, "`" );
	
	//検索key設定
	//Readした行に対して行えばいいのでこれでいい
	//IDを文字列化
	sprintf(strKeyTemp,"%d",DB_table[intSub].prevPoint);
	strcat( Sql, " WHERE ID = " );
	strcat( Sql, strKeyTemp);
	
	//SQL実行
	if(mysql_query(mysql, Sql) != 0){
		mysql_failure();
		ret = 1;
	}
	
	//ステータスの格納
	setTableStatus(&DB_table[intSub],'D',ret);
	
	return ret;
}

/*ファイルオープン*/
/*ファイルオープン*/
int DB_F_Open(char *strMode,char *iPName,char *lockMode,char *ioidlist,char *cArgc,...){
	char sql[256]="";
	int ret=0,loopCnt=0;
	char *iLName;
	int targ_Table_num = DB_Table_num;
	int i=0;
	char size_e[1];
	va_list list;
	int iArgc = atoi(cArgc);    //入力は必ず数字になっているはず
	
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	//DBの初期設定
	if(getTargTable(DB_table,iPName,targ_Table_num) == -1){
		//初期化するときだけ配列の最後を＋
		DB_Table_num++;
		
		//変数がつながってしまうためスプリット 20140715 koyama
		for(loopCnt=0;loopCnt < strlen(iPName) && iPName[loopCnt] != '\0';loopCnt++){
			if(iPName[loopCnt] == ' '){
				iPName[loopCnt] = '\0';
				loopCnt++;
				while(*(iPName + loopCnt) == ' '){
					//スペースの連続を飛ばして次に来るのがLName
					loopCnt++;
				}
				iLName = (iPName + loopCnt);
				break;
			}
		}
		
		//変数がつながってしまうためスプリット 20140715 koyama
		for(loopCnt=0;loopCnt < strlen(iLName) && iLName[loopCnt] != '\0';loopCnt++){
			if(iLName[loopCnt] == ' '){
				if(iLName[loopCnt + 1] == ' '){
					iLName[loopCnt] = '\0';
					break;
				}
				iLName[loopCnt] = '\0';
				iLName = (iLName + (loopCnt + 1));
				loopCnt = 0;
			}
		}
		
		
		//DBオブジェクト変数の初期化(念のため)
		strcpy(DB_table[targ_Table_num].tablePName,iPName);
		strcpy(DB_table[targ_Table_num].tableLName,iLName);
		setTableName(&DB_table[targ_Table_num],iPName);
	}else{
		//既存で値が入っていたらそちらを使う
		targ_Table_num = getTargTable(DB_table,iPName,DB_Table_num);
		iLName = DB_table[targ_Table_num].tableLName;
	}
	
	DB_table[targ_Table_num].prevPoint = 0;
	//キー値の初期化
	memset(DB_table[targ_Table_num].key1Name,'\0',TNAME_LENGTH); //名前はから文字列
	memset(DB_table[targ_Table_num].key2Name,'\0',TNAME_LENGTH);
	memset(DB_table[targ_Table_num].key3Name,'\0',TNAME_LENGTH);
	DB_table[targ_Table_num].key1 = 0;      //ポインタはNULLポインタ
	DB_table[targ_Table_num].key2 = 0;
	DB_table[targ_Table_num].key3 = 0;
	DB_table[targ_Table_num].key1Len = 0;   //変数の長さは0で初期化
	DB_table[targ_Table_num].key2Len = 0;
	DB_table[targ_Table_num].key3Len = 0;
	DB_table[targ_Table_num].sharedStatus[0] = 'O';
	DB_table[targ_Table_num].sharedStatus[1] = lockMode[0];    //一文字目で初期化
	//SELECT 用変数の初期化
	DB_table[targ_Table_num].strWhere = 0;
	DB_table[targ_Table_num].strWhereLen = 0;
	
	//argPointの内容が配列
	va_start( list, cArgc );
	if(iArgc > 0 && cob_call_params > 3){
		//va_listを受け取るよう変数
		char *subscript;
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
						DB_table[targ_Table_num].key1Len = getFieldLength(iLName,DB_table[targ_Table_num].key1Name);
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key1,'0',DB_table[targ_Table_num].key1Len);
						break;
					case 1:
						DB_table[targ_Table_num].key2 = subscript;
						DB_table[targ_Table_num].key2Len = getFieldLength(iLName,DB_table[targ_Table_num].key2Name);
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key2,'0',DB_table[targ_Table_num].key2Len);
						break;
					case 2:
						DB_table[targ_Table_num].key3 = subscript;
						DB_table[targ_Table_num].key3Len = getFieldLength(iLName,DB_table[targ_Table_num].key3Name);
						//対象のカラムを0で初期化
						memset(DB_table[targ_Table_num].key3,'0',DB_table[targ_Table_num].key3Len);
						break;
				}
			}
		}
		va_end( list );
	}
	
	//取得ポインタ用文字列を初期化(COBOLからくる文字列なので文字数初期化しておけばOK)
	memset(( void * )ioidlist , ( int )'\0', sizeof( size_e ) * strlen(ioidlist));
	//ポインタ位置を0にしておく
	move_to_cob_for_bin(ioidlist,"0",strlen("0"));
	
	//lockModeの確認
	if( lockMode[0] != ' ' && lockMode[0] != 'E' && lockMode[0] != 'P'  && lockMode[0] != 'S'){
		ret = 1;
		fprintf(stderr," Error lockMode Nothing : %s,table status:%s",DB_table[targ_Table_num].tablePName,DB_table[targ_Table_num].accessStatus);
		return ret;
	}
	//INPUT OUTPUT
	switch(strMode[0]){
	case 'O':
		//書き込みモードで開く
		strcat(sql,"TRUNCATE TABLE ");
		strcat(sql,"`");
		strcat(sql,DB_table[targ_Table_num].tableName);
		strcat(sql,"`;");
		ret = mysql_query(mysql, sql);
		
		if(ret){
			//mysqlのエラーを標準エラーに(なぜ最初4文字が\0か不明)
			mysql_failure();
			return 1;
		}
		
		//排他モード(OUTPUTモードは読み書きできないロック)
//		strcat(sql,"LOCK TABLE ");
//		strcat(sql,"`");
//		strcat(sql,DB_table[targ_Table_num].tableName);
//		strcat(sql,"` WRITE;");
//		ret = mysql_query(mysql, sql);
		
		if(ret){
			//mysqlのエラーを標準エラーに(なぜ最初4文字が\0か不明)
			mysql_failure();
			return 1;
		}
		

		
		break;
	default:
		//ロックモードをセット
		if(strncmp(lockMode,"EXCLUSIVE",strlen("EXCLUSIVE")) == 0){
			//排他モード
//			strcat(sql,"LOCK TABLE ");
//			strcat(sql,"`");
//			strcat(sql,DB_table[targ_Table_num].tableName);
//			strcat(sql,"` WRITE;");
//			ret = mysql_query(mysql, sql);
			
			if(ret){
				mysql_failure();
				return 1;
			}
			
		}else if( strncmp(strMode,"INPUT",strlen("INPUT"))==0 ||  strncmp(lockMode,"PROTECTED",strlen("PROTECTED")) == 0){
			//排他モード
			//互いの読み込みが可能
//			strcat(sql,"LOCK TABLE ");
//			strcat(sql,"`");
//			strcat(sql,DB_table[targ_Table_num].tableName);
//			strcat(sql,"` READ;");
//			ret = mysql_query(mysql, sql);
			
			if(ret){
				mysql_failure();
				return 1;
			}
		}else{
			//排他モード
			//弱い書き込みロック(読み込み可能)
//			strcat(sql,"LOCK TABLE ");
//			strcat(sql,"`");
//			strcat(sql,DB_table[targ_Table_num].tableName);
//			strcat(sql,"` WRITE CONCURRENT;");
//			ret = mysql_query(mysql, sql);
			
			if(ret){
				mysql_failure();
				return 1;
			}
		}
		break;

	}
	
	//終了処理
	//DB_Table_numを先に加算しているため一致しているときはこの中で増やしていない
	if(DB_Table_num == targ_Table_num){
		iLName = NULL;
	}
	
	//ステータスの格納
	setTableStatus(&DB_table[targ_Table_num],'O',ret);
	
	return ret;
}


/*ファイルクローズ*/
int DB_F_Close(char *ioidlist){
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	memset(( void * )ioidlist , ( int )'\0', strlen( ioidlist ));
	
	return 0;
}

/* コネクション作成 */
int DB_Open(){
	//MySql接続用
	MYSQL_ROW row;
	int ret =0;
	char sql[256];
    
	mysql = mysql_init(NULL);
	
	if (!mysql_real_connect(mysql, DB_db_server, DB_db_user, DB_db_password, DB_db_database, 0, NULL, 0)) {  
		fprintf(stderr, "db_connect Error :server :%s user:%s pass:%s database:%s", DB_db_server,DB_db_user,DB_db_password,DB_db_database);
		//接続不可
		//ファイル状態キーの初期化
		*(DB_filestat + 0) = '4';
		*(DB_filestat + 1) = '1';
		ret = 1;
	}else{
		//接続可
		ret = 0;
//		ret = DB_Transaction();
		
	}
	return ret;
}

/*コネクションクローズ*/
int DB_Close(){
	int ret =0;
	//DB接続の切断
	mysql_close(mysql);
	return ret;
}

/*ファイルの中身削除*/
int DB_Scratch(char *iPName){
	char sql[256] = "TRUNCATE TABLE ";
	int rc=0,loopCnt=0,intSub=0;
	
	//ファイル状態キーの初期化
	*(DB_filestat + 0) = '0';
	*(DB_filestat + 1) = '0';
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	
	strcat(sql,"`");
	strcat(sql,DB_table[intSub].tableName);
	strcat(sql,"`;");
	rc = mysql_query(mysql, sql);
	
	if(rc){
		mysql_failure();
		return 1;
	}
	
	return 0;
}


/*検索Key(Start対応)設定*/
int DB_Start(char *iPName,char *iLabel,char *iMark,char *iValue){
	int ret =1;
	int intSub = 0,sqlstrLen = 0,fLen = 0;
	char *iLName;
	char strKeyMid[64] = "";
	char strData[1024] = "";
	char strSql[600] = "SELECT ID FROM  `";
	MYSQL_ROW	res;
	MYSQL_RES 	*result;
	
	intSub = getTargTable(DB_table,iPName,DB_Table_num);
	iLName = DB_table[intSub].tableLName;
	
	getFieldSpecified(iLName,iLabel,strKeyMid);
	
	sqlstrLen += strlen(strSql);
	mystrncat( strSql, sqlstrLen,DB_table[intSub].tablePName,strlen(DB_table[intSub].tablePName));
	sqlstrLen += strlen(DB_table[intSub].tablePName);
	
	mystrncat( strSql, sqlstrLen,"` WHERE ",strlen("` WHERE "));
	sqlstrLen += strlen("` WHERE ");
	
	mystrncat( strSql, sqlstrLen,strKeyMid,strlen(strKeyMid));
	sqlstrLen += strlen(strKeyMid);
	
	//<
	if(strchr(iMark,'<') != 0 && strchr(iMark,'N') == 0 
		&& strchr(iMark,'=') == 0 
		&& strncmp(strchr(iMark,'<'),"<",strlen("<")) == 0){
		mystrncat( strSql, sqlstrLen," < '",strlen(" < '"));
		sqlstrLen += strlen(" < '");
	}
	
	//>
	if(strchr(iMark,'>') != 0 && strchr(iMark,'N') == 0 
		&& strchr(iMark,'=') == 0 
		&& strncmp(strchr(iMark,'>'),">",strlen(">")) == 0){
		mystrncat( strSql, sqlstrLen," > '",strlen(" > '"));
		sqlstrLen += strlen(" > '");
	}
	
	//>=
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N'),"NOT <",strlen("NOT <")) == 0){
		mystrncat( strSql, sqlstrLen," >= '",strlen(" >= '"));
		sqlstrLen += strlen(" >= '");
	}
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N'),"NOT LESS",strlen("NOT LESS")) == 0){
		mystrncat( strSql, sqlstrLen," >= '",strlen(" >= '"));
		sqlstrLen += strlen(" >= '");
	}
	//>=
	if(strchr(iMark,'N') != 0 && strncmp(strchr(iMark,'N'),"NOT >",strlen("NOT >")) == 0){

		sqlstrLen += strlen(" <= '");
	}
	
	fLen = getFieldLength(iLName,iLabel);
	if(fLen == 0){
		mysql_failure();
		ret = 1;
		fprintf(stderr,"Start Error : Coudn't get Field Status [%s],table Name:[%s] \n",iLabel,iLName);
		return ret;
	}
	strncpy(strData,iValue,fLen);
	
	mystrncat( strSql, sqlstrLen,strData, fLen);
	sqlstrLen += fLen;
	
	mystrncat( strSql, sqlstrLen,"' ",strlen("' "));
	sqlstrLen += strlen("' ");
	
	//getFieldLength
	if(mysql_real_query(mysql, strSql, sqlstrLen) != 0){
		mysql_failure();
		return ret;
	}
	
	//取得できなかった時のため初期化しなおす
	DB_table[intSub].prevPoint = 0;
	if(result = mysql_store_result(mysql)){
		res=mysql_fetch_row(result);
		if(res != NULL){
			//Readするときに ID > prevPointになる
			//STARTの後にReadを書くと次の行を読むことになるので-1
			DB_table[intSub].prevPoint = atoi(res[0]) - 1;
			
			//正しく読めたらreturnを0にセット
			ret = 0;
		}
	}
	mysql_free_result(result);
	
	//ステータスの格納
	setTableStatus(&DB_table[intSub],'S',ret);
	
	return ret;
}

/*トランザクション*/
int DB_Transaction(){
	int ret =0;
	ret = mysql_query(mysql, "START TRANSACTION");
	return ret;
}

/*ロールバック*/
int DB_Rollback(){
	int ret =0;
	ret = mysql_query(mysql, "ROLLBACK");
	if(!ret){
		ret = DB_Transaction();
	}
	return ret;
}

/*コミット*/
int DB_Commit(){
	int ret =0;
	ret = mysql_query(mysql, "COMMIT");
	if(!ret){
		ret = DB_Transaction();
	}
	return ret;
}

