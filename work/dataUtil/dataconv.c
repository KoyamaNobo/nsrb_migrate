#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mysql/mysql.h>
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/
#include <libcob.h>
#include <ctype.h>

struct lenStr {
	char *data;
	int len;
	int physicallen;
	int maxlen;
};

#define FILEMAXLEN 4096;

#ifndef CONF_PATH
#define CONF_PATH
const char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif

// MYSQL *connect;         //MySqlConnectin
//DBのタグ名
#ifndef CONF_DB_TAG
#define CONF_DB_TAG
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
#endif

const char* CONF_CONV_EXT = "convDefExt";

char db_server[48];
char db_user[48];
char db_password[48];
char db_database[48];
char convdata_ext[10];

int viewControlFlg = 0;
MYSQL            sql, *mysql=&sql;
static int       errout;

//長さ付文字列の文字列をlenの長さで確保
//ＩＮ：
//  allocv  ： 初期化する長さ付文字列のポインタ
// len     ： 定義する文字列の最大の長さ
//OUT    ： 成功 0、失敗 1
//author:koyama
int lenStr_allocLength(struct lenStr *allocv,int len){
	char size_e[1];
//	struct lenStr allocv;
//	allocv = *allocr;
	//エスケープで文字が増えることを考えて2倍でとっておく
	allocv->data = (char *)malloc((sizeof(size_e) * (len * 2)));
	if(allocv->data == NULL) {
		fprintf(stderr,"can't alloc memory!!");
		return 1;
	}
	//ほかのメンバの初期化
	allocv->len = 0;
	allocv->physicallen = 0;
	allocv->maxlen = len;
	return 0;
}


// 長さ付き文字列の解放
//ＩＮ：
//  allocv   ： 解放する長さ付文字列のポインタ
// OUT ： なし
//author:koyama
void lenStr_free(struct lenStr *allocv){
	free(allocv->data);
}

// 最終的に文字列が最大長より短かいときにスペース埋める
//ＩＮ：
//  targv   ： 対象となる長さ付文字列
// OUT ： なし
//author:koyama
void lenStr_setFillShortage(struct lenStr *targv){
	if((targv->maxlen - targv->len) > 0){
		memset((targv->data + targv->physicallen),' ',((targv->maxlen) - targv->len));
		targv->physicallen += ((targv->maxlen) - targv->len);
		targv->len += ((targv->maxlen) - targv->len);
	}
}


//エスケープする文字をエスケープしながらデータをつなぐ 
//エスケープの対象はシングルクォート(0x27)と円記号(0x5c) 
//ＩＮ： 
//  targv ： 対象となる長さ付文字列 
//  addtext ： つなぐ文字列 
//  len ： つなぐ文字列の論理的な長さ 
//OUT ： なし 
//author:koyama
int lenStr_addString(struct lenStr *targv,char *addtext,int len){
	int result = 0;
	int addlen = 0;
	char size_e[1];
	char *copyText;
	char *copyAddText;
	char *origText;
	char *origTextEnd;

	if((targv->len + len) > targv->maxlen){
		return 1;
	}
		
	//copyTextの初期化
	//つなぐのに必要
	copyAddText = (char *)malloc((sizeof(size_e) * (len * 2)));
	copyText = copyAddText;
	origText = addtext;
	origTextEnd = (addtext + len);
	
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
	
	memcpy((targv->data + targv->physicallen),copyAddText,addlen);
	targv->len += len;
	targv->physicallen += addlen;

	free(copyAddText);
	return result;
}

//文字列を空白文字と制御コード(0x1a)を飛ばす
//IN：
//  s：飛ばす文字列のポインタ
//OUT：飛ばした後の文字列ポインタ
//
char *myftrim(char *s){
	while(isspace(*s) || *s == 0x1A){
		s++;
	}
	return s;
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

//設定ファイルから設定を読み込みグローバル変数へセット
//IN ：なし
//OUT：0正常
//     1異常
//author:koyama
int conf_read(){
	int i;
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
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
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
				//設定ファイルから拡張子を取得
				if(strcmp(node->parent->name,CONF_CONV_EXT) == 0){
					strcpy(convdata_ext,node->content);
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

//グローバル変数から設定を取得し、データベースに接続する
//IN ：なし
//OUT：0正常
//     1異常
//author:koyama
int DB_Open(){
	//MySql接続用
	MYSQL_ROW row;
	int ret =0;
    
	mysql = mysql_init(NULL);
	
    if (!mysql_real_connect(mysql, db_server, db_user, db_password, db_database, 0, NULL, 0)) {  
        fprintf(stderr, "%s\n", mysql_error(mysql));  
        //接続不可
        exit(1);
        ret = 1;
    }else{
        //接続可
        ret = 0;
    }
    
    return ret;
}

//設定の読み込みとデータベースへの接続を行う 
//IN  ： なし
//OUT ： なし
//author:koyama
int db_init(){
	int result=0;
	if(conf_read() != 0){
		result = 1;
	}else{
		if(DB_Open() != 0){
			result = 1;
		}
	}
	cob_init(0, NULL);
	return result;
}

//定義より対象テーブルの1レコードの長さを取得
//IN：
//  tablename：長さを調べるテーブル名(定義のテーブル名)
//OUT：レコードの長さ（エラーのとき：0）
//author:koyama
int getRecordLen(char *tablename){
	int ret =0;
	char sqlstr[2048] = "";
	char tName[256] = "";
	MYSQL_ROW	res;
	MYSQL_RES 	*result;
	
	strcpy(tName,tablename);
	
	//対象テーブル名の空チェック
	if(isspace(tName[0]) == 1){
		ret = 0;
		return ret;
	}
	
	strcat(sqlstr,"SELECT max(Size) FROM M_ITEMELEMENT WHERE TableName ");
	
	//20140911 仕様変更 koyama
	if(tName[0] != '('){
		strcat(sqlstr," LIKE \"");
	}else{
		strcat(sqlstr," IN ");
	}
	
	strcat(sqlstr,tName);
		
	//20140911 仕様変更 koyama
	if(tName[0] != '('){
		strcat(sqlstr,"\" ");
	}
	
	strcat(sqlstr," AND S_Point = 1");
	
	if(mysql_query(mysql, sqlstr) != 0){
		ret = 1;
		return ret;
	}
	
	if(result = mysql_use_result(mysql)){
		res=mysql_fetch_row(result);
		if(res != NULL){
			ret = atoi(res[0]);
			mysql_free_result(result);
		}
	}
	return ret;
}

//変数をコピー。その時に指定した文字があれば飛ばす
//引数: toStr:コピー先文字列 ,fromStr:コピー元文字列 ,rmChar:削除する文字
//未使用
void strcpyRmChr(char *toStr,char *fromStr,char rmChar){
	char *toTmpStr;
	char *fromTmpStr;
	toTmpStr   = toStr;
	fromTmpStr = fromStr;
	for(;*fromTmpStr != '\0';fromTmpStr++){
		if(*fromTmpStr != rmChar){
			*toTmpStr = *fromTmpStr;
			toTmpStr++;
		}
	}
	return;
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
	
	strcat(sqlstr,"TRUNCATE TABLE `");
	strcat(sqlstr,tName);
	strcat(sqlstr,"`;");
	
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


//ファイルポインタからファイルを読み込み。データ変換 
//IN ： 
//  fp ： 読み込むdata.csvのファイルポインタ 
//  tablename ： 定義のテーブル名 
//  physicaltname ： 追加するテーブル名 
//OUT ： 0 正常 
//       1 異常 
//author:koyama
int getFElemAndExec(FILE *fp,char *tablename,char *physicaltname,int argCount,...){
// -------------------------------------------------------------------------------------------------define start
	cob_field_attr a_from = {0x21, 0, 0, 0, NULL};
	//1バイト文字列
	cob_field_attr a_to = {0x21, 0, 0, 0, NULL};
	//定義体?
	static cob_field *cob_user_parameters[COB_MAX_FIELD_PARAMS];
	static struct cob_module module = { NULL, NULL, NULL, NULL, cob_user_parameters, 0, '.', '\\', ',', 1, 1, 1, 0 };
	cob_current_module = &module;
	
	char tmp[2048]="";			//fileから取得した1行の文字列
	struct lenStr targData;		//結合した後の文字列オブジェクト
	int recordLen = 0;			//結合後の文字列長
	int lineCounter = 0;
	char sqlStr[600]="";		//フィールドのプロパティを取得するSQLの文字列
	char tName[256] = "";		//ローカル変数としてテーブル名を格納
	char ptName[256] = "";		//テーブルの物理名
	char *curTerm;				//ループでの現在のポインタ
	unsigned char b_dummy[512]__attribute__((aligned));	//__attribute__((aligned))は変数一つなのでいらない
	unsigned char csvColumn[512]__attribute__((aligned));
	
	unsigned long *f_length;		//フィールドの長さ
	int rr=0,cc=0;					//
	MYSQL_RES *result;				//mysqlのresult
	MYSQL_ROW row;
	MYSQL_ROW_OFFSET startOffset;
	MYSQL_ROW_OFFSET prevOffset;
	cob_field       f0;
	cob_field       f1;
	char strCondition[128] = "";
// -------------------------------------------------------------------------------------------------define end
// -------------------------------------------------------------------------------------------------initialize start
	//そのままだとstrcatできないので
	strcpy(tName,tablename);
	strcpy(ptName,physicaltname);
	//レコード長を取得
	recordLen = getRecordLen(tName);
	
	//ON OFFをしやすくするため外側に移動
	//	//いったんテーブルの中身をリセット
//	if(truncateTable(ptName) == 1){
//		fprintf(stderr,"error truncate table %s",physicaltname);
//		return;
//	}
	
	strcat(sqlStr,"SELECT max(Size),Data_Type,S_Point,Label FROM M_ITEMELEMENT WHERE TableName ");
	//20140911 仕様変更 koyama
	if(tName[0] != '('){
		strcat(sqlStr," LIKE \"");
	}else{
		strcat(sqlStr," IN ");
	}
	
	strcat(sqlStr,tName);
	
	//20140911 仕様変更 koyama
	if(tName[0] != '('){
		strcat(sqlStr,"\" ");
	}
	
	//マルチ対応
	if(argCount > 0){
		//中でしか使わない
		va_list list;
		va_start(list,argCount);
		char *tmpstr = va_arg( list , char * );
		strcpy(strCondition,tmpstr);
		va_end( list );
	}
	
	if(strcmp(strCondition,"") == 0){
		strcat(sqlStr," AND LeafCondition LIKE 'N%' AND LeafCondition IS NOT NULL ");
	}else{
		strcat(sqlStr," AND (LeafCondition LIKE '%CO%' OR LeafCondition LIKE '%");
		strcat(sqlStr, strCondition);
		strcat(sqlStr,"%') AND LeafCondition IS NOT NULL ");
	}
	strcat(sqlStr,"GROUP BY S_Point ");
	strcat(sqlStr,"ORDER BY S_Point ");
	
	//データの並びを取得
	if(mysql_query(mysql, sqlStr) != 0){
		fprintf(stderr,"property get error %s \n error %s\n",tablename,(mysql->net.last_error + 4));
		return 1;
	}
	//配列として操作したいのでstore
	result = mysql_store_result(mysql);
	if(result == 0){
		return 1;
	}
	
	//取得したmysqlのプロパティをセット
	f_length =  mysql_fetch_lengths(result);
	rr=mysql_num_rows(result);
	cc=mysql_num_fields(result);
	startOffset = mysql_row_tell(result);
	
// -------------------------------------------------------------------------------------------------initialize end
// -------------------------------------------------------------------------------------------------execute start
	//ファイルを1行ずつループ
	while(fgets(tmp,sizeof(tmp),fp)){
		int sqlstrlen = 0;
		lineCounter++;
		curTerm = tmp;
		//一行を示すデータを初期化
		if(strlen(myftrim(tmp)) == 0){
			continue;
		}
		lenStr_allocLength(&targData,recordLen);
		//------------------------------------------------------------------dataconvert start
		while((row=mysql_fetch_row(result)) != 0){
			if((int)curTerm == 1){
				break;
			}
			int from_datalen = 0,to_datalen=0;      //現在のdbの値に対応するデータの長さ
			char thisTerm[1024]="";   //ファイル側のデータひとつ分を格納
			char *commaPosition = strchr(curTerm,',');   //
			char colsize[12]="";    //int型だがこれで受け取る
			char datatype[12]="";   //char(3)だが念のため
			char startpoint[12]="";   //char(3)だが念のため
			strcpy(colsize,row[0]);
			strcpy(datatype,row[1]);
			strcpy(startpoint,row[2]);
			//データに変なものがついているものがあったので除去
			strRemCrLf(colsize);
			strRemCrLf(datatype);
			memset(csvColumn,'\0',512);    //初期化
			memset(b_dummy,'\0',512);    //初期化
			memset(b_dummy,' ',atoi(colsize));    //初期化
			//初期値0 初期値1 なので常にS_Pointのほうが大きくないとだめ
			if(targData.len >= atoi(startpoint)){
				continue;
			}else if((targData.len +1) < atoi(startpoint)){
				//datalen+1 と 次のStartPointが一致ないときはその分のFILLER(論理長)
				//startpointは次に入れるところを指しているので-1
				sprintf(colsize,"%d",((atoi(startpoint) - 1) - targData.len));
				strcpy(datatype,"X");
				sprintf(startpoint,"%d",(targData.len + 1));
				
				//FILLERの時はポインタを戻さなければいけない
				mysql_row_seek(result,prevOffset);
			}
			
			//fetch_rowのポインタを状況によって戻す必要がある
			//現在のポインタを格納
			prevOffset = mysql_row_tell(result);
			
			//------------------------------------------------------------------dataconvert init end
			//カンマの位置までをthisTermにコピー
			if(commaPosition != 0){
				strncpy(thisTerm,curTerm,((int)commaPosition - (int)curTerm));
			}else{
				strncpy(thisTerm,curTerm,strlen(curTerm));
				strRemCrLf(thisTerm);
			}
			//"囲みがあるならを除去
			//ここで代入するデータの長さを定義
			if(thisTerm[0] != '"'){
				strcpy(csvColumn,thisTerm);
				from_datalen = strlen(csvColumn);
				to_datalen = atoi(colsize);
			}else{
				strcpy(csvColumn,(thisTerm + 1));
				memset(strchr(csvColumn,'"'),'\0',1);
				from_datalen = strlen(csvColumn);
				to_datalen = atoi(colsize);
			}
			
			switch(datatype[0]){
			case 'X':
				//受ける側cob_field_attrの設定
				a_to.type   = (unsigned char)0x21;
				a_to.digits = atoi(colsize);
				//カンマの位置
				a_to.scale  = 0;
				a_to.flags  = 0;
				//picはeditingフォーマットなだけ
				break;
			case 'N':
				//受ける側cob_field_attrの設定
				a_to.type   = (unsigned char)0x40;
				a_to.digits = atoi(colsize);
				//カンマの位置
				a_to.scale  = 0;
				a_to.flags  = 0;
				//picはeditingフォーマットなだけ
				break;
			case '9':
				//受ける側cob_field_attrの設定
				a_to.type   = (unsigned char)0x10;
				if(strchr(csvColumn,module.decimal_point) == 0){
					a_to.digits = to_datalen;
				}else{
					a_to.digits  = (int)strchr(csvColumn,module.decimal_point) - (int)myftrim(csvColumn);
				}
				//カンマの位置
				if(strchr(csvColumn,module.decimal_point) == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = strlen(strchr(csvColumn,module.decimal_point) + 1);
				}
				a_to.flags  = 0;
				//picはeditingフォーマットなだけ
				break;
			case 'C':
				//受ける側のdata変数を初期化し直し
				memset(b_dummy,'\0',atoi(colsize));
				//受ける側cob_field_attrの設定
				a_to.type   = (unsigned char)0x12;
				if(strchr(datatype,'(')){
					//数字の後ろにある文字は切られるので
					a_to.digits = atoi((strchr(datatype,'(') + 1));
				}else{
					a_to.digits = (to_datalen - 1) * 2;
				}
				//カンマの位置
				a_to.scale  = 0;
				a_to.flags  = 0;
				//picはeditingフォーマットなだけ
				break;
			case 'S':
				//符号付数
				if(datatype[1] == '9'){
					//受ける側cob_field_attrの設定
					a_to.type   = (unsigned char)0x10;
					if(strchr(csvColumn,module.decimal_point) == 0){
						a_to.digits = to_datalen;
					}else{
						a_to.digits  = (int)strchr(csvColumn,module.decimal_point) - (int)myftrim(csvColumn);
					}
				//カンマの位置
					if(strchr(csvColumn,module.decimal_point) == 0){
						a_to.scale  = 0;
					}else{
						a_to.scale  = strlen(strchr(csvColumn,module.decimal_point) + 1);
//						a_to.scale  = (int)strchr(csvColumn,module.decimal_point) - (int)myftrim(csvColumn);
					}
					a_to.flags  = 1;
					//picはeditingフォーマットなだけ
					
					//
//					a_from.type = (unsigned char)0x10;
//					a_to.digits = atoi(colsize);
				}
				//符号つきCOMP
				if(datatype[1] == 'C'){
					//受ける側cob_field_attrの設定
					a_to.type   = (unsigned char)0x12;
					if(strchr(datatype,'(')){
						//数字の後ろにある文字は切られるので
						a_to.digits = atoi((strchr(datatype,'(') + 1));
					}else{
						a_to.digits = (to_datalen - 1) * 2;
					}
					//カンマの位置
					if(strchr(csvColumn,module.decimal_point) == 0){
						a_to.scale  = 0;
					}else{
						a_to.scale  = strlen(strchr(csvColumn,module.decimal_point) + 1);
//						a_to.scale  = (int)strchr(csvColumn,module.decimal_point) - (int)myftrim(csvColumn);
					}
					a_to.flags  = 1;
					//picはeditingフォーマットなだけ
				}
				break;
			default:
				break;
			}
			//from
			f0.size = from_datalen;
			f0.data = csvColumn;
			f0.attr = &a_from;
			//to
			f1.size = to_datalen;
			f1.data = b_dummy;
			f1.attr = &a_to;
			cob_move(&f0,&f1);
			if(viewControlFlg == 1){
				printf("[[[[[[[[[[[%s,%d,%s,%s]]]]]]]]]]%s:%s\n",datatype,atoi(colsize),row[2],row[3],b_dummy,csvColumn);
			}else{
				printf("[[[[[[[[[[[%s,%d,%s,%s]]]]]]]]]]\n",datatype,atoi(colsize),row[2],row[3]);
			}
//			printf("[%s,%d]",datatype,atoi(colsize));
			curTerm = (commaPosition + 1);
			if(lenStr_addString(&targData,b_dummy,atoi(colsize)) == 1){
				break;
			}
			
		}
		
		mysql_row_seek(result,startOffset);
		
//		if((targData.maxlen - targData.len) > 0){
//			//足りないところを埋める
//			memset((targData.data),' ',((targData.maxlen - 1) - targData.len));
//			targData.len += ((targData.maxlen - 1) - targData.len) ;
//		}
//			//足りないところを埋める
		lenStr_setFillShortage(&targData);
		//-----------------------------------------------------------------------------------------dataconvert end
		
		printf("\n");
		if(viewControlFlg == 1){
			printf("%d[%d]%s",lineCounter,targData.len,targData.data);
		}else{
			printf("%d[%d]",lineCounter,targData.len,targData.data);
		}
		printf("\n");
		//初期設定の長さを\0で初期化
		memset(sqlStr,'\0',600);
		strcat(sqlStr,"INSERT INTO `");
		strcat(sqlStr,ptName);
		strcat(sqlStr,"`\n (ITEM) \n VALUES('");
		sqlstrlen = strlen(sqlStr);
		mystrncat(sqlStr ,sqlstrlen ,targData.data,targData.physicallen);
		sqlstrlen += targData.physicallen;
		mystrncat(sqlStr ,sqlstrlen ,"') ",3);
		sqlstrlen += 3;
		
		//sqlにつなぎ終わったら解放
		lenStr_free(&targData);
		
		if(mysql_real_query(mysql, sqlStr,sqlstrlen) != 0){
			fprintf(stderr,"insert error %s", mysql_error(mysql));
			break;
		}
	}
	
// -------------------------------------------------------------------------------------------------finalize start
	//使い終わったオブジェクトを破棄
	mysql_free_result(result);
	return 0;
}


//ファイル名に拡張子をつけてコピー 
//IN ： 
//  fileName ： コピー先文字列
//  targName ： コピーする拡張子なしファイル名
//OUT ： なし
//author:koyama
void fileExtAdd(char *fileName,char *targName){
	strcpy(fileName,"./");
	strcpy(fileName,targName);
	strcat(fileName,convdata_ext);
	return;
}

//ファイル名 物理名を入れてもらう
//ファイル名と論理名と物理名
int main(int argc,char *argv[]){
	FILE *fp = 0;
	char filename[256]="";
	char ptName[256]="";
	
	//表示のバッファをしない設定
	setvbuf( stdout, NULL, _IOLBF, 0 );
	
	printf("%d",argc);
	//ファイル名と物理名がないとき0123と存在ため引数三つなら4となる
	if(argc >= 4 && argc <= 5 && db_init() == 0){
		//制御系の処理で入れ子に
		fileExtAdd(filename,argv[1]);
		if ((fp = fopen(filename, "r")) == NULL) {
			fprintf(stderr,"file open error!!fileName : %s \n",filename);
			exit(EXIT_FAILURE);	/* (3)エラーの場合は通常、異常終了する */
			
		}
		//4つ目の引数が存在したら
		if(argc == 5){
			//ファイルの中身を取得し処理
			//テーブル名を入れる
			getFElemAndExec(fp,argv[2],argv[3],1,argv[4]);
		}else{
			//引数が3つのときになる
			strcpy(ptName,argv[3]);
			if(truncateTable(ptName) == 1){
				fprintf(stderr,"error truncate table %s",argv[3]);
				return;
			}
			//ファイルの中身を取得し処理
			//テーブル名を入れる
			getFElemAndExec(fp,argv[2],ptName,0);
		}
		
	}else{
		fprintf(stderr,"error!!\nThis program need 3 options \n");
		return 1;
	}
	
	
	return 0;
}