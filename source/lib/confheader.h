#ifndef CONF_PATH_
#define CONF_PATH_ 1
extern const char* CONF_FLIEPATH;
extern const char* CONF_SHREPATH;
extern const char* CONF_DEFPATH;    //トップレベルから一直線を予定
extern const char* DEBUG_FLGNAME;
#endif

#ifndef CONF_DB_TAG_
#define CONF_DB_TAG_ 1
extern const char* CONF_DB_HOST;
extern const char* CONF_DB_USER;
extern const char* CONF_DB_PASS;
extern const char* CONF_DB_NAME;
extern const char* CONF_DB_PORT;
#endif

#ifndef CONF_PTHREAD_
#define CONF_PTHREAD_ 1
int screenThread();
#endif

#ifndef SD_COMMON
#define SD_COMMON 1

#define SCR_OBJ_MAX 512

#include <libcob.h>
//fromでつながる変数がどのような形をしているか
//OCCURSで定義されていることがあるので
struct fromObject{
	char *curPnt;                /*現在の変数ポインタ*/
	char *arrPnt;                /*変数の名前*/
	cob_field *bodyPnt;                /*cob変数*/
	int  iVarSize;               /*元の変数のサイズ*/
	int  iDim;                   /*次元数*/
	char *fromVar1;              /*配列の次元1を示す配列変数のポインタ*/
	int  actualFlg1;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var1Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var1Length;               /*配列の次元1を示す配列変数の長さ*/
	char *fromVar2;              /*配列の次元2を示す配列変数のポインタ*/
	int  actualFlg2;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var2Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var2Length;               /*配列の次元1を示す配列変数のサイズ*/
	char *fromVar3;              /*配列の次元3を示す配列変数のポインタ*/
	int  actualFlg3;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var3Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var3Length;               /*配列の次元1を示す配列変数のサイズ*/
};

//fromでつながる変数がどのような形をしているか
//OCCURSで定義されていることがあるので
struct inputObject{
	char *curPnt;                /*現在の変数ポインタ*/
	char *arrPnt;                /*変数の名前*/
	cob_field *bodyPnt;                /*cob変数*/
	int  iVarSize;               /*元の変数のサイズ*/
	int  iDim;                   /*次元数*/
	char *var1;              /*配列の次元1を示す配列変数のポインタ*/
	int  actualFlg1;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var1Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var1Length;               /*配列の次元1を示す配列変数のサイズ*/
	char *var2;              /*配列の次元2を示す配列変数のポインタ*/
	int  actualFlg2;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var2Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var2Length;               /*配列の次元1を示す配列変数のサイズ*/
	char *var3;              /*配列の次元3を示す配列変数のポインタ*/
	int  actualFlg3;             /*実態かどうか ポインタなら0 実態ならここに数値を*/
	int  var3Size;               /*配列の次元1を示す配列変数のサイズ*/
	int  var3Length;               /*配列の次元1を示す配列変数のサイズ*/
};

struct screenObject{
	char cobargname[31];             /* COBOL の仕様がデータ項目名の長さ・最大30文字まで*/
	int  y,x,length;                        /* y:画面の行の数,x:画面の列の数 */
	char yname[31];
	char xname[31];
	char cobtype[25];                /*型を格納しておくcomp3の数値桁数最大が18なのでサイズ20+a*/
	struct screenObject *nextObj;    /*並列の変数がある場合の次の変数*/
	struct screenObject *parentObj;  /*入れ子の親変数 トップレベルノードはNULL*/
	struct screenObject *childObj;   /*入れ子の子供がある場合の子変数(先頭)*/
	cob_field *bodyPnt;                /*cob変数*/
	int lineShift;                   /*LineがPLUSでずらせるためその数字*/
	int ColShift;                    /*LineがPLUSでずらせるためその数字*/
	struct inputObject  usingVar;    /*using fromなどの変数のつながりがある場合のフラグ？*/
	struct inputObject  inputVar;    /*using fromなどの変数のつながりがある場合のフラグ？*/
	struct fromObject fromVar;       /*using fromなどの変数のつながりがある場合のフラグ？*/
	int onViewFlg;
};

// 画面オブジェクトを共通化して別スレッドで読めるように
extern struct screenObject *SD_cobScreen;
extern int SD_cobScreenLen;

/** Function list (Screen)  **/
int SD_Output(char *argname,char *printarg,char *curLength);
struct screenObject *searchTargetObject(struct screenObject *screenObjArray,char *targName,int arrayLen);
/** Function list (Screen)  **/
#endif

#define LIB_ID_SCREEN 1
#define LIB_ID_DB     2
#define LIB_ID_PRINT  3
#define PATHNAME_SIZE 1024
//実行時のファイル名(コマンド名を取得)
//malloc直後には値を入れるようにする =>NULLでなければ中身がある状況を作る
extern char *source_file_name;
//debugflagを格納// 共通仕様
extern int myConfDebugFlg;
//エラーID出力
int mytoolgetErrorId(int libId,int funcId,int seqId);


#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 1024
char map_source_func[MAP_SRC_FUNC_LEN];
char map_source_filename[MAP_SRC_FUNC_LEN];
int MT_Initialize();
void mybacktrace(void);    //debug用関数
char *local_server_time(char *);
char *getConfFilename(char *);
char *getprocessName(char *,int );
int  getUserAndProcessName(char *,char *);
void mytool_runtime_error(const char *,const char *, ...);
void setCommonFunctionName(char *,char *,char *,...);
void unsetCommonFunctionName(char *,char *);
char *StrToLowerCpy(char *,char *);
#endif

#ifndef DB_COMMON
#define DB_COMMON 1
/** Function list (DB)  **/
int DB_Close();
int DB_PING();
int DB_M_PING();
int getDBReadWriteFlg();
int getDBMReadWriteFlg();
/** Function list (DB)  **/
#endif
