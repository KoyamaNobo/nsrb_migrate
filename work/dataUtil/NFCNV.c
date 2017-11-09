#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>    /*va_list���g�����߂ɕK�v*/
#include <string.h>
#include <stdbool.h>
#include <mysql.h>
#include <time.h>
#include <sys/time.h>
#include <math.h>
#include <signal.h>
#include<unistd.h>
#include <libcob.h>
#include <libxml/xmlreader.h>    /*conf�t�@�C����xml��ǂނ���*/
#include <libxml/xpath.h>        /*conf�t�@�C����xml��ǂނ���*/

// container_of �}�N��
/*
#define container_of(ptr, type, member) ({                              \
        const typeof( ((type *)0)->member ) *__mptr = (void *)(ptr);    \
        (type *)( (char *)__mptr - offsetof(type,member) );})
*/
#define container_of(ptr, type, member) ({ 								\
		(type *)( (char *)(ptr) - offsetof(type,member) );})

#define ITEMLST 256              // SORT�Ώۍ\���̗̂v�f��//�t�B�[���h�̐��̏��(64��𔭌�)
#define LOOP_MAX 10000000       //�f�[�^��ǂލő�s��
#define TOLERANCE 20            //�f�[�^���̌덷(������덷���������狭���I��)
#define MAP_SRC_FUNC_LEN 50
#define PATHNAME_SIZE 1024
#define CONVTARGLEN 8192
#define CONVTARGROW 16

//DB�̃^�O��
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
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";	//2015/08/28 kawada add ���[�J���� conf.xml ���Ȃ��Ƃ��̑Ή�
char* CONF_DEFPATH  = "//conf/*/text()";    //�g�b�v���x������꒼����\��
#endif
//�萔
const int  NSHIFT     = 4;              // �����J�n��������p�̈ړ�����
const int  ARGITM     = 5;              // �����\���̗̂v�f��
//const int  ITEMLST    = 20;           // SORT�Ώۍ\���̗̂v�f��
const char *EXCLUSIVE = "EXCLUSIVE";    //�I�[�v�����[�h�F�r��(EXCLUSIVE)
const char *PROTECT   = "PROTECT";      //�I�[�v�����[�h�F�ی�(PROTECT)
const char *SHARE     = "SHARE";        //�I�[�v�����[�h�F���p(SHARE)


//�^�w��\����
struct argv_inf {
	int s_point;                  // �J�n�ʒu
	int length;                   // ������
	char *type;                   // �o�̓^�C�v
	char *operand;                // ��r�Ώۃt�B�[���h
	char *value;                  // �o�̓t�B�[���h�\��
};
typedef struct argv_inf Argv_inf;

//DB�ݒ�\����
struct db_setting {
	char db_server[64];        // host��
	char db_user[64];          // user��
	char db_password[64];      // password
	char db_database[64];      // database��
	int db_port;               // port�ԍ�
	char db_manatbl[64];       // �Ǘ�table��	//2015/08/28 kawada add �Ǘ��e�[�u����
}db_setting={"","","","",3306};
typedef struct db_setting DB_SETTING;


//������񑢑�
struct trgt_inf {
//	�e�����l�̍ő啶����
//	char ifi[12];	// ���̓t�@�C����
//	char ofi[12];	// �o�̓t�@�C����
//	char mod[03];	// �������ݕ��@
//	char out[120];	// �o�̓t�B�[���h�\��
//	char sel[120];	// where��

	char     *ifi;			// AVX�t�@�C����
	char     *ofi;			// DOS�t�@�C����
	int      mod;			// �ϊ����@ 0:AV-X->DOS,1:DOS->AV-X
	int      wmod;          // �������ݕ��@ 0:NEW,1:MOD
	Argv_inf out[ITEMLST];	// COBOL���t�B�[���h�\��
	Argv_inf sel[ITEMLST];	// CSV���t�B�[���h�\��
	int      logical;		// �_�����Z�q 0:AND,1:OR,-1:���̑�
	FILE     *fp;           //�ϊ��Ώ�or�o�͑Ώ�file pointer
	char     deb;
	char     *opmod;        // DOS�`���̃t�@�C���I�[�v�����[�h Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE�ix�͕ϊ����@�ɂ��ς��j	//2015/08/28 kawada add
	int      dataLength;    //�e�[�u���̃o�C�g��

};
typedef struct trgt_inf Trgt_inf;

//������\����
struct string_t{
	size_t size;	// ������̒��� + NIL�����̒����̍��v�l
	char   *data;  // �f�[�^�̐擪
};
typedef struct string_t String_t;

///////////////////////////////////////////////Function liet AND prototype Start
//RTrim����
int isNullOrEmpty(char *targ);
char *local_server_time(char*);
int setTableLock(DB_SETTING*, Trgt_inf*);
int setTableUnlock(DB_SETTING*, Trgt_inf*);
//�󔒕������΂���������|�C���^��Ԃ�
char *myftrim(char *s);
//�G���[�Ƀv���Z�X����t���ďo��
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

int unqId = 0;                  //���j�[�NID(�Ǘ��e�[�u���F�ؗp)		//2015/08/28 kawada add
char *myself=0;                 //�������g�̃p�X���擾(�����ݒ�)

//conf����擾����debug_flg.���Ɣ��Ȃ��悤��original name
static int myConfDebugFlg;

//���[�J���� conf.xml ���Ȃ��Ƃ��̑Ή�
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
//in/out :retStr �Ԃ��Ώە�����̃|�C���^
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

//�󔒕������΂���������|�C���^��Ԃ�
//IN�F
//  s�F��΂�������̃|�C���^
//OUT�F��΂�����̕�����|�C���^
//author: koyama
char *myftrim(char *s){
	while(isspace(*s)){
		s++;
	}
	return s;
}

//�v���Z�X����Ԃ�
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
//in/out :setName �v���Z�X����Ԃ�������|�C���^ in:size �v���Z�X����Ԃ�������̒���
//author : koyama
char *getprocessName(char *setName,int size){
	FILE  *fp;
	char  buf[1024];   //�R�}���h�̎󂯎��p
	int   pid=0;
	char  cmd[1024]="";
	pid = getpid();
	sprintf(cmd,"ps -p %d -o comm= ",pid);

	memset(setName,'\0',size+1);
	if ( (fp=popen(cmd,"r")) ==NULL) {
		//�R�}���h�̎��s�Ɏ��s������󕶎����return
		return setName;
	}
	memset(setName,'\0',size+1);
	//���ʂ�size�������R�s�[
	//����`�͕K�v�H
	while(fgets(buf, 1024, fp) != NULL) {
		if((strlen(setName) + strlen(buf) ) > size){
			//�R�}���h�̎��s�Ɏ��s������󕶎����return
			memset(setName,'\0',size+1);
			return (char *)0;
		}
		strcat(setName,buf);
	}
	pclose(fp);
	//�ЂƂڂ̉��s�܂łŐ؂�
	if( strchr(setName, '\n') ){
		*(strchr(setName, '\n')) = '\0';
	}
	return setName;
}

//�G���[�Ƀv���Z�X����t���ďo��
//in/out :fmt �o�̓t�H�[�}�b�g,...�t�H�[�}�b�g�ɑΉ�����ϐ�
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
		//DBUNLOCK�����s
		setTableUnlock(NULL,NULL);
	}
}



//�ݒ�t�@�C���̃p�X���擾
//date:20150828
//auth: koyama
//�J�����gdir��conf�������J���Ƃǂ��łł����s�ł��Ȃ��̂�
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
//SORT�Ώۍ\���̂̏�����
//auth:iida
//*********************************************************
void argv_ini(Argv_inf *ioargv ) {
//	"�J�n�ʒu", "������", "�o�̓t�@�C����", "��r�Ώۃt�B�[���h", "�o�̓t�B�[���h�\��"
	Argv_inf wk_argv ={0,0,"","",""};

	*ioargv = wk_argv;
}


//*********************************************************
//SORT�Ώۍ\���̂̏�����
//auth:iida
//*********************************************************
void trgt_ini(Trgt_inf *iotrgt ) {
	int ii = 0;

	Trgt_inf wktrgt;

	*iotrgt = wktrgt;

	iotrgt->ifi = "";      //�e�[�u����
	iotrgt->ofi = "";      //�t�@�C�����ݒ�
	iotrgt->mod = -1;       //���[�hAD:0,DA:1
	iotrgt->wmod= 0;       //�ǋL���[�h �V�K:0,�ǋL:1
	iotrgt->opmod = (char *)malloc(3);		//DOS�`���̃t�@�C���I�[�v�����[�h	//2015/08/28 kawada add

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

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve interrupt signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve SegmentationFalet signal
// Author koyama
void* sigSegv( int args ){
	int i = 0;

	DB_Close();
	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
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
	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve Abort signal ");
	raise(SIGTERM);

	return NULL;
}


// recieve USER1 signal
// Author koyama
void* sigUsr1( int args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve usr1=kill signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve USER2 signal
// Author koyama
void* sigUsr2( void *args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	raise(SIGSTOP);

	return NULL;
}

//
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
//in/out :retStr �Ԃ��Ώە�����̃|�C���^
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
	//KILL����
	if (signal(SIGUSR1, (__sighandler_t)sigUsr1) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//STOP����
	if (signal(SIGUSR2, (__sighandler_t)sigUsr2) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGSEGV����
	if (signal(SIGSEGV, (__sighandler_t)sigSegv) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGABRT����
	if (signal(SIGABRT, (__sighandler_t)sigAbrt) == SIG_ERR){
		retval = 1;
		return retval;
	}
	return retval;

}

//RTrim����
//(���ꂼ���Rtim������̂ł����ł���p����������)
//date:20160520
//auth: koyama
int isNullOrEmpty(char *targ){
	char *strstart;
	char *strend;
	int num=0;
	strstart = targ;
	//�Ō�̕������画��Ώ�
	strend = targ + strlen(strstart);
	for(strend--;strend >= strstart;strend--){
		if(*strend == ' '){
			num++;
		}
	}
	//�S�Ă��X�y�[�X�Ȃ�
	if(num == strlen(strstart)){
		num = 1;
	}else{
		num = 0;
	}
	return num;
}

//����̕�����u������
//origText:�Ώە�����,target:�Ώە���
//,replacement:�u���㕶��,splitter����ň͂܂ꂽ����������B�w�肵�Ȃ��Ƃ���0
//return �Ȃ�
//author:koyama
void chgChar(char *origText,char target,char replacement,char splitter){
	char *origTextEnd;
	char *copyText;
	char elemflg = 0x00;
	char splitchar = 0x00;

	splitchar = splitter;
	if(splitchar == 0x00){
		//splitter�ɋ���w�肳�ꂽ�Ƃ��͔�΂��d�l
		elemflg = 0x01;
	}

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;copyText < origTextEnd;copyText++){
		//���s�������΂�
		if(splitchar == *copyText){
			if(elemflg == 0x00){
				elemflg = 0x01;
			}else{
				elemflg = 0x00;
			}
		}
		if(elemflg != 0x00 && *copyText == target){
			//������ɓ������Ƃ��͗]����+
			*copyText = replacement;

		}
	}
}

//�Ώۃe�[�u����truncate(���e���N���A)
//IN �F
//  tablename �F ���e���N���A����e�[�u����(�ǉ�����e�[�u����)
//OUT �F 0 ����
//       1 �ُ�
//author:koyama
int truncateTable(char *tablename){
	int ret =0;
	char sqlstr[2048] = "";
	char tName[256] = "";
	MYSQL_ROW    res;
	MYSQL_RES    *result;

	strcpy(tName,tablename);

	//�Ώۃe�[�u�����̋�`�F�b�N
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

//����̕������������Ȃ���Ȃ���
//return �Ȃ�
void remTargChar(char *origText,char targ){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//���s�������΂�
		if(*copyText == targ ){
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		//�����̃R�s�[
		*origText = *copyText;
		copyText++;
	}
}

//����̕������������Ȃ���Ȃ���
//return �Ȃ�
void remCRLF(char *origText){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//���s�������΂�
		if(*copyText == 0x0A || *copyText == 0x0D){
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		//�����̃R�s�[
		*origText = *copyText;
		copyText++;
	}
}

//������𒷂��(NULL��������)�Ō�������
//IN�F
//  mainstr�F�Ȃ����̑Ώە�����
//  mainLength�F�Ȃ����̌��݂̒���
//  substr�F�Ȃ�������
//  subLength�F�Ȃ�������̒���
//OUT�F�Ȃ�����̕�����|�C���^
//author:koyama
char *mystrncat(char *mainstr ,int mainLength ,char * substr,int subLength){
	memcpy((mainstr + (mainLength)),substr,subLength);
	return mainstr;
}

//*********************************************************
//�X�v���b�g�֐�(�ꕶ���Ή�)
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
//�X�v���b�g�֐�(��L�֐��ł͋󕶎���ɑΉ����Ȃ��̂ō��ς�)
//in/out :oList ��؂���������̔z��
//in     :�Ώە�����
//author : koyama
int strSplit( char *iStr, char *iDelim, char **oList ) {
	char	*tk;
	char    *preneedle,*needle,*postneedle;
	int		icnt = 0,ocnt=0;

	//token
	tk = iStr;

	//�ŏ���token
	oList[ocnt] = tk;
	ocnt++;

	//�ŏ���token�ŏ��������Ă���
	preneedle  = tk;
	needle     = tk;
	postneedle = tk;

	for(;preneedle != NULL && ocnt < 4096;ocnt++){
		//needle���Ȃ��Ȃ�܂�
		for(icnt = 0; *(iDelim + icnt) != '\0';icnt++){
			postneedle = strchr(preneedle,*(iDelim + icnt));
			//�����������A�����Ă���ꍇ�͔�΂�
			for(;postneedle !=NULL && *(postneedle + 1) == *(iDelim + icnt);){
				postneedle = (postneedle + 1);
			}
			if(icnt==0 || postneedle < needle){
				needle = postneedle;
			}
		}
		//�g�������Ƃ̂܂܂������甲����
		if(needle == NULL){
			oList[ocnt] = NULL;
			break;
		}
		//���݂�token���i�[
		oList[ocnt] = (needle + 1);
		*needle = '\0';
		preneedle  = (needle + 1);
		//�g�������Ƃ�needle��null��
		needle = NULL;
	}
	return ocnt;
}

//*********************************************************
//�X�v���b�g�֐�(�v�f�����u)�v����) &[@]<&[)]���O��
//�J���}��؂�ɏ�������b
//auth:iida
//upd :koyama
//*********************************************************
int strSplit_ELM( char *iStr, char *oList[] ) {
	char *tk;
	int  cnt = 0;
	int  str_flg = 0;
	char *str1;			//�u@�v�ʒu(�����ʒu�͔�r�����̐擪�|�C���^)
	char strDelim[] = ",";
	char chrDelim = ',';

	str1 = iStr;

	if(strchr(str1,chrDelim)){
		tk = strtok( str1, strDelim );
		oList[cnt] = tk;
		//��؂蕶��[,]������
		while(tk != NULL){
			cnt++;
			tk = strtok( NULL, strDelim );
			oList[cnt] = tk;
		}
	}else{
		//��؂肪�ЂƂ��Ȃ��Ƃ��͂��̑Ώۂ̂�
		oList[cnt] = iStr;
		cnt++;
	}

	return cnt;
}

//*********************************************************
//������u��(�o�C�i���f�[�^��Ή�)
//auth:iida
//*********************************************************
int strChg(char *ioBuf, const char *iStr1, const char *iStr2)
{
	int		ret = 1;
	char	tmp[1024 + 1];
	char	*p;
	char	s = '\0';

	while ((p = strstr(ioBuf, iStr1)) != NULL) {
		//������Ȃ��Ȃ�܂ŌJ��Ԃ��Bp�͋�������̐擪���w��
		*p = s;					// ���̕��������������̒��O�ŋ�؂���
		p += strlen(iStr1);		// �|�C���^����������̎��̕�����
		strcpy(tmp, p);			// �������񂩂���ۑ�
		strcat(ioBuf, iStr2);	// �V����������̌�ɂȂ�
		strcat(ioBuf, tmp);		// ����Ɏc����Ȃ�
		ret = 0;				// �q�b�g�����ꍇ�ɖ߂�l�ύX
	}

	return ret;
}
//*********************************************************
// �ϒ������� *ioData �� iStr �ŏ���������
//auth:iida
//*********************************************************
int string_init(char ** ioData, char const * iStr){
	int ret = 0;

	String_t * self;
	size_t size = strlen(iStr) + 1;

	self = (String_t *)malloc( sizeof(String_t) + sizeof(char) * size );

	/* �m�ۂ����̈��NULL�N���A */
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
// �ϒ������� *ioData ��j������
//auth:iida
//*********************************************************
int string_fin(char ** ioData){
	free(container_of(*ioData, String_t, data));
	return 0;
}
//*********************************************************
// �ϒ�������̒�����Ԃ�
//auth:iida
//*********************************************************
size_t string_length(char ** ioData){
	return (container_of(*ioData, String_t, data))->size - 1;
}

//*********************************************************
// �A������
//auth:iida
//*********************************************************
int string_concat(char ** ioData, char *iStr){
	int ret = 0;

	char * old = *ioData;
	String_t * self;
	size_t size = strlen(iStr) + string_length(ioData) + 1;

	self = (String_t *)malloc( sizeof(String_t) + sizeof(char) * size);

	/* �m�ۂ����̈��NULL�N���A */
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
// ������ String �� nShift �����ړ�����B
// �ړ���ւ̃|�C���^��Ԃ��B
//auth:iida
//*********************************************************
char *StrShift( char *String, size_t nShift ){
	char *start = String;
	char *stop  = String + strlen( String );
	memmove( start + nShift, start, stop-start+1 );

	return String + nShift;
}

//*********************************************************
// ������ String �̕����� From �𕶎��� To �Œu������B
// �u����̕����� String �̃T�C�Y�� String �̋L���̈�𒴂���ꍇ�̓���͖���`�B
//auth:iida
//*********************************************************
char *StrReplace( char *String, const char *From, const char *To ){
	int   nToLen;   // �u�����镶����̒���
	int   nFromLen; // �������镶����̒���
	int   nShift;
	char *start;    // �������J�n����ʒu
	char *stop;     // ������ String �̏I�[
	char *p;

	nToLen   = strlen( To );
	nFromLen = strlen( From );
	nShift   = nToLen - nFromLen;
	start    = String;
	stop     = String + strlen( String );

	// ������ String �̐擪���當���� From ������
	while( NULL != ( p = strstr( start, From ) ) )
	{
		// ������ To �����ʂł���悤�ɂ���
		start = StrShift( p + nFromLen, nShift );
		stop  = stop + nShift;

		// ������ To �𕡎�
		memmove( p, To, nToLen );
	}

	return String;
}//StrReplace

//�Ώۂ̃I�v�V�����̗v�f���̃`�F�b�N
//input:�Ώۂ̃I�v�V��������������������|�C���^,�Ώۂ̃I�v�V�����̗v�f��
//author:koyama
int checkParamFormat(char *strTarg,char *targ){
	char *currPnt;
	int retval=0,commaCnt=0;

	currPnt = targ;

	if(strncmp(strTarg,"PA5",strlen("PA5")) == 0){
		//�ŏ��̕����͌^�Ȃ̂Ŕ�΂�
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
		//V���ЂƂȏ�
		if(commaCnt > 1){
			retval=1;
		}
	}
	return retval;
}


//*********************************************************
//�v�f�����֐�
//auth:iida
//*********************************************************
int set_argv(Argv_inf *ioTrgt_arg, char * iArgv){
	int ii,jj, ret = 0;

	char *strOptArr[ ITEMLST +1] = {'\0'};		//�I�v�V������
	int result_ii = 0;
	int result_jj = ARGITM;
	int countUp_s_point = 1;
	char size_e[1];
	char strBuff[11] = "";              //�X�g�����O�̒����𑪂�_�~�[��buffer
	char strTime[] = "0000/00/00 00:00:00.000000";

	//�����͓r����'\'�����邱�Ƃ͂Ȃ����Ash�̏������Ƃ��ē����Ă��邱�Ƃ�����
	//���s�ƂƂ��ɏ���
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
		//������
		//�J�n�ʒu
		ioTrgt_arg[ii].s_point = countUp_s_point;

		if(checkParamFormat((iArgv - 4),strOptArr[ii])){
			mytool_runtime_error('E',source_file_name," Error C [%02d]:Format of the length is not correct     %s ",17,local_server_time(strTime));
			raise(SIGABRT);
		}
		//����
		//2�����ڂ��������ǂ�������
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

		//�����������Ă��Ȃ���Ώ����l0�����葱����
		ioTrgt_arg[ii].length = colLength;

		// �o�̓^�C�v
		ioTrgt_arg[ii].type    = malloc(sizeof(size_e) * (1 + 1));
		memset(ioTrgt_arg[ii].type,'\0',sizeof(size_e) * (1 + 1));
		if(ioTrgt_arg[ii].type == NULL){
			ret = 1;
			return ret;
		}
		ioTrgt_arg[ii].type[0] = *(strOptArr[ii] + 0);
		ioTrgt_arg[ii].value   = strOptArr[ii];
	}

	//�c���ڂ̏�����
	for(ii=ii; ii < ITEMLST; ii++){
		ioTrgt_arg[ii].s_point = 0;
		ioTrgt_arg[ii].value   = "";
	}

	return ret;
}

//�K�{���͓��̈����������������Ă��邩
//ioTrgt:�����I�u�W�F�N�g
//return �G���[1,����0
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
	//2015/08/28 kawada add S PB3�w��̃G���[(�w�蕶����K��O)����
	if (strcmp(ioTrgt->opmod, "_") == 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:File Open Mode is Incorrect             %s ",02,local_server_time(strTime));
		return 1;
	}

	return 0;
}

//*********************************************************
//���������֐�
//IN  �F ioTrgt->�Z�b�g������������\����
//       iArgv->����������
//OUT �F0����
//      1�ُ�
//auth: iida
int set_trgt(Trgt_inf *ioTrgt, char *iArgv){
	int ii,ret = 0, cnt =0;
	char wkstr[120+1] = "";
	char *strOptArr[20+1] = {'\0'};		//�I�v�V�����ʂ̕�����|�C���^
	char *strTmpArgv = iArgv;
	char strPara[20];		//�����̒l	//2015/08/28 kawada add

	//�萔�̊Ԃ�SP����������u��
	chgChar(strTmpArgv,(char)0x20,(char)0x07,(char)0x40);
	//���������u �v�ŃI�v�V�����ʂɋ敪������
	int result = strSplit(strTmpArgv," ",strOptArr);


	if(result > (20 + 1)){
		return 1;
	}

	for(ii=0; ii< result; ii++){
		chgChar(strOptArr[ii],(char)0x07,(char)0x20,(char)0x40);
		if (strstr(strOptArr[ii], "PA3=") != NULL ) {
			//�e�[�u�����ݒ�
			ioTrgt->ifi = (strchr(strOptArr[ii],'=') + 1);
			//�K�{���̓`�F�b�N
			if(strlen(ioTrgt->ifi) <= 0){
				ret = 1;
				break;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PB1=") != NULL ) {
			//�t�@�C�����ݒ�
			ioTrgt->ofi = (strchr(strOptArr[ii],'=') + 1);
			chgChar(ioTrgt->ofi,(char)0x20,(char)0x00,(char)0x00);
			//�K�{���̓`�F�b�N
			if(strlen(ioTrgt->ofi) <= 0){
				ret = 1;
				break;
			}
			continue;
		}else if (strstr(strOptArr[ii], "MN1=") != NULL ) {
			//�t�@�C�����ݒ�
			ioTrgt->deb = *(strchr(strOptArr[ii],'=') + 1);
			//�K�{���̓`�F�b�N
			continue;
		}else if (strstr(strOptArr[ii], "MN2=") != NULL ) {
			//�ϊ����@
			if((strstr(strOptArr[ii], "AD") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//AV-X->DOS�̏ꍇ
				ioTrgt->mod = 0;
			}else if((strstr(strOptArr[ii], "DA") != NULL ) ){
				//DOS->AV-X�̏ꍇ
				ioTrgt->mod = 1;
			}else{
				//��L�ȊO
				ioTrgt->mod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PAA=") != NULL ) {
			//�������ݕ��@
			if((strstr(strOptArr[ii], "NEW") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//�V�K
				ioTrgt->wmod = 0;
			}else if((strstr(strOptArr[ii], "MOD") != NULL ) ){
				//�ǉ�
				ioTrgt->wmod = 1;
			}else{
				//��L�ȊO
				ioTrgt->wmod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "PA5=") != NULL ) {
			//AV-X�t�B�[���h�\���ݒ�
			ret = set_argv(ioTrgt->out, (strchr(strOptArr[ii],'=') + 1));	//�v�f����
			if(ret){
				break;
			}else{
				continue;
			}
		}else if (strstr(strOptArr[ii], "PB7=") != NULL ) {
			//DOS�t�B�[���h�\���ݒ�
			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));	//�v�f����
			if(ret){
				break;
			}else{
				continue;
			}
		//2015/08/28 kawada add S PB3�w��̃`�F�b�N�A�l�ݒ�
		}else if (strstr(strOptArr[ii], "PB3=") != NULL ) {
			//DOS�`���̃t�@�C���I�[�v�����[�h�ݒ�
			memset(strPara, '\0', strlen(strPara));
			if (strlen(strOptArr[ii]) > strlen("PB3=")){
				strcpy(strPara, (strchr(strOptArr[ii],'=') + 1));	//�w�肪����Ƃ��̂ݕϐ��֕���
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
					//(�w��Ȃ�)
					strcpy(ioTrgt->opmod, "S");		//�w�肪�Ȃ��Ƃ���SHARE�Ƃ���
			}else{
					//�w�肪�����ċK��ȊO
					strcpy(ioTrgt->opmod, "_");
			}
			if (strcmp(ioTrgt->opmod, "_") != 0){
				//�������[�h�ɂ��t�@�C���I�[�v�����[�h���m��
				if (ioTrgt->mod == 0){
					//AV-X->DOS
					strcat(ioTrgt->opmod, "I");
				}else if (ioTrgt->mod == 1) {
					//DOS->AV-X
					strcpy(ioTrgt->opmod, "SO");	//������̎w��ł�SHARE OUTPUT(SO)�ɒu������
				}
			}
			continue;
		}
	}

	return ret;
}

//*********************************************************
//int �� char �ϊ�
//*********************************************************
char *itoa( int val, char *str, int radix )
{
	char *p = str;
	unsigned int v = val;				// ��Ɨp(�ϊ��Ώۂ̒l)
	int n = 1;							// �ϊ�������̌����L���p
	while(v >= radix){					// ���������߂�
		v /= radix;
		n++;
	}
	p = str + n;						//�ŉ��ʂ̈ʒu����ݒ肷��
	v = val;
	*p = '\0';							// ������I�[�̐ݒ�
	do {
		--p;
		*p = v % radix + '0';			// 1���̐��l�𕶎��ɕϊ�
		if(*p > '9') {					// �ϊ�����������10�i�ŕ\���ł��Ȃ��ꍇ
			*p = v % radix - 10 + 'A';	// �A���t�@�x�b�g���g��
		}
		v /= radix;
	} while ( p != str);

	return str;
}

//["]������
//IN �F
//  inStr �F �Ώە�����(�Q�ƂƂ��ĕԂ�)
//OUT �F �Ȃ�
//author:koyama
char *remDoubleQuote(char *inStr){
	char *strConv;
	char size_e[1];
	//���̕�����Ɠ����������Z���Ȃ邽��
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


//���s����(LF,CR)��NULL�����ɕς���
//IN �F
//  targStr �F �Ώە�����
//OUT �F �Ȃ�
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

//�G�X�P�[�v���镶�����G�X�P�[�v���Ȃ���f�[�^���Ȃ�
//�G�X�P�[�v�̑Ώۂ̓V���O���N�H�[�g(0x27)�Ɖ~�L��(0x5c)
// �N�G��������Ƃ���INSERT����Ƃ��Ɏg�p
//�h�m�F
//  targv �F �ΏۂƂȂ钷���t������
//  addtext �F �Ȃ�������
//  len �F �Ȃ�������̘_���I�Ȓ���
//OUT �F �Ȃ�
//author:koyama
int lenStr_addStringToSQL(char *targv,int *toLen,char *addtext,int fromLen){
	int result = 0;
	int addlen = 0;
	char size_e[1];
	char *copyText;
	char *copyAddText;
	char *origText;
	char *origTextEnd;

	//copyText�̏�����
	//�Ȃ��̂ɕK�v
	copyAddText = (char *)malloc((sizeof(size_e) * (fromLen * 2)));
	copyText    = copyAddText;
	origText    = addtext;
	origTextEnd = (addtext + fromLen);

	//
	for(;origText < origTextEnd;copyText++){
		//�V���O���N�H�[�g(':0x27)�~�L��(\:0x5c) ���G�X�P�[�v
		if(*origText == 0x27 || *origText == 0x5c){
			//�G�X�P�[�v�����͉~�L��(\:0x5c)
			*copyText = 0x5c;
			addlen++;
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		*copyText = *origText;
		origText++;
		addlen++;
	}

	//���ۂ̃R�s�[
	memcpy((targv + *toLen),copyAddText,addlen);
	*toLen += addlen;

	free(copyAddText);
	return result;
}

//�G�X�P�[�v���镶�����G�X�P�[�v���Ȃ���f�[�^���Ȃ�
//�G�X�P�[�v�̑Ώۂ̓V���O���N�H�[�g(0x27)�Ɖ~�L��(0x5c)
// CSV������Ƃ��ďo�͂���Ƃ��Ɏg�p
//�h�m�F
//  targv �F �ΏۂƂȂ钷���t������
//  addtext �F �Ȃ�������
//  len �F �Ȃ�������̘_���I�Ȓ���
//OUT �F �Ȃ�
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

	//copyText�̏�����
	//�Ȃ��̂ɕK�v
	copyAddText = (char *)malloc((sizeof(size_e) * (fromLen * 2)));
	copyText    = copyAddText;
	origText    = addtext;
	origTextEnd = (addtext + fromLen);

	//
	for(;origText < origTextEnd;copyText++){
		//�_�u���N�H�[�g(":0x27)���G�X�P�[�v
		if(*origText == 0x22 ){
			//�G�X�P�[�v�����͉~�L��(\:0x5c)
			*copyText = 0x22;
			addlen++;
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		*copyText = *origText;
		origText++;
		addlen++;
	}

	//���ۂ̃R�s�[
	memcpy((targv + *toLen),copyAddText,addlen);
	*toLen += addlen;

	free(copyAddText);
	return result;
}

//�O���[�o���ϐ�����ݒ���擾���A�f�[�^�x�[�X�ɐڑ�����
//IN �F�Ȃ�
//OUT�F0����
//     1�ُ�
//author:koyama
int DB_Open(DB_SETTING *targSetting){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	char strAutocommit[] = "SET AUTOCOMMIT=0;";
	mysql = mysql_init(NULL);

	if (!mysql_real_connect(mysql, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0)) {
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Connect Error %s                    %s", 02,mysql_error(mysql),local_server_time(strTime));
		//�ڑ��s��
//		raise(SIGABRT);
		ret = 1;
	}else{
	    //�ڑ���
	    ret = 0;
	}
	//AutoCommit Off
	ret = mysql_query(mysql, strAutocommit);

	unqId = getUid();		//2015/08/28 kawada add �Ǘ��e�[�u���̍X�V�p�Ƀ��j�[�NID���擾����

    return ret;
}

//�ݒ�̒l���g���ăf�[�^�x�[�X�����
//IN �F�Ȃ�
//OUT�F0����
//     1�ُ�
//author:koyama
int DB_Close(){
	int ret =0;
	//DB�ڑ��̐ؒf

	// �Ǘ��e�[�u���p�̃R�l�N�V�����G���h
	setTableUnlock(NULL,NULL);
	if(background_targSetting != NULL){
		//return void�Ȃ̂ŕK��0�ŕԂ�
		mysql_close(mysql);
	}
	return ret;
}

//*********************************************************
//�ݒ�t�@�C���̓ǂݍ���
//*********************************************************
int conf_read(DB_SETTING *targSetting){
	int ii;
	char strConfPath[1024]; //20150828 add koyama

	//���Œl���ݒ肳��Ȃ������炱�ꂪdefault
	myConfDebugFlg = 0;

	//�t�@�C���l�[�������Ƀ��[�_�|�C���^���쐬   //�t�@�C������ϐ��ɕύX 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//2015/08/28 kawda E
	//���[�_�����[�h�ł����Ԃ�
	xmlTextReaderRead(reader);
	//���݂̃m�[�h�̃|�C���^���Z�b�g�H
	xmlTextReaderExpand(reader);
	//���݂̃m�[�h����DOM�����o���Ă���?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return 1;
	//�h�L�������g����R���e�L�X�g()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
    if (!ctx) return 1;
	//xpath�Ŏw�肵���m�[�h���X�g���擾
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
    if (!xpobj) return 1;
	//�m�[�h���X�g���m�[�h�̔z��̂悤�Ȃ��̂�
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//�m�[�h���̎擾(�擾�ł��Ȃ��Ȃ�0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//�m�[�h���X�g����l��\��
	for (ii = 0; ii < size; ++ii) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, ii);
			if (node->content) {
				//�ݒ�t�@�C������DB�̃z�X�g�����擾
				if(strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(targSetting->db_server,node->content);
				}
				//�ݒ�t�@�C������DB�̃��[�U�����擾
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(targSetting->db_user,node->content);
				}
				//�ݒ�t�@�C������DB�̃p�X���[�h���擾
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(targSetting->db_password,node->content);
				}
				//�ݒ�t�@�C������DB�����擾
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(targSetting->db_database,node->content);
				}
				//�ݒ�t�@�C������|�[�g���擾
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					targSetting->db_port = atoi(node->content);
				}
				//2015/08/28 kawada add S �Ǘ��e�[�u��ID�̎擾
				//�ݒ�t�@�C�����狤�L���[�h�p�̃e�[�u�������擾
				if(strcmp(node->parent->name,MANA_TBL_NAME) == 0){
					strcpy(targSetting->db_manatbl,node->content);
				}
				//debug�t���O������Ȃ�擾
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//�Ԋ҂ł��Ȃ��������0�ɂȂ�͂�
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

//�ݒ�̓ǂݍ��݂ƃf�[�^�x�[�X�ւ̐ڑ����s��
//IN  �F targSetting->�f�[�^�x�[�X�ڑ����
//OUT �F �Ȃ�
//author:koyama
int db_init(DB_SETTING *targSetting){
	int result=0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	cob_init(0, NULL);
	//�G���[�n���h�����Z�b�g(���cob_init�����Ă���̂ł�����㏑������)
	setErrorHundle();
	if(conf_read(targSetting)){
		mytool_runtime_error('E',source_file_name," Error C [%02d] :can't read configuration file           %s ", 01, local_server_time(strTime));
		result = 1;
	}else{
		if(DB_Open(targSetting)){
			result = 1;
		}
	}
	//�G���[�ɂȂ������悤�ɃR�s�[���Ƃ��Ă���
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
//SQL���s�G���[���̌㏈��
//*********************************************************
int mysql_failure(){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//�G���[���e���o��
	mytool_runtime_error('E',source_file_name," Error C [%02d]:Query Error %s [%s] %s ", 32,mysql_error(mysql),myself,local_server_time(strTime));

	//���[���o�b�N
	ret = mysql_query(mysql, "ROLLBACK; ");
	//DB�ڑ��̐ؒf

//	mysql_close(mysql);

	return ret;
}

//�e�L�X�g�t�@�C���̍sorDB�̃f�[�^�����݂��邩
//author:koyama
int existRowData(Trgt_inf *trgt){
	int ret = 0;

	if(trgt->mod == 1){
		//�t�@�C��2DB

	}else if(trgt->mod == 0){
		//DB2�t�@�C��

	}

	return ret;
}

//�o��
//IN  �F itrgt->�������
//       ,strTo->�ϊ��㕶����̃|�C���^
//       ,intLength->������̒���
//OUT �F 0->����, 1->���s
//author:koyama
int outputConvData(Trgt_inf *itrgt,char *strTo,int intLength){
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : outputConvData :%.30s",strTo);

	if(itrgt->mod == 1){
		//�t�@�C��2DB
		//SQL�̍쐬
		char sqlStr[2048]="";     //SQL�i�[�p
		int sqlstrlen=0;          //SQL������̌��݂̒���


		strcat(sqlStr,"INSERT INTO `");
		strcat(sqlStr,itrgt->ifi);
		strcat(sqlStr,"`\n (ITEM) \n VALUES('");
		sqlstrlen = strlen(sqlStr);
		mystrncat(sqlStr ,sqlstrlen ,strTo,intLength);
		sqlstrlen += intLength;
		mystrncat(sqlStr ,sqlstrlen ,"') ",strlen("') "));
		sqlstrlen += strlen("') ");
		//�f�[�^�̒ǉ�
		if(mysql_real_query(mysql, sqlStr,sqlstrlen) != 0){
			ret = 1;
		}
	}else{
		//DB2�t�@�C��
		//fputs�͎��s�̂Ƃ�EOF(-1)��Ԃ�
		if(fputs(strTo,itrgt->fp) == EOF){
			ret = 1;
		}
	}

	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}

//�f�[�^�ϊ�(��s�P��)
//IN  �F itrgt->�������
//       ,strTo->�ϊ��㕶����̃|�C���^
//       ,strFrom->�ϊ��㕶����̃|�C���^
//OUT �F 0->����, 1->���s (ref)�ϊ��㕶����
//author:koyama
int cnvTargToChange(Trgt_inf *itrgt,char *strTo,char *strFrom,int *intLength){
	int ret = 0;
	int cnt = 0;
	char *strLocalFrom = strFrom;


	//copy�p��cobol
	cob_field_attr a_From = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field       f0;
	cob_field       f1;

	//0�ŏ��������Ă����A�Ō��intLength��add
	int intLocalLength = 0;
	int intLocalFromLength = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : cnvTargToChange :%.30s",strFrom);

	//��`��(������)
	cob_current_module = &module;

	//���[�h�̐؂�ւ�
	if(itrgt->mod == 1){
		char *pComma;        //CSV�Ȃ̂�,�̈ʒu�|�C���^������

		//�t�@�C��2DB
		while(cnt < ITEMLST){
			int fromLength = 0,toLength=0;      //���݂�db�̒l�ɑΉ�����f�[�^�̒���
			char strTempFrom[4096]="";              //�s�x���������邽�߃��[�v�̒�
			char strTempTo[4096]="";                //�s�x���������邽�߃��[�v�̒�

			//1�J�������̕������temporary�ɃR�s�[
			if ((pComma=strchr(strLocalFrom,',')) != NULL){
				//����,�̈ʒu�ŕ������؂�
				*pComma=0x00;
				strcat(strTempFrom,strLocalFrom);
			}else{
				strcat(strTempFrom,strLocalFrom);
			}


			//\r���c��\�������邽�ߍ폜
			remCRLF(strTempFrom);

			//"�݂͂�����Ȃ珜��
			remDoubleQuote(strTempFrom);
			//�������f�[�^�̒������`
			if(itrgt->out[cnt].type[0] == 'J'){
				//2�o�C�g�R�[�h�̂Ƃ���*2
				fromLength = strlen(strTempFrom);
				toLength = itrgt->out[cnt].length * 2;
			}else{
				fromLength = strlen(strTempFrom);
				toLength = itrgt->out[cnt].length;
			}


			//�ϊ�����
			switch(itrgt->out[cnt].type[0]){
			case 'C':
				//����
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_From.digits = fromLength;
				//�J���}�̈ʒu
				a_From.scale  = 0;
				a_From.flags  = 0;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'J':
				//
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_NATIONAL;
				a_From.digits = fromLength;
				//�J���}�̈ʒu
				a_From.scale  = 0;
				a_From.flags  = 0;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_NATIONAL;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'N':
				//����
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//�����͍��v�̂͂�
				a_From.digits = fromLength;
				a_From.scale = 0;
				a_From.flags  = 0;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_NUMERIC;
				a_to.digits  = toLength;
				a_to.flags = 0;
				a_to.pic   = NULL;
				//�J���}�̈ʒu
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				break;
			case 'S':
				//����������
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//�����͍��v�̂͂�
				a_From.digits = fromLength;


				//�ϊ���cob_field_attr�̐ݒ�
				a_to.type = (unsigned char)COB_TYPE_NUMERIC;
				//�����͍��v�̂͂�
				a_to.digits = toLength;

				//�J���}�̈ʒu
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				//SIGN�̂Ƃ��̓t���O
				a_to.flags = COB_FLAG_HAVE_SIGN;

				break;

				//pic��editing�t�H�[�}�b�g�Ȃ���
			case 'P':
			case 'Q':
				//�p�b�N10�i��
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				//�����͍��v�̂͂�
				a_From.digits = fromLength;


				//�ϊ���cob_field_attr�̐ݒ�
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_PACKED;
				//�����͂��̂܂܃T�C�Y�̓o�C�g��((�f�[�^��+����) / 2�̐���)
				a_to.digits = toLength;
				toLength = (int)ceil((toLength + 1) / 2.0);

				//�J���}�̈ʒu
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_to.scale  = 0;
				}else{
					a_to.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
				}

				//SIGN�̂Ƃ��̓t���O
				if(itrgt->out[cnt].type[0] == 'P'){
					a_to.flags = COB_FLAG_HAVE_SIGN;
				}
				break;

				//pic��editing�t�H�[�}�b�g�Ȃ���
			default:
				break;
			}

			//from
			f0.size = fromLength;
			f0.data = strTempFrom;
			f0.attr = &a_From;
			//to
			//�����̒����͌��̒����ɍ��킹�Ă���
			f1.size = toLength;
			f1.data = strTempTo;
			f1.attr = &a_to;
			cob_move(&f0,&f1);

			//�l�̃R�s�[
			//intLocalLength��SQL�ɂ����Ă����Ƃ��̒������A���Ă���
			lenStr_addStringToSQL(strTo,&intLocalLength,strTempTo,toLength);
//			fprintf(stdout,"FROM:%s",strTempFrom);
//			fprintf(stdout,"TO  :%s",strTempTo);
//			fprintf(stdout,"type:%s",itrgt->out[cnt].type);
//			fprintf(stdout,"type:%s",itrgt->sel[cnt].type);

			//���̂���
			cnt++;
			if(itrgt->out[0].s_point == 0){
				break;
			}

			/*'�'�̈ʒu�����炵�Ď��̈ʒu�� */
			if(pComma == NULL){
				break;
			}
			pComma++;
			strLocalFrom=pComma;
		}
	}else{
		//�f�[�^�̕������Ƃɕϊ�
		//DB2�t�@�C��
		while(cnt < ITEMLST){
			//�J������̃f�[�^��ێ�
			//���̎d�l�ł�4096�̒�����C�̍ő�炵��
			char strTempFrom[4096]="";              //�s�x���������邽�߃��[�v�̒�
			char strTempTo[4096]="";                //�s�x���������邽�߃��[�v�̒�
			int fromLength = itrgt->out[cnt].length;    //�Ȃ������̒���(1�J�����̒���)
			int toLength = fromLength;

			//from�̒������Ⴄ�P�[�X��������̂͂����Ŏw��
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
			//1�J�������̕������temporary�ɃR�s�[
			memcpy(strTempFrom,strLocalFrom,fromLength);

			//�Ȃ���[,]�����Ă���
			if(cnt != 0){
				strTo[intLocalLength] = ',';
				intLocalLength += 1;
			}

			//pic������
			// [�萔],[������],[��],[��],[��]
			// '-','0x01','0x00','0x00','0x00','9','0x[����]','0x00','0x00','0x00'
			char a_topic[48] = "";
			//mode������ׂ�?            check
			//�����Ȃ�["]�ň͂�
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

			//�ϊ�����
			switch(itrgt->out[cnt].type[0]){
			case 'C':
				//from
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_From.digits = fromLength;
				//�J���}�̈ʒu
				a_From.scale  = 0;
				a_From.flags  = 0;
				a_to.pic   = NULL;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'J':
				//2�o�C�g�R�[�h
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_NATIONAL;
				a_From.digits = fromLength;
				//�J���}�̈ʒu
				a_From.scale  = 0;
				a_From.flags  = 0;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_ALPHANUMERIC;
				a_to.digits  = toLength;
				a_to.scale = 0;
				a_to.flags = 0;
				a_to.pic   = NULL;
				break;
			case 'N':
				//����
				//Z(n-1)9(1)
				memcpy(a_topic,"Z\000\000\000\0009\001\000\000\000",10);
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC;
				//�����͍��v�̂͂�
				a_From.digits = fromLength;
				//�J���}�̈ʒu
				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//�����_�̕����炷
					toLength++;
				}
				a_to.flags  = 0;
				//pic��editing�t�H�[�}�b�g�Ȃ���

				//to
				//������
				a_to.type    = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits  = toLength;
				a_to.scale = a_From.scale;
				a_to.flags = 0;
				//Z(n-1)9(1)�Ȃ̂�-1
				a_topic[1] = ((char)atoi(itrgt->out[cnt].value + 1) - 1);
				a_to.pic   = a_topic;
				break;
			case 'S':
				//����������
				//-(n)9(1)
				memcpy(a_topic,"-\001\000\000\0009\001\000\000\000",10);
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC;
				//�������������炷�̂�+1
				toLength = toLength + 1;
				//�����͍��v�̂͂�
				a_From.digits = fromLength;

				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//�����_�̕����炷
					toLength++;
				}
				a_From.flags  = COB_FLAG_HAVE_SIGN;

				//�ϊ���cob_field_attr�̐ݒ�
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits = a_From.digits;
				a_to.scale = a_From.scale;
				a_to.flags = a_From.flags;

				//�t�H�[�}�b�g���ɒ���
				a_topic[1] = (char)atoi(itrgt->out[cnt].value + 1);
				a_to.pic = a_topic;

				break;
				//pic��editing�t�H�[�}�b�g�Ȃ���
			case 'P':
			case 'Q':
				//�p�b�N10�i��
				//�ϊ���cob_field_attr�̐ݒ�
				a_From.type   = (unsigned char)COB_TYPE_NUMERIC_PACKED;
				//�����͂��̂܂܃T�C�Y�̓o�C�g��((�f�[�^��+����) / 2�̐���)
				//toLength�𕄍��ɍ��킹�Ă��炷�\��������̂ł��̑O��
				a_From.digits = toLength;

				//�Ⴄ�p�^�[��
				if(itrgt->out[cnt].type[0] == 'P'){
					memcpy(a_topic,"-\001\000\000\0009\001\000\000\000",10);
					//�������������炷�̂�+1
					toLength = toLength + 1;
					a_From.flags  = COB_FLAG_HAVE_SIGN;
					//�t�H�[�}�b�g���ɒ���
					a_topic[1] = (char)atoi(itrgt->out[cnt].value + 1);
				}else{
					memcpy(a_topic,"Z\000\000\000\0009\001\000\000\000",10);
					//Z(n-1)9(1)�Ȃ̂�-1
					a_topic[1] = ((char)atoi(itrgt->out[cnt].value + 1) - 1);
				}

				if(strchr(itrgt->out[cnt].value,'V') == 0){
					a_From.scale  = 0;
				}else{
					a_From.scale  = atoi((char *)(strchr(itrgt->out[cnt].value,'V') + 1));
					memcpy((a_topic + 10),".\001\000\000\0009\000\000\000\000",10);
					*(a_topic + 16) = (char)a_From.scale;
					//�����_�̕����炷
					toLength++;
				}

				//�ϊ���cob_field_attr�̐ݒ�
				a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				a_to.digits = a_From.digits;
				a_to.scale = a_From.scale;
				a_to.flags = a_From.flags;

				a_to.pic = a_topic;

				break;
				//pic��editing�t�H�[�}�b�g�Ȃ���
			default:
				break;
			}

			//from
			f0.size = fromLength;
			f0.data = strTempFrom;
			f0.attr = &a_From;
			//to
			//�����̒����͌��̒����ɍ��킹�Ă���
			f1.size = toLength;
			f1.data = strTempTo;
			f1.attr = &a_to;
			cob_move(&f0,&f1);

			//�������擾���Ă����f�[�^���𒴂��Ă�����A�������l���R�s�[�ł��Ȃ��Ƃ���
			//�R�s�[�����Ɏ��̏�����
			intLocalFromLength += fromLength;
			if(intLocalFromLength > itrgt->dataLength){
				break;
			}

			//�l�̃R�s�[
			lenStr_addStringToCSV(strTo,&intLocalLength,strTempTo,toLength);

			//�����Ȃ�["]�ň͂�
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

			//���̃��[�h�̓|�C���^�����炷
			strLocalFrom = (strLocalFrom + fromLength);
			cnt++;
			if(itrgt->out[cnt].s_point == 0){
				break;
			}
		}
		//�Ō�ɉ��s�R�[�h��ǉ�
		strTo[intLocalLength] = '\r';
		intLocalLength += 1;
		//�Ō�ɉ��s�R�[�h��ǉ�
		strTo[intLocalLength] = '\n';
		intLocalLength += 1;
	}
	//�������񂾒�����Ԃ�
	*intLength = intLocalLength;

	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);

	return ret;
}


//�ϊ��Ώە�����̎擾
//IN  �F itrgt->�������,strRetTarget->�Ώە�����̃|�C���^
//OUT �F 0->����, 1->���s (ref)�ϊ��Ώە�����
//author:koyama
int getTargetString(Trgt_inf *itrgt,char *strRetTarget,int *dataLength){
	int ret = 0;
	char tmp[4096]="";			//file����擾����1�s�̕�����

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"NFCNV : getTargetString :%.30s",strRetTarget);

	if(itrgt->mod == 1){
		//�t�@�C��2DB
		if(fgets(tmp,sizeof(tmp),itrgt->fp)){
			char *replaceChar;
			//���CR������
			if(strchr(tmp,0x0A) != 0){
				replaceChar = strchr(tmp,0x0A);
				replaceChar[0] = '\0';
			}else{
				//����LF������
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
		//DB2�t�@�C��
		unsigned long *f_length;     //�t�B�[���h�̒���(�z��Ƃ��Ď󂯎��̂Ń|�C���^�錾)
		//�s�������Ă��Ȃ���ΏI���(���Ȃ����)
		if((row = mysql_fetch_row(res)) == NULL){
			return 1;
		}
		// if(res->row_count == 0){
		// 	//�s��1�s�������Ă��Ȃ���ΏI���
		// 	return 1;
		// }
		// row = mysql_fetch_row(res);
		if(row != NULL){
			f_length =  mysql_fetch_lengths(res);
			//�t�B�[���h�w�肪ITEM�݂̂Ȃ̂�0�Ԗڂ̂�
			memcpy(strRetTarget,row[0],f_length[0]);
			//�I�[�L���ǉ�
			strRetTarget[f_length[0]] = '\0';
			*dataLength = f_length[0];
			//�������`�������̂�Ԃ�
			itrgt->dataLength = f_length[0];
		}else{
			//�I���ł��G���[�R�[�h��Ԃ�
			ret = 1;
		}
	}else{
		ret = 1;
	}

	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return ret;
}



// -----------------------------------------------------Lock,Unlock Start
//##############################################################################
//#####  F I L E  O P E N  L O C K / U N L O C K ###############################
//##############################################################################
//##############################################################################
//##############################################################################
// ���j�[�NID����
//�Ԓl�F�������ꂽ�A��ӂ�ID
int getUid(){

	unsigned int now = (unsigned int)time( 0 );
	srand( now );
	// srand�֐��ŁA�����p�^�[��������������
	// �v���O�������s���ƂɈقȂ�p�^�[�����g����

	return rand();
}
//�����Ώۃt�@�C����LOCK����
//�����FtargSetting	DB�ݒ�\����
//�����Fitrgt		�󂯎������̏��\����
//�Ԓl�F���s����
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
	char strTime[] = "0000/00/00 00:00:00.000000";	//���Ԃ̕�������i�[

	//�Ǘ��e�[�u���X�V�p�g�����U�N�V����
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_query(mysql, sqlStr) != 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:can't run the Transaction %s %s  %s ", 81, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));

	// �Ώۃe�[�u���̃��b�N��Ԋm�F(���b�N����)
	//TableName�����s��PKey�Ȃ̂ň�s�������Ȃ�20150217
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
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));

	if(result = mysql_store_result(mysql)){
		int rr=0;

		rr=mysql_num_rows(result);

		// �����񏉊���
		memset(sqlStr,'\0',strlen(sqlStr));

		sprintf(strUid,"%d", unqId);
		sprintf(strSS,"%s", itrgt->opmod);

		//��s�̏������Ȃ���΂��̂܂ܐڑ�
		if(rr == 0){
			// �Ǘ��e�[�u���̓o�^
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

				//��s�̔���p2�����𔻒�ϐ��ɃR�s�[
				if (strcmp(res[0], "") != 0){
					memcpy(strDecision, res[0], 2);
				}else{
					memcpy(strDecision, "  ", 2);
				}
				memcpy((strDecision + 2), itrgt->opmod, 2);
				//Lock�ł��邩(Lock�ł��Ȃ��ꍇ�͏����s��)�`�F�b�N
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
					//��s�� E(XCLUSIVE)�Ȃ�Lock�ł��Ȃ�
					//P(ROTECT)�F(��s����s)��(PI��PI)(PI��SI)(P-��SI)(PE��SI)�ȊO��Lock�ł��Ȃ�
					//S(HARE)�F(��s����s)��(SI��PI)(SI��SI)(SI��P-)(SI��S-)(SI��PE)(SI��SE)(S-��SI)(S-��S-)(S-��SE)(SE��SI)(SE��S-)(SE��SE)�ȊO��Lock�ł��Ȃ�
						 mytool_runtime_error('E',source_file_name," Error C [%02d]:alredy locked %s:%s [%s] %s ", 82, itrgt->ifi, targSetting->db_manatbl,myself, local_server_time(strTime));
					ret = 1;
					break;
				}else{
				//�Ǘ��e�[�u����LockMode�̍X�V���K�v���`�F�b�N
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
					   	//(��)�F��(Lock�Ȃ�)��LockMode�u������
						//P(ROTECT)�F(��s����s)��(PI��PI)��LockMode�u������
						//S(HARE)�F(��s����s)��(SI��PI)(SI��SI)(SI��P-)(SI��S-)(SI��PE)(SI��SE)(S-��S-)(SE��S-)(SE��SE)��LockMode�u������
						// �Ǘ��e�[�u���̍X�V
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
			// SQL�̎��s��COMMIT
			if(mysql_query(mysql, sqlStr) != 0){
				mysql_failure();
				ret = 1;
				mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 82, itrgt->ifi);
				return ret;
			}else{
				//�g�����ϐ������Z�b�g
				memset(sqlStr,'\0',strlen(sqlStr));
				//�R�~�b�g
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

//�����Ώۃt�@�C����UNLOCK����
//�����FtargSetting	DB�ݒ�\����
//�����Fitrgt		�󂯎������̏��\����
//�Ԓl�F���s����
int setTableUnlock(DB_SETTING *targSetting, Trgt_inf *itrgt){
	int ret =0,intSub = 0,ii = 0;
	char sqlStr[256]="";
	char strUid[16]="";
	MYSQL_RES	*result;
	char strTime[] = "0000/00/00 00:00:00.000000";	//���Ԃ̕�������i�[

	if(targSetting == NULL && itrgt == NULL){
		targSetting = background_targSetting;
		itrgt       = background_itrgt;
	}
	//BackGround�ɂ��Ȃ��Ƃ���Unlock����K�v�Ȃ�
	if(targSetting == NULL && itrgt == NULL){
		return 1;
	}

	//�Ǘ��e�[�u���X�V�p�g�����U�N�V����
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_ping(mysql) && mysql_query(mysql, sqlStr) != 0){
		//unLock�ŃG���[�ɐ������ꍇ�G���[�̂����悤������
		//mytool_runtime_error(source_file_name," Error C [%02d]:can't run the Transaction %s %s          %s ", 91, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//�g�����ϐ������Z�b�g
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

	// SQL�̎��s
	if(mysql_query(mysql, sqlStr) != 0){
//		mysql_failure();
		ret = 1;
		return ret;
	}else{
		//�g�����ϐ������Z�b�g
		memset(sqlStr,'\0',strlen(sqlStr));
		//�R�~�b�g
		strcat(sqlStr,"COMMIT; ");
		if(mysql_query(mysql, sqlStr) != 0){
			mysql_failure();
			//unLock�ŃG���[�ɐ������ꍇ�G���[�̂����悤������
//			mytool_runtime_error(source_file_name," Error C [%02d]:can't lock %s ", 92, itrgt->ifi);
			DB_Close();
			ret = 1;
			return ret;
		}
	}

	return ret;
}
// -----------------------------------------------------Lock,Unlock END


//�f�[�^�ϊ��A���o��
//IN  �F �Ȃ�
//OUT �F �Ȃ�
//author:koyama
//2015/08/28 kawada �Ǘ��e�[�u��ID���K�v�Ȃ��߁A�����ɒǉ�
//2015/08/28 kawada del int dataconvert(Trgt_inf *itrgt){
int dataconvert(DB_SETTING *targSetting, Trgt_inf *itrgt){		//2015/08/28 kawada add
	int ret = 0;
	FILE *fp;
	int count = 0,dataRowCount = 0,dataLength = 0;
	char sqlStr[2048] = "";
	int intDefDataLen = 0;
	char strTime[] = "0000/00/00 00:00:00.000000";	//���Ԃ̕�������i�[

	//�G���[�ɂȂ������悤�ɃR�s�[���Ƃ��Ă���
	background_targSetting = targSetting;
	background_itrgt       = itrgt;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"%d : dataconvert",source_file_name);

	//���C�������X�^�[�g
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name," Error [%02d]: Info dataconvert Start %s "
		,99,local_server_time(strTime));
	}
	//2015/08/28 kawada add S �Ǘ��e�[�u����Lock���(LockMode)��Lock�ɍX�V
	if (setTableLock(targSetting, itrgt) != 0){
		return 1;
	}
	//2015/08/28 kawada add E

	//�ŏ��̑���
	//�܂��̓g�����U�N�V����
	strcat(sqlStr,"START TRANSACTION; ");
	if(mysql_query(mysql, sqlStr) != 0){
		//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
		mysql_failure();
		setTableUnlock(targSetting, itrgt);
		mytool_runtime_error('E',source_file_name
			," Error C [%02d]:can't run the Transaction %s %s          %s "
			, 10,itrgt->ifi,mysql_error(mysql), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));
	if(itrgt->mod == 1){
		//�t�@�C��2DB
		fp = fopen(itrgt->ofi,"r");
		//�t�@�C���I�[�v���Ɏ��s�����Ƃ��͋����I��
		if(fp == NULL){
			//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name," Error C [%02d]:can't open file %s                  %s ", 11,itrgt->ofi,local_server_time(strTime));
			DB_Close();
			return 1;
		}else{
			itrgt->fp = fp;
		}
	}else if(itrgt->mod == 0){
		//DB2�t�@�C��
		fp = fopen(itrgt->ofi,"w");
		strcat(sqlStr,"SELECT ITEM FROM `");
		strcat(sqlStr, itrgt->ifi);
		strcat(sqlStr,"` ORDER BY ID; ");
		//�t�@�C���I�[�v���Ɏ��s�����Ƃ��͋����I��
		if(fp == NULL){
			//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name
				," Error C [%02d]:can't open output file  %s [%s] %s "
				, 12,itrgt->ofi,myself,local_server_time(strTime));
			DB_Close();
			return 1;
		}else{
			itrgt->fp = fp;
		}
		//�f�[�^���擾
		if(mysql_query(mysql, sqlStr) != 0){
			//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			mytool_runtime_error('E',source_file_name
				," Error C [%02d]:property get error %s %s  [%s] %s "
				, 13,itrgt->ifi,mysql_error(mysql),myself,local_server_time(strTime));
			DB_Close();
			return 1;
		}
		//�O���[�o���Ȉʒu��response�ϐ��Ɏg�p����f�[�^���i�[
		res = mysql_use_result(mysql);
	}else{
		//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
		setTableUnlock(targSetting, itrgt);
		DB_Close();
		return 1;
	}

	//���[�hDA�̂Ƃ��ǉ����ǂ����𔻒f
	//wmod 0�̂Ƃ��͐V�K(default��0�Ȃ̂Ŏw�肪�Ȃ����0)
	if(itrgt->mod == 1 && itrgt->wmod == 0){
		//�e�[�u����truncate
		truncateTable(itrgt->ifi);
	}

	//�f�[�^���������X�^�[�g
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name
			," Error [%02d]: Info data processing Start %s ",99,local_server_time(strTime));
	}

	//�f�[�^�����R�[�h�����[�v
	for(count = 0; 1 ;count++){
		char strConvTarg[CONVTARGLEN]="";
		char strConvChanged[CONVTARGLEN]="";
		int rowLength = 0;

		//dataread�X�^�[�g
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info data_read Start %s ",99,local_server_time(strTime));
		}
		//�ǂݍ���(���[�h�ɂ���ĕ���)
		//strConvTarg�̃|�C���^�Ɍ��ʂ��i�[����ċA���Ă���
		if(getTargetString(itrgt,strConvTarg,&dataLength)){
			break;
		}
		//dataread�X�^�[�g
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info data_read End %s ",99,local_server_time(strTime));
		}

		//�Ώە�����̕ϊ�(���[�h�ɂ���ĕ���)
		//�s�P�� (return ��1�ɂȂ邱�Ƃ͂Ȃ�����)
		if(cnvTargToChange(itrgt,strConvChanged,strConvTarg,&rowLength)){;
			//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			DB_Close();
			fclose(fp);
			return 1;
		}
		if(count == 0){
			//�ŏ��̍s�̓f�[�^����ۑ�
			intDefDataLen = rowLength;
		}else{
			//�덷���傫���s�������狭���I��
			if(rowLength > (intDefDataLen + TOLERANCE) ||rowLength < (intDefDataLen - TOLERANCE)){
				break;
			}
			if(strchr(strConvTarg,0x1A) != NULL){
				break;
			}
		}
		//datawrite�X�^�[�g
		if(myConfDebugFlg){
			mytool_runtime_error('I',source_file_name
				," Error [%02d]: Info outputConvData Start %s ",99,local_server_time(strTime));
		}
		//�o��(���[�h�ɂ���ĕ���)
		if(outputConvData(itrgt,strConvChanged,rowLength)){
			//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			DB_Close();
			fclose(fp);
			return 1;
		}
		//datawrite�X�^�[�g
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

		//�������[�v�Ȃ̂ŏI����݂��Ă���
		if(count > LOOP_MAX){
			break;
		}
	}

	//read�悤�ɊJ��������
	if(itrgt->mod == 0){
		//�J�����Ă����Ȃ��ƃ_��
		mysql_free_result(res);
	}

	//�I������
	//�����܂Ő���ɗ�����commit
	memset(sqlStr,'\0',strlen(sqlStr));
	strcat(sqlStr,"COMMIT; ");
	if(mysql_query(mysql, sqlStr) != 0){
		//�G���[�ɂȂ�����Ƃ肠�����AunLock���s add koyama 20160313
		setTableUnlock(targSetting, itrgt);
		//�����܂ŗ��ăG���[���o��̂͂��������̂�mysql
		mytool_runtime_error('E',source_file_name," Error C [%2d]:commit error %s %s                       %s ", 19,itrgt->ifi,mysql_error(mysql),local_server_time(strTime));
		fclose(fp);
		return 1;
	}
	//���C�������G���h
	if(myConfDebugFlg){
		mytool_runtime_error('I',source_file_name," Error [%02d]: Info dataconvert End %s ",99,local_server_time(strTime));
	}

	fclose(fp);

	//2015/08/28 kawada add S �Ǘ��e�[�u����Lock���(LockMode)��Unlock�ɍX�V
	if (setTableUnlock(targSetting, itrgt) != 0){
		return 1;
	}

	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}


//Main
//IN  �F �Ȃ�
//OUT �F �Ȃ�
//author:koyama
int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";	//���Ԃ̕�������i�[
	Trgt_inf *trgt;                         //�Ώۃe�[�u��,�Ώۃt�@�C��,���o�̓��[�h�Ȃǂ�ێ�
	DB_SETTING targSetting;

	// �ϐ�������
    memset(source_file_name, '\0', PATHNAME_SIZE);
    // �J�����g�f�B���N�g���擾
//    getcwd(source_file_name, PATHNAME_SIZE);
	getprocessName(source_file_name, PATHNAME_SIZE);

	if(db_init(&targSetting)){
		//�G���[�͂��낼��̊֐��ŏo��
		//��ł͋����I���̂�
		raise(SIGABRT);
	}

	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //�������m��

	//�������S���Ȃ��̓_��(�v���O�������͕K������)
	if(argc <= 1){
		fprintf(stderr," %s : Error C [%02d]: Option is nothing                      %s \n",source_file_name, 00,local_server_time(strTime));
		raise(SIGABRT);
	}


	//������
	trgt_ini(trgt);

	//�������g�̖��O���擾 add koyama 20160506
	if(strrchr(argv[0],'/')){
		myself = strrchr(argv[0],'/') + 1;
	}else{
		myself = argv[0];
	}

	for(ii=1;ii < argc; ii++){
		ret = set_trgt(trgt,argv[ii]);
	}


	// DB����
	ret = dataconvert(&targSetting, trgt);      //2015/08/28 kawada upd

	// �I���������������
	free(trgt);
	if(ret==1){
		//�G���[�o�͂͂����܂łɏo�͂���Ă���͂�
//		fprintf(stderr," Error C [%02d]: %s                                      %s ", 9,argv[1], local_server_time(strTime));
	}else{
		//�G���[���o���炻�̎��_�ŃR�l�N�V�����������Ă���̂�
		DB_Close();
	}

	return ret;
}
